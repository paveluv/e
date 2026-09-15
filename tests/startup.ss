#!/usr/bin/env scheme-script

;; Early options and first head initialization need no terminal. Reinvoke
;; this fixture for each claim: an R6RS library initializes once per image.
(import (chezscheme))
(include "tests/roots.ss")
(define roots-runtime
  (and (pair? (command-line-arguments))
       (case (string->symbol (car (command-line-arguments)))
         [(roots-base) 'base] [(roots-client) 'client] [else #f])))
(test-roots! (or roots-runtime 'base))

;; Resolve real libraries before any head is imported; metadata is enough
;; to distinguish the owner and attached implementations without a socket.
(when roots-runtime
  (eval
    `(begin
       (import (prefix (store) store:) (prefix (kernel) kernel:))
       (unless
         (and (string=? (kernel:installation-directory) (current-directory))
              (string=? (kernel:module-source "store")
                        (format "~a/lib/~a/state/store.sls" (current-directory) ',roots-runtime))
              (for-all (lambda (root)
                         (string=? (cdr root) (format "~a/eo/~a" (current-directory) ',roots-runtime)))
                       (library-directories))
              (equal? (and (memq 'publish! (library-exports '(store))) #t)
                      ,(eq? roots-runtime 'base)))
         (error 'startup "test roots selected the wrong runtime" ',roots-runtime))))
  (exit 0))

(eval
  '(begin
     (import (prefix (startup) startup:) (prefix (daemon) daemon:) (prefix (actor) actor:)
             (prefix (store) store:) (prefix (kernel) kernel:)
             (prefix (string) string:) (prefix (sys) sys:)
             (prefix (test) test:))

     (define (options) (list (startup:name) (startup:file)))
     (define (contains? text needle)
       (and (string:search text needle 0 (string-length text)) #t))
     (define (start-head)
       ;; Chez invokes libraries lazily; a reference must stay in the scope.
       (eval '(begin (import (prefix (head) head:)) head:ui-actor)))

     ;; One lifecycle driver for both streams, including a caller-owned port
     ;; that must remain usable until every callback and capture reader ends.
     (define (capture-case exit failing)
       (let* ([entered (list (test:gate) (test:gate))] [release (test:gate)]
              [leaving (test:gate)] [returned (test:gate)] [records (test:recorder)]
              [destination (open-output-string)] [held '()] [expired #f]
              [before (test:fd-count)]
              [tail (if failing (make-string 131072 #\x) "tail λ")])
         (define (emit label ready)
           (lambda (line)
             (when (string=? line "live")
               (ready #t)
               (test:await 'release release)
               ;; Joining readers must still let their collection complete.
               (when (eq? label 'out) (collect-rendezvous)))
             (records (list label line (not (port-closed? destination))))
             (when (eq? label failing) (raise #f))))
         (let ([join
                (test:worker
                  (lambda ()
                    (let* ([out (current-output-port)] [err (current-error-port)]
                           [result
                            (guard (ex [(eq? ex 'capture-error) 'raised] [(eq? ex #f) 'callback-error])
                              (dynamic-wind void
                                (lambda ()
                                  ((make-engine
                                     (lambda ()
                                       (sys:call-with-streamed-output
                                         (emit 'out (car entered)) (emit 'err (cadr entered))
                                         (lambda ()
                                           (set! held (list (current-output-port) (current-error-port)))
                                           (for-each (lambda (port) (put-string port "live\n") (flush-output-port port)) held)
                                           (test:await 'entered (lambda () (for-all (lambda (ready) (ready)) entered)))
                                           (leaving #t)
                                           (for-each (lambda (port) (put-string port tail)) held)
                                           (case exit
                                             [(return) (values 'returned 'values)]
                                             [(raise) (raise 'capture-error)]
                                             [(close) (for-each close-port held) (raise 'capture-error)]
                                             [(fuel) (engine-block)])))))
                                   1000000 (lambda (ticks . results) results)
                                   (lambda (engine) (set! expired engine) 'expired)))
                                (lambda () (close-port destination))))]
                           [reentry-refused?
                            (or (not expired)
                                (test:raises?
                                  (lambda () (expired 1000000 (lambda args (void)) (lambda args (void))))
                                  (lambda (ex)
                                    (and (who-condition? ex) (eq? (condition-who ex) 'call-with-streamed-output)))))])
                      (returned
                        (list result (for-all port-closed? held) reentry-refused?
                              (and (eq? out (current-output-port)) (eq? err (current-error-port))))))))])
           (test:await 'leaving leaving)
           (sleep (make-time 'time-duration 100000000 0))
           (let ([returned-early? (and (returned) #t)])
             (release #t)
             (join)
             (list (list exit failing) (returned) returned-early?
                   (for-all caddr (records))
                   (map (lambda (label)
                          (equal? (map cadr (filter (lambda (record) (eq? (car record) label)) (records)))
                                  (if (eq? label failing) '("live") (list "live" tail))))
                        '(out err))
                   (equal? before (test:fd-count)))))))

     (define (claim-fixture kind)
       (let* ([explicit? (memq kind '(named conflict))]
              [seed (if explicit? "writing desk λ" (startup:default-name))]
              [args (if explicit? (list "--name" seed) '())]
              [occupied (case kind
                          [(conflict) (list seed)]
                          [(suffix) (list seed (string-append seed " 2"))]
                          [else '()])]
              [expected (list 'head (if (eq? kind 'suffix) (string-append seed " 3") seed))]
              [events (test:recorder)]
              [registered-at-creation? #f]
              [abandon (condition (make-error) (make-message-condition "abandon initializer"))])
         (for-each (lambda (name) (actor:register! (list 'head name) void)) occupied)
         ;; The base can predate this head. Initial discovery must honor
         ;; audience too; its inventory overlaps writes during the claim.
         (unless (eq? kind 'conflict)
           (for-each
             (lambda (entry)
               (store:create! '(agent startup) (car entry) '("already here")
                              (list (cons 'audience (cadr entry)))))
             (list (list "public before import" 'all)
                   (list "private before import" (list expected))
                   (list "hidden before import" '((head "elsewhere"))))))
         (store:subscribe! #f
           (lambda (event)
             (events event)
             (when (and (eq? (car event) 'create) (string=? (caddr event) "*scratch*"))
               (set! registered-at-creation? (actor:registered? (list-ref event 3))))))
         (when (eq? kind 'named)
           (actor:subscribe!
             (lambda (batch)
               (when (member (list 'attached expected) batch)
                 (store:create! '(agent startup) "during claim" '("already listening"))))))
         (if (eq? kind 'conflict)
             (let ([before (actor:attached)])
               (test:check 'explicit-collision-is-inert
                 (list
                   (guard (ex [else (contains? (kernel:condition-text ex) "head name already in use")])
                     (startup:call-with-options args start-head))
                   (equal? before (actor:attached)) (store:buffer-list) (events))
                 '(#t #t () ())))
             (begin
               (test:check 'head-survives-the-first-importers-failure
                 (test:raises?
                   (lambda ()
                     (parameterize ([kernel:registering-module 'startup-fixture])
                       (kernel:call-with-registration-update
                         (lambda ()
                           (startup:call-with-options args start-head)
                           (raise abandon)))))
                   (lambda (ex) (eq? ex abandon))) #t)
               (test:check 'named-before-shared-state
                 (list (eval 'head:ui-actor) registered-at-creation?
                       (map (lambda (event) (list-ref event 3))
                            (filter (lambda (event) (and (eq? (car event) 'create)
                                                         (string=? (caddr event) "*scratch*")))
                                    (events)))
                       (list-ref (actor:describe expected) 4)
                       (actor:send! expected 'wake))
                 (list expected #t (list expected) 'all #t))
               (store:create! '(agent startup) "after import" '("alive"))
               (eval '(head:before-frame!))
               (test:check 'root-subscription-and-marks-stay-live
                 (list (eval '(map head:buffer-name (head:buffers)))
                       (store:mark expected (store:find-named "*scratch*") 'point))
                 (list (if (eq? kind 'named)
                           '("*scratch*" "public before import" "private before import"
                             "during claim" "after import")
                           '("*scratch*" "public before import" "private before import" "after import"))
                       '(0 . 0)))))))

     (define (suite)
       (test:check 'capture-lifetime
         (map (lambda (row) (apply capture-case row))
              '((return #f) (raise #f) (fuel #f) (close #f) (return out) (raise err)))
         '(((return #f) ((returned values) #t #t #t) #f #t (#t #t) #t)
           ((raise #f) (raised #t #t #t) #f #t (#t #t) #t)
           ((fuel #f) (expired #t #t #t) #f #t (#t #t) #t)
           ((close #f) (raised #t #t #t) #f #t (#t #t) #t)
           ((return out) (callback-error #t #t #t) #f #t (#t #t) #t)
           ((raise err) (raised #t #t #t) #f #t (#t #t) #t)))
       (for-each
         (lambda (entry)
           (test:check (car entry)
             (startup:call-with-options (car entry) options)
             (list (caadr entry)
               (and (cadadr entry) (string-append (current-directory) "/" (cadadr entry))))))
         '((() (#f #f))
           (("notes") (#f "notes"))
           (("--name" "writing desk λ" "notes") ("writing desk λ" "notes"))
           (("notes" "--name=desk") ("desk" "notes"))
           (("--name" "desk" "--" "-notes") ("desk" "-notes"))
           (("--" "--help") (#f "--help"))))
       (test:check 'invalid-options-never-initialize
         (map (lambda (args)
                (guard (ex [(error? ex) 'rejected])
                  (startup:call-with-options args (lambda () 'initialized))))
              '(("--name") ("--name" "") ("--name=")
                ("--name=a" "--name" "b") ("--bogus") ("one" "two")
                ("--base" "--base") ("--base" "--name=desk") ("--base" "notes")
                ("--base-working-dir") ("--base-working-dir=") ("--base-working-dir" "")
                ("--base-working-dir=x" "--base-working-dir=y")
                ("--daemon") ("--attach") ("--socket=x")
                ("--force") ("--base" "--restart") ("--restart" "--restart")
                ("--restart" "--force" "--force")))
         (make-list 20 'rejected))
       (test:check 'restart-is-a-pre-attachment-option
         (list (startup:call-with-options '("--force" "--restart" "--name=desk" "notes")
                 (lambda () (list (startup:mode) (startup:restart?) (startup:force?) (startup:name))))
               (startup:restart?) (startup:force?))
         '((head #t #t "desk") #f #f))
       (test:check 'base-options-are-scoped-and-own-the-directory
         (let ([path (string-copy "/tmp/base λ")])
           (list
             (startup:call-with-options (list "--base-working-dir" path "--base")
               (lambda ()
                 (string-set! path 0 #\X)
                 (string-set! (startup:base-working-directory) 0 #\Y)
                 (list (startup:mode) (startup:base-working-directory) (options)
                       (startup:call-with-options '("--base-working-dir=another" "--name=desk" "notes")
                         (lambda () (list (startup:mode) (startup:base-working-directory) (options)))))))
             (startup:mode)))
         `((base "/tmp/base λ" (#f #f) (head ,(string-append (current-directory) "/another")
                                             ("desk" ,(string-append (current-directory) "/notes")))) head))
       ;; Installations never share a default base; normalize directory aliases.
       (test:check 'default-base-lives-in-the-installation-and-is-omitted-from-commands
         (parameterize ([kernel:installation-directory "/tmp/e-install λ/unused/.."])
           (map (lambda (args)
                  (startup:call-with-options args
                    (lambda ()
                      (list (startup:base-working-directory)
                            (contains? (daemon:head-command (startup:name) (startup:restart?)) "--base-working-dir")))))
                '(("--base") ("--name=desk")
                  ("--base-working-dir=/tmp/e-install λ/.base")
                  ("--restart" "--name=desk" "--base-working-dir=/tmp/e-install λ/unused/../.base/")
                  ("--base-working-dir=/tmp/another base"))))
         (append (make-list 4 '("/tmp/e-install λ/.base" #f)) '(("/tmp/another base" #t))))
       (for-each
         (lambda (flag)
           (test:check (list 'help flag)
             (list (startup:call-with-options (list flag "--restart" "--force" "--name=desk"
                                                    "--base-working-dir=/tmp/help base")
                     (lambda () (list (startup:mode) (startup:name) (startup:base-working-directory))))
                   (startup:mode))
             '((help "desk" "/tmp/help base") head)))
         '("-h" "--help"))
       (let ([name (string-copy "desk")] [file (string-copy "notes")])
         (test:check 'scoped-options-own-their-input-and-results
           (list
             (startup:call-with-options (list "--name" name file)
               (lambda ()
                 (string-set! name 0 #\X) (string-set! file 0 #\X)
                 (string-set! (startup:name) 0 #\Y) (string-set! (startup:file) 0 #\Y)
                 (list (startup:call-with-options '("inner") options) (options))))
             (options))
           `(((#f ,(string-append (current-directory) "/inner"))
              ("desk" ,(string-append (current-directory) "/notes"))) (#f #f))))
       ;; Real loader help/errors run without a terminal and do not get as
       ;; far as main's terminal check. Capture both process output streams.
       (for-each
         (lambda (entry)
           (let* ([out (test:recorder)] [err (test:recorder)]
                  [status (sys:call-with-streamed-output out err
                            (lambda () (system (car entry))))])
             (test:check (car entry)
               (list (zero? status)
                     (contains? (string:join (append (out) (err)) "\n") (caddr entry)))
               (list (cadr entry) #t))))
         `((,(format "./e --help --base-working-dir /tmp/e-help-missing-~a" (get-process-id)) #t "Usage: e")
           ("./e --name=''" #f "nonempty name")
           ("./e one two" #f "at most one file")))
       (for-each
         (lambda (kind)
           (test:check (list 'fresh-process kind)
             (system (format "scheme-script tests/startup.ss ~a" kind)) 0))
         '(named default suffix conflict roots-base roots-client))
       (test:finish! 'startup))

     (if (null? (command-line-arguments))
         (suite)
         (claim-fixture (string->symbol (car (command-line-arguments)))))))
