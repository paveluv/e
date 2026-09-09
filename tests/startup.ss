#!/usr/bin/env scheme-script

;; Early options and first head initialization need no terminal. Reinvoke
;; this fixture for each claim: an R6RS library initializes once per image.
(import (chezscheme))
(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (startup) startup:) (prefix (actor) actor:)
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
             (startup:call-with-options (car entry) options) (cadr entry)))
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
                ("--daemon" "--daemon") ("--daemon" "--name=desk") ("--daemon" "notes")
                ("--socket=x") ("--daemon" "--socket") ("--daemon" "--socket=")
                ("--daemon" "--socket=x" "--socket=y")
                ("--attach" "--attach") ("--daemon" "--attach") ("--attach" "--daemon")))
         (make-list 16 'rejected))
       (test:check 'daemon-options-are-scoped-and-own-the-socket-path
         (let ([path (string-copy "/tmp/base λ")])
           (list
             (startup:call-with-options (list "--socket" path "--daemon")
               (lambda ()
                 (string-set! path 0 #\X)
                 (string-set! (startup:socket) 0 #\Y)
                 (list (startup:mode) (startup:socket) (options)
                       (startup:call-with-options '("--attach" "--socket=another" "--name=desk" "notes")
                         (lambda () (list (startup:mode) (startup:socket) (options)))))))
             (startup:mode)))
         '((daemon "/tmp/base λ" (#f #f) (attach "another" ("desk" "notes"))) standalone))
       (for-each
         (lambda (flag)
           (test:check (list 'help flag)
             (let ([initialized? #f])
               (let ([output (call-with-string-output-port
                               (lambda (port)
                                 (parameterize ([current-output-port port])
                                   (startup:call-with-options (list flag)
                                     (lambda () (set! initialized? #t))))))])
                 (list initialized? (contains? output "--name NAME"))))
             '(#f #t)))
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
           '(((#f "inner") ("desk" "notes")) (#f #f))))
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
         '(("./e --help" #t "Usage: e")
           ("./e --name=''" #f "nonempty name")
           ("./e one two" #f "at most one file")))
       (for-each
         (lambda (kind)
           (test:check (list 'fresh-head kind)
             (system (format "scheme-script tests/startup.ss ~a" kind)) 0))
         '(named default suffix conflict))
       (test:finish! 'startup))

     (if (null? (command-line-arguments))
         (suite)
         (claim-fixture (string->symbol (car (command-line-arguments)))))))
