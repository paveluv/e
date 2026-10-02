;; Reuse the cold-launch fixture's blank head and base, including their pump.
(define (repl-scenarios! b)
  (copy-text "examples/repl.e" (string-append root "/repl.e"))
  (let ([a (start-command '("--name" "repl" "--start" "repl.e") 80)])
    (head-wait 'windowless-startup a (lambda () (head-sees? a "Scheme:")))
    (let* ([resources (head-read a
                        '(let* ([root (test-root)] [d (interaction:snapshot root)] [options (view:options d)])
                           (list root (view:source d) (cdr (assq 'draft options)) (cdr (assq 'history options)))))]
           [environment (cadr resources)] [draft (caddr resources)] [history (cadddr resources)]
           [gate (string-append root "/repl-gate")])
      (unless (zero? ((foreign-procedure "mkfifo" (string unsigned) int) gate #o600))
        (error 'fixture "cannot create evaluation gate"))
      (head-read a
        `(begin (store:reset! head:ui-actor ',draft
                  (list ,(format "(define shared 41) (display \"working\") (flush-output-port) (call-with-input-file ~s read) (+ shared 1)" gate))) #t))
      (head-send! a "\r")
      (head-wait 'repl-job-started a (lambda () (head-sees? a "working")))
      (head-read a `(begin (store:reset! head:ui-actor ',draft '("unfinished draft")) #t))
      (head-send! a "\x18;\x03;")
      (head-wait 'repl-detaches-while-job-runs a (lambda () (head-sees? a "e: detached")))
      (sys:reap-terminal-process! (vector-ref a 0))
      (call-with-port (open-file-output-port gate (file-options no-fail no-truncate) (buffer-mode block) (native-transcoder))
        (lambda (p) (write 'continue p)))
      (head-read b `(begin (load ,(string-append root "/repl.e")) #t))
      (head-read b `(let-values ([(status binding) (root:install! (root:current) (repl:create! ',environment) 'retire)]) status))
      (head-wait 'second-head-shares-explicit-environment b (lambda () (head-sees? b "Scheme:")))
      (head-read b
        '(begin (store:reset! head:ui-actor (repl:get (view:options (interaction:snapshot (test-root))) 'draft)
                  '("(+ shared 1)")) #t))
      (head-send! b "\r")
      (head-wait 'shared-definitions-work-on-another-head b (lambda () (head-sees? b "=> (42)")))
      (set! a (start-command '("--name" "repl" "--start" "repl.e") 100))
      (head-wait 'repl-resumes-private-draft a (lambda () (head-sees? a "unfinished draft")))
      (test:check 'windowless-repl-keeps-job-and-private-draft-across-detach
        (head-read a
          `(let* ([h (repl:record ',history)] [item (repl:record (repl:get (repl:get h 'value) 'tail))]
                  [job (repl:record (caddr (repl:get (repl:get item 'value) 'recipe)))])
             (list (equal? (test-root) ',(car resources)) (repl:get (repl:get h 'value) 'count)
               (repl:get (repl:get job 'value) 'result)
               (store:line ',draft 0) (store:property ',draft 'audience)
               (exists (lambda (row) (eq? (view:kind (cdr row)) 'window-manager)) (view:tree (test-root))))))
        '(#t 1 (value (42) "(42)") "unfinished draft" ((head "repl")) #f))
      (head-read b '(begin (head:quit!) #t))
      (head-wait 'shared-repl-detaches b (lambda () (head-sees? b "e: detached")))
      (sys:reap-terminal-process! (vector-ref b 0))
      (evaluator-close!)
      (let ([replacement (start-command '("--restart" "--name" "repl" "--start" "repl.e") 100)])
        (head-wait 'repl-restart-review replacement
          (lambda () (> (occurrences (vector-ref replacement 3) "Restart anyway?") 0)))
        (head-send! replacement "yes\n")
        (head-wait 'repl-restarts-with-authored-draft replacement
          (lambda () (head-sees? replacement "unfinished draft")))
        (sys:reap-terminal-process! (vector-ref a 0))
        (set! a replacement))
      (test:check 'repl-base-restart-preserves-history-and-fences-native-namespace
        (head-read a
          `(list (equal? (test-root) ',(car resources)) (store:line ',draft 0)
             (repl:get (repl:get (repl:record ',history) 'value) 'count)
             (> (repl:get (repl:get (repl:record ',environment) 'value) 'generation) 1)))
        '(#t "unfinished draft" 1 #t))
      ;; The explicit head action replaces the entire root with a plain manager.
      (head-read a '(begin (repl:edit! (test-root)) #t))
      (head-wait 'repl-replaces-root-with-editor a
        (lambda () (head-read a '(eq? (view:kind (interaction:snapshot (test-root))) 'window-manager))))
      (test:check 'replacement-preserves-borrowed-repl-work
        (head-read a
          `(list (window:document (test-root) (window:current (test-root)))
             (and (repl:record ',environment) #t) (and (repl:record ',history) #t)))
        (list draft #t #t))
      (head-read a '(begin (head:quit!) #t))
      (head-wait 'replacement-root-detaches a (lambda () (head-sees? a "e: detached")))
      (sys:reap-terminal-process! (vector-ref a 0)))))
