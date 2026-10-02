#!/usr/bin/env scheme-script

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)
(eval
  '(begin
     (import (prefix (apps eval) eval:) (prefix (head edit) edit:)
             (prefix (head head) head:) (prefix (head seat) seat:) (prefix (head window-host) window-host:) (prefix (head widget) widget:) (prefix (head echo) echo:)
             (prefix (service log) log:) (prefix (test) test:)
             (prefix (head suspension) suspension:) (prefix (head text-source) text-source:)
             (prefix (state store) store:) (prefix (state view) view:) (prefix (state model) model:)
             (prefix (foundation text) text:)
             (prefix (foundation string) string:) (prefix (core kernel) kernel:))

     (widget:init!) (edit:init!) (window-host:init!)
     (define (run thunk) (eval:call-with-evaluation! "test evaluation" thunk))
     (define (output channel)
       (map cdr (filter (lambda (d) (eq? (car d) channel))
                  (map log:datum (log:entries 'eval:call-with-evaluation!)))))
     (test:check 'values-are-retained-without-implicit-reporting
       (map (lambda (vals)
              (let ([result (run (lambda () (apply values vals)))])
                (list (eval:status result) (eval:values result) (eval:condition result))))
         (list '() '(#f) '(1 "two") (list (void))))
       (map (lambda (vals) (list 'ok vals #f)) (list '() '(#f) '(1 "two") (list (void)))))
     (define ex (condition (make-error) (make-message-condition "probe")))
     (test:check 'conditions-retain-the-original-and-distinguish-interruption
       (let ([failed (run (lambda () (raise ex)))]
             [stopped (run (lambda () ((keyboard-interrupt-handler))))])
         (list (eval:status failed) (eq? ex (eval:condition failed))
               (eval:status stopped) (head:interrupted? (eval:condition stopped))))
       '(error #t interrupted #t))

     (define b (seat:new-buffer! "evaluation"))
     (seat:show-buffer-mirror! b)
     (define nested
       (run (lambda ()
              (edit:insert-text! "outer")
              (display "outer\n")
              (let ([inner (run (lambda ()
                                  (edit:insert-text! "inner")
                                  (display "inner\n")
                                  (display "warning" (current-error-port))
                                  42))])
                (car (eval:values inner))))))
     (test:check 'nested-evaluation-streams-each-line-once
       (list (eval:values nested) (output 'stdout)
             (output 'stderr) (edit:buffer-text (seat:buffer-store-id b)))
       '((42) ("inner" "outer") ("warning") "outerinner\n"))
     (edit:undo!)
     (test:check 'nested-edits-share-one-undo (edit:buffer-text (seat:buffer-store-id b)) "\n")

     (define handler (keyboard-interrupt-handler))
     (define descriptors (test:fd-count))
     (test:check 'escape-cleans-up-without-fabricating-a-result
       (call/cc (lambda (leave) (run (lambda () (leave 'escaped))))) 'escaped)
     (test:check 'capture-and-interrupt-state-restored
       (list (eq? handler (keyboard-interrupt-handler)) (= descriptors (test:fd-count))
             (eval:values (run (lambda () 7)))) '(#t #t (7)))

     ;; A parked evaluation owns no capture descriptors or undo batch. Other
     ;; commands can edit, then the resumed segment gets its own undo step.
     (define ticket #f)
     (define result #f)
     (define batch #f)
     (suspension:call! head:ui-actor void
       (lambda ()
         (set! result
           (run (lambda ()
                  (set! batch (text-source:current-batch head:ui-actor))
                  (edit:insert-text! "before") (display "before pause\n")
                  (suspension:wait! (lambda (t) (set! ticket t) void))
                  (display "after pause" (current-error-port))
                  (edit:insert-text! "after")
                  (not (equal? batch (text-source:current-batch head:ui-actor))))))))
     (test:check 'suspended-evaluation-releases-process-state
       (list result (text-source:current-batch head:ui-actor)
         (= descriptors (test:fd-count)) (eq? handler (keyboard-interrupt-handler))) '(#f #f #t #t))
     (edit:insert-text! "other")
     (suspension:resolve! ticket #t) (suspension:drain! raise)
     (test:check 'evaluation-resumes-with-new-capture-and-batch
       (list (eval:status result) (eval:values result) (car (output 'stdout)) (car (output 'stderr))
         (= descriptors (test:fd-count))) '(ok (#t) "before pause" "after pause" #t))
     (edit:undo!)
     (test:check 'resumed-evaluation-keeps-intervening-edit-undo-separate (edit:buffer-text (seat:buffer-store-id b)) "beforeother\n")
     (edit:undo!) (edit:undo!)

     (eval:report! nested 'probe)
     (test:check 'extension-label-is-data-without-mx-history
       (list (log:history 'eval:report! car) (edit:copy-text) (map log:datum (log:entries 'eval:report!)))
       '(() "42" ((probe . "42"))))
     (test:check 'a-report-needs-a-destination (test:raises? (lambda () (eval:report! nested))) #t)
     (eval:report! (run (lambda () #f)) "#f")
     (test:check 'explicit-input-records-the-exchange
       (list (log:history 'eval:report! car) (log:datum (car (log:entries 'eval:report!))))
       '(("#f") ("#f" . "#f")))
     (echo:set-text! "before")
     (define spoken (run (lambda () (echo:set-text! "command message") (void))))
     (eval:report! spoken 'probe)
     (test:check 'void-report-preserves-the-command-message (echo:text) "command message")
     (let ([values-to-copy '((app describe) ((model 17) (model 18)) #((agent helper) "a\"b"))])
       (eval:report! (run (lambda () (apply values values-to-copy))) 'probe)
       (test:check 'copied-multiple-values-are-an-executable-expression
         (call-with-values (lambda () (eval (read (open-input-string (edit:copy-text))))) list) values-to-copy)
       (let ([copied (edit:copy-text)])
         (eval:report! (run (lambda () (list (current-output-port)))) 'probe)
         (test:check 'opaque-result-does-not-overwrite-a-usable-copy (edit:copy-text) copied)))
     ;; Explicit editor commands use one coherent source and selection without
     ;; adopting a legacy mirror or borrowing the focused editor's point.
     (let* ([source (store:create! head:ui-actor "evaluation receiver" '("(+ 1 2)" "(* 3 4)"))]
            [other (store:create! head:ui-actor "evaluation bystander" '("999"))]
            [root (view:create! head:ui-actor #f 'row 1 '() '())]
            [a (edit:create-view! head:ui-actor source '() root)]
            [b (edit:create-view! head:ui-actor other '() root)])
       (define (show!) (widget:pump!) (widget:present! (list (list (widget:prepare! root 40 3) 0 0))))
       (define (result command)
         (parameterize ([eval:copy-result #f]) (command a))
         (cdr (log:datum (car (log:entries 'eval:report!)))))
       (view:arrange! head:ui-actor (list (list root 0 (list (list 'a a '(grow 1)) (list 'b b '(grow 1))) '())) '())
       (widget:mount! root 'evaluation-fixture) (show!) (widget:focus! root b)
       (edit:select! a '(0 . 7) '(0 . 0))
       (test:check 'explicit-evaluation-uses-selected-text-in-either-direction
         (list (result eval:run!) (begin (edit:select! a '(0 . 0) '(0 . 7)) (result eval:run!))) '("3" "3"))
       (edit:select! a '(0 . 7) '(0 . 7))
       (test:check 'explicit-expression-and-buffer-evaluation-share-captured-receiver
         (list (result eval:last-expression!) (result eval:top-level-form!) (result eval:run!)
           (widget:focused root) (seat:buffer-of-store-id source)) (list "3" "3" "12" b #f))
       (store:edit! head:ui-actor source (store:revision source) (text:make-span 0 0 0 0) '("000 "))
       (text-source:open! head:ui-actor source)
       (test:check 'evaluation-reads-text-at-the-captured-selection-revision (result eval:last-expression!) "3")
       (widget:unmount! root)
       (test:check 'unmounted-evaluation-refuses (test:raises? (lambda () (eval:run! a))) #t)
       (view:retire! head:ui-actor root (model:revision root)))

     ;; A library compiled on import announces itself as a compile record for
     ;; the log alone; a broken one fails the evaluation, which the echo shows.
     (define root (format "/tmp/e-eval-compile-~a-~a" (get-process-id) (random 1000000)))
     (define (library! name text)
       (call-with-output-file (string-append root "/probe/" name ".sls") (lambda (p) (display text p))))
     (mkdir root) (mkdir (string-append root "/probe"))
     (library! "fresh" "(library (probe fresh) (export fresh) (import (rnrs)) (define fresh 'compiled))")
     (library! "broken" "(library (probe broken) (export) (import (rnrs)) (define))")
     (define shown '())
     (define printed '())
     (log:subscribe! (lambda (e presentation)
                       (when (eq? (log:component e) 'eval:call-with-evaluation!)
                         (case (car (log:datum e))
                           [(compile) (set! shown (cons presentation shown))]
                           [(stdout) (set! printed (cons (cons (cdr (log:datum e)) presentation) printed))]))))
     (run (lambda () (display "haha")))
     (test:check 'ordinary-output-still-reaches-the-echo-area printed '(("haha" . append)))
     (define stdout-before (length (output 'stdout)))
     (define-values (compiled failed)
       (parameterize ([library-directories (cons (cons root root) (library-directories))]
                      [compile-imported-libraries #t])
         (values (run (lambda () (eval '(begin (import (probe fresh)) fresh) (interaction-environment))))
                 (run (lambda () (eval '(import (probe broken)) (interaction-environment)))))))
     (test:check 'a-library-compiled-on-import-is-a-compile-record-shown-nowhere
       (list (eval:values compiled) (output 'compile) shown
             (- (length (output 'stdout)) stdout-before))
       (list '(compiled) (list (string-append root "/probe/fresh.sls") (string-append root "/probe/broken.sls")) '(#f #f) 0))
     (test:check 'a-failed-compilation-is-the-evaluation-error-naming-the-source
       (list (eval:status failed)
             (and (string:search (kernel:condition-text (eval:condition failed)) "broken.sls" 0
                                 (string-length (kernel:condition-text (eval:condition failed)))) #t))
       '(error #t))
     (for-each (lambda (name) (delete-file (string-append root "/probe/" name)))
               (list "fresh.sls" "fresh.so" "broken.sls"))
     (delete-directory (string-append root "/probe")) (delete-directory root)

     ;; a failed evaluation's message styles as plain error text, not Scheme;
     ;; the styler is registered by the app's install
     (eval:init!)
     (test:check 'evaluation-formatters-retain-labels-and-channels
       (map log:format-entry
         '((0 #f eval:report! (probe . "42"))
           (0 #f eval:call-with-evaluation! (stderr . "warning"))))
       '("probe => 42" "[stderr] warning"))
     (test:check 'a-failed-evaluations-message-is-error-text-not-scheme
       (let* ([text "(help) => error: Exception: variable help is not bound"]
              [styles ((log:styler 'eval:report!) text)]
              [at (string:search text "not" 0 (string-length text))])
         (list (vector-ref styles at) (vector-ref styles (- (string-length text) 1)) (not (eq? (vector-ref styles 1) 'error))))
       '(error error #t))
     (test:finish! 'evaluation)))
