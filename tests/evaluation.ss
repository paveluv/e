#!/usr/bin/env scheme-script

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(eval
  '(begin
     (import (prefix (apps eval) eval:) (prefix (head edit) edit:)
             (prefix (head head) head:) (prefix (head echo) echo:)
             (prefix (service log) log:) (prefix (test) test:)
             (prefix (foundation string) string:) (prefix (core kernel) kernel:))

     (define (run thunk) (eval:call-with-evaluation! "test evaluation" thunk))
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

     (define b (head:new-buffer! "evaluation"))
     (head:show-buffer! b)
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
       (list (eval:values nested) (map log:datum (log:entries 'stdout))
             (map log:datum (log:entries 'stderr)) (edit:buffer-text b))
       '((42) ("inner" "outer") ("warning") "outerinner\n"))
     (edit:undo!)
     (test:check 'nested-edits-share-one-undo (edit:buffer-text b) "\n")

     (define handler (keyboard-interrupt-handler))
     (define descriptors (test:fd-count))
     (test:check 'escape-cleans-up-without-fabricating-a-result
       (call/cc (lambda (leave) (run (lambda () (leave 'escaped))))) 'escaped)
     (test:check 'capture-and-interrupt-state-restored
       (list (eq? handler (keyboard-interrupt-handler)) (= descriptors (test:fd-count))
             (eval:values (run (lambda () 7)))) '(#t #t (7)))

     (eval:report! nested 'probe)
     (test:check 'extension-report-records-under-its-component-without-mx-history
       (list (log:history 'eval car) (head:copy-text) (map log:datum (log:entries 'probe))) '(() "42" ("42")))
     (test:check 'a-report-needs-a-destination (test:raises? (lambda () (eval:report! nested))) #t)
     (eval:report! (run (lambda () #f)) "#f")
     (test:check 'explicit-input-records-the-exchange
       (map log:datum (log:entries 'eval)) '(("#f" . "#f")))
     (echo:set-text! "before")
     (define spoken (run (lambda () (echo:set-text! "command message") (void))))
     (eval:report! spoken 'probe)
     (test:check 'void-report-preserves-the-command-message (echo:text) "command message")
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
                       (case (log:component e)
                         [(compile) (set! shown (cons presentation shown))]
                         [(stdout) (set! printed (cons (cons (log:datum e) presentation) printed))])))
     (run (lambda () (display "haha")))
     (test:check 'ordinary-output-still-reaches-the-echo-area printed '(("haha" . append)))
     (define stdout-before (length (log:entries 'stdout)))
     (define-values (compiled failed)
       (parameterize ([library-directories (cons (cons root root) (library-directories))]
                      [compile-imported-libraries #t])
         (values (run (lambda () (eval '(begin (import (probe fresh)) fresh) (interaction-environment))))
                 (run (lambda () (eval '(import (probe broken)) (interaction-environment)))))))
     (test:check 'a-library-compiled-on-import-is-a-compile-record-shown-nowhere
       (list (eval:values compiled) (map log:datum (log:entries 'compile)) shown
             (- (length (log:entries 'stdout)) stdout-before))
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
     (test:check 'a-failed-evaluations-message-is-error-text-not-scheme
       (let* ([text "(help) => error: Exception: variable help is not bound"]
              [styles ((log:styler 'eval) text)]
              [at (string:search text "not" 0 (string-length text))])
         (list (vector-ref styles at) (vector-ref styles (- (string-length text) 1)) (not (eq? (vector-ref styles 1) 'error))))
       '(error error #t))
     (test:finish! 'evaluation)))
