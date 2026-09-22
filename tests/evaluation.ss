#!/usr/bin/env scheme-script

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(eval
  '(begin
     (import (prefix (apps eval) eval:) (prefix (head edit) edit:)
             (prefix (head head) head:) (prefix (head echo) echo:)
             (prefix (service log) log:) (prefix (test) test:))

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
       (list (log:history 'eval car) (head:copy-buffer) (map log:datum (log:entries 'probe))) '(() "42" ("42")))
     (test:check 'a-report-needs-a-destination (test:raises? (lambda () (eval:report! nested))) #t)
     (eval:report! (run (lambda () #f)) "#f")
     (test:check 'explicit-input-records-the-exchange
       (map log:datum (log:entries 'eval)) '(("#f" . "#f")))
     (echo:set-text! "before")
     (define spoken (run (lambda () (echo:set-text! "command message") (void))))
     (eval:report! spoken 'probe)
     (test:check 'void-report-preserves-the-command-message (echo:text) "command message")
     (test:finish! 'evaluation)))
