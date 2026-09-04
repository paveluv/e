#!/usr/bin/env scheme-script

;; The structured log: appending, queries, per-component formatters,
;; histories, and the presenter hook.  Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (log) log:)
             (only (chezscheme) box unbox set-box! format))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'log-test label actual expected)))

     ;; -- appending and reading back ------------------------------------

     (define base (log:length))
     (define entry (log:add! 'probe "hello" #f))

     (check 'entry-shape
            (list (cadr entry) (caddr entry)) '(probe "hello"))
     (check 'length-grew (log:length) (+ base 1))
     (check 'record-by-index (log:record base) entry)

     (log:add! 'other '(a . b) #f)
     (log:add! 'probe "again" #f)

     (check 'entries-newest-first
            (map caddr (log:entries 'probe)) '("again" "hello"))
     (check 'entries-filter
            (map cadr (log:entries 'other)) '(other))

     ;; growth across the initial vector
     (do ([i 0 (+ i 1)]) ((= i 100))
       (log:add! 'bulk (format "line ~a" i) #f))
     (check 'store-grows
            (caddr (log:record (- (log:length) 1))) "line 99")

     ;; -- formatters and stylers -----------------------------------------

     (check 'string-datum-verbatim
            (log:format-entry entry) "hello")
     (check 'other-datum-written
            (log:format-entry (log:add! 'raw '(1 2) #f)) "(1 2)")

     (define (styler text) 'styles)
     (log:register-formatter!
       'probe (lambda (d) (string-append "P: " d)) styler)
     (check 'formatter-applies (log:format-entry entry) "P: hello")
     (check 'styler-retrievable (log:styler 'probe) styler)
     (check 'no-styler (log:styler 'other) #f)

     ;; a formatter that raises never loses the record's text
     (log:register-formatter! 'bad (lambda (d) (car d)))
     (check 'formatter-failure-falls-back
            (log:format-entry (log:add! 'bad "not-a-pair" #f))
            "\"not-a-pair\"")

     ;; -- histories --------------------------------------------------------

     (log:add! 'eval-like '("(+ 1 2)" . 3) #f)
     (log:add! 'eval-like '("(+ 1 2)" . 3) #f)
     (log:add! 'eval-like '("(car x)" . err) #f)
     (check 'history-selects-and-collapses
            (log:history 'eval-like car)
            '("(car x)" "(+ 1 2)"))

     ;; -- the presenter hook ------------------------------------------------

     (define presented (box '()))
     (log:set-presenter!
       (lambda (e show?)
         (set-box! presented
                   (cons (list (cadr e) show?) (unbox presented)))))
     (log:add! 'probe "loud")
     (log:add! 'probe "quiet" #f)
     (check 'presenter-hears-both-with-show-flags
            (reverse (unbox presented))
            '((probe #t) (probe #f)))

     ;; a failing presenter never loses the record
     (log:set-presenter! (lambda (e show?) (error 'presenter "boom")))
     (check 'presenter-failure-keeps-the-record
            (caddr (log:add! 'probe "kept"))
            "kept")
     (log:set-presenter! #f)

     (format #t "~a log checks passed\n" checks)))
