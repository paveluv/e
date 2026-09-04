#!/usr/bin/env scheme-script

;; The mode registry: registration and lookup, detection by extension
;; and interpreter line, hand-chosen modes, the memoized stylers, and
;; re-resolution after a re-registration -- v2 core dissolution
;; (docs/DESIGN2.md).  Headless: buffers come from (head), which
;; mirrors them into the store.  Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (modes) modes:)
             (prefix (head) head:)
             (only (chezscheme) format box unbox set-box!))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'modes-test label actual expected)))

     ;; -- registration and lookup -------------------------------------

     (define styler-calls (box 0))
     (define (probe-styler s)
       (set-box! styler-calls (+ (unbox styler-calls) 1))
       (make-vector (string-length s) 'keyword))

     (modes:register! "probe" '(".probe") '("probesh") probe-styler)

     (check 'find (modes:name (modes:find "probe")) "probe")
     (check 'find-missing (modes:find "no-such-mode") #f)
     (check 'extensions (modes:extensions (modes:find "probe")) '(".probe"))
     (check 'no-render (modes:render (modes:find "probe")) #f)

     ;; -- detection -----------------------------------------------------------

     (define by-file (head:new-buffer "x.probe"))
     (head:buffer-file-set! by-file "/nowhere/x.probe")
     (modes:assign! by-file)
     (check 'detect-by-extension (modes:name-of by-file) "probe")
     (check 'detected-is-auto (head:buffer-mode-auto by-file) #t)

     (define by-interpreter (head:new-buffer "script"))
     (head:buffer-lines-set! by-interpreter (vector "#!/usr/bin/env probesh" "x"))
     (modes:assign! by-interpreter)
     (check 'detect-by-interpreter (modes:name-of by-interpreter) "probe")

     (define plain (head:new-buffer "notes.txt"))
     (head:buffer-file-set! plain "/nowhere/notes.txt")
     (modes:assign! plain)
     (check 'detect-nothing (modes:name-of plain) #f)
     (check 'of-nothing (modes:of plain) #f)

     ;; -- choosing by hand ----------------------------------------------------

     (modes:choose! plain "probe")
     (check 'chosen (modes:name-of plain) "probe")
     (check 'chosen-is-not-auto (head:buffer-mode-auto plain) #f)
     (modes:choose! plain #f)
     (check 'unchosen (modes:of plain) #f)

     ;; -- extensions added later ----------------------------------------------

     (modes:add-extension! "probe" ".pr2")
     (define by-addition (head:new-buffer "y.pr2"))
     (head:buffer-file-set! by-addition "/nowhere/y.pr2")
     (modes:assign! by-addition)
     (check 'detect-by-added-extension (modes:name-of by-addition) "probe")
     (check 'bad-extension-refused
            (guard (ex [else 'refused]) (modes:add-extension! "probe" "pr3"))
            'refused)
     (check 'unknown-mode-refused
            (guard (ex [else 'refused]) (modes:add-extension! "no-such-mode" ".x"))
            'refused)

     ;; -- memoized line styles ------------------------------------------------

     (define styles-of (modes:line-styles by-file))
     (define line (string #\a #\b #\c))
     (set-box! styler-calls 0)
     (check 'line-styles (vector->list (styles-of line)) '(keyword keyword keyword))
     (styles-of line)
     (styles-of line)
     (check 'line-styles-memoized-by-identity (unbox styler-calls) 1)
     (styles-of (string #\a #\b #\c))
     (check 'line-styles-fresh-string (unbox styler-calls) 2)
     (check 'plain-buffer-styles ((modes:line-styles plain) "abc") #f)

     (modes:register! "raiser" '(".raise") '() (lambda (s) (error 'raiser "boom")))
     (modes:choose! plain "raiser")
     (check 'raising-styler-paints-plain ((modes:line-styles plain) "abc") #f)

     ;; -- memoized whole-buffer analysis --------------------------------------

     (define analyses (box 0))
     (define row-of
       (modes:memoize-analysis
         (lambda (lines)
           (set-box! analyses (+ (unbox analyses) 1))
           (vector-map string-length lines))))
     (head:buffer-lines-set! by-file (vector "one" "three"))
     (check 'analysis-row (row-of by-file 1) 5)
     (row-of by-file 0)
     (check 'analysis-once-per-revision (unbox analyses) 1)
     (check 'analysis-row-out-of-range (row-of by-file 7) #f)
     (head:buffer-lines-set! by-file (vector "changed"))
     (check 'analysis-after-edit (row-of by-file 0) 7)
     (check 'analysis-reran (unbox analyses) 2)

     ;; -- re-registration and refresh -----------------------------------------

     (define old (modes:find "probe"))
     (modes:register! "probe" '(".probe") '("probesh") probe-styler)
     (check 'newest-registration-wins (eq? (modes:find "probe") old) #f)
     (modes:refresh!)
     (check 'refresh-resolves-to-new (eq? (modes:of by-file) (modes:find "probe")) #t)
     (check 'refresh-keeps-name (modes:name-of by-file) "probe")

     (format #t "~a modes checks passed\n" checks)))
