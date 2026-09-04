#!/usr/bin/env scheme-script

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (core) (prefix (paint) paint:) (prefix (head) head:))

     (define (check label actual expected)
       (unless (equal? actual expected)
         (error 'hyperlink-test label actual expected)))

     (check 'detect-url
            (paint:detect-hyperlinks "See https://example.com/path.")
            '((4 28 "https://example.com/path")))

     ;; Exercise the generic buffer validation path. This catches accidental
     ;; shadowing of list procedures by the numeric line length.
     (let ([buffer (head:new-buffer "*hyperlink-test*")])
       (call-with-buffer
         buffer
         (lambda () (insert-text! "https://example.com/path")))
       (check 'buffer-link-ranges
              (buffer-line-hyperlinks buffer 0)
              '((0 24 "https://example.com/path"))))

     (display "2 hyperlink checks passed\n")))
