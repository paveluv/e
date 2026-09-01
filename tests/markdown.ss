#!/usr/bin/env scheme-script

;; The markdown viewer's renderer: markup strips into faces, paragraphs
;; join, tables align, fences frame. Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (md-view))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'markdown-test label actual expected)))

     (define (render lines)
       (let-values ([(text styles links rows) (markdown-render lines)])
         (list text styles links rows)))

     (define (rendered-lines lines) (car (render lines)))

     (check 'paragraphs-join-on-soft-breaks
            (rendered-lines '("one" "two" "" "three"))
            '("one two" "" "three"))

     (check 'blank-runs-collapse
            (rendered-lines '("a" "" "" "" "b"))
            '("a" "" "b"))

     (let* ([r (render '("**bold** plain *it*"))]
            [text (car (car r))]
            [styles (car (cadr r))])
       (check 'emphasis-strips text "bold plain it")
       (check 'emphasis-styles
              (list (vector-ref styles 0) (vector-ref styles 5)
                    (vector-ref styles 11))
              '(bold plain italic)))

     (let* ([r (render '("see [the docs](http://example.com) now"))]
            [text (car (car r))]
            [links (car (caddr r))])
       (check 'link-text-shown text "see the docs now")
       (check 'link-target-hidden links
              '((4 12 "http://example.com"))))

     (let* ([r (render '("# One" "## Two"))]
            [styles (cadr r)])
       (check 'heading-text (car r) '("One" "Two"))
       (check 'heading-faces
              (list (vector-ref (car styles) 0)
                    (vector-ref (cadr styles) 0))
              '(md-h1 md-h2)))

     (let* ([r (render '("> quoted words" "> across lines"))]
            [text (car (car r))]
            [styles (car (cadr r))])
       (check 'quote-marker-stripped-and-joined
              text "quoted words across lines")
       (check 'quote-face (vector-ref styles 0) 'md-quote))

     (check 'bullets-render
            (rendered-lines '("- item one" "  continued" "- two"))
            '("\x2022; item one continued" "\x2022; two"))

     (check 'table-columns-align
            (rendered-lines '("|a|bb|" "|-|-|" "|ccc|d|"))
            '("a    bb"
              "\x2500;\x2500;\x2500;  \x2500;\x2500;"
              "ccc  d "))

     (let* ([r (render '("```scheme" "(+ 1 2)" "```"))]
            [text (car r)]
            [styles (cadr r)])
       (check 'fence-frame
              text
              '("\x250c;\x2500; scheme \x2500;\x2510;"
                "\x2502; (+ 1 2)  \x2502;"
                "\x2514;\x2500;\x2500;\x2500;\x2500;\x2500;\x2500;\x2500;\x2500;\x2500;\x2500;\x2518;"))
       (check 'fence-interior-tinted
              (vector-ref (cadr styles) 2) 'md-code)
       (check 'fence-borders-chrome
              (list (vector-ref (cadr styles) 0)
                    (vector-ref (car styles) 0))
              '(chrome chrome)))

     (check 'hard-breaks-keep-their-lines
            (rendered-lines '("**keys**: C-c t  " "**source**: here  "
                              "" "prose one" "prose two"))
            '("keys: C-c t" "source: here" "" "prose one prose two"))

     (check 'rule-renders
            (car (rendered-lines '("---")))
            (make-string 40 #\x2500))

     (let* ([r (render '("# Top" "" "body text"))]
            [rows (cadddr r)])
       (check 'source-rows-tracked rows '(0 1 2)))

     (format #t "~a markdown checks passed\n" checks)))
