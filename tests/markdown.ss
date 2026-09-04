#!/usr/bin/env scheme-script

;; The markdown viewer's renderer: markup strips into faces, paragraphs
;; join, tables align, fences frame. Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (markdown) markdown:) (prefix (scheme-mode) scheme-mode:))

     (scheme-mode:init!)

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'markdown-test label actual expected)))

     (define (render lines)
       (let-values ([(text styles links rows) (markdown:render lines)])
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
              "ccc  d"))

     (let* ([r (render '("```scheme" "(+ 1 2)" "```"))]
            [text (car r)]
            [styles (cadr r)])
       (check 'fence-rules
              text
              '("\x2504; scheme \x2504;"
                "(+ 1 2)"
                "\x2504;\x2504;\x2504;\x2504;\x2504;\x2504;\x2504;\x2504;\x2504;\x2504;"))
       (check 'fence-interior-plain-cells
              (vector-ref (cadr styles) 1) 'md-code)
       (check 'fence-rules-chrome
              (list (vector-ref (car styles) 0)
                    (vector-ref (caddr styles) 0))
              '(chrome chrome)))

     (check 'tables-fit-a-narrow-width
            (let-values ([(text styles links rows)
                          (markdown:render
                            '("|alpha beta gamma|x|" "|-|-|" "|delta|y|")
                            12)])
              text)
            '("alpha      x"
              "beta"
              "gamma"
              "\x2500;\x2500;\x2500;\x2500;\x2500;\x2500;\x2500;\x2500;\x2500;  \x2500;"
              "delta      y"))

     (let* ([r (render '("|a|see [doc](x.md)|" "|-|-|" "|b|c|"))])
       (check 'table-cells-keep-links
              (list (car r) (car (caddr r)))
              '(("a  see doc" "\x2500;  \x2500;\x2500;\x2500;\x2500;\x2500;\x2500;\x2500;" "b  c")
                ((7 10 "x.md")))))

     (let* ([r (render '("```scheme" "(define x 1)" "```"))]
            [interior (cadr (cadr r))])
       (check 'fence-language-styles
              (list (vector-ref interior 0) (vector-ref interior 1)
                    (vector-ref interior 7))
              '(delimiter keyword md-code)))

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
