#!/usr/bin/env scheme-script

;; The markdown viewer's renderer: markup strips into faces, paragraphs
;; join, tables align, fences frame. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (prefix (foundation markup) markup:)
             (prefix (head markdown-layout) markdown-layout:)
             (prefix (head markdown-control) control:)
             (prefix (head head) head:) (prefix (head interaction) interaction:)
             (prefix (head widget) widget:) (prefix (head range) range:)
             (prefix (head layout) layout:)
             (prefix (state store) store:) (prefix (state collection) collection:)
             (prefix (state view) view:) (prefix (state model) model:)
             (prefix (foundation string) string:) (prefix (foundation text) text:)
             (prefix (apps markdown) markdown:) (prefix (modes scheme-mode) scheme-mode:))

     (scheme-mode:init!)


     (define check test:check)

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

     ;; One portable interpretation fits two mounts without changing source
     ;; anchors. Separator rows must not shift later table rows back by one.
     (let* ([blocks (markup:parse '("|name|notes|" "|---|---|" "|one|a [link](x)|" "|---|---|" "|two|long words here|"))]
            [before (format "~s" blocks)])
       (check 'semantic-table-source-anchors
         (map car (cadddr (car blocks))) '(0 2 4))
       (let-values ([(wide faces links rows anchors) (markdown-layout:render blocks 60)]
                    [(narrow narrow-faces narrow-links narrow-rows narrow-anchors) (markdown-layout:render blocks 12)])
         (check 'independent-table-fitting
           (list (> (length narrow) (length wide)) (car links)
             (filter (lambda (row) (> row 0)) rows) (equal? before (format "~s" blocks)))
           '(#t () (2 4) #t))))

     (let-values ([(text faces links rows) (markdown:render '("```界" "body" "```"))])
       (check 'code-faces-address-characters
         (map vector-length faces) (map string-length text)))

     (let-values ([(lines faces links rows anchors)
                   (markdown-layout:render (markup:parse '("|a|many words here to wrap|" "|b|c|")) 12)])
       (check 'table-continuation-padding-anchors-to-its-present-cell
         (let ([p (vector-ref (cadr anchors) 0)]) (and (= (cadr p) 2) (> (caddr p) 0))) #t))

     ;; A nested pair shares one base query, but neither geometry nor selection.
     (let ()
       (define copied #f)
       (define actor head:ui-actor)
       (define source (store:create! actor "Markdown widget" '("# Hello" "" "|one|two|" "|---|---|" "|words here|many more words here|")))
       (define a (markdown:create-view! actor source))
       (define b (view:fork! actor a))
       (define query (view:source (view:snapshot a)))
       (define root (view:create! actor #f 'row 1 '() '()))
       (define (state id) (view:state (interaction:snapshot id)))
       (define (show!)
         (let ([f (widget:prepare! root 60 8)]) (widget:present! (list (list f 0 0))) f))
       (define (pump!) (range:pump!) (widget:pump!) (show!))
       (define (body id) (find (lambda (f) (equal? (widget:frame-id f) id)) (widget:frame-children (widget:prepared root))))
       (define (contains? text) (and (exists (lambda (line) (string:search line text 0 (string-length line))) (widget:frame-lines (body a))) #t))
       (collection:init!) (widget:init!) (control:register! (lambda (s) (set! copied s)))
       (view:arrange! actor (list (list root 0 (list (list 'narrow a '(grow 1)) (list 'wide b '(grow 2))) '())) '())
       (widget:mount! root 'markdown-fixture)
       (test:await 'markdown-ranges (lambda () (pump!) (contains? "Hello")))
       (check 'markdown-shares-query-with-independent-fitting
         (list (equal? query (view:source (view:snapshot b)))
           (caddr (widget:frame-rect (body a))) (caddr (widget:frame-rect (body b)))
           (contains? "many more") (not (equal? (widget:frame-lines (body a)) (widget:frame-lines (body b)))))
         '(#t 21 39 #t #t))
       (markdown:select! a '(2 4 2 20) '(2 4 2 10))
       (markdown:copy! a)
       (check 'markdown-copies-displayed-text-and-keeps-independent-selection
         (list copied (car (state a)) (car (state b))) '("words here" (2 4 2 20) (0 0 0 0)))
       (let ([saved (state a)])
         (widget:prepare! root 100 8)
         (check 'markdown-resize-keeps-semantic-cell-anchors (state a) saved))
       (show!)
       (store:edit! actor source 0 (text:make-span 0 0 0 0) '("Intro" "" "") #f)
       (test:await 'markdown-source-rebase
         (lambda () (pump!) (equal? 1 (view:basis (interaction:snapshot a)))))
       (check 'markdown-rebases-source-rows-without-losing-cell-selection
         (list (car (state a)) (cadr (state a))) '((4 6 2 20) (4 6 2 10)))
       (let ([before (model:revision query)])
         (pump!) (pump!) (pump!)
         (check 'markdown-unchanged-frames-do-not-republish-query (model:revision query) before))
       (widget:prepare! root 0 0)
       (widget:unmount! root)
       (check 'markdown-hidden-view-releases-range-demand (model:demanded? query) #f)
       (store:delete! actor source))

     ;; Far navigation acquires a new page without publishing display-row
     ;; offsets. A later interaction cancels the queued intent.
     (let ()
       (define actor head:ui-actor)
       (define opened #f)
       (define source (store:create! actor "Long Markdown"
                        (apply append (map (lambda (i) (list (format "[Link ~a](target~a.md)" i i) "")) (iota 100)))))
       (define child (markdown:create-view! actor source))
       (define root (view:create! actor #f 'markdown-test 1 '() '()))
       (define (state) (view:state (interaction:snapshot child)))
       (define (show!)
         (let ([f (widget:prepare! root 30 5)]) (widget:present! (list (list f 0 0))) f))
       (define (pump!) (range:pump!) (widget:pump!) (show!))
       (widget:register! 'markdown-test 1
         (append (layout:container 'y)
           (list (cons 'actions (list (cons 'open (lambda (id document uri) (set! opened (list document uri)))))))))
       (view:arrange! actor
         (list (list root 0 (list (list 'body child '(grow 1))) '())
           (list child 0 '() (list (list 'commands (list 'open-uri root 'open '()))))) '())
       (widget:mount! root 'markdown-navigation)
       (test:await 'markdown-first-page
         (lambda () (pump!) (equal? 0 (view:basis (interaction:snapshot child)))))
       (markdown:move! child 'finish)
       (test:await 'markdown-last-page (lambda () (pump!) (= (caar (state)) 198)))
       (check 'markdown-end-acquires-last-page (caar (state)) 198)
       (markdown:move! child 'start)
       (interaction:set-state! actor child 0 (append (list-head (state) 3) '(#t)))
       (pump!)
       (check 'markdown-later-interaction-cancels-pending-motion (caar (state)) 198)
       (markdown:move! child 'start)
       (test:await 'markdown-return-to-first-page (lambda () (pump!) (= (caar (state)) 0)))
       (widget:pointer! '(pointer press primary ()) 2 0)
       (check 'markdown-link-uses-explicit-document-and-host opened (list source "target0.md"))
       (widget:pointer! '(pointer press primary (shift)) 4 0)
       (show!)
       (widget:pointer! '(pointer move primary ()) 6 0)
       (show!)
       (widget:pointer! '(pointer release primary ()) 6 0)
       (check 'markdown-pointer-drag-keeps-fixed-anchor (list (car (state)) (cadr (state)) (cadddr (state)))
         '((0 0 0 6) (0 0 0 0) #t))
       (widget:unmount! root)
       (store:delete! actor source))

     (test:finish! 'markdown)))
