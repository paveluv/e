#!/usr/bin/env scheme-script

;; Plain and surface projection share glyph, coordinate and output fixtures.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(eval
  '(begin
     (import (prefix (head render) render:) (prefix (head text-layout) text-layout:)
             (prefix (head layout) layout:) (prefix (state surface) surface:)
             (prefix (state store) store:) (prefix (foundation text) text:)
             (prefix (head tui) tui:) (prefix (service vt) vt:)
             (prefix (core kernel) kernel:) (prefix (foundation string) string:)
             (prefix (head style) style:) (prefix (sys glyph) glyph:)
             (prefix (sys sys) sys:) (prefix (test) test:))

     (define author '(app render-test))
     ;; The labels in prompts and tables fit the same clusters as the painter.
     ;; One matrix covers padding, either elision edge and tiny cell budgets.
     (test:check 'labels-fit-whole-clusters-at-either-edge
       (map (lambda (case) (apply glyph:fit (list-head case 3)))
         '(("abc" 5 right "abc  ") ("abcdef" 4 right "abc…")
           ("abcdef" 4 left "…def") ("界e\x301;Z" 3 right "界…")
           ("界e\x301;Z" 3 left "…e\x301;Z") ("a👩‍💻z" 3 right "a… ")
           ("abcdef" 1 left "…") ("界" 0 right "")))
       '("abc  " "abc…" "…def" "界…" "…e\x301;Z" "a… " "…" ""))
     ;; Expected maps include EOF. The same table checks plain source,
     ;; character styles, and both coordinate directions without one test
     ;; fixture per script or glyph family.
     (define plain-cases
       '(("" "" () (0) (0))
         ("abc" "abc" (0 1 2) (0 1 2 3) (0 1 2 3))
         ("a\tb\x7f;\x9b;c" #("a" " " "b" " " " " "c") (0 1 2 3 4 5)
          (0 1 2 3 4 5 6) (0 1 2 3 4 5 6))
         ("界e\x301;Z" #("界" "" "e\x301;" "Z") (0 0 1 3) (0 2 2 3 4) (0 0 1 3 4))
         ("\x301;x" #(" \x301;" "x") (0 1) (0 1 2) (0 1 2))
         ("🇺🇸x" #("🇺🇸" "" "x") (0 0 2) (0 0 2 3) (0 0 2 3))
         ("👩‍💻x" #("👩‍💻" "" "x") (0 0 3) (0 0 0 2 3) (0 0 3 4))
         ("1️⃣x" #("1️⃣" "" "x") (0 0 3) (0 0 0 2 3) (0 0 3 4))
         ("각x" #("각" "" "x") (0 0 3) (0 0 0 2 3) (0 0 3 4))))
     (test:check 'plain-clusters-share-glyph-style-and-coordinate-geometry
       (map
         (lambda (case)
           (let* ([text (car case)] [frame (render:prepare #f #f (vector text) 0 '((0 . 1)))]
                  [width (render:width frame 0 (string-length text))])
             (let-values ([(shown styles)
                           (render:present frame 0 text #f (list->vector (iota (string-length text))))])
               (list shown (vector->list styles)
                     (map (lambda (i) (render:column frame 0 i)) (iota (+ (string-length text) 1)))
                     (map (lambda (i) (render:character frame 0 i)) (iota (+ width 1)))))))
         plain-cases) (map cdr plain-cases))
     (test:check 'mode-substitutions-must-preserve-source-geometry
       (map
         (lambda (case)
           (let* ([text (car case)] [frame (render:prepare #f #f (vector text) 0 '((0 . 1)))])
             (let-values ([(shown styles) (render:present frame 0 text (cadr case) #f)])
               (if (vector? shown) (apply string-append (vector->list shown)) shown))))
         '(("ab" "àb") ("界e\x301;Z" "語a\x301;Y") ("界x" #("語" "y"))
           ("ab" "界b") ("界x" #("a" "b")) ("ab" "a") ("ab" #("a" #f))))
       '("àb" "語a\x301;Y" "語y" "ab" "界x" "ab" "ab"))
     (test:check 'plain-clipping-and-selection-respect-neighboring-cells
       (map
         (lambda (case)
           (let* ([text "界e\x301;Z"] [frame (render:prepare #f #f (vector text) 0 '((0 . 1)))]
                  [port (open-output-string)] [mirror (vt:make-emulator 2 16)])
             (let-values ([(shown styles) (render:present frame 0 text #f '#(red blue ignored green))])
               (parameterize ([sys:terminal-output-port port])
                 (tui:display-editor-line! shown shown
                   (cons (render:column frame 0 2) (render:column frame 0 3 #t))
                   '() '() (car case) styles #f (cadr case) 4)
                 (display "|right" port)))
             (vt:emulator-feed! mirror (get-output-string port))
             (let* ([expected (caddr case)] [selected (cadddr case)]
                    [style (and selected
                                (style:code (vector-ref (vector-ref (vt:emulator-styles mirror) 0) selected)))])
               (list (substring (vector-ref (vt:emulator-screen mirror) 0) 0 (string-length expected))
                     (and style (string:search style "44" 0 (string-length style)) #t)))))
         '((0 1 " |right" #f) (0 3 "界é|right" 2) (1 3 " éZ|right" 1)))
       '((" |right" #f) ("界é|right" #t) (" éZ|right" #t)))
     ;; The same source at two widths, without any editor/window state.
     ;; Expected hits cover continuation rows, blank space and wide graphemes.
     (let* ([lines '#("ab cd ef" "界e\x301;z" "" "tail")]
            [frame (render:prepare #f #f lines 0 '((0 . 4)))])
       (test:check 'explicit-text-layout-shares-motion-and-hit-geometry
         (list
           (map (lambda (c) (apply text-layout:move lines frame c))
             '((4 (0 . 1) 1 1) (4 (0 . 7) 1 1) (4 (1 . 3) -1 3) (#f (0 . 1) 1 1) (#f (1 . 3) 1 1)))
           (map (lambda (c) (apply text-layout:hit lines frame c))
             '((4 (0 . 1) 0 1 0) (4 (0 . 1) 0 1 2) (4 (0 . 1) 0 4 2) (4 (3 . 0) 0 1 2)))
           (text-layout:locate lines frame 4 '(0 . 1) 0 '(1 . 3))
           (text-layout:distance lines 4 '(1 . 0) '(0 . 4))
           (text-layout:anchor lines 4 '(0 . 1)))
         '(((0 . 4) (1 . 0) (0 . 8) (1 . 0) (2 . 0))
           ((0 . 4) (1 . 0) (1 . 4) (4 . 1)) (3 . 2) -2 (0 . 3)))
       (test:check 'explicit-viewport-scroll-and-page-edges
         (list
           (call-with-values (lambda () (text-layout:scroll lines frame 4 4 3 0 '(0 . 0) 0 '(3 . 2) 1)) list)
           (call-with-values (lambda () (text-layout:scroll lines frame #f 2 2 0 '(0 . 3) 0 '(1 . 3) 0)) list)
           (map (lambda (top)
                  (call-with-values (lambda () (text-layout:page lines frame 4 0 3 top 1 1 1)) list))
             '((0 . 0) (1 . 0)))
           (call-with-values (lambda () (text-layout:scroll '#("") #f 1 0 0 0 '(0 . 0) 0 '(3 . 10) 8)) list))
         '(((3 . 2) (1 . 0) 0) ((1 . 3) (0 . 0) 2)
           (((1 . 0) (2 . 0)) ((1 . 0) (3 . 1))) ((0 . 0) (0 . 0) 0))))
     (let* ([reads 0] [lines (render:defer 10000000 (lambda (row) (set! reads (+ reads 1)) "abcdefgh"))])
       (test:check 'distant-caret-and-page-work-is-bounded-by-viewport
         (list
           (call-with-values
             (lambda () (text-layout:scroll lines #f 4 4 5 0 '(0 . 0) 0 '(9999999 . 7) 1)) list)
           (< reads 40)
           (begin
             (set! reads 0)
             (call-with-values (lambda () (text-layout:page lines #f 4 0 5 '(0 . 0) 1 1 1)) list))
           (< reads 30)
           (begin
             (set! reads 0)
             (call-with-values (lambda () (text-layout:scroll lines #f 4 4 5 1 '(9999997 . 1) 0 '(0 . 7) 1)) list))
           (< reads 30))
         '(((9999999 . 7) (9999997 . 1) 0) #t ((2 . 1) (3 . 5)) #t ((0 . 7) (1 . 0) 0) #t)))

     (define uri "https://wide.example")
     (define clusters '((clusters (1 . 2) (2 . 1) (1 . 1))))
     (define lines (make-vector 50 "abc"))
     (vector-set! lines 0 "界e\x301;Z")
     (define id (store:create! author "*surface-render*" lines '((read-only . #t) (wrap . #t))))
     (define (grid face attrs)
       (list 0 (vector face 'ignored 'bold 'plain)
             (vector (list uri "wide") (list "https://ignored.example" #f)
                     '("https://accent.example" #f) #f) attrs))
     (define (publish changes)
       (call-with-values
         (lambda ()
           (let ([old (surface:snapshot id)])
             (surface:publish! id (and old (car old)) (store:revision id)
                               changes '(0 2 #t) '(4 4)))) list))
     (define frame #f)
     (define (prepare ranges)
       (let-values ([(text revision) (store:snapshot id)])
         (render:prepare frame id text revision ranges)))
     (define (header) (render:header frame))
     (define (data row) (render:row frame row))
     (define expected
       (list '#("界" "" "e\x301;" "Z") '#(red red bold plain)
             (list (list 0 2 uri "wide") '(2 3 "https://accent.example" #f))))
     (publish (list (grid 'red clusters) '(49 #(blue blue blue) #(#f #f #f) ())))
     (set! frame (prepare '((0 . 1))))
     (test:check 'surface-keeps-source-text-and-grid-layout
       (list (vector-ref lines 0) (data 0)) (list "界e\x301;Z" expected))
     (test:check 'one-cluster-map-drives-both-coordinate-directions
       (list (map (lambda (i) (render:column frame 0 i)) '(0 1 2 3 4 5))
             (map (lambda (i) (render:character frame 0 i)) '(0 1 2 3 4 5))
             (render:column frame 0 2 #t) (render:width frame 0 99))
       '((0 2 2 3 4 5) (0 0 1 3 4 5) 3 4))
     (test:check 'demand-reads-do-not-retain-scrollback
       (list (data 49) (render:row (prepare '((49 . 50))) 49))
       '(#f (#("a" "b" "c") #(blue blue blue) ())))
     (let ([row (data 0)] [header (header)])
       (string-set! (vector-ref (car row) 0) 0 #\X)
       (vector-set! (cadr row) 0 'damaged)
       (string-set! (caddr (car (caddr row))) 0 #\X)
       (set-car! (caddr header) 99))
     (test:check 'cached-readbacks-are-owned (list (data 0) (header))
       (list expected (surface:snapshot id)))
     (test:check 'unchanged-demand-reuses-frame
       (eq? frame (prepare '((0 . 1)))) #t)
     (let ([next (prepare '((49 . 50)))])
       (test:check 'viewport-refill-discards-undemanded-rows
         (list (render:row next 0) (and (render:row next 49) #t)) '(#f #t)))
     ;; Producer updates never mutate a retained projection. A text revision
     ;; ahead of its surface falls back rather than mixing publications.
     ((test:worker (lambda () (publish (list (grid 'green clusters))))))
     (test:check 'producer-does-not-mutate-retained-frame (data 0) expected)
     (store:edit! author id 0 (text:make-span 0 3 0 4) '("R"))
     (test:check 'text-ahead-of-surface-falls-back
       (render:header (prepare '((0 . 1)))) #f)
     ;; Multiple disjoint ranges must all come from one publication, even
     ;; while a producer changes both faster than the head can read them.
     (define (paired face)
       (list (grid face clusters) (list 49 (make-vector 3 face) '#(#f #f #f) '())))
     (publish (paired 'red))
     (define writer
       (test:worker (lambda ()
                      (do ([i 0 (+ i 1)]) ((= i 40))
                        (publish (paired (number->string (+ 30 (mod i 8)))))))))
     (define (coherent-range-read?)
       (let ([frame (prepare '((0 . 1) (49 . 50)))])
         (or (not (render:header frame))
             (equal? (vector-ref (cadr (render:row frame 0)) 0)
                     (vector-ref (cadr (render:row frame 49)) 0)))))
     (define coherent? (for-all (lambda (i) (coherent-range-read?)) (iota 40)))
     (writer)
     (test:check 'concurrent-demand-ranges-stay-coherent
       (and coherent? (coherent-range-read?)) #t)
     (publish (paired 'red))
     (let ([id (store:create! author "surface-controls" '("\x1b;X") '((audience . ())))])
       (surface:publish! id #f 0 '((0 #(red red) #(#f #f) ())) #f '(1 2))
       (let-values ([(text revision) (store:snapshot id)])
         (test:check 'surface-text-cannot-inject-terminal-controls
           (car (render:row (render:prepare #f id text revision '((0 . 1))) 0)) '#(" " "X")))
       (store:delete! author id))

     ;; Bad layout attributes are not executable or guessed from characters.
     ;; A valid seam frame can be unsupported by a head; it renders plain.
     (test:check 'invalid-cluster-layouts-fall-back-as-a-whole
       (for-all
         (lambda (bad)
           (publish (list (grid 'red (list (cons 'clusters bad)))))
           (not (render:header (prepare '((0 . 1))))))
         '(((0 . 1) (4 . 3)) ((4 . 3)) ((4 . 4.0)) ((-1 . 2) (5 . 2))
           ((1 . 1) (2 . 2) (1 . 1)) malformed)) #t)
     (publish (list (grid 'red clusters)))

     (set! frame (prepare '((0 . 1))))
     (let* ([port (open-output-string)] [mirror (vt:make-emulator 2 12)]
            [row (data 0)])
       (parameterize ([sys:terminal-output-port port])
         (tui:display-editor-line! (car row) (car row)
           (cons (render:column frame 0 2) (render:column frame 0 3 #t))
           '() (caddr row) 0 (cadr row) #f 12 4))
       (vt:emulator-feed! mirror (get-output-string port))
       (test:check 'surface-output-shares-glyph-selection-and-link-geometry
         (list (substring (vector-ref (vt:emulator-screen mirror) 0) 0 3)
           (and (string:search (style:code (vector-ref (vector-ref (vt:emulator-styles mirror) 0) 2)) "44" 0
                  (string-length (style:code (vector-ref (vector-ref (vt:emulator-styles mirror) 0) 2)))) #t)
           (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) 1))
         (list "界éR" #t (list uri "wide"))))
     (surface:withdraw! id (car (surface:snapshot id)))
     (test:check 'withdrawal-restores-plain-projection
       (render:header (prepare '((0 . 1)))) #f)
     (store:delete! author id)

     ;; The emulator's publication data must drive the ordinary surface
     ;; renderer, including physical glyph widths, without a terminal mode.
     (test:check 'emulator-frames-render-through-the-surface-contract
       (map
         (lambda (case)
           (let ([emulator (vt:make-emulator 2 8)])
             (vt:emulator-feed! emulator (cdr case))
             (let* ([frame (vt:emulator-frame emulator)] [text (car frame)]
                    [id (store:create! author "emulator-frame" text '((audience . ())))]
                    [height (vector-length text)] [mirror (vt:make-emulator (+ height 1) 8)]
                    [port (open-output-string)])
               (surface:publish! id #f 0 (cadr frame) (caddr frame) (cadddr frame))
               (let-values ([(text revision) (store:snapshot id)])
                 (let ([rendered (render:prepare #f id text revision (list (cons 0 height)))])
                   (parameterize ([sys:terminal-output-port port])
                     (do ([i 0 (+ i 1)]) ((= i height))
                       (let ([row (render:row rendered i)])
                         (tui:goto! (+ i 1) 1)
                         (tui:display-editor-line! (car row) (car row) #f '() (caddr row)
                           0 (cadr row) #f 8 8))))))
               (vt:emulator-feed! mirror (get-output-string port))
               (store:delete! author id)
               (cons (car case)
                     (equal? (vector->list text)
                       (map (lambda (i) (vector-ref (vt:emulator-screen mirror) i)) (iota height)))))))
         '((clusters . "界q\x301;Z")
           (decorated . "\x1b;#6界A")
           (scrollback . "one\r\ntwo\r\nthree\r\nfour")
           (alternate . "one\r\ntwo\r\nthree\x1b;[?1049hALT")))
       '((clusters . #t) (decorated . #t) (scrollback . #t) (alternate . #t)))
     (test:check 'linear-allocation-collapses-gaps-and-shares-remainders
       (map (lambda (c) (apply layout:linear c))
         '((9 1 ((1 2 fit) (1 3 (grow 1)) (1 3 (grow 2))))
           (1 2 ((4 4 (grow 1)) (2 2 (grow 1))))
           (0 1 ((1 1 fit) (1 1 (grow 1))))
           (5 3 ((2 2 fit) (2 2 fit)))
           (11 1 ((0 0 (grow 1)) (0 0 (grow 3))))))
       '(((0 2) (3 2) (6 3)) ((0 1) (1 0)) ((0 0) (0 0)) ((0 2) (3 2)) ((0 3) (4 7))))
     (test:check 'half-open-clipping-and-composition-preserve-whole-clusters
       (list (layout:intersect '(0 0 2 2) '(2 1 3 3))
         (layout:contains? '(0 0 2 2) 2 1) (layout:contains? '(0 0 0 2) 0 1)
         (map (lambda (c) (apply glyph:slice c)) '(("界éZ" 1 2) ("界éZ" 0 1) ("界éZ" 2 3)
                                                   ("abcd" 1 2) ("abcd" 3 3) ("abcd" 6 2) ("" 0 0))))
       '((2 1 0 1) #f #f (" é" " " "éZ " "bc" "d  " "  " "")))
     (test:finish! 'render)))
