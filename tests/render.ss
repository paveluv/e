#!/usr/bin/env scheme-script

;; Plain/surface projection, head adoption, and the real painter share fixtures.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(eval
  '(begin
     (import (prefix (head render) render:) (prefix (state surface) surface:)
             (prefix (head head) head:) (prefix (state store) store:) (prefix (foundation text) text:)
             (prefix (head paint) paint:) (prefix (service vt) vt:)
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
                 (paint:display-editor-line! shown shown
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
     (define uri "https://wide.example")
     (define clusters '((clusters (1 . 2) (2 . 1) (1 . 1))))
     (define lines (make-vector 50 "abc"))
     (vector-set! lines 0 "界e\x301;Z")
     (define id (store:create! author "*surface-render*" lines '((read-only . #t) (wrap . #t))))
     (define b (head:adopt-store-buffer! id))
     (define w (head:current-window))
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
     (define (header) (render:header (head:buffer-rendition b)))
     (define (data row) (render:row (head:buffer-rendition b) row))
     (define expected
       (list '#("界" "" "e\x301;" "Z") '#(red red bold plain)
             (list (list 0 2 uri "wide") '(2 3 "https://accent.example" #f))))
     (publish (list (grid 'red clusters) '(49 #(blue blue blue) #(#f #f #f) ())))
     (head:window-size-set! w 4)
     (head:window-width-set! w 12)
     (define observed (test:recorder))
     (head:set-repaint-hook!
       (lambda () (observed (list (head:buffer-store-rev b) (header) (data 0)))))
     (head:set-window-buffer! w b)
     (test:check 'show-publishes-text-and-rendition-before-one-callback
       (observed) (list (list 0 (surface:snapshot id) expected)))
     (test:check 'surface-keeps-source-text-and-grid-layout
       (list (vector-ref (head:buffer-lines b) 0) (paint:window-wrapped? w)) '("界e\x301;Z" #f))
     (define frame (head:buffer-rendition b))
     (test:check 'one-cluster-map-drives-both-coordinate-directions
       (list (map (lambda (i) (render:column frame 0 i)) '(0 1 2 3 4 5))
             (map (lambda (i) (render:character frame 0 i)) '(0 1 2 3 4 5))
             (render:column frame 0 2 #t) (render:width frame 0 99))
       '((0 2 2 3 4 5) (0 0 1 3 4 5) 3 4))
     (test:check 'public-links-are-character-ranges
       (paint:buffer-line-hyperlinks b 0)
       (list (list 0 1 uri "wide") '(1 3 "https://accent.example" #f)))
     (test:check 'demand-reads-do-not-fill-the-head-with-scrollback
       (list (data 49) (render:row (head:read-rendition b '((49 . 50))) 49)
             (eq? frame (head:buffer-rendition b)))
       '(#f (#("a" "b" "c") #(blue blue blue) ()) #t))
     (let ([row (data 0)] [header (header)])
       (string-set! (vector-ref (car row) 0) 0 #\X)
       (vector-set! (cadr row) 0 'damaged)
       (string-set! (caddr (car (caddr row))) 0 #\X)
       (set-car! (caddr header) 99))
     (test:check 'cached-readbacks-are-owned (list (data 0) (header))
       (list expected (surface:snapshot id)))
     (head:refresh-renditions!)
     (test:check 'unchanged-ranges-reuse-frame-and-do-not-notify
       (list (eq? frame (head:buffer-rendition b)) (length (observed))) '(#t 1))
     (head:window-prow-set! w 49)
     (head:window-top-set! w 49)
     (head:refresh-renditions!)
     (test:check 'viewport-refill-discards-old-rows-without-invalidating-paint
       (list (data 0) (and (data 49) #t) (length (observed))) '(#f #t 1))
     (head:window-prow-set! w 0)
     (head:window-top-set! w 0)
     (head:refresh-renditions!)

     ;; The producer's thread only wakes the head. A complete adoption can
     ;; reenter, commit another text/frame, and leave the newer result intact.
     (define before-worker (head:buffer-rendition b))
     ((test:worker (lambda () (publish (list (grid 'green clusters))))))
     (test:check 'worker-does-not-mutate-the-head
       (eq? before-worker (head:buffer-rendition b)) #t)
     (define once #t)
     (define callbacks (test:recorder))
     (head:set-repaint-hook! paint:invalidate-screen-cache!)
     (parameterize ([kernel:registering-module 'render-observer])
       (head:add-pre-redraw-hook!
         (lambda ()
           (callbacks (list (vector-ref (head:buffer-lines b) 0)
                            (head:buffer-store-rev b) (cadr (header))
                            (vector-ref (cadr (data 0)) 0)))
           (when once
             (set! once #f)
             (store:edit! author id 0 (text:make-span 0 3 0 4) '("Q"))
             (publish (list (grid 'blue clusters)))
             (head:before-frame!)))))
     (head:before-frame!)
     (test:check 'reentrant-adoption-never-mixes-text-and-rendition
       (list (callbacks) (vector-ref (head:buffer-lines b) 0) (cadr (header)))
       '((("界e\x301;Z" 0 0 green) ("界e\x301;Q" 1 1 blue)) "界e\x301;Q" 1))
     (kernel:retract-module! 'render-observer)
     (store:edit! author id 1 (text:make-span 0 3 0 4) '("R"))
     (head:before-frame!)
     (test:check 'text-ahead-of-surface-clears-rendition
       (list (header) (data 0) (paint:window-wrapped? w)
             (paint:buffer-line-hyperlinks b 0)) '(#f #f #t ()))
     (publish (list (grid 'red clusters)))
     (head:before-frame!)

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
       (let ([frame (head:read-rendition b '((0 . 1) (49 . 50)))])
         (or (not (render:header frame))
             (equal? (vector-ref (cadr (render:row frame 0)) 0)
                     (vector-ref (cadr (render:row frame 49)) 0)))))
     (define coherent? (for-all (lambda (i) (coherent-range-read?)) (iota 40)))
     (writer)
     (test:check 'concurrent-demand-ranges-stay-coherent
       (and coherent? (coherent-range-read?) (and (head:read-rendition b '((0 . 1))) #t)) #t)
     (publish (paired 'red))
     (head:before-frame!)
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
           (head:before-frame!)
           (not (header)))
         '(((0 . 1) (4 . 3)) ((4 . 3)) ((4 . 4.0)) ((-1 . 2) (5 . 2))
           ((1 . 1) (2 . 2) (1 . 1)) malformed)) #t)
     (publish (list (grid 'red clusters)))
     (head:before-frame!)

     ;; Capture actual ANSI in a VT emulator, including cursor geometry and
     ;; selection of only the combining character (the complete glyph paints).
     (define (painted)
       (let ([port (open-output-string)])
         (parameterize ([sys:terminal-output-port port]) (paint:redraw!))
         (get-output-string port)))
     (painted)
     (paint:set-screen-rows! 8)
     (paint:set-screen-cols! 12)
     (head:window-pcol-set! w 3)
     (head:buffer-mark-row-set! b 0)
     (head:buffer-mark-col-set! b 2)
     (head:buffer-marked-set! b #t)
     (define mirror (vt:make-emulator 8 12))
     (vt:emulator-feed! mirror (painted))
     (test:check 'painter-emits-real-glyphs-at-cell-coordinates
       (let ([selection (style:code (vector-ref (vector-ref (vt:emulator-styles mirror) 0) 2))])
         (list (substring (vector-ref (vt:emulator-screen mirror) 0) 0 3)
               (and (string:search selection "44" 0 (string-length selection)) #t)))
       '("界éR" #t))
     (test:check 'painter-emits-surface-hyperlinks
       (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) 1) (list uri "wide"))
     (test:check 'cursor-uses-the-same-cell-projection
       (paint:window-screen-position w 0 1) '(1 . 3))
     (head:buffer-marked-set! b #f)
     (painted)
     (surface:publish! id (car (surface:snapshot id)) (store:revision id) '() '(0 1 #t) '(4 4))
     (test:check 'cursor-only-publication-does-not-repaint-unchanged-rows
       (let ([output (painted)])
         (list (string:search output uri 0 (string-length output)) (caddr (header))))
       '(#f (0 1 #t)))

     (let ([other (head:tool-buffer! "projection-resize")])
       (head:set-layout-root!
         (head:make-layout-split 'below w
           (head:make-window other 0 0 0 0 2 12 80 80 'default) 1 1))
       (paint:set-screen-rows! 5)
       (paint:set-screen-cols! 80)
       (publish '((2 #(blue blue blue) #(#f #f #f) ())))
       (painted)
       ;; one ordinary window remains beside the hidden pop-up
       (test:check 'collapsed-layout-demands-its-final-visible-rows
         (list (length (remq (head:popup) (head:windows))) (and (data 2) #t)) '(1 #t))
       (head:forget-buffer! other))

     (test:check 'unavailable-store-denies-rendition-and-retries-next-frame
       (let* ([cell (kernel:persistent-cell 'store (lambda () #f))]
              [saved (unbox cell)]
              [unavailable
               (dynamic-wind
                 (lambda () (set-box! cell #f))
                 (lambda ()
                   (list (head:buffer-rendition b) (head:read-rendition b '((0 . 1)))
                         (begin (head:before-frame!) (head:buffer-rendition b))))
                 (lambda () (set-box! cell saved)))])
         (head:before-frame!)
         (list unavailable (equal? (header) (surface:snapshot id))))
       '((#f #f #f) #t))

     ;; Hiding/deletion denies new metadata reads before queued retirement.
     (store:set-property! author id 'audience '())
     (test:check 'hidden-buffer-denies-cached-and-demanded-rendition
       (list (head:buffer-rendition b) (head:read-rendition b '((0 . 1)))
             (paint:buffer-line-hyperlinks b 0)) '(#f #f ()))
     (head:before-frame!)
     (test:check 'retired-reference-does-not-revive-on-readmission
       (begin (store:set-property! author id 'audience 'all)
              (head:before-frame!)
              (list (head:buffer-rendition b) (head:read-rendition b '((0 . 1))))) '(#f #f))
     (define current (head:buffer-of-store-id id))
     (head:set-window-buffer! w current)
     (test:check 'readmitted-buffer-adopts-current-surface
       (render:header (head:buffer-rendition current)) (surface:snapshot id))
     (surface:withdraw! id (car (surface:snapshot id)))
     (head:before-frame!)
     (test:check 'withdrawal-restores-ordinary-text
       (list (render:header (head:buffer-rendition current)) (paint:window-wrapped? w)
             (paint:buffer-line-hyperlinks current 0)
             (paint:window-screen-position w 0 1)) '(#f #t () (1 . 3)))
     (publish (list (grid 'green clusters)))
     (head:before-frame!)
     (store:delete! author id)
     (test:check 'delete-denies-metadata-before-cleanup
       (list (head:buffer-rendition current) (head:read-rendition current '((0 . 1)))) '(#f #f))
     (head:before-frame!)

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
                         (paint:goto! (+ i 1) 1)
                         (paint:display-editor-line! (car row) (car row) #f '() (caddr row)
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
     (test:finish! 'render)))
