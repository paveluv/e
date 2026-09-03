;; head.e -- the head's window tree: the library (head), v2 core
;; dissolution (docs/DESIGN2.md), first slice.  Pure infrastructure
;; with no init!.
;;
;; A head is one user's seat: its windows, their layout, and which
;; one is selected.  This slice owns the records and the geometry --
;; the window record, the persistent split tree (leaves are windows;
;; an internal node splits its rectangle into stacked or side-by-side
;; children, weights retaining the user's proportions across
;; resizes), and the tiling that realizes goals into rectangles.
;; Routing (apps, capture), wrap policy, and the main loop stay with
;; the core until the rest of the head moves; the core reaches this
;; state through identifier-syntax facades.

(library (head)
  (export buffer make-buffer buffer?
          buffer-name buffer-name-set!
          buffer-lines buffer-lines-raw-set!
          buffer-revision buffer-revision-set!
          buffer-history buffer-history-set!
          buffer-mark-row buffer-mark-row-set!
          buffer-mark-col buffer-mark-col-set!
          buffer-marked buffer-marked-set!
          buffer-spot-row buffer-spot-row-set!
          buffer-spot-col buffer-spot-col-set!
          buffer-spot-top buffer-spot-top-set!
          buffer-line-numbers-setting buffer-line-numbers-setting-set!
          buffer-wrap-setting buffer-wrap-setting-set!
          buffer-state-id buffer-state-id-set!
          buffer-state-rev buffer-state-rev-set!
          buffers set-buffers!
          make-window window?
          window-buffer window-buffer-set!
          window-top window-top-set!
          window-topseg window-topseg-set!
          window-left window-left-set!
          window-prow window-prow-set!
          window-pcol window-pcol-set!
          window-shown-top window-shown-top-set!
          window-size window-size-set!
          window-goal window-goal-set!
          window-xoff window-xoff-set!
          window-width window-width-set!
          window-wgoal window-wgoal-set!
          window-wrap window-wrap-set!
          make-layout-split layout-split?
          layout-split-orientation
          layout-split-first layout-split-first-set!
          layout-split-second layout-split-second-set!
          layout-split-first-weight layout-split-first-weight-set!
          layout-split-second-weight layout-split-second-weight-set!
          layout-leaves layout-replace layout-parent
          layout-min-width layout-min-height weighted-first
          layout-node!
          min-window-lines
          windows set-windows! root set-root! current set-current!
          dividers set-dividers!)
  (import (rnrs) (rnrs r5rs)
          (only (chezscheme) make-parameter))

  ;;; The records ----------------------------------------------------------------

  ;; The seat's working record for a store buffer: a client-side cache
  ;; of the store's immutable text (adopted, never mutated in place)
  ;; plus what only this seat cares about -- its selection, where it
  ;; last was, its presentation toggles.  Buffer-level facts (file,
  ;; mode, read-only, disk base) are store properties, not fields:
  ;; every head, local or across the wire, reads the same truth.
  (define-record-type buffer
    (fields (mutable name)          ; a cache of the store's label
            (mutable lines buffer-lines buffer-lines-raw-set!)
            (mutable revision)      ; the seat's repaint counter
            (mutable history)       ; the seat's snapshot undo
            (mutable mark-row) (mutable mark-col)
            (mutable marked)
            ;; where point was when the buffer was last displayed
            (mutable spot-row) (mutable spot-col) (mutable spot-top)
            ;; #t/#f after a local toggle, or default to follow the global
            ;; line-numbers parameter
            (mutable line-numbers buffer-line-numbers-setting
                     buffer-line-numbers-setting-set!)
            ;; #t/#f overrides wrapping for every window showing this buffer;
            ;; default leaves wrapping as a window/global presentation choice.
            (mutable wrap buffer-wrap-setting buffer-wrap-setting-set!)
            ;; the buffer's twin in the (state) store, and the store
            ;; revision this buffer's lines last agreed with
            (mutable state-id) (mutable state-rev)))

  (define-record-type window
    (fields (mutable buffer) (mutable top)
            ;; a soft-wrapping window may start mid-line: the first
            ;; visible segment of the top line (0 elsewhere)
            (mutable topseg)
            (mutable left)
            (mutable prow) (mutable pcol)
            ;; the top row last drawn, for native scrolling
            (mutable shown-top)
            ;; text height in screen lines, written by the layout: the
            ;; goal is the user's chosen proportion, and the layout
            ;; realizes the goals in whatever space is there --
            ;; recomputed fresh each redraw, so temporary changes (a
            ;; grown echo area, a pop-up split) never drift them
            (mutable size)
            (mutable goal)
            ;; horizontal band geometry, written by the layout: the
            ;; window's first screen column and its width
            (mutable xoff)
            (mutable width)
            ;; column proportion within a band shared side by side
            (mutable wgoal)
            ;; soft-wrap long lines onto continuation rows instead of
            ;; scrolling horizontally
            (mutable wrap)))

  (define-record-type layout-split
    (fields orientation (mutable first) (mutable second)
            (mutable first-weight) (mutable second-weight)))

  ;;; The head's seat -------------------------------------------------------------

  ;; Core-linked (never reloaded in place): plain module state.  The
  ;; core initializes these at startup and reads/writes them through
  ;; its facades; set-layout-root! (app-aware) stays there.

  (define the-buffers '())      ; the seat's buffers, most recent first
  (define the-windows '())      ; every live window, layout order
  (define the-root #f)          ; the persistent split tree
  (define the-current #f)       ; the selected window
  (define the-dividers '())     ; layout output: divider rectangles

  (define (buffers) the-buffers)
  (define (set-buffers! bs) (set! the-buffers bs))
  (define (windows) the-windows)
  (define (set-windows! ws) (set! the-windows ws))
  (define (root) the-root)
  (define (set-root! node) (set! the-root node))
  (define (current) the-current)
  (define (set-current! w) (set! the-current w))
  (define (dividers) the-dividers)
  (define (set-dividers! ds) (set! the-dividers ds))

  (define min-window-lines
    ;; the smallest text height a split may leave a window
    (make-parameter 3 (lambda (v) (max 1 v))))

  ;;; Tree geometry ----------------------------------------------------------------

  (define (layout-leaves node)
    (if (layout-split? node)
        (append (layout-leaves (layout-split-first node))
                (layout-leaves (layout-split-second node)))
        (list node)))

  (define (layout-replace node old replacement)
    (cond
      [(eq? node old) replacement]
      [(layout-split? node)
       (layout-split-first-set!
         node (layout-replace (layout-split-first node) old replacement))
       (layout-split-second-set!
         node (layout-replace (layout-split-second node) old replacement))
       node]
      [else node]))

  (define (layout-parent node child)
    (and (layout-split? node)
         (if (or (eq? child (layout-split-first node))
                 (eq? child (layout-split-second node)))
             node
             (or (layout-parent (layout-split-first node) child)
                 (layout-parent (layout-split-second node) child)))))

  (define (layout-min-width node)
    (if (layout-split? node)
        (if (eq? (layout-split-orientation node) 'right)
            (+ 1 (layout-min-width (layout-split-first node))
               (layout-min-width (layout-split-second node)))
            (max (layout-min-width (layout-split-first node))
                 (layout-min-width (layout-split-second node))))
        20))

  (define (layout-min-height node)
    (if (layout-split? node)
        (if (eq? (layout-split-orientation node) 'below)
            (+ (layout-min-height (layout-split-first node))
               (layout-min-height (layout-split-second node)))
            (max (layout-min-height (layout-split-first node))
                 (layout-min-height (layout-split-second node))))
        (+ (min-window-lines) 1)))

  (define (weighted-first total minimum-first minimum-second a b)
    (min (- total minimum-second)
         (max minimum-first (quotient (* total (max 1 a))
                                      (+ (max 1 a) (max 1 b))))))

  (define (layout-node! node x y width height)
    ;; Lay out one persistent subtree. Rectangles include leaf status rows;
    ;; side-by-side nodes reserve one visible divider column.  Divider
    ;; rectangles accumulate in (dividers) for the painter and the
    ;; mouse's drag hit-testing.
    (if (not (layout-split? node))
        (begin
          (window-xoff-set! node x)
          (window-width-set! node (max 1 width))
          (window-size-set! node (max 1 (- height 1)))
          (list (list node y (max 1 (- height 1)))))
        (let* ([first (layout-split-first node)]
               [second (layout-split-second node)]
               [below? (eq? (layout-split-orientation node) 'below)]
               [total (- (if below? height width) (if below? 0 1))]
               [m1 (if below? (layout-min-height first)
                       (layout-min-width first))]
               [m2 (if below? (layout-min-height second)
                       (layout-min-width second))]
               [one (weighted-first total m1 m2
                                    (layout-split-first-weight node)
                                    (layout-split-second-weight node))]
               [two (- total one)])
          (if below?
              (begin
                (set! the-dividers
                  (cons (list 'below node x (+ y one -1) width)
                        the-dividers))
                (append (layout-node! first x y width one)
                        (layout-node! second x (+ y one) width two)))
              (begin
                (set! the-dividers
                  (cons (list 'right node (+ x one) y height)
                        the-dividers))
                (append (layout-node! first x y one height)
                        (layout-node! second (+ x one 1) y two
                                      height))))))))
