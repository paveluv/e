#!/usr/bin/env scheme-script

;; Rendered positions follow complete source edit chains, and callbacks
;; observe one rendering with all of its positions already installed.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (edit) init!)
             (prefix (head) head:)
             (prefix (store) store:)
             (prefix (text) text:)
             (prefix (mode) mode:)
             (prefix (md-mode) md-mode:)
             (prefix (markdown) markdown:)
             (prefix (paint) paint:)
             (prefix (kernel) kernel:))

     (define bot '(agent markdown-anchor))
     (define check test:check)
     (define (foreign! b span replacement)
       (let ([id (head:buffer-store-id b)])
         (store:edit! bot id (store:revision id) span replacement)))
     (define (find-row b line)
       (let find ([row 0])
         (cond [(= row (head:buffer-line-count b)) (error 'find-row "missing content" line)]
               [(string=? (head:buffer-line b row) line) row]
               [else (find (+ row 1))])))
     (define (fresh name local?)
       (let ([b ((if local? head:new-local-buffer! head:new-buffer!) name)])
         (head:buffer-lines-set! b '#("# Alpha" "" "# Middle" "" "# Omega"))
         (mode:choose! b "markdown")
         (head:show-buffer! b)
         (markdown:view!)
         b))
     (define (place! b first second backward?)
       (head:set-window-buffer! w2 b)
       (let ([a (find-row b first)] [z (find-row b second)])
         (goto-point! (if backward? (cons z 2) (cons a 3)))
         (head:window-top-set! w1 a)
         (head:window-topseg-set! w1 0)
         (head:window-prow-set! w2 z)
         (head:window-pcol-set! w2 2)
         (head:window-top-set! w2 z)
         (head:window-topseg-set! w2 0)
         (head:buffer-spot-row-set! b z)
         (head:buffer-spot-col-set! b 2)
         (head:buffer-spot-top-set! b a)
         (head:buffer-mark-row-set! b (if backward? a z))
         (head:buffer-mark-col-set! b (if backward? 3 2))
         (head:buffer-marked-set! b #t)))
     (define (anchors b)
       (append
         (map (lambda (w)
                (list (head:buffer-line b (head:window-prow w)) (head:window-pcol w)
                      (head:buffer-line b (head:window-top w)) (head:window-topseg w)))
              (list w1 w2))
         (list (list (head:buffer-line b (head:buffer-spot-row b)) (head:buffer-spot-col b)
                     (head:buffer-line b (head:buffer-spot-top b)))
               (list (head:buffer-line b (head:buffer-mark-row b)) (head:buffer-mark-col b)
                     (head:buffer-marked b)))))
     (define (expected first second backward?)
       (list (list (if backward? second first) (if backward? 2 3) first 0)
             (list second 2 second 0)
             (list second 2 first)
             (list (if backward? first second) (if backward? 3 2) #t)))
     (define (refresh! b) ((head:app-refresh! (head:app-of b))))

     (md-mode:init!)
     (parameterize ([kernel:registering-module 'markdown-anchor]) (markdown:init!))
     (define w1 (head:current-window))
     (head:window-width-set! w1 80)
     (head:window-size-set! w1 12)
     (define source (fresh "anchors.md" #f))
     (define view (head:current-buffer))
     (define w2 (head:make-window view 4 0 0 4 2 12 80 80 'default))
     (head:set-layout-root! (head:make-layout-split 'right w1 w2 1 1))
     (goto-point! '(2 . 3))
     (head:window-top-set! w1 2)
     (head:buffer-spot-row-set! view 4)
     (head:buffer-spot-col-set! view 2)
     (head:buffer-spot-top-set! view 2)
     (head:buffer-mark-row-set! view 4)
     (head:buffer-mark-col-set! view 2)
     (head:buffer-marked-set! view #t)

     ;; Two disjoint changes must not be inferred as one large replacement
     ;; that destroys the unchanged middle's source anchor.
     (foreign! source (text:make-span 0 0 0 0) '("# Before" "" ""))
     (foreign! source (text:make-span 6 7 6 7) '("!"))
     (head:before-frame!)
     (check 'foreign-insertion-keeps-middle
            (head:buffer-line view (head:window-prow w1)) "Middle")
     (check 'foreign-insertion-keeps-other-window
            (head:buffer-line view (head:window-prow w2)) "Omega!")
     (check 'foreign-insertion-keeps-all-anchors (anchors view) (expected "Middle" "Omega!" #f))
     (foreign! source (text:make-span 0 0 2 0) '(""))
     (head:before-frame!)
     (check 'foreign-deletion-keeps-all-anchors (anchors view) (expected "Middle" "Omega!" #f))
     (place! view "Middle" "Omega!" #t)
     (foreign! source (text:make-span 0 0 0 0) '("# Before" "" ""))
     (head:before-frame!)
     (check 'foreign-edit-keeps-backward-selection (anchors view) (expected "Middle" "Omega!" #t))

     ;; A view refresh must not adopt source text ahead of the head.  Its
     ;; row map is also used by the command returning to that source.
     (define before-unadopted (head:buffer-lines view))
     (foreign! source (text:make-span 0 0 0 0) '("# Pending" "" ""))
     (refresh! view)
     (check 'refresh-uses-adopted-source (head:buffer-lines view) before-unadopted)
     (markdown:edit!)
     (check 'toggle-before-adoption-finds-current-source-row
            (head:buffer-line source (car (point))) "# Omega!")
     (head:before-frame!)
     (check 'source-point-follows-later-adoption
            (head:buffer-line source (car (point))) "# Omega!")

     (for-each
       (lambda (local?)
         (let* ([source (fresh "private-or-shared.md" local?)] [view (head:current-buffer)]
                [source-window (head:make-window source 0 0 0 0 0 12 80 80 'default)])
           (head:set-windows! (list w1 w2 source-window))
           (place! view "Middle" "Omega" #f)
           (head:with-buffer source (goto-point! '(0 . 0)) (insert-text! "# Before\n\n"))
           (head:before-frame!)
           (check 'own-edit-follows-source (anchors view) (expected "Middle" "Omega" #f))
           (head:with-buffer source (undo!))
           (head:before-frame!)
           (check 'undo-follows-source (anchors view) (expected "Middle" "Omega" #f))
           (place! view "Middle" "Omega" #t)
           (head:with-buffer source (redo!))
           (head:before-frame!)
           (check 'redo-keeps-backward-selection (anchors view) (expected "Middle" "Omega" #t))
           (head:with-buffer source (undo!))
           (head:before-frame!)
           (check 'undo-keeps-backward-selection (anchors view) (expected "Middle" "Omega" #t))

           ;; Same rendered characters, different heading face.  Rendered
           ;; column 3 is not source column 3 after changing markup.
           (let* ([old (head:buffer-lines view)] [revision (head:buffer-revision view)]
                  [styles (vector-ref (head:buffer-fact view 'markdown-rendering #f) 0)])
             (head:store-edit! source (text:make-span 2 0 2 0) '("#"))
             (head:before-frame!)
             (check 'markup-keeps-rendered-text (head:buffer-lines view) old)
             (check 'markup-rebuilds-styles
                    (equal? (vector-ref (head:buffer-fact view 'markdown-rendering #f) 0) styles) #f)
             (check 'style-only-change-invalidates-paint (> (head:buffer-revision view) revision) #t)
             (check 'markup-keeps-view-columns (anchors view) (expected "Middle" "Omega" #t)))

           ;; Replacing content with equal bytes still removes its old
           ;; identity.  Follow deletion and insertion even if a text-only
           ;; cache check would say that nothing changed.
           (place! view "Middle" "Omega" #f)
           (let ([old (head:buffer-lines source)])
             (head:store-edit! source (text:make-span 2 0 4 0) '(""))
             (head:store-edit! source (text:make-span 2 0 2 0) '("## Middle" "" ""))
             (check 'equal-text-history-fixture (head:buffer-lines source) old)
             (head:before-frame!)
             (check 'equal-text-history-follows-surviving-content
                    (anchors view) (expected "Omega" "Omega" #f)))

           ;; A row anchor can temporarily land inside a joined line.
           ;; Keep its column through the entire chain, then project its
           ;; final row; dropping that column after each edit loses it.
           (place! view "Middle" "Omega" #f)
           (head:store-edit! source (text:make-span 0 7 2 0) '(""))
           (head:store-edit! source (text:make-span 0 7 0 7) '("" ""))
           (head:before-frame!)
           (check 'joined-then-split-anchor-follows-full-chain
                  (anchors view) (expected "Middle" "Omega" #f))

           ;; A baseline plus an available suffix is still missing the
           ;; old view's basis.  Clamp old source rows; never replay just
           ;; that suffix or invent an edit between unrelated baselines.
           (head:buffer-lines-set! source '#("# Alpha" "" "# Middle" "" "# Omega"))
           (head:before-frame!)
           (place! view "Middle" "Omega" #f)
           (if local?
               (head:buffer-lines-set! source '#("# Reset" "" "# Other"))
               (store:reset! bot (head:buffer-store-id source) '("# Reset" "" "# Other")))
           ((if local? head:store-edit! foreign!) source (text:make-span 0 0 0 0) '("# New" "" ""))
           (head:before-frame!)
           (check 'reset-clamps-source-rows-without-partial-replay
                  (anchors view) (expected "Reset" "Other" #f))

           ;; Keep adopting edits while the view is stale, exhausting
           ;; the head's bounded provenance for either source owner.
           (head:buffer-lines-set! source '#("# Alpha" "" "# Middle" "" "# Omega"))
           (head:before-frame!)
           (place! view "Middle" "Omega" #f)
           (do ([i 0 (+ i 1)]) ((= i 257))
             (head:store-edit! source (text:make-span 0 0 0 0) '("" "")))
           (head:before-frame!)
           (check 'expired-source-history-clamps-instead-of-guessing
                  (head:buffer-line view (head:window-prow w1)) "")
           (check 'expired-source-history-adopts-current-rendering
                  (vector-ref (head:buffer-fact view 'markdown-rendering #f) 8)
                  (caddr (head:edit-basis source)))
           (head:set-windows! (list w1 w2))))
       '(#f #t))

     ;; Reentrant replacement advances the source and refits a table.
     ;; Both the callback's first observation and the final state must
     ;; contain matching text, styles, source provenance, and positions.
     (define table (fresh "reentrant-table.md" #f))
     (define table-view (head:current-buffer))
     (head:buffer-lines-set! table
       '#("|alpha beta gamma delta epsilon|x|" "|-|-|" "|long entry|y|"
          "" "# After table" "" "# Tail"))
     (head:before-frame!)
     (place! table-view "After table" "Tail" #f)
     (define before-row (head:window-prow w1))
     (define entered #f)
     (define coherent #f)
     (define once #t)
     (dynamic-wind
       (lambda ()
         (head:set-repaint-hook!
           (lambda ()
             (paint:invalidate-screen-cache!)
             (when once
               (set! once #f)
               (set! entered (anchors table-view))
               (let ([r (head:buffer-fact table-view 'markdown-rendering #f)])
                 (set! coherent
                   (and (eq? (vector-ref r 3) (head:buffer-lines table))
                        (= (vector-length (vector-ref r 0)) (head:buffer-line-count table-view))
                        (string=? (vector-ref (vector-ref r 3)
                                              (vector-ref (vector-ref r 2) (head:window-prow w1)))
                                  "# After table"))))
               (head:store-edit! table (text:make-span 0 0 0 0) '("# Later" "" ""))
               (head:window-width-set! w1 24)
               (head:before-frame!)))))
       (lambda ()
         (head:store-edit! table (text:make-span 0 0 0 0) '("# Before" "" ""))
         (head:before-frame!))
       (lambda () (head:set-repaint-hook! (lambda () (paint:invalidate-screen-cache!)))))
     (check 'repaint-observes-installed-anchors entered (expected "After table" "Tail" #f))
     (check 'repaint-observes-coherent-rendering coherent #t)
     (check 'reentrant-render-keeps-newest-text (head:buffer-line table-view 0) "Later")
     (check 'reentrant-render-keeps-newest-anchors (anchors table-view) (expected "After table" "Tail" #f))
     (check 'reentrant-refit-moves-the-table-end (> (head:window-prow w1) (+ before-row 4)) #t)
     (check 'reentrant-render-keeps-newest-basis
            (vector-ref (head:buffer-fact table-view 'markdown-rendering #f) 8)
            (caddr (head:edit-basis table)))

     ;; Rebinding the current renderer preserves source anchors on refit.
     (head:window-width-set! w1 80)
     (kernel:retract-module! 'markdown-anchor)
     (parameterize ([kernel:registering-module 'markdown-anchor]) (markdown:init!))
     (head:before-frame!)
     (check 'renderer-rebind-preserves-anchors (anchors table-view) (expected "After table" "Tail" #f))
     (check 'renderer-rebind-keeps-provenance
            (vector-ref (head:buffer-fact table-view 'markdown-rendering #f) 8)
            (caddr (head:edit-basis table)))

     ;; A new companion has no row cache. Restore from the source revision,
     ;; then refit at a different width through the ordinary anchor path.
     (let ([old table-view] [first (head:window-index w1)] [second (head:window-index w2)])
       (head:checkpoint!)
       (head:forget-buffer! old)
       (foreign! table (text:make-span 0 0 0 0) '("# Offline" "" ""))
       (let ([revision (store:revision (head:buffer-store-id table))])
         (check 'resume-rebuilds-the-source-companion (head:resume!) #t)
         (set! table-view (markdown:companion table))
         (set! w1 (head:window-numbered first))
         (set! w2 (head:window-numbered second))
         (head:window-width-set! w1 24)
         (head:before-frame!)
         (check 'resumed-companion-refits-with-all-source-anchors
           (list (not (eq? old table-view)) (anchors table-view)
                 (store:revision (head:buffer-store-id table)))
           (list #t (expected "After table" "Tail" #f) revision))))

     (test:finish! 'markdown-anchor)))
