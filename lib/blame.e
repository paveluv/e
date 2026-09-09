;; blame.e -- in-UI attribution: who wrote what, painted and asked.
;;
;; Blame is recent-memory attribution over the store's delta log
;; (store:blame): bounded by delta-log-limit, cleared by resets --
;; deep history stays git's job.  Two consumers:
;;
;;   - a transient tint: another actor's edit paints its written span
;;     in that actor's face (a stable hash into a small palette, so
;;     (agent claude 3) is always the same color) and fades after
;;     (blame:tint-seconds).  Foreign edits already appear without a
;;     keypress; now they appear identified.
;;     App actors publish ordinary output without a tint; their authorship
;;     remains available through the same attribution queries.
;;
;;   - (blame:at-point!): who recently wrote the text at point,
;;     reported in the echo area.
;;
;; The module holds no truth: overlays are derived from the store's
;; adopted revision chain, rebased through every edit like span marks,
;; and dropped by resets. All bookkeeping runs before paint on the main
;; thread; a stalled head retains no independent raw-event backlog.

(library (blame)
  (export init! (rename (blame-at-point! at-point!)) (rename (blame-tint-seconds tint-seconds)))
  (import (rnrs)
          (only (chezscheme)
                box unbox set-box! format make-parameter void
                make-time current-time add-duration time<? make-weak-eq-hashtable)
          (except (edit) init!)
          (prefix (paint) paint:)
          (prefix (head) head:)
          (prefix (style) style:)
          (prefix (store) store:)
          (prefix (text) text:)
          (prefix (doc) doc:))

  (define blame-tint-seconds
    ;; how long another actor's edit stays tinted (0 turns tinting off)
    (make-parameter 8))

  (define per-buffer-cap 8)

  (define faces '#(blame-1 blame-2 blame-3 blame-4 blame-5 blame-6))

  (define (actor-face actor)
    (vector-ref faces (mod (equal-hash actor) (vector-length faces))))

  ;;; The live overlays -------------------------------------------------------

  ;; (#(buffer-id span actor deadline) ...), newest first, main-thread
  ;; only.
  (define overlays (box '()))
  (define observed (make-weak-eq-hashtable))

  (define (expires-at)
    (let-values ([(seconds nanos)
                  (div-and-mod (exact (round (* (blame-tint-seconds) 1000000000))) 1000000000)])
      (add-duration (current-time 'time-monotonic) (make-time 'time-duration nanos seconds))))

  (define (rebase-lenient s d)
    (or (text:rebase-span s d)
        (let ([start (text:rebase-position (text:span-start s) d)]
              [end (text:rebase-position (text:span-end s) d 'stay)])
          (text:make-span (car start) (cdr start) (car end) (cdr end)))))

  (define (note-edit! id actor d)
    ;; tints follow the text
    (set-box! overlays
              (map (lambda (o)
                     (if (eqv? (vector-ref o 0) id)
                         (vector id
                                 (rebase-lenient (vector-ref o 1) d)
                                 (vector-ref o 2)
                                 (vector-ref o 3))
                         o))
                   (unbox overlays)))
    ;; App output still rebases existing tints above, without creating one.
    (when (and (not (eq? (car actor) 'app))
               (not (equal? actor head:ui-actor))
               (> (blame-tint-seconds) 0))
      (add-overlay! id actor d)))

  (define (refresh!)
    (let ([buffers (filter (lambda (b) (head:buffer-store-id b)) (head:buffers))]
          [now (current-time 'time-monotonic)])
      (set-box! overlays
        (filter (lambda (o)
                  (and (time<? now (vector-ref o 3))
                       (exists (lambda (b) (eqv? (head:buffer-store-id b) (vector-ref o 0))) buffers)))
          (unbox overlays)))
      (for-each
        (lambda (b)
          (let* ([id (head:buffer-store-id b)] [basis (hashtable-ref observed b #f)])
            (let-values ([(text revision changes) (head:snapshot-since b basis)])
              (hashtable-set! observed b revision)
              (if changes
                  (for-each (lambda (change) (note-edit! id (cadr change) (caddr change))) changes)
                  (set-box! overlays
                    (filter (lambda (o) (not (eqv? (vector-ref o 0) id))) (unbox overlays))))))) buffers)
      (for-each (lambda (o) (head:request-frame-at! (vector-ref o 3))) (unbox overlays))))

  (define (add-overlay! id actor d)
    (let* ([s (text:span-start (text:delta-span d))]
           [e (text:delta-new-end d)]
           [span (text:make-span (car s) (cdr s) (car e) (cdr e))])
      (unless (text:span-empty? span)   ; a pure deletion leaves no ink
        (set-box! overlays
                  (capped id (cons (vector id span actor (expires-at))
                                   (unbox overlays)))))))

  (define (capped id entries)
    ;; keep the newest per-buffer-cap overlays of one buffer
    (let loop ([entries entries] [kept 0])
      (cond [(null? entries) '()]
            [(not (eqv? (vector-ref (car entries) 0) id))
             (cons (car entries) (loop (cdr entries) kept))]
            [(< kept per-buffer-cap)
             (cons (car entries) (loop (cdr entries) (+ kept 1)))]
            [else (loop (cdr entries) kept)])))

  ;;; Painting ----------------------------------------------------------------

  (define (span-ranges b span face)
    ;; scoped highlighter entries for a span: (buffer row start end
    ;; style), row by row; the painter clips columns to the line
    (let* ([s (text:span-start span)]
           [e (text:span-end span)]
           [wide 100000])
      (cond
        [(= (car s) (car e))
         (list (list b (car s) (cdr s) (cdr e) face))]
        [else
         (let loop ([row (car s)] [acc '()])
           (if (> row (car e))
               (reverse acc)
               (loop (+ row 1)
                     (cons (cond [(= row (car s))
                                  (list b row (cdr s) wide face)]
                                 [(= row (car e))
                                  (list b row 0 (cdr e) face)]
                                 [else (list b row 0 wide face)])
                           acc))))])))

  (define (buffer-of-id id)
    (find (lambda (b) (eqv? (head:buffer-store-id b) id))
          (buffer-list)))

  (define (blame-highlights)
    (let* ([now (current-time 'time-monotonic)]
           [live (filter (lambda (o) (time<? now (vector-ref o 3)))
                         (unbox overlays))])
      (fold-left
        (lambda (acc o)
          (let ([b (buffer-of-id (vector-ref o 0))])
            (if b
                (append (span-ranges b (vector-ref o 1)
                                     (actor-face (vector-ref o 2)))
                        acc)
                acc)))
        '() live)))

  ;;; Asking ------------------------------------------------------------------

  (define (blame-at-point!)
    ;; who recently wrote the text at point, from the store's log
    (let* ([b (current-buffer)]
           [id (head:buffer-store-id b)]
           [p (point)])
      (set-message!
        (cond
          [(not id) "This buffer has no store twin"]
          [(find (lambda (entry)
                   (let ([s (car entry)])
                     (or (text:contains? s p)
                         (text:position=? p (text:span-start s)))))
                 (store:blame id 64))
           => (lambda (hit)
                (format "~a wrote this at revision ~a"
                        (cadr hit) (caddr hit)))]
          [else "No recent edit here (blame reaches the delta log; resets clear it)"]))
      (void)))

  ;;; Wiring ------------------------------------------------------------------

  (define (init!)
    (head:add-pre-redraw-hook! refresh!)
    (paint:add-highlighter! blame-highlights)
    ;; muted per-actor backgrounds, overridable from config.e
    (style:set! 'blame-1 '((background 17)))   ; deep blue
    (style:set! 'blame-2 '((background 22)))   ; deep green
    (style:set! 'blame-3 '((background 52)))   ; deep red
    (style:set! 'blame-4 '((background 54)))   ; deep purple
    (style:set! 'blame-5 '((background 23)))   ; deep teal
    (style:set! 'blame-6 '((background 58)))   ; olive
    (doc:register!
      '(((blame:at-point!)
         (("procedure" . "(blame:at-point!)")) "void"
         ("(blame)") blame "Blame" #f
         "Report in the echo area which actor most recently wrote the text at point, from the buffer's attributed edit log (store:blame). Reach is the delta log (256 edits); a buffer reset clears it -- deep history stays git's job.")
        ((blame:tint-seconds)
         (("parameter" . "(blame:tint-seconds [seconds])")) "number"
         ("(blame)") blame "Blame" #f
         "How long another actor's fresh edit stays tinted in that actor's color (default 8; 0 prevents new tints). App output does not create tints.")))))
