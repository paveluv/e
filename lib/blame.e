;; blame.e -- in-UI attribution: who wrote what, painted and asked.
;;
;; Blame is recent-memory attribution over the store's delta log
;; (state:blame): bounded by delta-log-limit, cleared by resets --
;; deep history stays git's job.  Two consumers:
;;
;;   - a transient tint: another actor's edit paints its written span
;;     in that actor's face (a stable hash into a small palette, so
;;     (agent claude 3) is always the same color) and fades after
;;     (blame-tint-seconds).  Foreign edits already appear without a
;;     keypress; now they appear identified.
;;
;;   - (blame-at-point!): who recently wrote the text at point,
;;     reported in the echo area.
;;
;; The module holds no truth: overlays are derived from the store's
;; subscription events, rebased through every edit like span marks,
;; and dropped by resets.  All bookkeeping runs on the main thread
;; (events marshal through head:run-on-main!), so there are no locks.

(library (blame)
  (export init! (rename (blame-at-point! at-point!)) (rename (blame-tint-seconds tint-seconds)))
  (import (rnrs)
          (only (chezscheme)
                box unbox set-box! format make-parameter void
                fork-thread sleep make-time current-time time-second)
          (prefix (edit) edit:)
          (prefix (paint) paint:)
          (prefix (head) head:)
          (prefix (style) style:)
          (prefix (state) state:)
          (prefix (text) text:)
          (prefix (doc) doc:))

  (define blame-tint-seconds
    ;; how long another actor's edit stays tinted (0 turns tinting off)
    (make-parameter 8))

  (define per-buffer-cap 8)

  ;; the local head's own edits are not news to the local head
  (define local-head '(head main))

  (define faces '#(blame-1 blame-2 blame-3 blame-4 blame-5 blame-6))

  (define (actor-face actor)
    (vector-ref faces (mod (equal-hash actor) (vector-length faces))))

  ;;; The live overlays -------------------------------------------------------

  ;; (#(buffer-id span actor deadline) ...), newest first, main-thread
  ;; only.
  (define overlays (box '()))

  (define (now-seconds) (time-second (current-time 'time-monotonic)))

  (define (rebase-lenient s d)
    (or (text:rebase-span s d)
        (let ([start (text:rebase-position (text:span-start s) d)]
              [end (text:rebase-position (text:span-end s) d 'stay)])
          (text:make-span (car start) (cdr start) (car end) (cdr end)))))

  (define (note-event! event)
    (case (car event)
      [(edit)
       (let ([id (cadr event)]
             [actor (list-ref event 3)]
             [d (list-ref event 4)])
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
         (when (and (not (equal? actor local-head))
                    (> (blame-tint-seconds) 0))
           (add-overlay! id actor d)))]
      [(reset delete)
       ;; a new baseline (or no buffer at all): nothing left to tint
       (let ([id (cadr event)])
         (set-box! overlays
                   (filter (lambda (o) (not (eqv? (vector-ref o 0) id)))
                           (unbox overlays))))]
      [else (void)]))

  (define (add-overlay! id actor d)
    (let* ([s (text:span-start (text:delta-span d))]
           [e (text:delta-new-end d)]
           [span (text:make-span (car s) (cdr s) (car e) (cdr e))])
      (unless (text:span-empty? span)   ; a pure deletion leaves no ink
        (set-box! overlays
                  (capped id (cons (vector id span actor
                                           (+ (now-seconds)
                                              (blame-tint-seconds)))
                                   (unbox overlays))))
        ;; wake a frame after the tint should fade; wakes coalesce
        (fork-thread
          (lambda ()
            (sleep (make-time 'time-duration 100000000
                              (blame-tint-seconds)))
            (head:wake-main!))))))

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
    (find (lambda (b) (eqv? (head:buffer-state-id b) id))
          (edit:buffer-list)))

  (define (blame-highlights)
    (let* ([now (now-seconds)]
           [live (filter (lambda (o) (< now (vector-ref o 3)))
                         (unbox overlays))])
      (set-box! overlays live)
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
    (let* ([b (edit:current-buffer)]
           [id (head:buffer-state-id b)]
           [p (edit:point)])
      (edit:set-message!
        (cond
          [(not id) "This buffer has no state twin"]
          [(find (lambda (entry)
                   (let ([s (car entry)])
                     (or (text:contains? s p)
                         (text:position=? p (text:span-start s)))))
                 (state:blame id 64))
           => (lambda (hit)
                (format "~a wrote this at revision ~a"
                        (cadr hit) (caddr hit)))]
          [else "No recent edit here (blame reaches the delta log; resets clear it)"]))
      (void)))

  ;;; Wiring ------------------------------------------------------------------

  (define (init!)
    ;; the subscription is registry-owned like every registration:
    ;; reloading this module retracts it before init! subscribes afresh
    (state:subscribe!
      #f
      (lambda (event)
        (head:run-on-main! (lambda () (note-event! event)))))
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
         "Report in the echo area which actor most recently wrote the text at point, from the buffer's attributed edit log (state:blame). Reach is the delta log (256 edits); a buffer reset clears it -- deep history stays git's job.")
        ((blame:tint-seconds)
         (("parameter" . "(blame:tint-seconds [seconds])")) "number"
         ("(blame)") blame "Blame" #f
         "How long another actor's fresh edit stays tinted in that actor's color (default 8; 0 turns tinting off).")))))
