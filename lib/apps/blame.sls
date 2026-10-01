;; Attribution is base data; expiring source annotations are a head effect.
(import (only (foundation edoc) elibrary))
(elibrary (apps blame)
  (export at-point! describe init! tint-seconds)
  (import (chezscheme) (prefix (foundation text) text:)
          (prefix (head edit) edit:) (prefix (head editor) editor:)
          (prefix (head editor-state) editor-state:) (prefix (head head) head:)
          (prefix (head style) style:) (prefix (head text-control) text-control:)
          (prefix (head text-source) text-source:) (prefix (state store) store:))

  (edoc "How long another actor's newly adopted edit stays tinted, in seconds; zero disables tinting. Fractional values are accepted."
        (value number))
  (define tint-seconds
    (make-parameter 8 (lambda (n) (unless (and (real? n) (>= n 0)) (error 'tint-seconds "expected nonnegative seconds")) n)))
  (define faces '#(blame-1 blame-2 blame-3 blame-4 blame-5 blame-6))
  ;; Shared by different widths of the same acquired source, bounded to eight
  ;; surviving spans. Mirrors own lifetime; no buffer/window catalogue scan.
  (define observed (make-weak-eq-hashtable))
  (define (expires-at now)
    (let-values ([(seconds nanos) (div-and-mod (exact (round (* (tint-seconds) 1000000000))) 1000000000)])
      (add-duration now (make-time 'time-duration nanos seconds))))
  (define (note-edit ink actor delta now)
    (let* ([kept (filter values
                   (map (lambda (o)
                          (let ([s (text:rebase-span (car o) delta)]) (and s (list s (cadr o) (caddr o))))) ink))]
           [start (text:span-start (text:delta-span delta))] [end (text:delta-new-end delta)]
           [span (text:make-span (car start) (cdr start) (car end) (cdr end))])
      (if (or (eq? (car actor) 'app) (equal? actor head:ui-actor) (zero? (tint-seconds)) (text:span-empty? span)) kept
        (let ([ink (cons (list span (vector-ref faces (mod (equal-hash actor) (vector-length faces))) (expires-at now)) kept)])
          (if (> (length ink) 8) (list-head ink 8) ink)))))
  (define (refresh! source)
    (let* ([old (hashtable-ref observed source #f)] [now (current-time 'time-monotonic)])
      (let-values ([(lines revision changes) (text-source:snapshot source (and old (car old)))])
        (let* ([kept (if (or (not old) (zero? (tint-seconds))) '()
                       (filter (lambda (o) (time<? now (caddr o))) (cdr old)))]
               [ink (if changes (fold-left (lambda (out c) (note-edit out (cadr c) (caddr c) now)) kept changes) '())])
          (hashtable-set! observed source (cons revision ink))
          (values (list (text-source:id source) revision
                    (map (lambda (o) (list (text:span->datum (car o)) (cadr o))) (reverse ink)))
            (fold-left (lambda (earliest o) (if (or (not earliest) (time<? (caddr o) earliest)) (caddr o) earliest)) #f ink))))))
  (define (adopt! source basis lines revision changes)
    (when (hashtable-contains? observed source) (let-values ([(batch deadline) (refresh! source)]) (void))))

  (edoc "Describe recent authorship at an explicit document position. Return text suitable for a host's label, pop-up or message; the bounded base delta journal owns the attribution."
        (document buffer "shared document identity") (position position "logical source position") (returns string) (public))
  (define (describe document position)
    (cond [(find (lambda (entry) (or (text:contains? (car entry) position) (text:position=? position (text:span-start (car entry)))))
             (store:blame document 64))
           => (lambda (hit) (format "~a wrote this at revision ~a" (cadr hit) (caddr hit)))]
      [else "No recent edit here (blame reaches the delta log; resets clear it)"]))

  (edoc "Report recent authorship at this editor view's logical caret."
        (receiver id (view editor)) (id model "mounted editor") (public))
  (define (at-point! id)
    (let-values ([(source d) (text-control:context id 'editor 'current)])
      (let ([ps (editor-state:points (text-control:mirror source) (text-control:revision source) d)])
        (edit:set-message! (if ps (describe (text-source:id (text-control:mirror source)) (car ps)) "Attribution position is unavailable")))))

  (edoc "Register source-scoped attribution effects and their semantic faces." (public))
  (define (init!)
    (editor:register-effect! refresh!)
    (text-source:observe! adopt!)
    (for-each (lambda (face background) (style:set! face (list (list 'background background))))
      (vector->list faces) '(17 22 52 54 23 58))))
