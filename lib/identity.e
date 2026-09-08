;; identity.e -- owned actor values and attribution, independent of endpoints.
(library (identity)
  (export (rename (identity? valid?)) audience? in-audience? current call-as)
  (import (rnrs) (only (chezscheme) make-thread-parameter parameterize)
          (prefix (datum) datum:))

  (define (identity? actor)
    (and (list? actor) (>= (length actor) 2) (symbol? (car actor))
         (or (symbol? (cadr actor))
             (and (string? (cadr actor)) (> (string-length (cadr actor)) 0)))))

  (define (audience? audience)
    (or (eq? audience 'all) (and (list? audience) (for-all identity? audience))))

  (define (in-audience? actor audience)
    (or (eq? audience 'all) (and (member actor audience) #t)))

  ;; Attribution context, not a capability. Callbacks may run on another
  ;; actor's thread; their identity follows the work, not that thread's head.
  (define current-actor (make-thread-parameter #f))

  (define (current) (datum:copy (current-actor)))

  (define (call-as actor thunk)
    (parameterize ([current-actor (datum:copy actor)]) (thunk)))

)
