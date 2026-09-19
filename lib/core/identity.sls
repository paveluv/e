;; identity.sls -- owned actor values and attribution, independent of endpoints.
(import (only (edoc) elibrary))
(elibrary (identity)
  (export (rename (identity? valid?)) audience? in-audience? current call-as)
  (import (rnrs)
          (only (chezscheme) make-thread-parameter parameterize)
          (prefix (datum) datum:))

  (edoc "Whether a value is an actor identity: a list of a kind symbol and a symbol or nonempty string, then more."
        (actor any "the value")
        (returns boolean))
  (define (identity? actor)
    (and (list? actor) (>= (length actor) 2) (symbol? (car actor))
         (or (symbol? (cadr actor))
             (and (string? (cadr actor)) (> (string-length (cadr actor)) 0)))))

  (edoc "Whether a value is an audience: all, or a list of identities."
        (audience any "the value")
        (returns boolean))
  (define (audience? audience)
    (or (eq? audience 'all) (and (list? audience) (for-all identity? audience))))

  (edoc "Whether an actor is in an audience."
        (actor any "the identity")
        (audience any "the audience")
        (returns boolean))
  (define (in-audience? actor audience)
    (or (eq? audience 'all) (and (member actor audience) #t)))

  ;; Attribution context, not a capability. Callbacks may run on another
  ;; actor's thread; their identity follows the work, not that thread's head.
  (define current-actor (make-thread-parameter #f))

  (edoc "A copy of the identity whose work is running, or #f."
        (returns any))
  (define (current)
    (datum:copy (current-actor)))

  (edoc "Run a thunk attributed to an actor."
        (actor any "the identity")
        (thunk thunk "the work")
        (returns any))
  (define (call-as actor thunk)
    (parameterize ([current-actor (datum:copy actor)]) (thunk)))

)
