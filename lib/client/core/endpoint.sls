(import (only (foundation edoc) elibrary))
(elibrary (core endpoint)
  (export implementation runtime start!)
  (import (chezscheme)
          (prefix (core client) client:))

  (edoc "The active implementation roots." (value (one-of base client)))
  (define runtime 'client)

  (edoc "Connect transport wakeups to the head and return its main-thread delivery procedure. Event delivery does not depend on a window or text-store reader."
        (wake thunk "head wakeup") (returns procedure))
  (define (start! wake) (client:set-wake! wake) client:pump!)

  (edoc "Replace a shared operation's body with one request, retaining its positional signature. Interrupted calls are never replayed.")
  (define-syntax implementation
    (syntax-rules ()
      [(_ name (argument ...) body remote-call)
       (lambda (argument ...) (remote-call name (list argument ...) client:request))]))
)
