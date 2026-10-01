(import (only (foundation edoc) elibrary))
(elibrary (core endpoint)
  (export implementation runtime start!)
  (import (chezscheme))

  (edoc "The active implementation roots." (value (one-of base client)))
  (define runtime 'base)

  (edoc "Connect head wakeups to the active endpoint and return its main-thread delivery procedure. In-process base fixtures have no transport to drain."
        (wake thunk "head wakeup") (returns procedure) (effects remote))
  (define (start! wake) void)

  (edoc "Keep a shared operation's base implementation.")
  (define-syntax implementation
    (syntax-rules ()
      [(_ name formals body remote-call) body]))
)
