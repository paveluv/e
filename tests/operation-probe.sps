;; Shared library source copied into extension and real-wire fixtures.
(import (only (foundation edoc) elibrary))
(elibrary (operation-probe)
  (export attached? broken init! remember! snapshot)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (core operation) operation:))

  (edoc "Whether the operation has an authenticated attachment." (returns boolean))
  (define-operation (attached?) (and (operation:attachment) #t))

  (edoc "Read the authenticated caller, implementation generation and retained value."
        (returns (values actor integer datum)))
  (define-operation (snapshot)
    (let ()
      (import (prefix (state actor) actor:) (prefix (operation-support) support:))
      (values (actor:current) support:generation
        (unbox (kernel:persistent-cell 'operation-probe (lambda () #f))))))

  (edoc "Keep an owned value; this command deliberately returns void."
        (value datum "portable state"))
  (define-operation (remember! value)
    (set-box! (kernel:persistent-cell 'operation-probe (lambda () #f)) value))

  (edoc "Exercise result-contract failures and zero values."
        (kind (one-of native count throw zero) "outcome") (returns (values)))
  (define-operation (broken kind)
    (case kind
      [(native) void]
      [(count) (values 1 2)]
      [(throw) (error 'probe "implementation failure")]
      [else (values)]))

  (edoc "Publish the fixture's operations through normal module ownership.")
  (define (init!)
    (operation:register! 'operation-probe:attached? attached? 'read)
    (operation:register! 'operation-probe:snapshot snapshot 'read)
    (operation:register! 'operation-probe:remember! remember! 'control)
    (operation:register! 'operation-probe:broken broken 'read))
)
