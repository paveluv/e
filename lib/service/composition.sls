;; The transport contract is shared; persistent state belongs only to the base.
(import (only (foundation edoc) elibrary))
(elibrary (service composition)
  (export acquire! admit! finish! init!)
  (import (chezscheme) (prefix (core operation) operation:) (prefix (state model) model:))

  (edoc "Acquire this named head's persistent profile and canonical root descriptors. The binding value distinguishes uninitialized state from an initialized empty root. A new attachment fences the former view leases; repeating acquisition on this attachment is idempotent."
        (profile string "nonempty profile name") (returns (values list list)))
  (define-operation (acquire! profile)
    (import (prefix (service root-binding) root-binding:))
    (root-binding:acquire! profile))

  (edoc "Admit a prepared composition against its binding. Disposition is retire or an existing persistent owner referencing the old root. Retirement atomically closes the old owned model graph and records output disposal for finish!. Pending cleanup refuses further replacements. Borrowed resources survive. Return status, binding and coherent view descriptors; failed admission changes no ownership."
        (expected list "acquired binding envelope") (candidate (or model #f) "next root or explicit empty")
        (basis list "prepared (view . descriptor) rows") (disposition (or model #f (one-of retire)) "retire, retaining owner, or false when no old root is replaced")
        (returns (values symbol datum list)))
  (define-operation (admit! expected candidate basis disposition)
    (import (prefix (service root-binding) root-binding:))
    (root-binding:admit! expected candidate basis disposition))

  (edoc "Finish the admitted root's owned output disposal after releasing the old head mount. The binding guards the attachment; cleanup is idempotent and also resumes on departure or base recovery. Return status and the current binding."
        (expected list "admitted binding snapshot") (returns (values symbol datum)))
  (define-operation (finish! expected)
    (import (prefix (service root-binding) root-binding:))
    (root-binding:finish! expected))

  (edoc "Register the composition operations through the module lifecycle.")
  (define (init!)
    (operation:register! 'composition:acquire! acquire! 'control)
    (operation:register! 'composition:admit! admit! 'control)
    (operation:register! 'composition:finish! finish! 'control))
)
