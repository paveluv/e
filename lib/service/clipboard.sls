;; The copy buffer is an ordinary journalled document owned by one actor.
(import (only (foundation edoc) elibrary))
(elibrary (service clipboard)
  (export init! open!)
  (import (chezscheme) (prefix (core operation) operation:) (prefix (state store) store:))

  (edoc "Find this actor's copy document, optionally creating an empty one. Its identity and undo history survive head detach; other actors' copies are independent. This operation never resets an existing document."
    (create? boolean "create when absent") (returns (or buffer #f)))
  (define-operation (open! create?)
    (import (prefix (state actor) actor:) (prefix (state store) store:))
    (let ([actor (actor:current)])
      (or (store:publication actor 'clipboard)
        ;; Adopt a pre-composition copy document without rewriting its text
        ;; or journal. The restricted audience is part of its identity.
        (find (lambda (id) (and (store:property id 'copy #f)
                             (equal? (store:property id 'audience #f) (list actor))))
          (store:buffer-list))
        (and create?
          (or (store:publish! actor 'clipboard "*copy*" '("")
                (list '(copy . #t) (cons 'audience (list actor)) '(disposable . #t) '(trailing . #f)) #f)
            (store:publication actor 'clipboard))))))

  (edoc "Register the actor-owned copy document operation.")
  (define (init!) (operation:register! 'clipboard:open! open! 'control)))
