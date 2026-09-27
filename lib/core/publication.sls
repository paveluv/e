;; One in-flight state and one latest replacement. Used by checkpoints and
;; widget interaction publication; callbacks never read live head state.
(import (only (foundation edoc) elibrary))
(elibrary (core publication)
  (export changed? flush! make! submit!)
  (import (chezscheme))

  (define-record-type writer
    (fields send! notify! own lock ready
            (mutable pending) (mutable active?) (mutable wanted)
            (mutable saved) (mutable failure)))

  (define (deliver! writer)
    (guard (ex [else
                (with-mutex (writer-lock writer)
                  (writer-failure-set! writer ex)
                  (writer-pending-set! writer #f)
                  (writer-active?-set! writer #f)
                  (condition-broadcast (writer-ready writer)))
                ((writer-notify! writer))])
      (let loop ()
        (let ([state
               (with-mutex (writer-lock writer)
                 (let wait ()
                   (unless (writer-pending writer)
                     (condition-wait (writer-ready writer) (writer-lock writer))
                     (wait)))
                 (let ([state (writer-pending writer)])
                   (writer-pending-set! writer #f)
                   (writer-active?-set! writer #t)
                   state))])
          (unless (equal? state (writer-saved writer))
            ((writer-send! writer) state (writer-saved writer)))
          (with-mutex (writer-lock writer)
            (writer-saved-set! writer state)
            (writer-active?-set! writer #f)
            (condition-broadcast (writer-ready writer)))
          (loop)))))

  (edoc "Make a process-lifetime publisher. Failure wakes its owner and is raised by the next offer, change check or fence; delivery is never retried implicitly."
        (send! procedure "(send! owned-state previous-acknowledged-state), returning after acknowledgement; do not mutate either argument")
        (notify! thunk "wake the owner on failure")
        (own procedure "copy a submitted state; #f is reserved") (returns any))
  (define (make! send! notify! own)
    (let ([writer (make-writer send! notify! own (make-mutex) (make-condition) #f #f #f #f #f)])
      (fork-thread (lambda () (deliver! writer))) writer))

  (edoc "Whether state differs from the last submitted snapshot."
        (writer any "the publisher") (state any "the candidate") (returns boolean))
  (define (changed? writer state)
    (with-mutex (writer-lock writer)
      (when (writer-failure writer) (raise (writer-failure writer)))
      (not (equal? state (writer-wanted writer)))))

  (edoc "Queue owned state without waiting. Replace pending work, preserving the in-flight write; return whether state changed."
        (writer any "the publisher") (state any "the candidate, other than #f") (returns boolean))
  (define (submit! writer state)
    (with-mutex (writer-lock writer)
      (when (writer-failure writer) (raise (writer-failure writer)))
      (and (not (equal? state (writer-wanted writer)))
           (let ([owned ((writer-own writer) state)])
             (unless owned (error 'submit! "#f is reserved"))
             (writer-wanted-set! writer owned)
             (writer-pending-set! writer owned)
             (condition-broadcast (writer-ready writer)) #t))))

  (edoc "Fence all queued delivery, or raise its failure."
        (writer any "the publisher") (effects remote))
  (define (flush! writer)
    (with-mutex (writer-lock writer)
      (let wait ()
        (cond [(writer-failure writer) (raise (writer-failure writer))]
              [(or (writer-active? writer) (writer-pending writer))
               (condition-wait (writer-ready writer) (writer-lock writer)) (wait)])))))
