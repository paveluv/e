;; Named-screen checkpoint delivery. The head captures state; this writer
;; owns at most one in-flight snapshot and its latest pending replacement.
(import (only (foundation edoc) elibrary))
(elibrary (head checkpoint)
  (export changed? flush! make! submit! text)
  (import (rnrs)
          (only (chezscheme) make-mutex with-mutex make-condition condition-wait
                condition-broadcast fork-thread)
          (prefix (foundation datum) datum:))

  (define-record-type saved-text (fields source lines))

  (edoc "Own a local buffer's immutable line vector for queued checkpoints, reusing its previous snapshot while that vector is unchanged."
        (previous any "the previous snapshot, or #f")
        (lines vector "the immutable text")
        (returns any))
  (define (text previous lines)
    (if (and previous (eq? (saved-text-source previous) lines)) previous
        (make-saved-text lines (datum:copy (vector->list lines)))))

  (define-record-type writer
    (fields send! notify! lock ready
            (mutable pending) (mutable active?) (mutable wanted)
            (mutable saved) (mutable failure)))

  (define (wire-state state previous)
    ;; Each queued snapshot contains every local text. Only the immediately
    ;; preceding acknowledgement authorizes kept, including after removal,
    ;; rename, or replacement of a buffer with the same name and revision.
    (define old-entries (if previous (list-ref previous 4) '()))
    (list (car state) (cadr state) (caddr state) (cadddr state)
      (map
        (lambda (entry)
          (let ([reference (car entry)])
            (if (and (pair? reference) (eq? (car reference) 'local))
                (let* ([name (cadr reference)] [text (list-ref reference 4)]
                       [kept? (exists (lambda (entry)
                                        (let ([old (car entry)])
                                          (and (pair? old) (eq? (car old) 'local)
                                               (equal? name (cadr old))
                                               (eq? text (list-ref old 4))))) old-entries)])
                  (cons (list 'local name (caddr reference) (cadddr reference)
                          (if kept? 'kept (saved-text-lines text))) (cdr entry)))
                entry)))
        (list-ref state 4))))

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
            ((writer-send! writer) (datum:copy (wire-state state (writer-saved writer)))))
          (with-mutex (writer-lock writer)
            (writer-saved-set! writer state)
            (writer-active?-set! writer #f)
            (condition-broadcast (writer-ready writer)))
          (loop)))))

  (edoc "Make one checkpoint writer for the head's lifetime. Failed delivery wakes the head and is raised by its next submit, change check or flush; it is never retried implicitly."
        (send! procedure "(send! state), returning only after acknowledgement")
        (notify! thunk "wake the head on failure")
        (returns any))
  (define (make! send! notify!)
    (let ([writer (make-writer send! notify! (make-mutex) (make-condition) #f #f #f #f #f)])
      (fork-thread (lambda () (deliver! writer)))
      writer))

  (edoc "Whether a captured screen differs from the last submitted snapshot."
        (writer any "the writer") (state datum "the screen, with local text snapshots")
        (returns boolean))
  (define (changed? writer state)
    (with-mutex (writer-lock writer)
      (when (writer-failure writer) (raise (writer-failure writer)))
      (not (equal? state (writer-wanted writer)))))

  (edoc "Queue an owned screen snapshot without waiting for transmission. A newer snapshot replaces pending work; an in-flight write completes in order. Return whether the state changed."
        (writer any "the writer") (state datum "the screen, with local text snapshots")
        (returns boolean))
  (define (submit! writer state)
    (with-mutex (writer-lock writer)
      (when (writer-failure writer) (raise (writer-failure writer)))
      (and (not (equal? state (writer-wanted writer)))
           (let ([owned (datum:copy state
                          (lambda (leaf)
                            (if (saved-text? leaf) leaf
                                (error 'submit! "expected a local text snapshot" leaf))))])
             (writer-wanted-set! writer owned)
             (writer-pending-set! writer owned)
             (condition-broadcast (writer-ready writer))
             #t))))

  (edoc "Wait until queued checkpoint delivery is acknowledged, or raise its failure. This is the fence before detach, shutdown or reading a checkpoint for resume."
        (writer any "the writer") (effects remote))
  (define (flush! writer)
    (with-mutex (writer-lock writer)
      (let wait ()
        (cond [(writer-failure writer) (raise (writer-failure writer))]
              [(or (writer-active? writer) (writer-pending writer))
               (condition-wait (writer-ready writer) (writer-lock writer))
               (wait)])))))
