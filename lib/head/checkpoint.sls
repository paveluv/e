;; Named-screen checkpoint delivery. The head captures state; this writer
;; owns at most one in-flight snapshot and its latest pending replacement.
(import (only (foundation edoc) elibrary))
(elibrary (head checkpoint)
  (export make! text)
  (import (rnrs)
          (prefix (core publication) publication:)
          (prefix (foundation datum) datum:))

  (define-record-type saved-text (fields source lines))

  (edoc "Own a local buffer's immutable line vector for queued checkpoints, reusing its previous snapshot while that vector is unchanged."
        (previous any "the previous snapshot, or #f")
        (lines vector "the immutable text")
        (returns any))
  (define (text previous lines)
    (if (and previous (eq? (saved-text-source previous) lines)) previous
        (make-saved-text lines (datum:copy (vector->list lines)))))

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

  (edoc "Make a screen checkpoint publisher with acknowledged local-text elision. Use publication: operations to submit and fence it."
        (send! procedure "(send! screen), returning after acknowledgement")
        (notify! thunk "wake the head on failure") (returns any))
  (define (make! send! notify!)
    (publication:make!
      (lambda (state previous) (send! (datum:copy (wire-state state previous))))
      notify!
      (lambda (state)
        (datum:copy state
          (lambda (leaf) (if (saved-text? leaf) leaf
                             (error 'make! "expected a local text snapshot" leaf)))))))
)
