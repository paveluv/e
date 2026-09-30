;; Shared names for live model namespaces. No evaluation or paint-time RPC.
(import (only (foundation edoc) elibrary))
(elibrary (head namespace)
  (export acquire! init! pump! release! snapshot)
  (import (chezscheme) (prefix (head head) head:) (prefix (service environment) environment:)
    (prefix (state model) model:))
  (define-record-type cache
    (fields id (mutable readers) (mutable subscription) (mutable packet)
      (mutable active?) (mutable completed)))
  (define entries (make-hashtable equal-hash equal?))
  (define lock (make-mutex))
  (define (get r k) (cdr (assq k r)))
  (define (basis e)
    (let ([r (model:snapshot (cache-id e))])
      (and r (eq? (get r 'kind) 'environment)
        (let ([v (get r 'value)]) (list (get v 'generation) (get v 'catalogue) (get v 'count))))))
  (define (fetch e b)
    (let loop ([offset 0] [pages '()])
      (cond [(not (with-mutex lock (> (cache-readers e) 0))) #f]
        [(>= offset (caddr b)) (list b (apply append (reverse pages)) 'ready)]
        [else
         (let ([page (environment:completion (cache-id e) (car b) (cadr b) offset 256)])
           (and page
             (let ([names (list-ref page 3)])
               (and (pair? names) (loop (+ offset (length names)) (cons names pages))))))])))

  (edoc "Acquire a shared local symbol catalogue for an environment. The returned token owns one subscription; view forks share pages and never start workers."
    (id model "environment") (returns any))
  (define (acquire! id)
    (let ([e (or (hashtable-ref entries id #f)
               (let ([e (make-cache id 0 #f #f #f #f)])
                 (hashtable-set! entries id e)
                 (guard (ex [else (hashtable-delete! entries id) (raise ex)])
                   (cache-subscription-set! e (model:subscribe! (list id) (lambda (event) (head:wake-main!)))) e)))])
      (with-mutex lock (cache-readers-set! e (+ 1 (cache-readers e))))
      (head:wake-main!) (cons e #t)))

  (edoc "Release one catalogue reader. Last release drops names and subscriptions; pending pages cannot revive it."
    (token any "acquisition token"))
  (define (release! token)
    (when (cdr token)
      (set-cdr! token #f)
      (let ([e (car token)])
        (when (with-mutex lock (cache-readers-set! e (- (cache-readers e) 1)) (zero? (cache-readers e)))
          (model:unsubscribe! (cache-subscription e)) (hashtable-delete! entries (cache-id e))))))

  (edoc "Read (basis names ready|pending|unavailable) locally. Basis is (generation catalogue count); callers must not mutate returned names. No request or evaluation is performed."
    (token any "live acquisition") (returns list))
  (define (snapshot token)
    (unless (cdr token) (error 'snapshot "released namespace reader"))
    (let* ([e (car token)] [b (basis e)] [p (cache-packet e)])
      (cond [(not b) '(#f () unavailable)] [(and p (equal? b (car p))) p]
        [else (list b '() 'pending)])))

  (edoc "Adopt completed catalogues and fetch missing bounded pages off the head thread. One active fetch and one latest basis are retained per shared environment."
    (returns any))
  (define (pump!)
    (vector-for-each
      (lambda (e)
        (let* ([b (basis e)]
               [completed (with-mutex lock
                            (let ([p (cache-completed e)]) (cache-completed-set! e #f) p))])
          (when completed
            (cache-active?-set! e #f)
            (when (and (car completed) (equal? b (caar completed))) (cache-packet-set! e (car completed))))
          (when (and b (not (cache-active? e))
                  (not (and (cache-packet e) (equal? b (car (cache-packet e))))))
            (if (zero? (caddr b)) (cache-packet-set! e (list b '() 'ready))
              (begin
                (cache-active?-set! e #t)
                (fork-thread
                  (lambda ()
                    (let ([packet (guard (ex [else (list b '() 'unavailable)]) (fetch e b))])
                      (with-mutex lock (cache-completed-set! e (list packet)))
                      (head:wake-main!)))))))))
      (hashtable-values entries)))

  (edoc "Integrate namespace catalogue adoption with the ordinary head service pump.")
  (define (init!) (head:add-pre-redraw-hook! pump!)))
