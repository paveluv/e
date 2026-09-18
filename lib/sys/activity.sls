;; activity.sls -- the process's reversible side-effect barrier.
;; Thread coordination sits beneath process creation; lifecycle policy
;; and review ownership remain in the base control loop.
;; Enter before service locks or side effects, including child creation.
;; A scope includes callbacks; reentry on that same thread is already
;; admitted. Forked workers inherit parameters, but not their parent's id.
(library (activity)
  (export lock phase call-with call-with-retirement wrap pause! resume! stop! stopped?)
  (import (chezscheme))

  (define lock (make-mutex))
  (define changed (make-condition))
  (define state 'running)
  (define active 0)
  (define entered (make-thread-parameter #f))
  (define-condition-type &stopped &error make-stopped stopped?)

  ;; Lifecycle coordination reads this while holding lock, which also
  ;; serializes head admission and departure. No service lock nests it.
  (define (phase) state)

  (define (call-with-interrupts-enabled thunk)
    ;; Chez keeps a thread that blocks on a mutex or condition with
    ;; interrupts disabled counted as active, so a pending collection waits
    ;; for it while every other thread stops at its next allocation. Callers
    ;; arrive that way from the in thunk of a critical dynamic-wind, as
    ;; command scopes do. Release their disable count around the admission
    ;; alone: nothing is acquired before it, and an escape rewinds the count.
    (let ([depth (- (disable-interrupts) 1)])
      (enable-interrupts)
      (if (zero? depth) (thunk)
          (dynamic-wind
            (lambda () (do ([i 0 (+ i 1)]) ((= i depth)) (enable-interrupts)))
            thunk
            (lambda () (do ([i 0 (+ i 1)]) ((= i depth)) (disable-interrupts)))))))

  (define (scope thunk retiring? admit!)
    (if (eqv? (entered) (get-thread-id)) (thunk)
        (dynamic-wind
          (lambda ()
            (call-with-interrupts-enabled
              (lambda ()
                (with-mutex lock
                  (admit!)
                  (let wait ()
                    (when (eq? state 'paused) (condition-wait changed lock) (wait)))
                  (when (and (eq? state 'stopping) (not retiring?))
                    (raise (condition (make-stopped) (make-message-condition "The base is stopping"))))
                  (set! active (+ active 1))))))
          (lambda () (parameterize ([entered (get-thread-id)]) (thunk)))
          (lambda ()
            (call-with-interrupts-enabled
              (lambda ()
                (with-mutex lock
                  (set! active (- active 1))
                  (when (zero? active) (condition-broadcast changed)))))))))

  (define call-with
    (case-lambda
      [(thunk) (scope thunk #f void)]
      [(thunk admit!) (scope thunk #f admit!)]))
  (define (call-with-retirement thunk) (scope thunk #t void))
  (define (wrap procedure)
    (lambda args (call-with (lambda () (apply procedure args)))))

  (define (resume!)
    (with-mutex lock
      (when (eq? state 'paused)
        (set! state 'running)
        (condition-broadcast changed))))

  (define (pause! deadline)
    (when (eqv? (entered) (get-thread-id)) (error 'pause! "cannot pause inside an admitted operation"))
    (let ([ready? #f])
      (dynamic-wind void
        (lambda ()
          (with-mutex lock
            (unless (eq? state 'running) (error 'pause! "another pause or stop is in progress"))
            (set! state 'paused)
            (let wait ()
              (cond [(zero? active) (set! ready? #t)]
                    [else
                     (let ([now (current-time 'time-monotonic)])
                       (unless (time<? now deadline) (error 'pause! "timed out waiting for active work"))
                       (condition-wait changed lock (time-difference deadline now))
                       (wait))]))))
        (lambda () (unless ready? (resume!))))))

  (define (stop!)
    (with-mutex lock
      (unless (and (eq? state 'paused) (zero? active)) (error 'stop! "expected a drained pause"))
      (set! state 'stopping)
      (condition-broadcast changed)))
)
