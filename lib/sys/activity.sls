;; activity.sls -- the process's reversible side-effect barrier.
;; Thread coordination sits beneath process creation; lifecycle policy
;; and review ownership remain in the base control loop.
;; Enter before service locks or side effects, including child creation.
;; A scope includes callbacks; reentry on that same thread is already
;; admitted. Forked workers inherit parameters, but not their parent's id.
(import (only (foundation edoc) elibrary))
(elibrary (sys activity)
  (export lock phase call-with call-with-retirement wrap pause! resume! stop! stopped?)
  (import (chezscheme))

  (edoc "The mutex serializing lifecycle coordination, head admission and departure."
        (value any))
  (define lock
    (make-mutex))
  (define changed (make-condition))
  (define state 'running)
  (define active 0)
  (define entered (make-thread-parameter #f))
  (edoc "An operation was refused because the activity is stopping.")
  (define-condition-type &stopped &error make-stopped stopped?)

  ;; Lifecycle coordination reads this while holding lock, which also
  ;; serializes head admission and departure. No service lock nests it.
  (edoc "The lifecycle phase: running, paused or stopping."
        (returns symbol))
  (define (phase)
    state)

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

  (edoc "Run a thunk as an admitted operation, waiting while the activity is paused and refusing once it stops; admit! runs under the lock once it is admitted."
        (thunk thunk "the operation")
        (admit! thunk "run on admission")
        (returns any))
  (define call-with
    (case-lambda
      [(thunk)
       (scope thunk #f void)]
      [(thunk admit!)
       (scope thunk #f admit!)]))
  (edoc "Run a thunk as an operation that may still be admitted while paused, for retiring work."
        (thunk thunk "the operation")
        (returns any))
  (define (call-with-retirement thunk)
    (scope thunk #t void))
  (edoc "A procedure whose every call is an admitted operation."
        (procedure procedure "the procedure")
        (returns procedure))
  (define (wrap procedure)
    (lambda args (call-with (lambda () (apply procedure args)))))

  (edoc "Let operations be admitted again after a pause.")
  (define (resume!)
    (with-mutex lock
      (when (eq? state 'paused)
        (set! state 'running)
        (condition-broadcast changed))))

  (edoc "Stop admitting operations and wait for the active ones to finish, by a monotonic deadline; not from inside an operation."
        (deadline any "a monotonic time"))
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

  (edoc "Enter the stopping phase from a drained pause.")
  (define (stop!)
    (with-mutex lock
      (unless (and (eq? state 'paused) (zero? active)) (error 'stop! "expected a drained pause"))
      (set! state 'stopping)
      (condition-broadcast changed)))
)
