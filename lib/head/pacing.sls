;; Input-relative deadlines and bounded coalescing of expired keyboard input.

(import (only (foundation edoc) elibrary))
(elibrary (head pacing)
  (export defer? input! make presented! wait!)
  (import (rnrs)
          (only (chezscheme) add-duration copy-time make-time time<? time<=? time-difference))

  (edoc "A presentation clock driven by a monotonic time source and a duration-taking pause procedure."
        (now thunk "the current monotonic time")
        (pause procedure "wait for a duration, possibly returning early on interruption")
        (received (or any #f) "the most recently consumed input's receipt time")
        (presented (or any #f) "the last completed publication")
        (constructor now pause))
  (define-record-type (clock make clock?)
    (fields now pause (mutable received) (mutable presented))
    (protocol (lambda (new) (lambda (now pause) (new now pause #f #f)))))

  (define coalescing-interval (make-time 'time-duration 16000000 0))

  (edoc "Record a completed publication, starting a new bounded coalescing interval."
        (clock any "the presentation clock"))
  (define (presented! clock)
    (clock-presented-set! clock (copy-time ((clock-now clock)))))

  (edoc "Whether another expired key may replace this input's frame within 16 ms of the last publication; zero budget disables coalescing."
        (clock any "the presentation clock")
        (next any "the next key's monotonic receipt time")
        (milliseconds integer "the input-to-presentation budget")
        (returns boolean))
  (define (defer? clock next milliseconds)
    (and (> milliseconds 0) (clock-received clock) (clock-presented clock)
         (let ([now ((clock-now clock))])
           (and (time<=? (add-duration next (make-time 'time-duration (* milliseconds 1000000) 0)) now)
                (time<? now (add-duration (clock-presented clock) coalescing-interval))))))

  (edoc "Remember when the input being handled arrived, independently of dispatch and rendering time."
        (clock any "the presentation clock")
        (received any "its monotonic receipt time"))
  (define (input! clock received)
    (clock-received-set! clock (copy-time received)))

  (edoc "Wait only for the unused part of an input's presentation budget; overdue frames and frames without new input are immediate."
        (clock any "a clock made with a monotonic now thunk and duration-taking pause procedure")
        (milliseconds integer "the nonnegative input-to-presentation budget"))
  (define (wait! clock milliseconds)
    (let ([received (clock-received clock)])
      ;; Consume before waiting: reentrant publication must not wait twice.
      (clock-received-set! clock #f)
      (when (and received (> milliseconds 0))
        (let ([deadline (add-duration received
                          (make-time 'time-duration (* milliseconds 1000000) 0))])
          (let wait ()
            (let ([now ((clock-now clock))])
              (when (time<? now deadline)
                ((clock-pause clock) (time-difference deadline now))
                ;; A signal can interrupt a sleep. Keep the same deadline.
                (wait)))))))))
