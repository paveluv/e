;; One owner for a process fixture's private base directory and child.
(library (fixture)
  (export start! stop! call-with-base command directory process diagnostics quote-shell)
  (import (except (chezscheme) process) (prefix (sys) sys:) (prefix (string) string:))

  (define-record-type base (fields installation directory process))
  (define directory base-directory)
  (define process base-process)

  (define (quote-shell text)
    (string-append "'" (apply string-append
                         (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list text))) "'"))

  (define (command base . args)
    (string-append "exec "
      (string:join
        (map quote-shell (append (list "scheme-script" (string-append (base-installation base) "/e")
                                   "--base-working-dir" (directory base)) args)) " ")))

  (define (diagnostics base)
    (let ([dir (string-append (directory base) "/log")])
      (if (not (file-directory? dir)) ""
          (string:join
            (map (lambda (name) (call-with-input-file (string-append dir "/" name) get-string-all))
              (list-sort string<? (directory-list dir))) "\n"))))

  (define (start! installation directory)
    (let* ([path (or directory (format "/tmp/e-base-~a-~a" (get-process-id) (random 1000000000)))]
           [process (sys:open-process (list "scheme-script" (string-append installation "/e")
                                        "--base" "--base-working-dir" path))]
           [base (make-base installation path process)]
           [deadline (add-duration (current-time 'time-monotonic) (make-time 'time-duration 0 120))])
      (guard (ex [else (sys:close-process! process) (raise ex)])
        (sys:write-process! process #f)
        (let wait ()
          (cond [(sys:process-status process)
                 (get-bytevector-all (sys:process-input process))
                 (let-values ([(code errors) (sys:process-result process)])
                   (error 'fixture "base failed to start" code errors (diagnostics base)))]
                [(sys:try-connect-local (string-append path "/socket") deadline)
                 => (lambda (connection)
                      (sys:close-connection! connection)
                      (let ([record (call-with-input-file (string-append path "/pid") read)])
                        (unless (= (cadr record) (sys:process-pid process))
                          (error 'fixture "another process owns the requested base directory" path))))]
                [else (sleep (make-time 'time-duration 50000000 0)) (wait)]))
        base)))

  (define stop!
    (case-lambda
      [(base) (stop! base 15)]
      [(base signal)
       (let* ([process (process base)]
              [deadline (add-duration (current-time 'time-monotonic) (make-time 'time-duration 0 5))])
         (dynamic-wind void
           (lambda ()
             (sys:signal-process! process signal)
             (let wait ()
               (unless (sys:process-status process)
                 (when (time>=? (current-time 'time-monotonic) deadline)
                   (error 'fixture "base did not stop" (diagnostics base)))
                 (sleep (make-time 'time-duration 10000000 0)) (wait)))
             (unless (= (sys:process-status process) 0)
               (error 'fixture "base exited unsuccessfully" (sys:process-status process) (diagnostics base)))
             (when (exists (lambda (name) (file-exists? (string-append (directory base) "/" name))) '("socket" "pid"))
               (error 'fixture "base left a socket or process record" (directory base)))
             (unless (file-exists? (string-append (directory base) "/lock"))
               (error 'fixture "base removed its lock inode" (directory base)))
             (let ([lock (sys:acquire-file-lock (string-append (directory base) "/lock"))])
               (unless lock (error 'fixture "base left a locked directory" (directory base)))
               (sys:release-file-lock! lock)))
           (lambda () (sys:close-process! process))))]))

  (define (call-with-base installation directory thunk)
    (let ([base (start! installation directory)])
      (dynamic-wind void (lambda () (thunk base)) (lambda () (stop! base)))))
)
