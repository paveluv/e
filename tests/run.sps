#!/usr/bin/env scheme-script

;; run.sps -- run the suites concurrently and report each one's time and
;; last line. A standalone tool like those in tools/, kept beside the suites:
;;
;;   tests/run.sps [--jobs N] [suite ...]
;;
;; from any directory, so also ./run.sps inside tests/ or scheme-script
;; tests/run.sps from the root.
;; Long suites are split into groups that run as separate processes. The
;; product deadlines are scaled and evaluation mail is enabled for every
;; suite; a suite run by hand sees the unscaled product. Each process runs in
;; its own session under an address-space cap and a wall-clock limit, so a
;; runaway test dies alone instead of taking the machine with it.

(import (chezscheme))

;; Every path below is relative to the product root, the parent of this
;; script's own directory; scheme-script names the script first on the
;; command line however it was invoked.
(let* ([invocation (command-line)]
       [script (if (and (pair? invocation) (string? (car invocation))) (car invocation) "run.ss")]
       [directory (path-parent script)])
  (current-directory (if (string=? directory "") "." directory))
  (current-directory "..")
  (unless (file-exists? "tests/roots.ss")
    (error 'run "tests/run.sps must live in a product checkout" (current-directory))))

;; Loaded at run time, after the directory change above has taken effect,
;; and called through eval: scheme-script compiles this whole file before
;; running any of it, so the name is not bound yet at compile time.
(load "tests/roots.ss")
(eval '(test-roots! 'base))

(eval
  '(begin
     (import (prefix (sys sys) sys:) (prefix (foundation string) string:))

     (define arguments (command-line-arguments))
     (define jobs
       (let loop ([args arguments])
         (cond [(null? args) 64]
               [(and (string=? (car args) "--jobs") (pair? (cdr args))) (string->number (cadr args))]
               [else (loop (cdr args))])))
     (define selected
       (let loop ([args arguments] [out '()])
         (cond [(null? args) (reverse out)]
               [(string=? (car args) "--jobs") (loop (cddr args) out)]
               [else (loop (cdr args) (cons (car args) out))])))
     (define excluded '("roots.ss" "warm.ss" "vttest-drive.ss"))
     (define groups
       '(("wire.ss" "protocol" "attached" "resume" "overload" "bootstrap" "cold" "automatic"
          "shutdown" "final-shutdown" "session" "recovery" "restart" "restart-accept" "restart-file" "force" "help")))
     (define (suite-files)
       (list-sort string<?
         (filter (lambda (name)
                   (and (string:suffix? ".ss" name) (not (member name excluded))
                        (or (null? selected) (member name selected))))
           (directory-list "tests"))))
     ;; Every process may run at once; the cap bounds the worst case of a
     ;; runaway suite to what the machine holds, at 64 jobs 128 GiB of address space.
     (define memory-limit-kb (* 2 1024 1024))   ; per process, address space
     (define wall-limit 120)                     ; seconds per process
     (define (commands)
       ;; (label . shell command) per process. Grouped suites are the long
       ;; ones, so they start first and the short suites fill in behind them.
       (let-values ([(long short)
                     (partition (lambda (name) (assoc name groups)) (suite-files))])
         (append
           (apply append
             (map (lambda (name)
                    (map (lambda (group) (cons (format "~a ~a" name group) (format "tests/~a ~a" name group)))
                      (cdr (assoc name groups))))
               long))
           (map (lambda (name) (cons name (format "tests/~a" name))) short))))
     (define logs (format "/tmp/e-tests-~a" (get-process-id)))
     (mkdir logs #o700)
     ;; Suites compile stale objects on import without a lock, and the wire
     ;; fixtures seed their copies from eo/: bring both runtimes up to date
     ;; first, base before client, since a client runtime loads the common
     ;; libraries from the base cache as the loader does.
     (define (warm!)
       (let ([started (now)])
         (for-each
           (lambda (runtime)
             (let ([process (sys:open-process (list "scheme" "--script" "tests/warm.ss" runtime))])
               (sys:write-process! process #f)
               (let ([output (get-bytevector-all (sys:process-input process))])
                 (let-values ([(code errors) (sys:process-result process)])
                   (sys:close-process! process)
                   (unless (eqv? code 0)
                     (display (if (eof-object? output) "" (utf8->string output)))
                     (display errors)
                     (error 'run "warm-up compilation failed" runtime))))))
           '("base" "client"))
         (format #t "~7,2f  ok    warm-up (both runtimes current)\n" (- (now) started))
         (flush-output-port (current-output-port))))
     (define (log-path label)
       (string-append logs "/"
         (list->string (map (lambda (c) (if (char=? c #\space) #\- c)) (string->list label))) ".log"))
     (define (now) (let ([t (current-time 'time-monotonic)]) (+ (time-second t) (/ (time-nanosecond t) 1e9))))
     (define started (now))
     (warm!)
     (define (start! entry)
       ;; setsid puts the process and everything it spawns in one session, so
       ;; a wall-clock kill reaches the bases and heads too; without it the
       ;; process still runs, only the kill is narrower.
       (let ([process (sys:open-process
                        (list "/bin/sh" "-c"
                          (format "ulimit -v ~a; export E_TIME_SCALE=0.2 E_TEST_EVAL=1; if command -v setsid >/dev/null 2>&1; then exec setsid scheme --script ~a > ~s 2>&1; else exec scheme --script ~a > ~s 2>&1; fi"
                            memory-limit-kb (cdr entry) (log-path (car entry)) (cdr entry) (log-path (car entry)))))])
         (sys:write-process! process #f)
         (list (car entry) process (now))))
     (define (kill-late! running)
       ;; The whole session first, then the process itself in case setsid
       ;; was unavailable; the status poll below reaps it.
       (let ([pid (sys:process-pid (cadr running))])
         (system (format "kill -KILL -- -~a 2>/dev/null; kill -KILL ~a 2>/dev/null" pid pid))
         (call-with-output-file (log-path (car running))
           (lambda (p) (format p "\nrun: killed after ~a s of wall-clock time\n" wall-limit)) 'append)))
     (define (last-line path)
       (if (file-exists? path)
           (call-with-input-file path
             (lambda (p) (let loop ([line (get-line p)] [prev ""])
                           (if (eof-object? line) prev (loop (get-line p) (if (string=? line "") prev line))))))
           ""))
     (define results '())
     (define (finish! running)
       (let* ([status (sys:process-status (cadr running))] [elapsed (- (now) (caddr running))])
         (get-bytevector-all (sys:process-input (cadr running)))
         (sys:close-process! (cadr running))
         (set! results (cons (list (car running) elapsed status) results))
         ;; duration, start offset from the run's beginning, status, label, last line
         (format #t "~7,2f @~5,2f  ~a  ~a  ~a\n" elapsed (- (caddr running) started)
           (if (eqv? status 0) "ok  " "FAIL") (car running)
           (string:elide (last-line (log-path (car running))) 100))
         (flush-output-port (current-output-port))))
     (let schedule ([pending (commands)] [running '()])
       (cond
         [(and (null? pending) (null? running)) (void)]
         [(and (pair? pending) (< (length running) jobs))
          (schedule (cdr pending) (cons (start! (car pending)) running))]
         [else
          (for-each (lambda (r) (when (> (- (now) (caddr r)) wall-limit) (kill-late! r)))
            (filter (lambda (r) (not (sys:process-status (cadr r)))) running))
          (let ([done (filter (lambda (r) (sys:process-status (cadr r))) running)])
            (if (null? done)
                (begin (sleep (make-time 'time-duration 20000000 0)) (schedule pending running))
                (begin (for-each finish! done)
                       (schedule pending (filter (lambda (r) (not (memq r done))) running)))))]))
     (let ([failed (filter (lambda (r) (not (eqv? (caddr r) 0))) results)])
       (format #t "~7,2f  total wall time for ~a processes; logs in ~a\n" (- (now) started) (length results) logs)
       (for-each
         (lambda (r)
           (format #t "\n--- ~a ---\n" (car r))
           (let ([text (call-with-input-file (log-path (car r)) get-string-all)])
             (display (if (> (string-length text) 4000) (string:tail text 4000) text))))
         failed)
       (exit (if (null? failed) 0 1)))))
