#!/usr/bin/env scheme-script

;; Run the suites concurrently and report each one's time and last line.
;; Usage from the repository root: scheme --script tests/run.ss [--jobs N] [suite ...]
;; Long suites are split into groups that run as separate processes. The
;; product deadlines are scaled and evaluation mail is enabled for every
;; suite; a suite run by hand sees the unscaled product.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (sys) sys:) (prefix (string) string:))

     (define arguments (command-line-arguments))
     (define jobs
       (let loop ([args arguments])
         (cond [(null? args) 16]
               [(and (string=? (car args) "--jobs") (pair? (cdr args))) (string->number (cadr args))]
               [else (loop (cdr args))])))
     (define selected
       (let loop ([args arguments] [out '()])
         (cond [(null? args) (reverse out)]
               [(string=? (car args) "--jobs") (loop (cddr args) out)]
               [else (loop (cdr args) (cons (car args) out))])))
     (define excluded '("roots.ss" "run.ss" "warm.ss" "vttest-drive.ss"))
     (define groups '(("wire.ss" "protocol" "bootstrap" "shutdown final-shutdown session" "recovery restart force help")))
     (define (suite-files)
       (list-sort string<?
         (filter (lambda (name)
                   (and (string:suffix? ".ss" name) (not (member name excluded))
                        (or (null? selected) (member name selected))))
           (directory-list "tests"))))
     (define (commands)
       ;; (label . shell command) per process; a grouped suite expands.
       (apply append
         (map (lambda (name)
                (let ([split (assoc name groups)])
                  (if split
                      (map (lambda (group) (cons (format "~a ~a" name group) (format "tests/~a ~a" name group)))
                        (cdr split))
                      (list (cons name (format "tests/~a" name))))))
           (suite-files))))
     (define logs (format "/tmp/e-tests-~a" (get-process-id)))
     (mkdir logs #o700)
     ;; Suites compile stale objects on import without a lock: bring every
     ;; library up to date once, serially, before the processes fan out.
     (define (warm!)
       (let* ([started (now)]
              [process (sys:open-process (list "scheme" "--script" "tests/warm.ss"))])
         (sys:write-process! process #f)
         (let ([output (get-bytevector-all (sys:process-input process))])
           (let-values ([(code errors) (sys:process-result process)])
             (sys:close-process! process)
             (unless (eqv? code 0)
               (display (if (eof-object? output) "" (utf8->string output)))
               (display errors)
               (error 'run "warm-up compilation failed"))
             (format #t "~7,2f  ok    warm-up (all libraries current)\n" (- (now) started))
             (flush-output-port (current-output-port))))))
     (define (log-path label)
       (string-append logs "/"
         (list->string (map (lambda (c) (if (char=? c #\space) #\- c)) (string->list label))) ".log"))
     (define (now) (let ([t (current-time 'time-monotonic)]) (+ (time-second t) (/ (time-nanosecond t) 1e9))))
     (define started (now))
     (warm!)
     (define (start! entry)
       (let ([process (sys:open-process
                        (list "/bin/sh" "-c"
                          (format "E_TIME_SCALE=0.2 E_TEST_EVAL=1 exec scheme --script ~a > ~s 2>&1"
                            (cdr entry) (log-path (car entry)))))])
         (sys:write-process! process #f)
         (list (car entry) process (now))))
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
         (format #t "~7,2f  ~a  ~a  ~a\n" elapsed (if (eqv? status 0) "ok  " "FAIL") (car running)
           (string:elide (last-line (log-path (car running))) 100))
         (flush-output-port (current-output-port))))
     (let schedule ([pending (commands)] [running '()])
       (cond
         [(and (null? pending) (null? running)) (void)]
         [(and (pair? pending) (< (length running) jobs))
          (schedule (cdr pending) (cons (start! (car pending)) running))]
         [else
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
