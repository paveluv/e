#!/usr/bin/env scheme-script

;; multihead-bench.sps -- the acceptance measurement for attached editing.
;;
;; Runs the installation in place: a daemon on a private socket, real
;; heads under PTYs, and a standalone editor for the reference column.
;; Keys are written to the PTY in one burst; a scenario settles when the
;; status line shows the expected position, so the number is the editor's
;; own cost per key.  Bytes are the head's rchar/wchar deltas from
;; /proc, which count the terminal too (a few hundred bytes per key).
;;
;;   scheme-script tools/multihead-bench.sps   (from the repository root)
;;
;; Prints a Markdown table; see dev/MULTIHEAD_IMPROVEMENTS.md for the
;; baseline this reproduces.

(import (chezscheme))
(library-directories
  (map (lambda (root)
         (cons (string-append (current-directory) "/lib/" root)
               (string-append (current-directory) "/eo/base")))
       '("base/state" "base/service" "foundation" "sys" "core" "service"
         "head" "apps" "modes" "run")))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (sys) sys:) (prefix (string) string:) (prefix (vt) vt:)
             (prefix (kernel) kernel:))

     (kernel:installation-directory (current-directory))

     (define here (cd))
     (define root (format "/tmp/e-bench-~a" (get-process-id)))
     (define socket (string-append root "/socket"))
     (define big-file (string-append root "/big.txt"))
     (define rows 40)
     (define cols 120)
     (define keys 50)

     (define (quote-shell text)
       (string-append "'" (apply string-append
                            (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list text))) "'"))
     (define (now-ms)
       (let ([t (current-time 'time-monotonic)])
         (+ (* 1000.0 (time-second t)) (/ (time-nanosecond t) 1e6))))
     (define (pause ms)
       (let ([ns (* ms 1000000)])
         (sleep (make-time 'time-duration (mod ns 1000000000) (div ns 1000000000)))))

     ;; The large file: every base-runtime library twice, sorted by stem
     ;; as before the layout move. Client implementations are not added.
     (define (write-big-file!)
       (let ([sources (list-sort string<?
                        (apply append
                          (map (lambda (root)
                                 (filter (lambda (name) (string:suffix? ".sls" name))
                                         (directory-list (car root))))
                               (library-directories))))])
         (call-with-output-file big-file
           (lambda (out)
             (do ([pass 0 (+ pass 1)]) ((= pass 2))
               (for-each (lambda (name)
                           (display (call-with-input-file (kernel:module-source (path-root name)) get-string-all) out))
                         sources)))
           'replace)))
     (define (count-lines path)
       (call-with-input-file path
         (lambda (in) (let loop ([n 0]) (if (eof-object? (get-line in)) n (loop (+ n 1)))))))

     ;;; Processes under PTYs -------------------------------------------------

     (define-record-type screen (fields process port emulator (mutable io-start)))

     (define (spawn command)
       (let ([process (sys:spawn-terminal-process "/bin/sh" command here rows cols)])
         (make-screen process
           (transcoded-port (sys:terminal-process-input process)
                            (make-transcoder (utf-8-codec) 'none 'replace))
           (vt:make-emulator rows cols) #f)))
     (define (pump! s)
       (let drain ()
         (when (guard (ex [else #f]) (char-ready? (screen-port s)))
           (let ([c (guard (ex [else (eof-object)]) (get-char (screen-port s)))])
             (unless (eof-object? c)
               (vt:emulator-feed! (screen-emulator s) (string c))
               (drain))))))
     (define (send! s text)
       (let ([out (sys:terminal-process-output (screen-process s))])
         (put-bytevector out (string->utf8 text))
         (flush-output-port out)))
     (define (screen-lines s)
       (pump! s)
       (vector->list (vt:emulator-screen (screen-emulator s))))
     (define (position s)
       ;; the last "L<n> C<m>" on the screen: the status line of the window
       (let find ([lines (reverse (screen-lines s))])
         (cond [(null? lines) #f]
               [(status-position (car lines)) => values]
               [else (find (cdr lines))])))
     (define (status-position line)
       (let ([n (string-length line)])
         (let scan ([i 0])
           (cond
             [(>= (+ i 4) n) #f]
             [(and (char=? (string-ref line i) #\L) (char-numeric? (string-ref line (+ i 1)))
                   (or (= i 0) (char=? (string-ref line (- i 1)) #\space)))
              (let* ([end (let run ([j (+ i 1)]) (if (and (< j n) (char-numeric? (string-ref line j))) (run (+ j 1)) j))]
                     [row (string->number (substring line (+ i 1) end))])
                (if (and (< (+ end 2) n) (char=? (string-ref line end) #\space)
                         (char=? (string-ref line (+ end 1)) #\C) (char-numeric? (string-ref line (+ end 2))))
                    (let* ([cend (let run ([j (+ end 2)]) (if (and (< j n) (char-numeric? (string-ref line j))) (run (+ j 1)) j))]
                           [col (string->number (substring line (+ end 2) cend))])
                      (cons row col))
                    (scan (+ i 1))))]
             [else (scan (+ i 1))]))))
     (define (wait-for s label predicate . timeout)
       (let ([deadline (+ (now-ms) (if (pair? timeout) (car timeout) 60000))])
         (let wait ()
           (cond [(predicate) (void)]
                 [(> (now-ms) deadline)
                  (error 'bench (format "~a did not settle" label) (position s) (screen-lines s))]
                 [else (pause 10) (wait)]))))
     (define (wait-position s label row col)
       (wait-for s label (lambda () (equal? (position s) (cons row col)))))
     (define (io-bytes s)
       (guard (ex [else #f])
         (call-with-input-file (format "/proc/~a/io" (sys:terminal-process-pid (screen-process s)))
           (lambda (in)
             (let loop ([total 0])
               (let ([line (get-line in)])
                 (cond [(eof-object? line) total]
                       [(or (string:prefix? "rchar: " line) (string:prefix? "wchar: " line))
                        (loop (+ total (string->number (substring line 7 (string-length line)))))]
                       [else (loop total)])))))))
     (define (stop! s)
       (guard (ex [else (void)]) (send! s "\x18;\x03;"))
       (pause 300)
       (guard (ex [else (void)]) (sys:close-terminal-process! (screen-process s))))

     ;;; Scenarios ------------------------------------------------------------

     (define (typing s from-row)
       ;; keys typed at the start of from-row, settled when the column shows
       (send! s "\x1b;<")
       (do ([i 1 (+ i 1)]) ((= i from-row)) (send! s "\x0e;"))
       (wait-position s 'typing-start from-row 1)
       (pause 200)
       (let ([bytes (io-bytes s)] [start (now-ms)])
         (send! s (make-string keys #\q))
         (wait-position s 'typing from-row (+ keys 1))
         (let ([ms (- (now-ms) start)] [after (io-bytes s)])
           (list (/ ms keys) (and bytes after (- after bytes))))))
     (define (motion s)
       (send! s "\x1b;<")
       (wait-position s 'motion-start 1 1)
       (pause 200)
       (let ([start (now-ms)])
         (send! s (make-string keys (integer->char 14)))
         (wait-position s 'motion (+ keys 1) 1)
         (/ (- (now-ms) start) keys)))
     (define (kill-and-undo s lines)
       (send! s "\x1b;<")
       (wait-position s 'kill-start 1 1)
       (pause 200)
       (send! s "\x00;\x1b;>")
       (wait-for s 'select-all (lambda () (let ([p (position s)]) (and p (= (car p) lines)))))
       (let ([start (now-ms)])
         (send! s "\x17;")
         (wait-position s 'kill 1 1)
         (let ([killed (- (now-ms) start)] [start (now-ms)])
           (send! s "\x1f;")
           (wait-for s 'undo (lambda () (let ([p (position s)]) (and p (= (car p) lines)))))
           (list killed (- (now-ms) start)))))

     (define (ms value) (if value (format "~,1f ms" value) "--"))
     (define (per-key value) (if value (format "~,2f ms/key" value) "--"))
     (define (kb value) (if value (format "~,1f KB" (/ value 1024.0)) "--"))

     (define (run!)
       (mkdir root #o700)
       (write-big-file!)
       (let ([lines (count-lines big-file)]
             [e (quote-shell (string-append here "/e"))])
         (define (editor . args)
           (spawn (format "exec scheme-script ~a ~a 2>~a" e
                          (apply string-append (map (lambda (a) (string-append a " ")) args))
                          (quote-shell (string-append root "/stderr")))))
         (define (attached name file)
           (editor "--attach" "--socket" (quote-shell socket) "--name" name (quote-shell file)))
         (define (measure file with-kill?)
           ;; -> (typing-ms/key typing-bytes motion-ms/key kill-ms undo-ms) for one screen kind
           (lambda (open)
             (let ([s (open file)])
               (wait-for s 'editor-starts (lambda () (position s)))
               (pause 500)
               (let* ([typed (typing s 3)]
                      [moved (motion s)]
                      [killed (if with-kill? (kill-and-undo s lines) '(#f #f))])
                 (stop! s)
                 (append typed (list moved) killed)))))
         (define daemon
           (spawn (format "exec scheme-script ~a --daemon --socket ~a" e (quote-shell socket))))
         (wait-for daemon 'daemon-starts (lambda () (file-exists? socket)) 30000)
         (dynamic-wind void
           (lambda ()
             (let* ([vt-file (kernel:module-source "vt")]
                    [solo-vt ((measure vt-file #f) (lambda (file) (editor "--name" "solo" (quote-shell file))))]
                    [head-vt ((measure vt-file #f) (lambda (file) (attached "bench" file)))]
                    [solo-big ((measure big-file #t) (lambda (file) (editor "--name" "solo" (quote-shell file))))]
                    [head-big ((measure big-file #t) (lambda (file) (attached "bench" file)))]
                    ;; a second head showing vt.sls while the first types
                    [watch
                     (let* ([typist (attached "typist" vt-file)] [watcher (attached "watcher" vt-file)])
                       (for-each (lambda (s) (wait-for s 'editor-starts (lambda () (position s)))) (list typist watcher))
                       (pause 500)
                       (let ([before (io-bytes watcher)])
                         (typing typist 3)
                         (pause 1500)
                         (let ([after (io-bytes watcher)])
                           (stop! typist) (stop! watcher)
                           (and before after (- after before)))))])
               (printf "\n| Scenario | Standalone | Attached |\n|---|---|---|\n")
               (printf "| Typing, vt.sls (~a keys) | ~a | ~a |\n" keys (per-key (car solo-vt)) (per-key (car head-vt)))
               (printf "| Cursor motion, vt.sls | ~a | ~a |\n" (per-key (caddr solo-vt)) (per-key (caddr head-vt)))
               (printf "| Typing, ~a-line file | ~a | ~a |\n" lines (per-key (car solo-big)) (per-key (car head-big)))
               (printf "| Kill the whole ~a-line buffer | ~a | ~a |\n" lines (ms (cadddr solo-big)) (ms (cadddr head-big)))
               (printf "| Undo that kill | ~a | ~a |\n" (ms (list-ref solo-big 4)) (ms (list-ref head-big 4)))
               (printf "| Head I/O bytes per ~a typed keys, vt.sls | ~a | ~a |\n" keys (kb (cadr solo-vt)) (kb (cadr head-vt)))
               (printf "| Watching head I/O bytes per ~a foreign keys | -- | ~a |\n\n" keys (kb watch))))
           (lambda ()
             (system (format "kill -TERM ~a 2>/dev/null" (sys:terminal-process-pid (screen-process daemon))))
             (pause 500)
             (guard (ex [else (void)]) (sys:close-terminal-process! (screen-process daemon)))
             (system (format "rm -rf ~a" (quote-shell root)))))))

     (run!)))
