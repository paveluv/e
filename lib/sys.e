;; sys.e -- the e editor's system-specific layer: the library (sys).
;;
;; Everything that touches the operating system through libc lives here:
;; terminal modes via termios, the window size via ioctl, SIGWINCH
;; registration, and pipes for evaluated programs' process output. The core
;; imports this library and stays free of foreign procedures and platform
;; constants. Terminal operations degrade softly: without a terminal (or
;; without libc) they become no-ops and terminal-size returns #f.

(library (sys)
  (export terminal-raw! terminal-restore! terminal-isig!
          terminal-size watch-terminal-resize! call-with-streamed-output
          duplicate-standard-output-port duplicate-output-port
          terminal-output-port
          terminal-character-width
          canonical-file-path
          spawn-terminal-process terminal-process?
          terminal-process-input terminal-process-output
          terminal-process-pid resize-terminal-process!
          close-terminal-process! reap-terminal-process!)
  (import (chezscheme))

  (define os
    ;; From the machine type's suffix: ...osx is macOS, ...fb is FreeBSD,
    ;; anything else is treated as Linux.
    (let* ([mt (symbol->string (machine-type))]
           [n (string-length mt)])
      (define (suffix? s)
        (let ([m (string-length s)])
          (and (>= n m) (string=? (substring mt (- n m) n) s))))
      (cond [(suffix? "osx") 'macos]
            [(suffix? "fb") 'freebsd]
            [else 'linux])))

  (define-syntax os-case   ; (os-case linux-value macos-value freebsd-value)
    (syntax-rules ()
      [(_ l m f) (case os [(macos) m] [(freebsd) f] [else l])]))

  (define libc-loaded?
    (let try ([names (os-case '("libc.so.6" "libc.so")
                              '("libSystem.dylib" "libc.dylib")
                              '("libc.so.7" "libc.so"))])
      (cond [(null? names) #f]
            [(guard (ex [else #f]) (load-shared-object (car names)) #t) #t]
            [else (try (cdr names))])))

  (define tcgetattr
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "tcgetattr" (int u8*) int))))

  (define tcsetattr
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "tcsetattr" (int int u8*) int))))

  (define cfmakeraw
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "cfmakeraw" (u8*) void))))

  (define c-pipe
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "pipe" (u8*) int))))
  (define c-dup
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "dup" (int) int))))
  (define c-dup2
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "dup2" (int int) int))))
  (define c-close
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "close" (int) int))))
  (define c-realpath
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "realpath" (string u8*) uptr))))
  (define c-setlocale
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "setlocale" (int string) uptr))))
  (define c-wcwidth
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "wcwidth" (unsigned-int) int))))

  ;; wcwidth follows LC_CTYPE. Chez strings are Unicode regardless of the C
  ;; locale, so initialize libc's character classification explicitly.
  (define libc-character-locale
    (and c-setlocale (c-setlocale 0 "")))

  (define (terminal-character-width character)
    (let ([width (and c-wcwidth (c-wcwidth (char->integer character)))])
      (cond [(and width (>= width 0)) width]
            [(memq (char-general-category character) '(Mn Me Cf)) 0]
            [else 1])))

  ;; PTYs are deliberately kept in the system layer.  The terminal emulator
  ;; consumes byte ports and never needs platform constants or libc details.
  ;; openpty lives in libc on Linux and libutil on the BSD family.
  (define libutil-loaded?
    (or (and libc-loaded?
             (guard (ex [else #f])
               (load-shared-object
                 (os-case "libutil.so.1" "libutil.dylib" "libutil.so"))
               #t))
        #f))
  (define c-openpty
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "openpty" (u8* u8* u8* u8* u8*) int))))
  (define c-fork
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "fork" () int))))
  (define c-setsid
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "setsid" () int))))
  (define c-execv
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "execv" (string uptr) int))))
  (define c-strdup
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "strdup" (string) uptr))))
  (define c-free
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "free" (uptr) void))))
  (define c-perror
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "perror" (string) void))))
  (define c-chdir
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "chdir" (string) int))))
  (define c-setenv
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "setenv" (string string int) int))))
  (define c-kill
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "kill" (int int) int))))
  (define c-exit
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "_exit" (int) void))))
  (define c-waitpid
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "waitpid" (int u8* int) int))))
  (define c-close-range
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "close_range"
                              (unsigned-int unsigned-int unsigned-int) int))))
  (define c-closefrom
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "closefrom" (int) void))))
  (define c-getdtablesize
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "getdtablesize" () int))))
  (define pty-ioctl
    (and libc-loaded?
         (or (guard (ex [else #f])
               (eval '(foreign-procedure (__varargs_after 2) "ioctl"
                                         (int unsigned-long u8*) int)))
             (guard (ex [else #f])
               (foreign-procedure "ioctl" (int unsigned-long u8*) int)))))

  (define-record-type terminal-process
    (fields input output pid master (mutable closed) (mutable reaped) lock))

  (define tiocsctty-request (os-case #x540e #x20007461 #x20007461))
  (define tiocswinsz-request (os-case #x5414 #x80087467 #x80087467))

  (define (winsize rows cols)
    (let ([size (make-bytevector 8 0)])
      (bytevector-u16-native-set! size 0 rows)
      (bytevector-u16-native-set! size 2 cols)
      size))

  (define (close-child-descriptors!)
    ;; M-x temporarily owns extra stdout/stderr pipe descriptors. A PTY child
    ;; must not inherit them: otherwise the evaluator waits forever for pipe
    ;; EOF while the interactive shell keeps their hidden copies open.
    (cond [c-close-range (c-close-range 3 #xffffffff 0)]
          [c-closefrom (c-closefrom 3)]
          [c-getdtablesize
           (do ([fd 3 (+ fd 1)]) ((= fd (min 65536 (c-getdtablesize))))
             (c-close fd))]))

  (define (resize-terminal-process! process rows cols)
    (unless (terminal-process? process)
      (error 'resize-terminal-process! "expected a terminal process" process))
    (with-mutex (terminal-process-lock process)
      (when (and pty-ioctl (not (terminal-process-closed process)))
        (pty-ioctl (terminal-process-master process) tiocswinsz-request
                   (winsize rows cols))
        ;; The child is a session and process-group leader after setsid.
        (when c-kill (c-kill (- (terminal-process-pid process)) 28)))))

  (define (make-exec-arguments shell command)
    (let* ([values (if command (list shell "-c" command) (list shell))]
           [strings (map c-strdup values)])
      (when (exists zero? strings)
        (for-each (lambda (string) (unless (zero? string) (c-free string)))
                  strings)
        (error 'spawn-terminal-process "could not allocate exec arguments"))
      (let* ([width (foreign-sizeof 'uptr)]
             [arguments (foreign-alloc (* (+ (length strings) 1) width))])
        (do ([items strings (cdr items)] [index 0 (+ index 1)])
            ((null? items))
          (foreign-set! 'uptr arguments (* index width) (car items)))
        (foreign-set! 'uptr arguments (* (length strings) width) 0)
        (cons arguments strings))))

  (define (free-exec-arguments! arguments)
    (for-each c-free (cdr arguments))
    (foreign-free (car arguments)))

  (define (spawn-terminal-process shell command directory rows cols)
    (unless (and c-openpty c-fork c-setsid c-execv c-strdup c-free
                 c-dup2 c-close c-exit)
      (error 'spawn-terminal-process "PTY processes are unavailable"))
    (let ([master (make-bytevector 4 0)]
          [slave (make-bytevector 4 0)]
          [size (winsize rows cols)]
          [arguments (make-exec-arguments shell command)])
      (unless (= (c-openpty master slave #f #f size) 0)
        (free-exec-arguments! arguments)
        (error 'spawn-terminal-process "openpty failed"))
      (let* ([master-fd (bytevector-s32-native-ref master 0)]
             [slave-fd (bytevector-s32-native-ref slave 0)]
             [pid (c-fork)])
        (cond
          [(< pid 0)
           (c-close master-fd)
           (c-close slave-fd)
           (free-exec-arguments! arguments)
           (error 'spawn-terminal-process "fork failed")]
          [(= pid 0)
           (c-close master-fd)
           (when (< (c-setsid) 0)
             (when c-perror (c-perror "setsid"))
             (c-exit 127))
           (when pty-ioctl
             (when (< (pty-ioctl slave-fd tiocsctty-request #f) 0)
               (when c-perror (c-perror "TIOCSCTTY"))
               (c-exit 127))
             (when (< (pty-ioctl slave-fd tiocswinsz-request size) 0)
               (when c-perror (c-perror "TIOCSWINSZ"))
               (c-exit 127)))
           (when (or (< (c-dup2 slave-fd 0) 0)
                     (< (c-dup2 slave-fd 1) 0)
                     (< (c-dup2 slave-fd 2) 0))
             (when c-perror (c-perror "dup2"))
             (c-exit 127))
           (when (> slave-fd 2) (c-close slave-fd))
           (close-child-descriptors!)
           (when (and c-chdir (< (c-chdir directory) 0))
             (when c-perror (c-perror "chdir"))
             (c-exit 127))
           (when (and c-setenv (< (c-setenv "TERM" "xterm-256color" 1) 0))
             (when c-perror (c-perror "setenv TERM"))
             (c-exit 127))
           (c-execv shell (car arguments))
           (when c-perror (c-perror "execv terminal shell"))
           (c-exit 127)]
          [else
           (c-close slave-fd)
           (free-exec-arguments! arguments)
           (let ([input (open-fd-input-port (c-dup master-fd) 'block #f)]
                 [output (open-fd-output-port (c-dup master-fd) 'none #f)])
             (make-terminal-process input output pid master-fd #f #f
                                    (make-mutex)))]))))

  (define (close-terminal-descriptors! process)
    (unless (terminal-process-closed process)
      (terminal-process-closed-set! process #t)
      (guard (ex [else (void)]) (close-port (terminal-process-input process)))
      (guard (ex [else (void)]) (close-port (terminal-process-output process)))
      (when c-close (c-close (terminal-process-master process)))))

  (define (wait-terminal-process! process options)
    (and c-waitpid
         (not (terminal-process-reaped process))
         (let ([result (c-waitpid (terminal-process-pid process)
                                  (make-bytevector 4 0) options)])
           (when (or (= result (terminal-process-pid process)) (< result 0))
             (terminal-process-reaped-set! process #t))
           result)))

  (define (close-terminal-process! process)
    (with-mutex (terminal-process-lock process)
      (close-terminal-descriptors! process)
      (unless (terminal-process-reaped process)
        (when c-kill (c-kill (- (terminal-process-pid process)) 15))
        ;; Give cooperative programs a short chance to clean up. Never let a
        ;; terminal buffer kill or editor shutdown block on a stubborn child.
        (let poll ([attempts 8])
          (let ([result (wait-terminal-process! process 1)]) ; WNOHANG
            (cond [(or (not result) (terminal-process-reaped process)) (void)]
                  [(> attempts 0)
                   (sleep (make-time 'time-duration 25000000 0))
                   (poll (- attempts 1))]
                  [else
                   (when c-kill (c-kill (- (terminal-process-pid process)) 9))
                   (wait-terminal-process! process 0)]))))))

  (define (reap-terminal-process! process)
    ;; Called after the master reports EOF: the child has closed the slave and
    ;; can be waited without delaying the editor.
    (with-mutex (terminal-process-lock process)
      (close-terminal-descriptors! process)
      (wait-terminal-process! process 0)))

  (define (canonical-file-path path)
    ;; The absolute, symlink-resolved spelling of an existing path, or #f.
    ;; PATH_MAX is commonly 4096; realpath fails instead of overflowing the
    ;; caller-provided buffer.
    (and c-realpath
         (let ([out (make-bytevector 4096 0)])
           (and (not (= (c-realpath path out) 0))
                (let find ([n 0])
                  (if (= (bytevector-u8-ref out n) 0)
                      (let ([trimmed (make-bytevector n)])
                        (bytevector-copy! out 0 trimmed 0 n)
                        (utf8->string trimmed))
                      (find (+ n 1))))))))

  ;; The destination for terminal-control output. Normally this is stdout;
  ;; clients that temporarily redirect process stdout can preserve a separate
  ;; terminal descriptor here so the interface remains drawable.
  (define terminal-output-port (make-parameter (current-output-port)))

  (define (make-pipe)
    (and c-pipe
         (let ([fds (make-bytevector 8 0)])
           (and (= (c-pipe fds) 0)
                (cons (bytevector-s32-native-ref fds 0)
                      (bytevector-s32-native-ref fds 4))))))

  (define (duplicate-standard-output-port)
    ;; A stable route to the terminal while fd 1 is temporarily redirected
    ;; into an evaluated program's stdout pipe.
    (unless c-dup
      (error 'duplicate-standard-output-port "dup is unavailable"))
    (open-fd-output-port (c-dup 1) 'block (native-transcoder)))

  (define (duplicate-output-port port)
    ;; Keep an independently closeable route to an existing descriptor.  PTY
    ;; readers use this to outlive M-x's temporary evaluation display port.
    ;; Chez may represent an interactive terminal as a combined custom port,
    ;; for which port-file-descriptor raises; outside redirected evaluation,
    ;; fd 1 is the same terminal and is the safe fallback.
    (unless c-dup
      (error 'duplicate-output-port "dup is unavailable"))
    (guard (ex [else (duplicate-standard-output-port)])
      (open-fd-output-port (c-dup (port-file-descriptor port))
                           'block (native-transcoder))))

  (define (stream-lines fd emit!)
    (let ([p (open-fd-input-port fd 'block (native-transcoder))])
      (let loop ()
        (let ([line (get-line p)])
          (unless (eof-object? line)
            (emit! line)
            (loop))))
      (close-port p)))

  (define (call-with-streamed-output stdout! stderr! thunk)
    ;; Run thunk with Scheme's current ports and the process-level stdout and
    ;; stderr descriptors connected to pipes. Reader threads emit each line
    ;; as it arrives, including output inherited by child processes.
    (let ([out-pipe (make-pipe)] [err-pipe (make-pipe)])
      (unless (and out-pipe err-pipe c-dup c-dup2 c-close)
        (error 'call-with-streamed-output "output capture is unavailable"))
      (let* ([saved-out (c-dup 1)]
             [saved-err (c-dup 2)]
             [out (open-fd-output-port (c-dup (cdr out-pipe)) 'line
                                       (native-transcoder))]
             [err (open-fd-output-port (c-dup (cdr err-pipe)) 'line
                                       (native-transcoder))]
             [out-reader (fork-thread
                           (lambda () (stream-lines (car out-pipe) stdout!)))]
             [err-reader (fork-thread
                           (lambda () (stream-lines (car err-pipe) stderr!)))]
             [value #f])
        (dynamic-wind
          (lambda ()
            (flush-output-port (standard-output-port))
            (flush-output-port (standard-error-port))
            (c-dup2 (cdr out-pipe) 1)
            (c-dup2 (cdr err-pipe) 2))
          (lambda ()
            (set! value
              (parameterize ([current-output-port out]
                             [current-error-port err])
                (thunk))))
          (lambda ()
            (flush-output-port out)
            (flush-output-port err)
            (close-port out)
            (close-port err)
            (c-dup2 saved-out 1)
            (c-dup2 saved-err 2)
            (c-close saved-out)
            (c-close saved-err)
            (c-close (cdr out-pipe))
            (c-close (cdr err-pipe))))
        (thread-join out-reader)
        (thread-join err-reader)
        value)))

  (define winsize-ioctl
    ;; ioctl is variadic, and on ARM64 macOS variadic C functions use a
    ;; different calling convention -- say so where Chez supports it
    ;; (the plain declaration remains correct on the other platforms).
    (and libc-loaded?
         (or (guard (ex [else #f])
               (eval '(foreign-procedure (__varargs_after 2) "ioctl"
                                         (int unsigned-long u8*) int)))
             (guard (ex [else #f])
               (foreign-procedure "ioctl" (int unsigned-long u8*) int)))))

  ;; TIOCGWINSZ: Linux's own encoding; macOS and FreeBSD share BSD's.
  (define winsize-request (os-case #x5413 #x40087468 #x40087468))

  ;; struct termios differs across the three: the width and offset of the
  ;; local-modes word (c_lflag), the ISIG bit, the offset and indices of
  ;; the control-character array, and the disabling value -- all verified
  ;; against the platform headers.
  (define lflag-offset (os-case 12 24 12))
  (define lflag-64bit? (os-case #f #t #f))
  (define isig-bit (os-case #x1 #x80 #x80))
  (define cc-offset (os-case 17 32 16))
  (define vintr (os-case 0 8 8))
  (define vquit (os-case 1 9 9))
  (define vsusp 10)
  (define vdisable (os-case 0 #xff #xff))
  (define tcsanow 0)

  (define (get-lflag t)
    (if lflag-64bit?
        (bytevector-u64-native-ref t lflag-offset)
        (bytevector-u32-native-ref t lflag-offset)))

  (define (set-lflag! t v)
    (if lflag-64bit?
        (bytevector-u64-native-set! t lflag-offset v)
        (bytevector-u32-native-set! t lflag-offset v)))

  (define saved-termios #f)

  (define (terminal-raw!)
    ;; Switch the terminal to raw mode, remembering how to put it back.
    (when (and tcgetattr tcsetattr cfmakeraw)
      (guard (ex [else (void)])
        (let ([orig (make-bytevector 128 0)])
          (when (= (tcgetattr 0 orig) 0)
            (set! saved-termios orig)
            (let ([raw (bytevector-copy orig)])
              (cfmakeraw raw)
              (tcsetattr 0 tcsanow raw)))))))

  (define (terminal-restore!)
    (when (and tcsetattr saved-termios)
      (guard (ex [else (void)])
        (tcsetattr 0 tcsanow saved-termios))))

  (define (terminal-isig! on)
    ;; Let the terminal turn C-g into SIGINT (the interrupt character is
    ;; set to C-g; quit and suspend stay disabled), or stop doing so.
    (when (and tcgetattr tcsetattr)
      (guard (ex [else (void)])
        (let ([t (make-bytevector 128 0)])
          (when (= (tcgetattr 0 t) 0)
            (set-lflag! t (if on
                              (bitwise-ior (get-lflag t) isig-bit)
                              (bitwise-and (get-lflag t)
                                           (bitwise-not isig-bit))))
            (when on
              (bytevector-u8-set! t (+ cc-offset vintr) 7)   ; C-g
              (bytevector-u8-set! t (+ cc-offset vquit) vdisable)
              (bytevector-u8-set! t (+ cc-offset vsusp) vdisable))
            (tcsetattr 0 tcsanow t))))))

  (define (terminal-size)
    ;; (rows . cols) via TIOCGWINSZ, or #f.
    (and winsize-ioctl
         (guard (ex [else #f])
           (let ([size (make-bytevector 8 0)])
             (and (= (winsize-ioctl
                       (port-file-descriptor (standard-output-port))
                       winsize-request size) 0)
                  (let ([r (bytevector-u16-native-ref size 0)]
                        [c (bytevector-u16-native-ref size 2)])
                    (and (> r 0) (> c 0) (cons r c))))))))

  (define (watch-terminal-resize! thunk)
    ;; Call thunk on window-size changes.  SIGWINCH is signal 28 on both
    ;; Linux and macOS; #f when registration is unavailable.
    (guard (ex [else #f])
      (register-signal-handler 28 (lambda args (thunk)))
      #t)))
