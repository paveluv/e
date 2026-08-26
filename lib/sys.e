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
          duplicate-standard-output-port terminal-output-port)
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
