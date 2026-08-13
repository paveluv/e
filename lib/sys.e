;; sys.e -- the e editor's system-specific layer: the library (sys).
;;
;; Everything that touches the operating system through libc lives here:
;; terminal modes via termios, the window size via ioctl, and the
;; SIGWINCH registration.  The core imports this library and stays free
;; of foreign procedures and platform constants.  Everything degrades
;; softly: without a terminal (or without libc) the calls below become
;; no-ops and terminal-size returns #f.

(library (sys)
  (export terminal-raw! terminal-restore! terminal-isig!
          terminal-size watch-terminal-resize!)
  (import (chezscheme))

  (define macos?
    (let* ([mt (symbol->string (machine-type))]
           [n (string-length mt)])
      (and (>= n 3) (string=? (substring mt (- n 3) n) "osx"))))

  (define libc-loaded?
    (guard (ex [else #f])
      (guard (ex2 [else (load-shared-object
                          (if macos? "libc.dylib" "libc.so"))])
        (load-shared-object (if macos? "libSystem.dylib" "libc.so.6")))
      #t))

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

  ;; TIOCGWINSZ
  (define winsize-request (if macos? #x40087468 #x5413))

  ;; struct termios differs between Linux and macOS: the width and offset
  ;; of the local-modes word (c_lflag), the ISIG bit, the offset and
  ;; indices of the control-character array, and the disabling value.
  (define lflag-offset (if macos? 24 12))
  (define isig-bit (if macos? #x80 #x1))
  (define cc-offset (if macos? 32 17))
  (define vquit (if macos? 9 1))
  (define vsusp 10)
  (define vdisable (if macos? #xff 0))
  (define tcsanow 0)

  (define (get-lflag t)
    (if macos?
        (bytevector-u64-native-ref t lflag-offset)
        (bytevector-u32-native-ref t lflag-offset)))

  (define (set-lflag! t v)
    (if macos?
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
    ;; Let the terminal turn C-c into SIGINT (with the quit and suspend
    ;; characters still disabled), or stop doing so.
    (when (and tcgetattr tcsetattr)
      (guard (ex [else (void)])
        (let ([t (make-bytevector 128 0)])
          (when (= (tcgetattr 0 t) 0)
            (set-lflag! t (if on
                              (bitwise-ior (get-lflag t) isig-bit)
                              (bitwise-and (get-lflag t)
                                           (bitwise-not isig-bit))))
            (when on
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
