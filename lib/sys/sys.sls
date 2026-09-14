;; sys.sls -- the e editor's system-specific layer: the library (sys).
;;
;; Everything that touches the operating system through libc lives here:
;; terminal modes via termios, the window size via ioctl, SIGWINCH
;; registration, and pipes for evaluated programs' process output. The rest
;; of the editor imports this library and stays free of foreign procedures
;; and platform constants. Terminal operations degrade softly: without a
;; terminal (or without libc) they become no-ops and sys:terminal-size
;; returns #f.

(library (sys)
  (export terminal-raw! terminal-restore! terminal-isig!
          terminal-size watch-terminal-resize! call-with-streamed-output
          duplicate-standard-output-port duplicate-output-port
          duplicate-standard-input-port
          terminal-output-port
          terminal-character-width
          canonical-file-path file-info host-name terminal-name
          listen-local accept-local connect-local try-connect-local close-local-listener!
          call-with-connection-deadline unresponsive?
          connection-input connection-output connection-alive? close-connection! watch-daemon-signals!
          open-process write-process! process-input process-result close-process!
          (rename (poll-process! process-status) (command-process-pid process-pid))
          release-process! signal-process!
          ensure-private-directory! acquire-file-lock release-file-lock!
          remove-stale-socket! call-with-private-output-file redirect-daemon-ports!
          process-identity process-exited? remove-session! write-session! archive-session!
          call-with-private-input-file durability-uncertain? call-with-verified-base
          spawn-terminal-process terminal-process?
          terminal-process-input terminal-process-output
          terminal-process-pid resize-terminal-process!
          close-terminal-process! reap-terminal-process!)
  (import (chezscheme) (prefix (activity) activity:))

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
  (define c-statx
    (and libc-loaded? (eq? os 'linux)
         (guard (ex [else #f])
           (foreign-procedure __collect_safe "statx" (int u8* int unsigned u8*) int))))
  (define c-gethostname
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "gethostname" (u8* uptr) int))))
  (define c-ttyname-r
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "ttyname_r" (int u8* uptr) int))))
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
  ;; Both exec entries take the program as the argv copy's first string, so
  ;; a forked child converts no Scheme string on its way to exec.
  (define c-execv
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "execv" (uptr uptr) int))))
  (define c-execvp
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "execvp" (uptr uptr) int))))
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
  (define c-sigemptyset (and libc-loaded? (foreign-procedure "sigemptyset" (uptr) int)))
  (define c-sigaddset (and libc-loaded? (foreign-procedure "sigaddset" (uptr int) int)))
  (define c-sigprocmask (and libc-loaded? (foreign-procedure "sigprocmask" (int uptr uptr) int)))
  (define empty-signal-mask
    ;; sigset_t fits in 128 bytes on Linux, Darwin and FreeBSD. Only libc
    ;; accesses its layout. This immutable mask is also safe after fork.
    (and libc-loaded? (let ([mask (foreign-alloc 128)]) (c-sigemptyset mask) mask)))
  (define c-exit
    (and libc-loaded?
         (guard (ex [else #f]) (foreign-procedure "_exit" (int) void))))
  (define c-waitpid
    (and libc-loaded?
         (guard (ex [else #f])
           (foreign-procedure "waitpid" (int u8* int) int))))
  (define c-poll
    (and libc-loaded?
         (eval `(foreign-procedure __collect_safe "poll"
                  (uptr ,(os-case 'uptr 'unsigned-int 'unsigned-int) int) int))))
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

  (define (prepare-child!)
    ;; A child must not inherit the base's blocked service signals.
    (when (< (c-sigprocmask (os-case 2 3 3) empty-signal-mask 0) 0)
      (when c-perror (c-perror "sigprocmask"))
      (c-exit 127))
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

  (define (make-exec-arguments values)
    (let ([strings (map c-strdup values)])
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
    (activity:call-with
      (lambda ()
        (unless (and c-openpty c-fork c-setsid c-execv c-strdup c-free
                     c-dup2 c-close c-exit)
          (error 'spawn-terminal-process "PTY processes are unavailable"))
        (let ([master (make-bytevector 4 0)]
              [slave (make-bytevector 4 0)]
              [size (winsize rows cols)]
              [arguments (make-exec-arguments (if command (list shell "-c" command) (list shell)))])
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
               (prepare-child!)
               (when (and c-chdir (< (c-chdir directory) 0))
                 (when c-perror (c-perror "chdir"))
                 (c-exit 127))
               (when (and c-setenv (< (c-setenv "TERM" "xterm-256color" 1) 0))
                 (when c-perror (c-perror "setenv TERM"))
                 (c-exit 127))
               (c-execv (cadr arguments) (car arguments))
               (when c-perror (c-perror "execv terminal shell"))
               (c-exit 127)]
              [else
               (c-close slave-fd)
               (free-exec-arguments! arguments)
               (let ([input (open-fd-input-port (c-dup master-fd) 'block #f)]
                     [output (open-fd-output-port (c-dup master-fd) 'none #f)])
                 (make-terminal-process input output pid master-fd #f #f
                                        (make-mutex)))]))))))

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

  (define (nul-terminated-string bytes)
    (let find ([n 0])
      (cond [(= n (bytevector-length bytes)) #f]
            [(zero? (bytevector-u8-ref bytes n))
             (let ([trimmed (make-bytevector n)])
               (bytevector-copy! bytes 0 trimmed 0 n)
               (utf8->string trimmed))]
            [else (find (+ n 1))])))

  ;;; Local sockets -----------------------------------------------------------

  (define c-socket (and libc-loaded? (foreign-procedure "socket" (int int int) int)))
  (define c-bind (and libc-loaded? (foreign-procedure "bind" (int u8* unsigned) int)))
  (define c-listen (and libc-loaded? (foreign-procedure "listen" (int int) int)))
  (define c-accept
    (and libc-loaded? (foreign-procedure __collect_safe "accept" (int uptr uptr) int)))
  (define c-connect
    (and libc-loaded? (foreign-procedure __collect_safe "connect" (int u8* unsigned) int)))
  (define c-shutdown (and libc-loaded? (foreign-procedure "shutdown" (int int) int)))
  (define c-chmod (and libc-loaded? (foreign-procedure "chmod" (string unsigned) int)))
  (define c-geteuid (and libc-loaded? (foreign-procedure "geteuid" () unsigned)))
  (define c-getsockopt
    (and libc-loaded? (foreign-procedure "getsockopt" (int int int u8* u8*) int)))
  (define c-getpeereid
    (and libc-loaded? (not (eq? os 'linux))
         (foreign-procedure "getpeereid" (int u8* u8*) int)))
  (define c-errno
    (and libc-loaded?
         (foreign-procedure (os-case "__errno_location" "__error" "__error") () uptr)))
  (define c-fcntl
    (and libc-loaded?
         (or (guard (ex [else #f])
               (eval '(foreign-procedure (__varargs_after 2) "fcntl" (int int int) int)))
             (foreign-procedure "fcntl" (int int int) int))))
  (define c-open
    (and libc-loaded?
         (or (guard (ex [else #f])
               (eval '(foreign-procedure (__varargs_after 2) "open" (string int unsigned) int)))
             (foreign-procedure "open" (string int unsigned) int))))
  (define c-flock (and libc-loaded? (foreign-procedure "flock" (int int) int)))
  (define c-fchmod (and libc-loaded? (foreign-procedure "fchmod" (int unsigned) int)))
  (define c-strerror (and libc-loaded? (foreign-procedure "strerror" (int) string)))
  (define c-lstat
    (and libc-loaded? (not (eq? os 'linux))
         (or (guard (ex [else #f])
               (foreign-procedure (os-case "lstat" "lstat$INODE64" "lstat") (string u8*) int))
             (foreign-procedure "lstat" (string u8*) int))))
  (define c-fstat
    (and libc-loaded? (not (eq? os 'linux))
         (or (guard (ex [else #f])
               (foreign-procedure (os-case "fstat" "fstat$INODE64" "fstat") (int u8*) int))
             (foreign-procedure "fstat" (int u8*) int))))

  (define (os-error who path code)
    (error who (c-strerror code) path code))

  (define (descriptor-check who path result)
    (when (< result 0) (os-error who path (foreign-ref 'int (c-errno) 0)))
    result)

  (define (ownership-info path-or-fd)
    ;; Only the type, mode and uid are needed for private runtime files.
    ;; Linux uses statx's fixed ABI; Darwin/FreeBSD use their stat64 prefix.
    ;; Unlike browsing metadata, errors other than absence are not hidden.
    (let* ([out (make-bytevector 512 0)] [fd? (integer? path-or-fd)]
           [result
            (if (eq? os 'linux)
                (begin
                  (unless c-statx (error 'base "statx is required for private base ownership"))
                  (let ([path (string->utf8 (string-append (if fd? "" path-or-fd) (string #\nul)))])
                    (dynamic-wind (lambda () (lock-object path) (lock-object out))
                      (lambda () (c-statx (if fd? path-or-fd -100) path
                                   (if fd? #x1000 #x100) #xb out))
                      (lambda () (unlock-object out) (unlock-object path)))))
                ((if fd? c-fstat c-lstat) path-or-fd out))])
      (if (zero? result)
          (begin
            (when (and (eq? os 'linux) (not (= (logand (bytevector-u32-native-ref out 0) #xb) #xb)))
              (error 'base "filesystem did not supply ownership metadata" path-or-fd))
            (cons (bytevector-u16-native-ref out (os-case 28 4 24))
                  (bytevector-u32-native-ref out (os-case 20 16 28))))
          (let ([code (foreign-ref 'int (c-errno) 0)])
            (if (= code 2) #f (os-error 'base path-or-fd code))))))

  (define (private-info! path info kind)
    (unless (and info (= (logand (car info) #o170000) kind)
                 (= (cdr info) (c-geteuid)) (zero? (logand (car info) #o7077)))
      (error 'base "expected a private path owned by this user" path))
    info)

  (define (ensure-private-directory! path)
    (unless (ownership-info path)
      (guard (ex [(i/o-file-already-exists-error? ex) (void)] [else (raise ex)])
        (mkdir path #o700)))
    (private-info! path (ownership-info path) #o040000)
    (unless (= (logand (get-mode path) #o777) #o700)
      (error 'base "base directory must have mode 700" path)))

  (define (private-file-fd path append?)
    ;; NOFOLLOW and NONBLOCK prevent a symlink or FIFO from turning startup
    ;; into arbitrary file writes or an unbounded open. Validate before use.
    (let ([fd (descriptor-check 'base path
                (c-open path (logor 2 (os-case #o100 #x200 #x200)
                               (os-case #o400000 #x100 #x100)
                               (os-case #o4000 #x4 #x4)
                               (if append? (os-case #o2000 #x8 #x8) 0)) #o600))])
      (guard (ex [else (c-close fd) (raise ex)])
        (close-on-exec! fd)
        (private-info! path (ownership-info fd) #o100000)
        (descriptor-check 'base path (c-fchmod fd #o600))
        fd)))

  (define (acquire-file-lock path)
    (let ([fd (private-file-fd path #f)])
      (if (zero? (c-flock fd 6)) fd ; LOCK_EX | LOCK_NB
          (let ([code (foreign-ref 'int (c-errno) 0)])
            (c-close fd)
            (if (= code (os-case 11 35 35)) #f (os-error 'base path code))))))

  (define (release-file-lock! fd) (c-close fd)) ; Never unlink the lock inode.

  (define (remove-stale-socket! path)
    (cond [(ownership-info path)
           => (lambda (info)
                (private-info! path info #o140000)
                (delete-file path))]))

  (define-condition-type &durability-uncertain &error make-durability-uncertain durability-uncertain?)

  (define (call-with-directory-fd directory thunk)
    (let ([fd (descriptor-check 'session directory
                (c-open directory (logor (os-case #o200000 #x100000 #x20000)
                                         (os-case #o400000 #x100 #x100)) 0))])
      (dynamic-wind void
        (lambda ()
          (close-on-exec! fd)
          (private-info! directory (ownership-info fd) #o040000)
          (thunk fd))
        (lambda () (c-close fd)))))

  (define (sync-directory! fd directory changed)
    (guard (ex [else
                (if changed
                    (raise (condition (make-durability-uncertain) (make-who-condition 'session)
                             (make-message-condition (string-append changed "; directory sync failed and durability is uncertain"))
                             (make-irritants-condition (list directory ex))))
                    (raise ex))])
      (descriptor-check 'session directory
        ((foreign-procedure __collect_safe "fsync" (int) int) fd))))

  (define (remove-session! directory)
    (call-with-directory-fd directory
      (lambda (fd)
        (let* ([path (string-append directory "/session")] [info (ownership-info path)])
          (when info
            (private-info! path info #o100000)
            (descriptor-check 'shutdown path
              ((foreign-procedure "unlinkat" (int string int) int) fd "session" 0)))
          (sync-directory! fd directory (and info "session was removed"))))))

  (define (write-session! directory write!)
    (call-with-directory-fd directory
      (lambda (fd)
        (let ([path (string-append directory "/session")]
              [temporary (string-append directory "/session.tmp")]
              [opened? #f] [renamed? #f])
          (cond [(ownership-info path) => (lambda (info) (private-info! path info #o100000))])
          (dynamic-wind void
            (lambda ()
              (call-with-private-output-file temporary
                (lambda (port)
                  (set! opened? #t)
                  (write! port)
                  (flush-output-port port)
                  (descriptor-check 'session temporary
                    ((foreign-procedure __collect_safe "fsync" (int) int) (port-file-descriptor port)))))
              ;; The temporary port has closed successfully before rename.
              (descriptor-check 'session path
                ((foreign-procedure "renameat" (int string int string) int) fd "session.tmp" fd "session"))
              (set! renamed? #t)
              (sync-directory! fd directory "new session is installed"))
            (lambda ()
              (when (and opened? (not renamed?))
                (guard (ex [else (void)]) (delete-file temporary)))))))))

  (define (archive-session! directory)
    ;; A genuine no-replace rename preserves earlier recovery evidence.
    ;; If the OS cannot provide it, startup refuses instead of overwriting.
    (let ([rename (guard (ex [else (error 'session "atomic recovery-file preservation is unavailable" directory)])
                    (foreign-procedure (os-case "renameat2" "renameatx_np" "renameat2")
                      (int string int string unsigned) int))])
      (call-with-directory-fd directory
        (lambda (fd)
          (private-info! directory (ownership-info (string-append directory "/session")) #o100000)
          (let choose ([suffix 0])
            (let ([name (if (zero? suffix) "session.incompatible" (format "session.incompatible.~a" suffix))])
              (if (zero? (rename fd "session" fd name (os-case 1 4 1)))
                  (begin
                    (sync-directory! fd directory (string-append "saved session was preserved at " name))
                    (string-append directory "/" name))
                  (let ([code (foreign-ref 'int (c-errno) 0)])
                    (if (= code 17) (choose (+ suffix 1)) (os-error 'session name code))))))))))

  (define (call-with-private-input-file path read!)
    ;; Return #f only for absence. Type, ownership, permission and read
    ;; errors remain startup errors, never evidence of malformed data.
    (let ([fd (c-open path (logor (os-case #o400000 #x100 #x100) (os-case #o4000 #x4 #x4)) 0)])
      (if (< fd 0)
          (let ([code (foreign-ref 'int (c-errno) 0)])
            (if (= code 2) #f (os-error 'session path code)))
          (let ([port #f])
            (dynamic-wind void
              (lambda ()
                (close-on-exec! fd)
                (private-info! path (ownership-info fd) #o100000)
                (set! port (open-fd-input-port fd 'block #f))
                (read! port))
              (lambda () (if port (close-port port) (c-close fd))))))))

  (define (call-with-private-output-file path procedure)
    (let ([port (open-fd-output-port (private-file-fd path #f) 'block (native-transcoder))])
      (dynamic-wind void
        (lambda () (truncate-file port 0) (procedure port) (flush-output-port port))
        (lambda () (close-port port)))))

  (define (redirect-daemon-ports! path first?)
    (let ([fd (private-file-fd path #t)])
      (dynamic-wind void
        (lambda ()
          (flush-output-port (current-output-port))
          (flush-output-port (current-error-port))
          (for-each (lambda (target) (descriptor-check 'base path (c-dup2 fd target))) '(1 2)))
        (lambda () (c-close fd))))
    (when first?
      (c-setsid) ; A foreground process-group leader may already own its session.
      (let ([fd (descriptor-check 'base "/dev/null" (c-open "/dev/null" 0 0))])
        (dynamic-wind void
          (lambda () (descriptor-check 'base "/dev/null" (c-dup2 fd 0)))
          (lambda () (c-close fd))))))

  (define (process-record pid)
    (and (eq? os 'linux)
         (guard (ex [else #f])
           (let* ([stat (call-with-input-file (format "/proc/~a/stat" pid) get-line)]
                  [end (let scan ([i (- (string-length stat) 1)])
                         (cond [(< i 0) (error 'base "invalid process stat")]
                               [(char=? (string-ref stat i) #\)) i] [else (scan (- i 1))]))]
                  [in (open-string-input-port (substring stat (+ end 2) (string-length stat)))]
                  [fields (let read-all ([out '()])
                            (let ([field (read in)])
                              (if (eof-object? field) (reverse out) (read-all (cons field out)))))])
             (cons (list 'linux (call-with-input-file "/proc/sys/kernel/random/boot_id" get-line)
                     (list-ref fields 19)) (car fields))))))

  (define (process-generation pid)
    (let ([record (process-record pid)]) (and record (car record))))

  (define (process-identity)
    ;; Linux records boot identity and /proc's start ticks, not a reusable
    ;; pid alone. Other systems explicitly lack a verified force target until
    ;; their stable process-reference implementation is added with restart.
    (list (get-process-id) (process-generation (get-process-id))))

  (define (process-exited? identity)
    ;; Read-only waiting for an announced stop. A reused pid is already a
    ;; different instance; platforms without generation data may time out
    ;; conservatively. Forced signals instead require the pidfd proof below.
    (let ([pid (car identity)])
      (if (zero? (c-kill pid 0))
          (and (cadr identity)
               (let ([current (process-record pid)])
                 (and current (or (eq? (cdr current) 'Z) (eq? (cdr current) 'X)
                                  (not (equal? (car current) (cadr identity)))))))
          (let ([code (foreign-ref 'int (c-errno) 0)])
            (if (= code 3) #t (os-error 'restart pid code))))))

  (define (split-fields text separator?)
    (let loop ([start 0] [end 0] [out '()])
      (cond [(= end (string-length text))
             (reverse (if (= start end) out (cons (substring text start end) out)))]
            [(separator? (string-ref text end))
             (loop (+ end 1) (+ end 1) (if (= start end) out (cons (substring text start end) out)))]
            [else (loop start (+ end 1) out)])))

  (define (lock-identity fd)
    (let ([out (make-bytevector 256 0)] [path #vu8(0)])
      (dynamic-wind (lambda () (lock-object path) (lock-object out))
        (lambda ()
          (descriptor-check 'restart fd (c-statx fd path #x1000 #x100 out))
          (unless (logtest #x100 (bytevector-u32-native-ref out 0))
            (error 'restart "filesystem did not identify the ownership lock"))
          (list (bytevector-u32-native-ref out 136) (bytevector-u32-native-ref out 140)
            (bytevector-u64-native-ref out 32)))
        (lambda () (unlock-object out) (unlock-object path)))))

  (define (holds-lock? pid identity)
    (call-with-input-file "/proc/locks"
      (lambda (port)
        (let scan ()
          (let ([line (get-line port)])
            (and (not (eof-object? line))
                 (let ([fields (split-fields line char-whitespace?)])
                   (or (and (= (length fields) 8)
                            (equal? (list-head (cdr fields) 3) '("FLOCK" "ADVISORY" "WRITE"))
                            (equal? (string->number (list-ref fields 4)) pid)
                            (let ([device (split-fields (list-ref fields 5) (lambda (c) (char=? c #\:)))])
                              (and (= (length device) 3)
                                   (equal? (map string->number device '(16 16 10)) identity))))
                       (scan)))))))))

  (define (call-with-verified-base directory thunk)
    ;; Keep a pidfd, not a numeric pid, from proof through both signals.
    ;; /proc/locks ties the record to this actual flock inode and holder;
    ;; process generation and a live pidfd exclude a recycled process id.
    (let ([lock (private-file-fd (string-append directory "/lock") #f)])
      (dynamic-wind void
        (lambda ()
          (if (zero? (c-flock lock 6)) #f
              (begin
                (unless (= (foreign-ref 'int (c-errno) 0) (os-case 11 35 35))
                  (os-error 'restart directory (foreign-ref 'int (c-errno) 0)))
                (unless (and (eq? os 'linux) c-statx)
                  (error 'restart "cannot verify a force target on this OS; manual recovery is required"))
                (let* ([record (call-with-private-input-file (string-append directory "/pid")
                                 (lambda (port)
                                   (let* ([bytes (get-bytevector-all port)]
                                          [in (open-bytevector-input-port (if (eof-object? bytes) #vu8() bytes) (native-transcoder))]
                                          [value (read in)])
                                     (and (eof-object? (read in)) value))))]
                       [pid (and (list? record) (= (length record) 3) (eq? (car record) 'base) (cadr record))]
                       [open (guard (ex [else (error 'restart "pidfd support is unavailable; manual recovery is required")])
                               (foreign-procedure "pidfd_open" (int unsigned) int))]
                       [send (guard (ex [else (error 'restart "pidfd signals are unavailable; manual recovery is required")])
                               (foreign-procedure "pidfd_send_signal" (int int uptr unsigned) int))])
                  (unless (and (integer? pid) (exact? pid) (> pid 0) (caddr record))
                    (error 'restart "incomplete base identity; manual recovery is required"))
                  (let ([fd (descriptor-check 'restart directory (open pid 0))] [pollfd (foreign-alloc 8)])
                    (dynamic-wind void
                      (lambda ()
                        (define (wait! deadline)
                          (let loop ()
                            (foreign-set! 'int pollfd 0 fd)
                            (foreign-set! 'short pollfd 4 1)
                            (foreign-set! 'short pollfd 6 0)
                            (let* ([remaining (time-difference deadline (current-time 'time-monotonic))]
                                   [ms (max 0 (min 50 (+ (* (time-second remaining) 1000)
                                                        (div (time-nanosecond remaining) 1000000))))]
                                   [result (c-poll pollfd 1 ms)])
                              (when (and (< result 0) (not (= (foreign-ref 'int (c-errno) 0) 4)))
                                (os-error 'restart directory (foreign-ref 'int (c-errno) 0)))
                              (cond [(logtest #x20 (foreign-ref 'short pollfd 6)) (error 'restart "lost process reference")]
                                    [(logtest #x11 (foreign-ref 'short pollfd 6)) #t]
                                    [(time>=? (current-time 'time-monotonic) deadline) #f]
                                    [else (loop)]))))
                        (unless (and (equal? (process-generation pid) (caddr record))
                                     (holds-lock? pid (lock-identity lock))
                                     (not (wait! (current-time 'time-monotonic))))
                          (error 'restart "base identity does not match the live ownership lock; manual recovery is required"))
                        (thunk
                          (lambda (signal)
                            (let ([result (send fd signal 0 0)])
                              (when (and (< result 0) (not (= (foreign-ref 'int (c-errno) 0) 3)))
                                (os-error 'restart directory (foreign-ref 'int (c-errno) 0)))))
                          wait!))
                      (lambda () (c-close fd) (foreign-free pollfd))))))))
        (lambda () (c-close lock)))))

  (define-condition-type &unresponsive &error make-unresponsive unresponsive?)
  (define (unresponsive! message)
    (raise (condition (make-unresponsive) (make-who-condition 'e) (make-message-condition message))))

  (define-record-type local-listener
    (fields fd path lock (mutable closed)))
  (define-record-type connection
    (fields fd input output lock (mutable closed)))

  (define (socket-check who result)
    (when (< result 0)
      (error who "local socket operation failed" (foreign-ref 'int (c-errno) 0)))
    result)

  (define (close-on-exec! fd)
    (socket-check 'local-socket (c-fcntl fd 2 1))) ; F_SETFD, FD_CLOEXEC

  (define (call-with-local-address path proc)
    (unless (and (string? path) (> (string-length path) 0)
                 (not (memv #\nul (string->list path))))
      (error 'local-socket "expected a nonempty path without NUL" path))
    (let* ([bytes (string->utf8 path)] [n (bytevector-length bytes)]
           [size (+ n 3)] [address (make-bytevector size 0)])
      (unless (< n (os-case 108 104 104))
        (error 'local-socket "socket path is too long" path))
      (if (eq? os 'linux)
          (bytevector-u16-native-set! address 0 1) ; AF_UNIX
          (begin (bytevector-u8-set! address 0 size) (bytevector-u8-set! address 1 1)))
      (bytevector-copy! bytes 0 address 2 n)
      ;; connect can block; collect-safe calls must not retain movable data.
      (dynamic-wind (lambda () (lock-object address))
        (lambda () (proc address size))
        (lambda () (unlock-object address)))))

  (define (same-user! fd)
    ;; Local access belongs to this OS user. Directory/socket permissions
    ;; alone differ across Unix systems; check peer credentials at both ends.
    (let ([uid (make-bytevector 4 0)])
      (if c-getpeereid
          (socket-check 'local-socket (c-getpeereid fd uid (make-bytevector 4 0)))
          (let ([credentials (make-bytevector 12 0)] [size (make-bytevector 4 0)])
            (bytevector-u32-native-set! size 0 12)
            (socket-check 'local-socket (c-getsockopt fd 1 17 credentials size)) ; SO_PEERCRED
            (bytevector-copy! credentials 4 uid 0 4)))
      (unless (= (bytevector-u32-native-ref uid 0) (c-geteuid))
        (error 'local-socket "peer belongs to another OS user"))))

  (define (connection-from-fd fd)
    (let ([input #f] [output #f] [out-fd #f])
      (guard (ex [else
                  (if input (close-port input) (c-close fd))
                  (if output (close-port output) (when out-fd (c-close out-fd)))
                  (raise ex)])
        (same-user! fd)
        (close-on-exec! fd)
        (set! input (open-fd-input-port fd 'block #f))
        (set! out-fd (socket-check 'local-socket (c-dup fd)))
        (close-on-exec! out-fd)
        (set! output (open-fd-output-port out-fd 'none #f))
        (make-connection fd input output (make-mutex) #f))))

  (define (listen-local path)
    (unless c-socket (error 'listen-local "local sockets are unavailable"))
    (call-with-local-address path
      (lambda (address size)
        (let ([fd (socket-check 'listen-local (c-socket 1 1 0))] [bound? #f])
          (guard (ex [else (c-close fd) (when bound? (delete-file path)) (raise ex)])
            (close-on-exec! fd)
            ;; Never unlink before binding: an existing endpoint or ordinary
            ;; file belongs to its current owner, including after a crash.
            (socket-check 'listen-local (c-bind fd address size))
            (set! bound? #t)
            (socket-check 'listen-local (c-chmod path #o600))
            (socket-check 'listen-local (c-listen fd 32))
            (make-local-listener fd (string-copy path) (make-mutex) #f))))))

  (define (accept-local listener)
    (let again ()
      (if (local-listener-closed listener) #f
          (let ([fd (c-accept (local-listener-fd listener) 0 0)])
            (cond [(>= fd 0) (guard (ex [else (again)]) (connection-from-fd fd))]
                  [(local-listener-closed listener) #f]
                  [(= (foreign-ref 'int (c-errno) 0) 4) (again)] ; EINTR
                  [else (socket-check 'accept-local fd)])))))

  (define (try-connect-local path deadline)
    ;; #f means absent/refused, the only failures that permit automatic start.
    ;; AF_UNIX EAGAIN (a full backlog on Linux) needs a fresh connect attempt;
    ;; SO_ERROR=0 after EAGAIN would falsely report an established connection.
    (unless c-socket (error 'connect-local "local sockets are unavailable"))
    (cond [(ownership-info path) => (lambda (info) (private-info! path info #o140000))])
    (call-with-local-address path
      (lambda (address size)
        (let again ()
          (when (time>=? (current-time 'time-monotonic) deadline)
            (unresponsive! "the base is unresponsive (connection timed out)"))
          (let* ([fd (socket-check 'connect-local (c-socket 1 1 0))]
                 [result
                  (guard (ex [else (c-close fd) (raise ex)])
                    (close-on-exec! fd)
                    (socket-check 'connect-local (c-fcntl fd 4 (os-case #o4000 4 4)))
                    (if (zero? (c-connect fd address size)) 'connected
                        (let ([code (foreign-ref 'int (c-errno) 0)])
                          (cond [(memv code (list 2 (os-case 111 61 61))) 'absent]
                                [(memv code (list 4 (os-case 11 35 35))) 'retry]
                                [(= code (os-case 115 36 36))
                                 (let ([pollfd (foreign-alloc 8)])
                                   (dynamic-wind void
                                     (lambda ()
                                       (foreign-set! 'int pollfd 0 fd)
                                       (foreign-set! 'short pollfd 4 4) ; POLLOUT
                                       (let wait ()
                                         (let* ([remaining (time-difference deadline (current-time 'time-monotonic))]
                                                [ms (+ (* (time-second remaining) 1000)
                                                       (div (time-nanosecond remaining) 1000000))])
                                           (when (<= ms 0) (unresponsive! "the base is unresponsive (connection timed out)"))
                                           (when (<= (c-poll pollfd 1 (min ms 50)) 0) (wait))))
                                       (let ([error (make-bytevector 4)] [size (make-bytevector 4)])
                                         (bytevector-u32-native-set! size 0 4)
                                         (socket-check 'connect-local
                                           (c-getsockopt fd (os-case 1 #xffff #xffff) (os-case 4 #x1007 #x1007) error size))
                                         (let ([code (bytevector-s32-native-ref error 0)])
                                           (cond [(zero? code) 'connected]
                                                 [(memv code (list 2 (os-case 111 61 61))) 'absent]
                                                 [else (os-error 'connect-local path code)]))))
                                     (lambda () (foreign-free pollfd))))]
                                [else (os-error 'connect-local path code)]))))])
            (case result
              [(connected)
               (guard (ex [else (c-close fd) (raise ex)])
                 (socket-check 'connect-local (c-fcntl fd 4 0)))
               (connection-from-fd fd)]
              [else
               (c-close fd)
               (and (eq? result 'retry)
                    (begin (sleep (make-time 'time-duration 50000000 0)) (again)))]))))))

  (define connect-local
    (case-lambda
      [(path) (connect-local path (add-duration (current-time 'time-monotonic) (make-time 'time-duration 0 10)))]
      [(path deadline)
       (or (try-connect-local path deadline) (error 'connect-local "no base is listening" path))]))

  (define (call-with-connection-deadline connection deadline thunk)
    ;; A single watchdog bounds the complete exchange, including partial
    ;; frames and blocked writes. Shutdown wakes the blocked port operation.
    (let* ([lock (make-mutex)] [ready (make-condition)] [finished? #f] [expired? #f]
           [watchdog
            (fork-thread
              (lambda ()
                (when (with-mutex lock
                        (let wait ()
                          (let ([now (current-time 'time-monotonic)])
                            (cond [finished? #f]
                              [(time>=? now deadline) (set! expired? #t) #t]
                              [else
                               (condition-wait ready lock (time-difference deadline now))
                               (wait)]))))
                  (close-connection! connection))))])
      (guard (ex [expired? (unresponsive! "the base is unresponsive (exchange timed out)")]
                 [else (raise ex)])
        (dynamic-wind void thunk
          (lambda ()
            (with-mutex lock (set! finished? #t) (condition-signal ready))
            (thread-join watchdog)
            (when expired? (unresponsive! "the base is unresponsive (exchange timed out)")))))))

  (define (close-connection! connection)
    (with-mutex (connection-lock connection)
      (unless (connection-closed connection)
        (connection-closed-set! connection #t)
        ;; Wake blocked reads/writes before closing their port descriptors.
        (c-shutdown (connection-fd connection) 2)
        (for-each (lambda (port) (guard (ex [else (void)]) (close-port port)))
                  (list (connection-input connection) (connection-output connection))))))

  (define (connection-alive? connection)
    ;; A lifecycle caller's reader is waiting for its RPC, so it cannot
    ;; discover EOF during the pause itself. Peek without consuming bytes
    ;; or changing descriptor flags; a dead requester has not accepted yet.
    (with-mutex (connection-lock connection)
      (and (not (connection-closed connection))
           (let ([n ((foreign-procedure "recv" (int u8* uptr int) iptr)
                     (connection-fd connection) (make-bytevector 1) 1
                     (logor 2 (os-case #x40 #x80 #x80)))]) ; MSG_PEEK | MSG_DONTWAIT
             (or (> n 0)
                 (and (< n 0) (= (foreign-ref 'int (c-errno) 0) (os-case 11 35 35))))))))

  (define (close-local-listener! listener)
    (with-mutex (local-listener-lock listener)
      (unless (local-listener-closed listener)
        (local-listener-closed-set! listener #t)
        (c-shutdown (local-listener-fd listener) 2)
        (c-close (local-listener-fd listener))
        (delete-file (local-listener-path listener)))))

  (define (watch-daemon-signals! stop!)
    ;; Install before creating any worker: each inherits this blocked mask.
    ;; Chez 10.0 queues a registered signal on whichever thread receives it;
    ;; a worker may then block in I/O before servicing that queue. One POSIX
    ;; receiver gives these process-lifetime events a predictable owner.
    (let ([mask (foreign-alloc 128)] [received (foreign-alloc 4)]
          [block (foreign-procedure "pthread_sigmask" (int uptr uptr) int)]
          [wait (foreign-procedure __collect_safe "sigwait" (uptr uptr) int)])
      (guard (ex [else (foreign-free mask) (foreign-free received) (raise ex)])
        (c-sigemptyset mask)
        (for-each (lambda (signal) (c-sigaddset mask signal)) '(1 2 15))
        (let ([code (block (os-case 0 1 1) mask 0)])
          (unless (zero? code) (os-error 'watch-daemon-signals! "pthread_sigmask" code)))
        (fork-thread
          (lambda ()
            (dynamic-wind void
              (lambda ()
                (guard (ex [else
                            (display-condition ex (current-error-port))
                            (newline (current-error-port)) (stop!)])
                  (let loop ()
                    (let ([code (wait mask received)])
                      (unless (zero? code) (os-error 'watch-daemon-signals! "sigwait" code)))
                    (unless (= (foreign-ref 'int received 0) 1) (stop!)) ; ignore HUP
                    (loop))))
              (lambda () (foreign-free mask) (foreign-free received))))))))

  ;;; Foreground commands -----------------------------------------------------

  ;; One caller owns a command, its ports and its completion. Service stderr
  ;; alongside stdout/stdin rather than leaving reader threads behind on an
  ;; escape. Only a poll blocks, with foreign memory and the collector released.
  ;; Exec one argument list; cancellation owns that PID, not the
  ;; editor's process group or arbitrary children of other callers.
  (define-record-type command-process
    (fields to from errors pid buffer capture
            (mutable code) (mutable input process-input set-process-input!)
            (mutable prefix) (mutable closed) (mutable complaint)))

  (define (open-process arguments)
    (activity:call-with
      (lambda ()
        (unless (and c-waitpid c-kill c-poll c-errno c-pipe c-fork c-execvp
                     c-dup2 c-close c-exit c-strdup c-free)
          (error 'open-process "command processes are unavailable"))
        (unless (and (list? arguments) (pair? arguments) (for-all string? arguments))
          (error 'open-process "expected a nonempty argument list" arguments))
        (with-interrupts-disabled
          ;; Chez registers open-process-ports children for reaping during GC,
          ;; discarding their status. Fork directly, as for PTYs, so this owner
          ;; alone can reap the child even if collection runs before output EOF.
          (let ([fds '()] [ports '()] [pid #f] [argv #f] [process #f] [ready? #f])
            (define (pipe!)
              (let ([pair (make-pipe)])
                (unless pair (error 'open-process "pipe failed"))
                (set! fds (cons (car pair) (cons (cdr pair) fds)))
                pair))
            (define (close-fd! fd)
              (c-close fd)
              (set! fds (remv fd fds)))
            (define (port! fd output?)
              (let ([port ((if output? open-fd-output-port open-fd-input-port) fd 'none #f)])
                (set! fds (remv fd fds))
                (set! ports (cons port ports))
                (set-port-nonblocking! port #t)
                (close-on-exec! fd)
                port))
            (dynamic-wind void
              (lambda ()
                (set! argv (make-exec-arguments arguments))
                (let* ([to (pipe!)] [from (pipe!)] [errors (pipe!)])
                  (set! pid (c-fork))
                  (cond
                    [(< pid 0) (error 'open-process "fork failed")]
                    [(zero? pid)
                     (when (or (< (c-dup2 (car to) 0) 0)
                               (< (c-dup2 (cdr from) 1) 0)
                               (< (c-dup2 (cdr errors) 2) 0))
                       (when c-perror (c-perror "dup2"))
                       (c-exit 127))
                     (prepare-child!)
                     (c-execvp (cadr argv) (car argv))
                     (when c-perror (c-perror (car arguments)))
                     (c-exit 127)]
                    [else
                     (for-each close-fd! (list (car to) (cdr from) (cdr errors)))
                     (let* ([to (port! (cdr to) #t)]
                            [from (port! (car from) #f)] [errors (port! (car errors) #f)])
                       (set! process (make-command-process to from errors pid (make-bytevector 4096)
                                                           (call-with-values open-bytevector-output-port cons) #f #f #f #f #f)))
                     (set-process-input! process
                                         (make-custom-binary-input-port "command output"
                                           (lambda (bytes start count) (read-process! process bytes start count))
                                           #f #f (lambda () (close-process! process))))
                     (set! ready? #t)
                     process])))
              (lambda ()
                (when argv (free-exec-arguments! argv))
                (for-each c-close fds)
                (unless ready?
                  (if process (close-process! process)
                      (begin
                        (for-each (lambda (port) (guard (ex [else (void)]) (close-port port))) ports)
                        (when (and pid (> pid 0))
                          (c-kill pid 9)
                          (let wait ()
                            (when (and (< (c-waitpid pid (make-bytevector 4) 0) 0)
                                       (= (foreign-ref 'int (c-errno) 0) 4))
                              (wait))))))))))))))

  (define (wait-process-io! process writing?)
    (let* ([ports (filter (lambda (port) (not (port-closed? port)))
                    (append (list (command-process-from process) (command-process-errors process))
                            (if writing? (list (command-process-to process)) '())))]
           [polls #f])
      (dynamic-wind #t
        (lambda () (set! polls (foreign-alloc (* 8 (length ports)))))
        (lambda ()
          (do ([ports ports (cdr ports)] [offset 0 (+ offset 8)]) ((null? ports))
            (foreign-set! 'int polls offset (port-file-descriptor (car ports)))
            (foreign-set! 'short polls (+ offset 4)
              (if (eq? (car ports) (command-process-to process)) 4 1)) ; POLLOUT / POLLIN
            (foreign-set! 'short polls (+ offset 6) 0))
          (let again ()
            (when (< (c-poll polls (length ports) -1) 0)
              (if (= (foreign-ref 'int (c-errno) 0) 4) (again) ; EINTR
                  (error 'process "poll failed" (foreign-ref 'int (c-errno) 0))))))
        (lambda () (foreign-free polls)))))

  (define (capture-process! process port sink)
    ;; One available chunk per turn: a noisy stream cannot starve the other.
    (if (port-closed? port) 0
        (let* ([buffer (command-process-buffer process)]
               [count (get-bytevector-some! port buffer 0 (bytevector-length buffer))])
          (cond [(eof-object? count) (close-port port) 0]
                [else (put-bytevector sink buffer 0 count) count]))))

  (define (capture-process-errors! process)
    (capture-process! process (command-process-errors process)
                      (car (command-process-capture process))))

  (define (write-process! process bytes)
    ;; Submit one optional input body and then EOF. Keep early stdout while
    ;; sending a large body, so even a program writing before reading can run.
    (when (command-process-closed process) (error 'write-process! "command is closed"))
    (let ([to (command-process-to process)])
      (when (port-closed? to) (error 'write-process! "command input was already sent"))
      (let-values ([(out take) (open-bytevector-output-port)])
        (when bytes
          (let loop ([start 0])
            (when (< start (bytevector-length bytes))
              (capture-process-errors! process)
              (capture-process! process (command-process-from process) out)
              (let ([count (put-bytevector-some to bytes start (- (bytevector-length bytes) start))])
                (when (zero? count) (wait-process-io! process #t))
                (loop (+ start count))))))
        (command-process-prefix-set! process (open-bytevector-input-port (take)))
        (close-port to))))

  (define (poll-process! process)
    ;; A returned status and its adoption are indivisible: never signal a PID
    ;; after reaping it, including on engine expiry. Other owners' PIDs stay out.
    (or (command-process-code process)
        (with-interrupts-disabled
          (let* ([status (make-bytevector 4)]
                 [pid (command-process-pid process)] [result (c-waitpid pid status 1)])
            (cond [(= result pid)
                   (let* ([bits (bytevector-s32-native-ref status 0)]
                          [signal (bitwise-and bits #x7f)]
                          [code (if (zero? signal) (bitwise-and (bitwise-arithmetic-shift-right bits 8) #xff)
                                    (- signal))])
                     (command-process-code-set! process code)
                     code)]
                  [(zero? result) #f]
                  [(= (foreign-ref 'int (c-errno) 0) 4) (poll-process! process)]
                  [else
                   (command-process-code-set! process 'unavailable)
                   (error 'process "waitpid failed" (foreign-ref 'int (c-errno) 0))])))))

  (define (finish-process! process)
    (let wait ()
      (let ([count (capture-process-errors! process)])
        (unless (poll-process! process)
          (when (zero? count) (sleep (make-time 'time-duration 10000000 0)))
          (wait))))
    (let drain ()
      (when (> (capture-process-errors! process) 0) (drain)))
    (close-port (command-process-errors process)))

  (define (read-process! process bytes start count)
    (when (command-process-closed process) (error 'process "command is closed"))
    (cond
      [(zero? count) 0]
      [(command-process-prefix process)
       => (lambda (prefix)
            (let ([got (get-bytevector-n! prefix bytes start count)])
              (if (not (eof-object? got)) got
                  (begin (close-port prefix) (command-process-prefix-set! process #f)
                         (read-process! process bytes start count)))))]
      [else
       (capture-process-errors! process)
       (let* ([from (command-process-from process)]
              [got (if (port-closed? from) (eof-object) (get-bytevector-some! from bytes start count))])
         (cond [(eof-object? got) (close-port from) (finish-process! process) 0]
               [(zero? got) (wait-process-io! process #f) (read-process! process bytes start count)]
               [else got]))]))

  (define (process-result process)
    ;; Ask after output EOF or explicit close. Status matches Chez's system:
    ;; an exit code, or the negated terminating signal.
    (unless (integer? (command-process-code process))
      (error 'process-result "command completion is unavailable"))
    (unless (command-process-complaint process)
      (command-process-complaint-set! process
        (bytevector->string ((cdr (command-process-capture process)))
                            (make-transcoder (utf-8-codec) 'none 'replace))))
    (values (command-process-code process) (string-copy (command-process-complaint process))))

  (define (close-process! process)
    (with-interrupts-disabled
      (unless (command-process-closed process)
        (command-process-closed-set! process #t)
        (dynamic-wind void
          (lambda ()
            (unless (poll-process! process)
              (c-kill (command-process-pid process) 15)
              (let wait ([attempts 8])
                (unless (poll-process! process)
                  (if (zero? attempts) (c-kill (command-process-pid process) 9)
                      (begin (sleep (make-time 'time-duration 25000000 0))
                             (wait (- attempts 1))))))
              (let wait ()
                (unless (poll-process! process)
                  (sleep (make-time 'time-duration 1000000 0)) (wait)))))
          (lambda ()
            (guard (ex [else (void)]) (capture-process-errors! process))
            (for-each (lambda (port) (when port (guard (ex [else (void)]) (close-port port))))
              (list (command-process-to process) (command-process-from process)
                    (command-process-errors process) (command-process-prefix process)
                    (process-input process))))))))

  (define (release-process! process)
    ;; Give up cancellation ownership without signalling the child. Bootstrap
    ;; uses this after its head exits, or after a startup timeout. Reap by the
    ;; owned child pid, even when the child outlives the calling runtime.
    (with-interrupts-disabled
      (unless (command-process-closed process)
        (let ([done? (poll-process! process)])
          (command-process-closed-set! process #t)
          (unless done?
            (fork-thread
              (lambda ()
                (let ([status (make-bytevector 4)]
                      [waitpid (foreign-procedure __collect_safe "waitpid" (int u8* int) int)])
                  (dynamic-wind (lambda () (lock-object status))
                    (lambda ()
                      (let wait ()
                        (when (and (< (waitpid (command-process-pid process) status 0) 0)
                                   (= (foreign-ref 'int (c-errno) 0) 4)) (wait))))
                    (lambda () (unlock-object status)))))))
          (for-each (lambda (port) (when port (guard (ex [else (void)]) (close-port port))))
            (list (command-process-to process) (command-process-from process)
                  (command-process-errors process) (command-process-prefix process)
                  (process-input process)))))))

  (define (signal-process! process signal)
    (with-interrupts-disabled
      (and (not (command-process-closed process)) (not (poll-process! process))
           (zero? (descriptor-check 'process (command-process-pid process)
                    (c-kill (command-process-pid process) signal))))))

  (define (host-name)
    (and c-gethostname
         (let ([out (make-bytevector 1024 0)])
           (and (zero? (c-gethostname out (bytevector-length out)))
                (nul-terminated-string out)))))

  (define (terminal-name)
    ;; ttyname_r owns its output buffer, unlike ttyname's shared C storage.
    (and c-ttyname-r
         (let ([out (make-bytevector 1024 0)])
           (and (zero? (c-ttyname-r 0 out (bytevector-length out)))
                (nul-terminated-string out)))))

  (define (canonical-file-path path)
    ;; The absolute, symlink-resolved spelling of an existing path, or #f.
    ;; PATH_MAX is commonly 4096; realpath fails instead of overflowing the
    ;; caller-provided buffer.
    (and c-realpath
         (let ([out (make-bytevector 4096 0)])
           (and (not (= (c-realpath path out) 0))
                (nul-terminated-string out)))))

  (define file-info
    ;; #(kind permissions byte-size modified-ns created-ns), or #f when
    ;; inaccessible. Never open the file to obtain metadata: a device/FIFO
    ;; must not turn directory browsing into a read. Unknown fields are #f.
    ;; Linux's fixed statx ABI includes birth time; ctime is not creation.
    (case-lambda
      [(path) (file-info path #f)]
      [(path follow?)
       (define (portable)
         (guard (ex [else #f])
           (and (file-exists? path follow?)
                (vector (cond [(and (not follow?) (file-symbolic-link? path)) 'link]
                              [(file-directory? path follow?) 'directory]
                              [(file-regular? path follow?) 'file]
                              [else 'special])
                  (guard (ex [else #f]) (get-mode path)) #f
                  (guard (ex [else #f])
                    (let ([t (file-modification-time path)])
                      (+ (* (time-second t) 1000000000) (time-nanosecond t)))) #f))))
       (unless (and (string? path) (not (memv #\nul (string->list path))))
         (error 'file-info "expected a path without NUL" path))
       (if (not c-statx) (portable)
           (let ([name (string->utf8 (string-append path (string #\nul)))]
                 [out (make-bytevector 256 0)])
             (define (stamp offset)
               (+ (* (bytevector-s64-native-ref out offset) 1000000000)
                  (bytevector-u32-native-ref out (+ offset 8))))
             (dynamic-wind
               (lambda () (lock-object name) (lock-object out))
               (lambda ()
                 ;; AT_FDCWD, AT_NO_AUTOMOUNT, optional NOFOLLOW, and the
                 ;; requested TYPE/MODE/SIZE/MTIME/BTIME fields. Both buffers
                 ;; are pinned while a slow filesystem lets other threads GC.
                 (if (zero? (c-statx -100 name (if follow? #x800 #x900) #xa43 out))
                     (let ([mask (bytevector-u32-native-ref out 0)]
                           [mode (bytevector-u16-native-ref out 28)])
                       (vector
                         (case (logand mode #o170000)
                           [(#o040000) 'directory] [(#o100000) 'file]
                           [(#o120000) 'link] [else 'special])
                         (and (not (zero? (logand mask 2))) (logand mode #o7777))
                         (and (not (zero? (logand mask #x200))) (bytevector-u64-native-ref out 40))
                         (and (not (zero? (logand mask #x40))) (stamp 112))
                         (and (not (zero? (logand mask #x800))) (stamp 80))))
                     ;; Older kernels can have libc's entry but no syscall.
                     (and (= (foreign-ref 'int (c-errno) 0) 38) (portable))))
               (lambda () (unlock-object out) (unlock-object name)))))]))

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

  (define (duplicate-standard-input-port)
    ;; A private route to the terminal's input: its own port object,
    ;; its own lock -- a thread blocked reading it never starves
    ;; writers on the console ports.
    (unless c-dup
      (error 'duplicate-standard-input-port "dup is unavailable"))
    (open-fd-input-port (c-dup 0) 'block (native-transcoder)))

  (define (output-file-descriptor port)
    ;; Chez may represent an interactive terminal as a combined custom port,
    ;; for which port-file-descriptor raises; outside redirected evaluation,
    ;; fd 1 is the same terminal and is the safe fallback.
    (guard (ex [else 1]) (port-file-descriptor port)))

  (define (duplicate-output-port port)
    ;; Keep an independently closeable route to an existing descriptor.  PTY
    ;; readers use this to outlive M-x's temporary evaluation display port.
    (unless c-dup
      (error 'duplicate-output-port "dup is unavailable"))
    (open-fd-output-port (c-dup (output-file-descriptor port))
                         'block (native-transcoder)))

  (define-record-type capture-stream
    (fields target standard emit
            (mutable pipe) (mutable saved) (mutable input) (mutable output)
            (mutable reader) (mutable failure)))

  (define (read-capture! stream)
    (define (failed! ex)
      (unless (capture-stream-failure stream)
        (capture-stream-failure-set! stream (list ex))))
    (dynamic-wind #t void
      (lambda ()
        (guard (ex [else (failed! ex)])
          (let loop ()
            (let ([line (get-line (capture-stream-input stream))])
              (unless (eof-object? line)
                ;; A failed callback stops delivery, but keep draining so the
                ;; producer can finish. The owner reports the failure after join.
                (unless (capture-stream-failure stream)
                  (guard (ex [else (failed! ex)]) ((capture-stream-emit stream) line)))
                (loop))))))
      (lambda ()
        (guard (ex [else (failed! ex)]) (close-port (capture-stream-input stream))))))

  (define (call-with-streamed-output stdout! stderr! thunk)
    ;; Run thunk with Scheme's current ports and the process-level stdout and
    ;; stderr descriptors connected to pipes. Reader threads emit each line
    ;; as it arrives, including output inherited by child processes.
    (unless (and c-pipe c-dup c-dup2 c-close)
      (error 'call-with-streamed-output "output capture is unavailable"))
    (let ([streams (map (lambda (target standard emit)
                          (make-capture-stream target standard emit #f #f #f #f #f #f))
                        '(1 2) (list (standard-output-port) (standard-error-port)) (list stdout! stderr!))]
          [ended? #f] [failure #f] [interrupted #f])
      (define (attempt thunk)
        (guard (ex [else (unless failure (set! failure (list ex)))]) (thunk)))
      (define (descriptor result)
        (when (< result 0) (error 'call-with-streamed-output "descriptor operation failed"))
        result)
      (define (start! stream)
        (capture-stream-pipe-set! stream (or (make-pipe) (error 'call-with-streamed-output "pipe failed")))
        (capture-stream-saved-set! stream (descriptor (c-dup (capture-stream-target stream))))
        (for-each close-on-exec!
          (list (car (capture-stream-pipe stream)) (cdr (capture-stream-pipe stream)) (capture-stream-saved stream)))
        (capture-stream-input-set! stream
          (open-fd-input-port (car (capture-stream-pipe stream)) 'block (native-transcoder)))
        (capture-stream-output-set! stream
          (open-fd-output-port (cdr (capture-stream-pipe stream)) 'line (native-transcoder)))
        (capture-stream-reader-set! stream (fork-thread (lambda () (read-capture! stream)))))
      (define (release!)
        ;; Release all writers and restore both descriptors before either join.
        ;; A callback may still use the caller's ports until that join finishes.
        (for-each
          (lambda (stream)
            (attempt (lambda ()
                       (unless (port-closed? (capture-stream-standard stream))
                         (flush-output-port (capture-stream-standard stream)))))
            (attempt (lambda ()
                       (cond [(capture-stream-output stream) => close-port]
                             [(capture-stream-pipe stream) => (lambda (pipe) (c-close (cdr pipe)))]))))
          streams)
        (for-each
          (lambda (stream)
            (when (capture-stream-saved stream)
              (attempt (lambda () (descriptor (c-dup2 (capture-stream-saved stream) (capture-stream-target stream)))))
              (c-close (capture-stream-saved stream))))
          streams)
        (for-each
          (lambda (stream)
            (attempt (lambda ()
                       (cond [(capture-stream-reader stream) => thread-join]
                             [(capture-stream-input stream) => close-port]
                             [(capture-stream-pipe stream) => (lambda (pipe) (c-close (car pipe)))]))))
          streams))
      (define (finish!)
        (set! ended? #t)
        ;; Waiting with all interrupts disabled blocks collection needed by
        ;; readers. Defer cancellation, then release this winder's disable count
        ;; while flushing/joining. Restore it before returning to the winder.
        (let ([keyboard (keyboard-interrupt-handler)] [timer (timer-interrupt-handler)])
          (define (defer! handler) (unless interrupted (set! interrupted handler)))
          (parameterize ([keyboard-interrupt-handler (lambda () (defer! keyboard))]
                         [timer-interrupt-handler (lambda () (defer! timer))])
            (dynamic-wind enable-interrupts release! disable-interrupts))))
      (call-with-values
        (lambda ()
          (dynamic-wind #t
            (lambda ()
              (when ended? (error 'call-with-streamed-output "capture scope has ended"))
              (guard (ex [else (finish!) (raise ex)])
                (for-each start! streams)
                (for-each (lambda (stream)
                            (flush-output-port (capture-stream-standard stream))
                            (descriptor (c-dup2 (cdr (capture-stream-pipe stream)) (capture-stream-target stream))))
                          streams)))
            (lambda ()
              (parameterize ([current-output-port (capture-stream-output (car streams))]
                             [current-error-port (capture-stream-output (cadr streams))])
                (thunk)))
            finish!))
        (lambda results
          ;; Escapes retain their original cause. Report worker/cleanup errors
          ;; on normal return only, after all captured resources are released.
          (let ([failure (or failure (exists capture-stream-failure streams))])
            (when failure (raise (car failure))))
          (when interrupted (interrupted))
          (apply values results)))))

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
    ;; (rows . cols) for the display's tty via TIOCGWINSZ, or #f.
    ;; Process stdout may be a capture pipe while M-x is running.
    (and winsize-ioctl
         (guard (ex [else #f])
           (let ([size (make-bytevector 8 0)])
             (and (= (winsize-ioctl
                       (output-file-descriptor (terminal-output-port))
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
