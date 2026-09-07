;; startup.e -- options admitted before importing the head. No editor state.

(library (startup)
  (export call-with-options mode name file socket default-name)
  (import (rnrs)
          (only (chezscheme) make-thread-parameter parameterize getenv get-process-id)
          (prefix (string) string:)
          (prefix (sys) sys:))

  ;; Requested process role and arguments, scoped over library initialization.
  ;; Direct library clients get the same generated identity as the loader.
  (define options (make-thread-parameter '(standalone #f #f #f)))

  (define (mode) (car (options)))
  (define (name) (and (cadr (options)) (string-copy (cadr (options)))))
  (define (file) (and (caddr (options)) (string-copy (caddr (options)))))
  (define (socket)
    (cond [(cadddr (options)) => string-copy]
          [(nonempty (getenv "XDG_RUNTIME_DIR")) => (lambda (dir) (string-append dir "/e/base"))]
          [(nonempty (getenv "HOME")) => (lambda (dir) (string-append dir "/.e/base"))]
          [else (error 'e "--socket is required without HOME or XDG_RUNTIME_DIR")]))

  (define (nonempty text) (and text (> (string-length text) 0) text))

  (define (default-name)
    (string-append
      (or (nonempty (getenv "USER")) (nonempty (getenv "LOGNAME")) "unknown")
      "@" (or (nonempty (sys:host-name)) "unknown")
      ":" (or (nonempty (sys:terminal-name))
              (string-append "pid-" (number->string (get-process-id))))))

  (define (parse args)
    (let loop ([args args] [mode 'standalone] [name #f] [file #f] [socket #f] [help? #f] [flags? #t])
      (define (valued flag value rest)
        (when (if (eq? flag 'name) name socket)
          (error 'e "option may be supplied only once" flag))
        (unless (nonempty value) (error 'e "option requires a nonempty name or path" flag))
        (loop rest mode (if (eq? flag 'name) (string-copy value) name)
              file (if (eq? flag 'socket) (string-copy value) socket) help? flags?))
      (cond
        [(null? args)
         (when (and (eq? mode 'daemon) (or name file))
           (error 'e "--daemon does not take a head name or file"))
         (when (and socket (eq? mode 'standalone))
           (error 'e "--socket requires --daemon"))
         (list mode name file socket help?)]
        [(and flags? (string=? (car args) "--"))
         (loop (cdr args) mode name file socket help? #f)]
        [(and flags? (member (car args) '("-h" "--help")))
         (loop (cdr args) mode name file socket #t flags?)]
        [(and flags? (string=? (car args) "--daemon"))
         (unless (eq? mode 'standalone) (error 'e "--daemon may be supplied only once"))
         (loop (cdr args) 'daemon name file socket help? flags?)]
        [(and flags? (member (car args) '("--name" "--socket")))
         (when (null? (cdr args)) (error 'e "option requires a value" (car args)))
         (valued (if (string=? (car args) "--name") 'name 'socket) (cadr args) (cddr args))]
        [(and flags? (string:prefix? "--name=" (car args)))
         (valued 'name (string:tail (car args) 7) (cdr args))]
        [(and flags? (string:prefix? "--socket=" (car args)))
         (valued 'socket (string:tail (car args) 9) (cdr args))]
        [(and flags? (string:prefix? "-" (car args)))
         (error 'e "unknown option (use -- before a file beginning with -)" (car args))]
        [file (error 'e "expected at most one file" (car args))]
        [else (loop (cdr args) mode name (string-copy (car args)) socket help? flags?)])))

  (define (call-with-options args thunk)
    ;; Help and malformed arguments never import the editor or load config.
    (let ([parsed (parse args)])
      (if (list-ref parsed 4)
          (begin
            (display "Usage: e [--name NAME] [--] [file]\n")
            (display "       e --daemon [--socket PATH]\n")
            (display "A tiny Emacs-like terminal editor.\n")
            (display "Head names default to user@host:tty (pid without a terminal).\n"))
          (parameterize ([options (list (car parsed) (cadr parsed) (caddr parsed) (cadddr parsed))])
            (thunk)))))
)
