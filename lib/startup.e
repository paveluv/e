;; startup.e -- options admitted before importing the head. No editor state.

(library (startup)
  (export call-with-options name file default-name)
  (import (rnrs)
          (only (chezscheme) make-thread-parameter parameterize getenv get-process-id)
          (prefix (string) string:)
          (prefix (sys) sys:))

  ;; Requested name and file, scoped over library initialization and main.
  ;; Direct library clients get the same generated identity as the loader.
  (define options (make-thread-parameter '(#f #f)))

  (define (name) (and (car (options)) (string-copy (car (options)))))
  (define (file) (and (cadr (options)) (string-copy (cadr (options)))))

  (define (nonempty text) (and text (> (string-length text) 0) text))

  (define (default-name)
    (string-append
      (or (nonempty (getenv "USER")) (nonempty (getenv "LOGNAME")) "unknown")
      "@" (or (nonempty (sys:host-name)) "unknown")
      ":" (or (nonempty (sys:terminal-name))
              (string-append "pid-" (number->string (get-process-id))))))

  (define (parse args)
    (let loop ([args args] [name #f] [file #f] [help? #f] [flags? #t])
      (define (named value rest)
        (when name (error 'e "--name may be supplied only once"))
        (unless (nonempty value) (error 'e "--name requires a nonempty name"))
        (loop rest (string-copy value) file help? flags?))
      (cond
        [(null? args) (list name file help?)]
        [(and flags? (string=? (car args) "--"))
         (loop (cdr args) name file help? #f)]
        [(and flags? (member (car args) '("-h" "--help")))
         (loop (cdr args) name file #t flags?)]
        [(and flags? (string=? (car args) "--name"))
         (when (null? (cdr args)) (error 'e "--name requires a name"))
         (named (cadr args) (cddr args))]
        [(and flags? (string:prefix? "--name=" (car args)))
         (named (string:tail (car args) 7) (cdr args))]
        [(and flags? (string:prefix? "-" (car args)))
         (error 'e "unknown option (use -- before a file beginning with -)" (car args))]
        [file (error 'e "expected at most one file" (car args))]
        [else (loop (cdr args) name (string-copy (car args)) help? flags?)])))

  (define (call-with-options args thunk)
    ;; Help and malformed arguments never import the editor or load config.
    (let ([parsed (parse args)])
      (if (caddr parsed)
          (begin
            (display "Usage: e [--name NAME] [--] [file]\n")
            (display "A tiny Emacs-like terminal editor.\n")
            (display "Head names default to user@host:tty (pid without a terminal).\n"))
          (parameterize ([options (list (car parsed) (cadr parsed))])
            (thunk)))))
)
