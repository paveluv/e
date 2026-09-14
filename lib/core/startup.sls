;; startup.sls -- options admitted before importing the head. No editor state.

(library (startup)
  (export call-with-options mode name file base-working-directory default-name)
  (import (rnrs)
          (only (chezscheme) make-thread-parameter parameterize getenv get-process-id
                current-directory path-absolute? path-parent path-last)
          (prefix (kernel) kernel:)
          (prefix (path) path:)
          (prefix (string) string:)
          (prefix (sys) sys:))

  ;; Requested process role and arguments, scoped over library initialization.
  ;; Direct library clients get the same generated identity as the loader.
  (define options (make-thread-parameter '(head #f #f #f)))

  (define (mode) (car (options)))
  (define (name) (and (cadr (options)) (string-copy (cadr (options)))))
  (define (file) (and (caddr (options)) (string-copy (caddr (options)))))
  (define (resolve-directory directory)
    ;; Resolve existing symlinks before collapsing .., including an existing
    ;; parent of a directory that has not been created yet. No startup effects.
    (let resolve ([path (let ([expanded (path:expand directory)])
                          (if (path-absolute? expanded) expanded
                            (string-append (current-directory) "/" expanded)))])
      (or (sys:canonical-file-path path)
          (let ([parent (path-parent path)])
            (if (or (not parent) (string=? path parent)) (path:canonical path)
                (path:canonical (string-append (resolve parent) "/" (path-last path))))))))

  (define (base-working-directory)
    (cond [(cadddr (options)) => string-copy]
          [else (resolve-directory (string-append (kernel:installation-directory) "/.base"))]))

  (define (nonempty text) (and text (> (string-length text) 0) text))

  (define (default-name)
    (string-append
      (or (nonempty (getenv "USER")) (nonempty (getenv "LOGNAME")) "unknown")
      "@" (or (nonempty (sys:host-name)) "unknown")
      ":" (or (nonempty (sys:terminal-name))
              (string-append "pid-" (number->string (get-process-id))))))

  (define (parse args)
    (let loop ([args args] [mode 'head] [name #f] [file #f] [directory #f] [help? #f] [flags? #t])
      (define (valued flag value rest)
        (when (if (eq? flag 'name) name directory)
          (error 'e "option may be supplied only once" flag))
        (unless (nonempty value) (error 'e "option requires a nonempty name or path" flag))
        (loop rest mode (if (eq? flag 'name) (string-copy value) name)
              file (if (eq? flag 'directory) (string-copy value) directory) help? flags?))
      (cond
        [(null? args)
         (when (and (eq? mode 'base) (or name file))
           (error 'e "--base does not take a head name or file"))
         (list mode name file directory help?)]
        [(and flags? (string=? (car args) "--"))
         (loop (cdr args) mode name file directory help? #f)]
        [(and flags? (member (car args) '("-h" "--help")))
         (loop (cdr args) mode name file directory #t flags?)]
        [(and flags? (string=? (car args) "--base"))
         (unless (eq? mode 'head) (error 'e "--base may be supplied only once"))
         (loop (cdr args) 'base name file directory help? flags?)]
        [(and flags? (member (car args) '("--name" "--base-working-dir")))
         (when (null? (cdr args)) (error 'e "option requires a value" (car args)))
         (valued (if (string=? (car args) "--name") 'name 'directory) (cadr args) (cddr args))]
        [(and flags? (string:prefix? "--name=" (car args)))
         (valued 'name (string:tail (car args) 7) (cdr args))]
        [(and flags? (string:prefix? "--base-working-dir=" (car args)))
         (valued 'directory (string:tail (car args) 19) (cdr args))]
        [(and flags? (string:prefix? "-" (car args)))
         (error 'e "unknown option (use -- before a file beginning with -)" (car args))]
        [file (error 'e "expected at most one file" (car args))]
        [else (loop (cdr args) mode name (string-copy (car args)) directory help? flags?)])))

  (define (call-with-options args thunk)
    ;; Help and malformed arguments never import the editor or load config.
    (let ([parsed (parse args)])
      (if (list-ref parsed 4)
          (begin
            (display "Usage: e [--name NAME] [--base-working-dir DIR] [--] [file]\n")
            (display "       e --base [--base-working-dir DIR]\n")
            (display "A tiny Emacs-like terminal editor.\n")
            (display "Head names default to user@host:tty (pid without a terminal).\n"))
          (parameterize ([options (list (car parsed) (cadr parsed)
                                    (and (caddr parsed) (path:canonical (path:expand (caddr parsed))))
                                    (resolve-directory (or (cadddr parsed)
                                                         (string-append (kernel:installation-directory) "/.base"))))])
            (thunk)))))
)
