;; startup.sls -- options admitted before importing the head. No editor state.

(import (only (foundation edoc) elibrary))
(elibrary (core startup)
  (export base-working-directory call-with-options default-base-working-directory default-name
          file force? mode name restart? start-file)
  (import (rnrs)
          (only (chezscheme) make-thread-parameter parameterize getenv get-process-id
                current-directory path-absolute? path-parent path-last)
          (prefix (core kernel) kernel:)
          (prefix (foundation string) string:)
          (prefix (sys path) path:)
          (prefix (sys sys) sys:))

  ;; Requested process role and arguments, scoped over library initialization.
  ;; Direct library clients get the same generated identity as the loader.
  (define options (make-thread-parameter '(head #f #f #f #f #f #f)))

  (edoc "What the command line asked for: head, base or help."
        (returns symbol))
  (define (mode)
    (car (options)))

  (edoc "The head name from the command line, or #f."
        (returns (or string #f)))
  (define (name)
    (and (cadr (options)) (string-copy (cadr (options)))))

  (edoc "The file argument, canonical, or #f."
        (returns (or file #f)))
  (define (file)
    (and (caddr (options)) (string-copy (caddr (options)))))

  (edoc "The explicitly selected trusted head startup script, resolved against the invocation directory, or false for the installed default. Saved state never selects a script."
        (returns (or file #f)))
  (define (start-file)
    (let ([path (list-ref (options) 6)]) (and path (string-copy path))))

  (edoc "Whether a base restart was asked for."
        (returns boolean))
  (define (restart?)
    (list-ref (options) 4))

  (edoc "Whether the restart may proceed despite modified buffers."
        (returns boolean))
  (define (force?)
    (list-ref (options) 5))
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

  (edoc "The base's working directory when none is given: .base in the installation."
        (returns directory))
  (define (default-base-working-directory)
    (resolve-directory (string-append (kernel:installation-directory) "/.base")))

  (edoc "The base's working directory, from the command line or the default."
        (returns directory))
  (define (base-working-directory)
    (cond [(cadddr (options)) => string-copy]
          [else (default-base-working-directory)]))

  (define (nonempty text) (and text (> (string-length text) 0) text))

  (edoc "A head's default name: user@host:tty, or the pid without a terminal."
        (returns string))
  (define (default-name)
    (string-append
      (or (nonempty (getenv "USER")) (nonempty (getenv "LOGNAME")) "unknown")
      "@" (or (nonempty (sys:host-name)) "unknown")
      ":" (or (nonempty (sys:terminal-name))
              (string-append "pid-" (number->string (get-process-id))))))

  (define (parse args)
    (let loop ([args args] [mode 'head] [name #f] [file #f] [directory #f]
               [restart? #f] [force? #f] [script #f] [help? #f] [flags? #t])
      (define (valued flag value rest)
        (when (case flag [(name) name] [(directory) directory] [(script) script])
          (error 'e "option may be supplied only once" flag))
        (unless (nonempty value) (error 'e "option requires a nonempty name or path" flag))
        (loop rest mode (if (eq? flag 'name) (string-copy value) name)
              file (if (eq? flag 'directory) (string-copy value) directory) restart? force? (if (eq? flag 'script) (string-copy value) script) help? flags?))
      (cond
        [(null? args)
         (when (and (eq? mode 'base) (or name file script))
           (error 'e "--base does not take a head name, file or startup script"))
         (when (and (eq? mode 'base) restart?) (error 'e "--base and --restart cannot be combined"))
         (when (and force? (not restart?)) (error 'e "--force requires --restart"))
         (list mode name file directory restart? force? script help?)]
        [(and flags? (string=? (car args) "--"))
         (loop (cdr args) mode name file directory restart? force? script help? #f)]
        [(and flags? (member (car args) '("-h" "--help")))
         (loop (cdr args) mode name file directory restart? force? script #t flags?)]
        [(and flags? (string=? (car args) "--base"))
         (unless (eq? mode 'head) (error 'e "--base may be supplied only once"))
         (loop (cdr args) 'base name file directory restart? force? script help? flags?)]
        [(and flags? (member (car args) '("--restart" "--force")))
         (let ([restart-flag? (string=? (car args) "--restart")])
           (when (if restart-flag? restart? force?) (error 'e "option may be supplied only once" (car args)))
           (loop (cdr args) mode name file directory (or restart? restart-flag?)
             (or force? (not restart-flag?)) script help? flags?))]
        [(and flags? (member (car args) '("--name" "--base-working-dir" "--start")))
         (when (null? (cdr args)) (error 'e "option requires a value" (car args)))
         (valued (cond [(string=? (car args) "--name") 'name] [(string=? (car args) "--start") 'script] [else 'directory]) (cadr args) (cddr args))]
        [(and flags? (string:prefix? "--start=" (car args)))
         (valued 'script (string:tail (car args) 8) (cdr args))]
        [(and flags? (string:prefix? "--name=" (car args)))
         (valued 'name (string:tail (car args) 7) (cdr args))]
        [(and flags? (string:prefix? "--base-working-dir=" (car args)))
         (valued 'directory (string:tail (car args) 19) (cdr args))]
        [(and flags? (string:prefix? "-" (car args)))
         (error 'e "unknown option (use -- before a file beginning with -)" (car args))]
        [file (error 'e "expected at most one file" (car args))]
        [else (loop (cdr args) mode name (string-copy (car args)) directory restart? force? script help? flags?)])))

  (edoc "Parse the command line and run a thunk with the options in effect."
        (args (list-of string) "the arguments")
        (thunk thunk "the program")
        (returns any))
  (define (call-with-options args thunk)
    ;; Select help before the loader imports a runtime or loads config.
    (let ([parsed (parse args)])
      (parameterize ([options (list (if (list-ref parsed 7) 'help (car parsed)) (cadr parsed)
                                    (and (caddr parsed) (path:canonical (path:expand (caddr parsed))))
                                    (if (cadddr parsed) (resolve-directory (cadddr parsed))
                                        (default-base-working-directory))
                                    (list-ref parsed 4) (list-ref parsed 5)
                                    (and (list-ref parsed 6) (path:canonical (path:expand (list-ref parsed 6)))))])
        (thunk))))
)
