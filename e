#!/usr/bin/env scheme-script
;; e -- loader for the e editor.
;; Run: ./e [--name NAME] [--] [file], ./e --daemon [--socket PATH],
;; or ./e --attach [--socket PATH] [--name NAME] [--] [file].
;;
;; scheme-script is the interpreter name Chez's man page recommends for
;; scripts; Linux distributions and Homebrew install it under exactly
;; that name.  The FreeBSD port renames it chez-scheme-script, which
;; defeats Chez's dispatch on its own program name and loses script
;; semantics -- FreeBSD users: change the line above to
;; "#!/usr/bin/env -S chez-scheme --script" (FreeBSD's env and kernel
;; both support the multi-argument form), or invoke
;; `chez-scheme --script e` directly.
;;
;; The editor lives under lib next to this script as flat-named R6RS
;; libraries with the .sls extension, grouped by kind. This is bootstrap --
;; only what must run before the libraries can exist: locate the
;; installation, point Chez's library system at it (sources compile on
;; demand into eo/base or eo/client and recompile when stale),
;; and start the editor.  Everything else, the loading of the extension
;; modules included, is the kernel's and main's business
;; (kernel:load-modules!, main:run).
;;
;; scheme-script compiles this whole file before running any of it, and
;; a literal (import (main)) would be resolved during that compilation
;; -- before the code below has said where the libraries live.  Hence
;; the eval at the bottom: it defers importing and starting the editor
;; until run time.  It evaluates in the interaction environment, the
;; editor's top level -- the same place M-x expressions run, which is
;; why it imports the command layer bare and every other library
;; prefixed: that is the environment M-x sees.

(import (chezscheme))

(define (directory-part path)
  (let loop ([i (- (string-length path) 1)])
    (cond [(< i 0) "."]
          [(char=? (string-ref path i) #\/) (substring path 0 (max 1 i))]
          [else (loop (- i 1))])))

(define e-home
  ;; Where this installation of the editor lives: strictly the directory
  ;; of the script itself, so a checkout runs in place -- as ~/.e or
  ;; inside a project -- and every installation is self-contained.  An
  ;; invocation without script semantics leaves command-line empty; the
  ;; current directory then stands in for the script's.
  (let ([dir (directory-part
               (let ([cl (command-line)])
                 (if (and (pair? cl) (string? (car cl))) (car cl) "")))])
    (unless (file-directory? (string-append dir "/lib"))
      (display (format "e: no lib directory in ~a\n" dir)
               (current-error-port))
      (exit 1))
    (if (path-absolute? dir) dir (string-append (current-directory) "/" dir))))

(define (runtime-roots runtime)
  ;; Every leaf directory under lib is a source root: the selected
  ;; runtime's implementation tree (lib/base or lib/client) and the
  ;; common kinds beside them.  Stems are unique within a runtime
  ;; (tests/layers.ss), so the search order is immaterial: a stale
  ;; object is recompiled from wherever its source is found.
  (let ([objects (string-append e-home "/eo/" runtime)])
    (define (leaves parent)
      (map (lambda (name) (cons (string-append parent "/" name) objects))
        (list-sort string<?
          (filter (lambda (name)
                    (and (not (member name '("base" "client")))
                         (file-directory? (string-append parent "/" name))))
                  (directory-list parent)))))
    (append (leaves (string-append e-home "/lib/" runtime))
            (leaves (string-append e-home "/lib")))))

;; Option admission imports only common facilities. Runtime consumers
;; are imported after choosing their implementation roots below.
(library-directories (runtime-roots "base"))
(compile-imported-libraries #t)

(eval `(begin
         (import (prefix (startup) startup:) (prefix (kernel) kernel:))
         (kernel:installation-directory ,e-home)
         (startup:call-with-options (command-line-arguments)
           (lambda ()
             (when (and (not (eq? (startup:mode) 'daemon))
                        (or (not (getenv "TERM")) (string=? (getenv "TERM") "dumb")))
               (display "e: an interactive terminal is required\n" (current-error-port))
               (exit 1))
             ;; Choose a service implementation before importing any head.
             ;; Separate objects keep daemon and client library identities
             ;; from overwriting one another in a shared installation.
             (if (eq? (startup:mode) 'attach)
                 (begin
                   (library-directories
                     ',(runtime-roots "client"))
                   (eval '(begin
                            (import (prefix (client) client:))
                            (client:call-with-runtime
                              (lambda ()
                                (eval '(begin
                                         (import (edit) (prefix (main) main:))
                                         (main:run))))))))
                 (eval '(begin
                          (import (prefix (base) base:))
                          (base:call-with-runtime
                            (lambda ()
                              (if (eq? (startup:mode) 'daemon)
                                (base:run)
                                (eval '(begin
                                         (import (edit) (prefix (main) main:))
                                         (main:run)))))))))))))
