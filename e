#!/usr/bin/env scheme-script
;; e -- loader for the e editor.
;; Run:  ./e [file]
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
;; The editor lives in the lib directory next to this script as R6RS
;; libraries with the .e extension.  This script is pure bootstrap --
;; only what must run before the (core) library can exist: locate the
;; installation, point Chez's library system at it (sources compile on
;; demand into the eo directory next to lib and recompile when stale),
;; and start the editor.  Everything else, the loading of the extension
;; modules included, is the core's business (load-modules! there).
;;
;; scheme-script runs this file with R6RS program semantics, where a
;; literal (import (core)) would resolve before library-directories is
;; set below -- so the core is imported at run time, in the interaction
;; environment, which is also where M-x evaluates and where the modules
;; must land anyway.

(import (chezscheme))

(define (directory-part path)
  (let loop ([i (- (string-length path) 1)])
    (cond [(< i 0) "."]
          [(char=? (string-ref path i) #\/) (substring path 0 i)]
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
    dir))

(library-directories
  (list (cons (string-append e-home "/lib") (string-append e-home "/eo"))))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval '(import (core)) (interaction-environment))

;; Saving a module source (a .e file in lib) reloads it into the running
;; editor.  Comment this line out to disable that.
(eval '(auto-reload #t) (interaction-environment))

(eval '(main) (interaction-environment))
