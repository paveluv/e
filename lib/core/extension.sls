;; Local extension checkouts use the normal module lifecycle and cache.

(import (only (foundation edoc) elibrary))
(elibrary (core extension)
  (export load!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (core startup) startup:)
          (prefix (foundation string) string:)
          (prefix (sys path) path:)
          (prefix (sys sys) sys:))

  (define (directory role name base)
    ;; the canonical directory a repository or root names, ~ expanded and
    ;; a relative name resolved against base; the role names it on error
    (unless (and (string? name) (> (string-length name) 0))
      (error 'extension:load! (format "expected a ~a directory" role) name))
    (let* ([expanded (path:expand name)]
           [absolute (if (path-absolute? expanded) expanded (string-append base "/" expanded))]
           [canonical (sys:canonical-file-path absolute)])
      (unless (and canonical (file-directory? canonical))
        (error 'extension:load! (format "no such ~a directory" role) absolute))
      canonical))

  (define (ensure-directory! path)
    (unless (file-directory? path)
      (ensure-directory! (path-parent path))
      (guard (ex [(i/o-file-already-exists-error? ex) (void)] [else (raise ex)])
        (mkdir path #o700))))

  (define (missing-library? ex)
    ;; Chez's condition for an import nobody provides
    (and (message-condition? ex) (irritants-condition? ex)
         (let ([message (condition-message ex)])
           (and (string:prefix? "library " message) (string:suffix? " not found" message)))))

  (edoc "Load an entry module from a local repository's lib directory, managing compiled objects outside the checkout. Repeated loading is harmless. Additional R6RS roots are optional, one directory or a list; relative repository paths use the installation, relative library roots use the repository."
        (repository directory "the extension checkout")
        (entry string "its module name, such as worksheet-mode")
        (roots (or directory (list-of directory)) "additional R6RS source roots, one or several"))
  (define load!
    (case-lambda
      [(repository entry) (load! repository entry '())]
      [(repository entry roots)
       (define root-list (if (string? roots) (list roots) roots))
       (when (eq? (startup:mode) 'base)
         (error 'extension:load! "load head extensions from config.e" repository))
       (unless (and (string? entry) (> (string-length entry) 0)
                    (for-all (lambda (c) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\_))))
                      (string->list entry)))
         (error 'extension:load! "expected a module name" entry))
       (unless (and (list? root-list) (for-all string? root-list))
         (error 'extension:load! "expected library root paths, one directory or a list" roots))
       (let* ([checkout (directory "repository" repository (kernel:installation-directory))]
              [lib (begin
                     (unless (file-directory? (string-append checkout "/lib"))
                       (error 'extension:load! (format "the checkout ~a has no lib directory" checkout)))
                     (string-append checkout "/lib"))]
              [existing (library-directories)]
              [cache (cdar existing)]
              [requested (cons lib (map (lambda (root) (directory "library root" root checkout)) root-list))]
              [combined
               (fold-left
                 (lambda (all root)
                   (if (exists (lambda (pair) (equal? (sys:canonical-file-path (car pair)) root)) all) all
                       (begin
                         ;; Mirroring the absolute source path is an injective
                         ;; cache key and needs no hash or sidecar metadata.
                         (append all (list (cons root (string-append cache "/extensions" root)))))))
                 existing requested)])
         (parameterize ([library-directories combined])
           (let ([source (guard (ex [else (error 'extension:load!
                                                 (format "the checkout ~a has no module ~a under lib" checkout entry))])
                           (kernel:module-source entry))])
             (unless (string:prefix? (string-append lib "/") (path:canonical source))
               (error 'extension:load!
                      (format "module ~a resolves to ~a, not to the checkout ~a" entry source checkout)))))
         (for-each (lambda (pair) (unless (member pair existing) (ensure-directory! (cdr pair)))) combined)
         ;; Roots, like imported Scheme libraries, live for this process even
         ;; when initialization fails. Kernel membership/registrations still
         ;; follow the enclosing config transaction, so retry can initialize.
         (library-directories combined)
         (compile-imported-libraries #t)
         ;; an import nobody provides names the missing library and where a
         ;; root for it goes, instead of surfacing as Chez's bare complaint
         (guard (ex [(missing-library? ex)
                     (error 'extension:load!
                            (format "loading ~a from ~a: library ~s was not found; the directory providing it goes in the third argument, one root or a list"
                                    entry checkout (car (condition-irritants ex))))])
           (kernel:call-with-registration-update
             (lambda ()
               (kernel:load-module! entry))))
         (void))]))
)
