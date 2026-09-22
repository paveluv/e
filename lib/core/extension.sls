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

  (define (directory name base)
    (unless (and (string? name) (> (string-length name) 0))
      (error 'extension:load! "expected a directory" name))
    (let* ([expanded (path:expand name)]
           [absolute (if (path-absolute? expanded) expanded (string-append base "/" expanded))]
           [canonical (sys:canonical-file-path absolute)])
      (unless (and canonical (file-directory? canonical))
        (error 'extension:load! "no such directory" absolute))
      canonical))

  (define (ensure-directory! path)
    (unless (file-directory? path)
      (ensure-directory! (path-parent path))
      (guard (ex [(i/o-file-already-exists-error? ex) (void)] [else (raise ex)])
        (mkdir path #o700))))

  (define (sources root)
    ;; Paths relative to this R6RS root. A visited directory set prevents
    ;; symlink loops; full relative names, not leaf stems, identify libraries.
    (define seen (make-hashtable string-hash string=?))
    (define (files path relative)
      (let ([identity (sys:canonical-file-path path)])
        (if (or (not identity) (hashtable-ref seen identity #f)) '()
            (begin
              (hashtable-set! seen identity #t)
              (apply append
                (map (lambda (name)
                       (let ([full (string-append path "/" name)] [rel (string-append relative name)])
                         (cond [(file-directory? full) (files full (string-append rel "/"))]
                               [(string:suffix? ".sls" name) (list rel)]
                               [else '()])))
                  (directory-list path)))))))
    (files root ""))

  (define (check-conflicts! root others)
    (for-each
      (lambda (relative)
        (let* ([source (string-append root "/" relative)]
               [identity (sys:canonical-file-path source)])
          (for-each
            (lambda (other)
              (let* ([candidate (string-append (car other) "/" relative)]
                     [found (sys:canonical-file-path candidate)])
                (when (and found (not (equal? found identity)))
                  (error 'extension:load! "conflicting library sources" source candidate))))
            others)))
      (sources root)))

  (edoc "Load an entry module from a local repository's lib directory, managing compiled objects outside the checkout. Repeated loading is harmless. Additional R6RS roots are optional; relative repository paths use the installation, relative library roots use the repository."
        (repository directory "the extension checkout")
        (entry string "its module name, such as worksheet-mode")
        (roots (list-of directory) "additional R6RS source roots"))
  (define load!
    (case-lambda
      [(repository entry) (load! repository entry '())]
      [(repository entry roots)
       (when (eq? (startup:mode) 'base)
         (error 'extension:load! "load head extensions from config.e" repository))
       (unless (and (string? entry) (> (string-length entry) 0)
                    (for-all (lambda (c) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\- #\_))))
                      (string->list entry)))
         (error 'extension:load! "expected a module name" entry))
       (unless (and (list? roots) (for-all string? roots))
         (error 'extension:load! "expected library root paths" roots))
       (let* ([repository (directory repository (kernel:installation-directory))]
              [lib (directory "lib" repository)]
              [existing (library-directories)]
              [cache (cdar existing)]
              [requested (cons lib (map (lambda (root) (directory root repository)) roots))]
              [combined
               (fold-left
                 (lambda (all root)
                   (if (exists (lambda (pair) (equal? (sys:canonical-file-path (car pair)) root)) all) all
                       (begin
                         (check-conflicts! root all)
                         ;; Mirroring the absolute source path is an injective
                         ;; cache key and needs no hash or sidecar metadata.
                         (append all (list (cons root (string-append cache "/extensions" root)))))))
                 existing requested)])
         (parameterize ([library-directories combined])
           (let ([source (kernel:module-source entry)])
             (unless (string:prefix? (string-append lib "/") (path:canonical source))
               (error 'extension:load! "entry belongs to another source" entry source lib))))
         (for-each (lambda (pair) (unless (member pair existing) (ensure-directory! (cdr pair)))) combined)
         ;; Roots, like imported Scheme libraries, live for this process even
         ;; when initialization fails. Kernel membership/registrations still
         ;; follow the enclosing config transaction, so retry can initialize.
         (library-directories combined)
         (compile-imported-libraries #t)
         (kernel:load-module! entry)
         (void))]))
)
