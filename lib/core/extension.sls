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

  ;; The relative .sls paths under a root, indexed once per canonical root
  ;; for the process: conflict detection intersects two indexes instead of
  ;; testing every file against every root, so a large dependency root such
  ;; as a whole source tree costs one walk, and a repeated load none.
  (define indexes (make-hashtable string-hash string=?))

  (define (index-of root)
    (or (hashtable-ref indexes root #f)
        (let ([table (make-hashtable string-hash string=?)] [seen (make-hashtable string-hash string=?)])
          (let files ([path root] [relative ""])
            ;; a visited directory set prevents symlink loops; hidden
            ;; directories, .git among them, hold no libraries
            (let ([identity (sys:canonical-file-path path)])
              (when (and identity (not (hashtable-ref seen identity #f)))
                (hashtable-set! seen identity #t)
                (for-each
                  (lambda (name)
                    (let ([full (string-append path "/" name)])
                      (cond [(string:prefix? "." name) (void)]
                            [(file-directory? full) (files full (string-append relative name "/"))]
                            [(string:suffix? ".sls" name) (hashtable-set! table (string-append relative name) full)])))
                  (guard (ex [else '()]) (directory-list path))))))
          (hashtable-set! indexes root table)
          table)))

  ;; The dependency roots admitted so far, whole source trees perhaps: a
  ;; root not among them is small, e's own or an extension's lib, and is
  ;; the side whose index is walked when the two are compared.
  (define dependency-roots '())

  (define (check-conflicts! root others)
    ;; a library path under root and under an admitted root is a conflict
    ;; unless both name one file. The smaller side's index is walked and
    ;; each of its paths tested in the other, so a large dependency root is
    ;; never walked; two dependency roots are compared only through the
    ;; libraries actually loaded, in check-closure!.
    (define (large? r) (member r dependency-roots))
    (define (against! indexed other)
      (vector-for-each
        (lambda (relative)
          (let ([candidate (string-append other "/" relative)])
            (when (file-exists? candidate)
              (let ([mine (string-append indexed "/" relative)])
                (unless (equal? (sys:canonical-file-path mine) (sys:canonical-file-path candidate))
                  (error 'extension:load!
                         (format "library ~a is under two roots, ~a and ~a" relative mine candidate)))))))
        (hashtable-keys (index-of indexed))))
    (for-each
      (lambda (other)
        (cond [(not (large? root)) (against! root (car other))]
              [(not (large? (car other))) (against! (car other) root)]
              [else (void)]))
      others))

  (define (library-relative lib)
    ;; where a library's source sits under a root, a/b.sls for (a b); #f
    ;; for a name with a version or another non-symbol part
    (and (list? lib) (pair? lib) (for-all symbol? lib)
         (string-append (string:join (map symbol->string lib) "/") ".sls")))

  (define (check-closure! entry roots)
    ;; every library the entry builds on sits under one root only: the
    ;; check that two dependency roots get, over the libraries in use
    (let ([seen (make-hashtable equal-hash equal?)])
      (let walk ([lib (kernel:module-library entry)])
        (unless (hashtable-ref seen lib #f)
          (hashtable-set! seen lib #t)
          (let ([relative (library-relative lib)])
            (when relative
              (let ([found (filter values
                             (map (lambda (root)
                                    (let ([path (string-append root "/" relative)])
                                      (and (file-exists? path) (sys:canonical-file-path path))))
                               roots))])
                (let distinct ([paths found])
                  (when (and (pair? paths) (pair? (cdr paths)))
                    (if (equal? (car paths) (cadr paths))
                        (distinct (cdr paths))
                        (error 'extension:load!
                               (format "library ~s is under two roots, ~a and ~a" lib (car paths) (cadr paths)))))))))
          (for-each walk
            (guard (ex [else '()]) (library-requirements lib (library-requirements-options import))))))))

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
              ;; the requested dependency roots count as large from here on,
              ;; so the conflict check never walks them, even if the load fails
              [admitted-dependencies
               (let ([fresh (filter (lambda (root) (not (member root dependency-roots))) (cdr requested))])
                 (set! dependency-roots (append fresh dependency-roots))
                 (cdr requested))]
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
         ;; root for it goes, instead of surfacing as Chez's bare complaint;
         ;; the closure check runs in the module's registration transaction,
         ;; so a library found under two roots rolls the module back
         (guard (ex [(missing-library? ex)
                     (error 'extension:load!
                            (format "loading ~a from ~a: library ~s was not found; the directory providing it goes in the third argument, one root or a list"
                                    entry checkout (car (condition-irritants ex))))])
           (kernel:call-with-registration-update
             (lambda ()
               (kernel:load-module! entry)
               (check-closure! entry (map car combined)))))
         (void))]))
)
