;; cache.sls -- serialize access to one installation's compiled libraries.
;; Loaded from source before the loader's first cached import. Only Chez is
;; a dependency: consulting the cache to install its lock would itself race.
(import (only (edoc) elibrary))
(elibrary (cache)
  (export install!)
  (import (chezscheme))

  (define libc
    (let try ([names '("libc.so.6" "libSystem.dylib" "libc.so.7" "libc.so")])
      (cond [(null? names) (error 'cache "cannot load the system library")]
            [(guard (ex [else #f]) (load-shared-object (car names)) #t) #t]
            [else (try (cdr names))])))
  (define flock (foreign-procedure __collect_safe "flock" (int int) int))
  (define fcntl (foreign-procedure "fcntl" (int int int) int))

  (edoc "Use a directory, created private, as the compiled object cache, locked against other installations."
        (directory directory "the cache directory"))
  (define (install! directory)
    (unless (file-directory? directory)
      (guard (ex [(i/o-file-already-exists-error? ex) (void)] [else (raise ex)])
        (mkdir directory #o700)))
    (let* ([port (open-file-output-port (string-append directory "/lock")
                   (file-options no-fail no-truncate))]
           [fd (port-file-descriptor port)] [lock (make-mutex)]
           [inside (make-thread-parameter #f)] [expand (current-expand)])
      (unless (zero? (fcntl fd 2 1)) ; F_SETFD, FD_CLOEXEC
        (close-port port) (error 'cache "cannot protect the compiler lock across exec"))
      (current-expand
        (lambda args
          ;; The entire expansion includes dependency checks, object loading
          ;; and recursive compilation. Locking just the write is too late:
          ;; different compilers generate different library identities.
          (if (eqv? (inside) (get-thread-id)) (apply expand args)
              (with-mutex lock
                (dynamic-wind
                  (lambda ()
                    ;; Retain the port as well as its descriptor for GC.
                    (when (or (port-closed? port) (not (zero? (flock fd 2))))
                      (error 'cache "cannot acquire the compiler lock"))) ; LOCK_EX
                  (lambda () (parameterize ([inside (get-thread-id)]) (apply expand args)))
                  (lambda () (flock fd 8))))))))) ; LOCK_UN
)
