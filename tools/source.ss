;; Source-only helpers shared by the convention and API analyzers. Reading a
;; library never loads it or executes its macros or initialization code.

(define (sls-files directory)
  (apply append
    (map (lambda (name)
           (let ([path (string-append directory "/" name)])
             (cond [(file-directory? path) (sls-files path)]
                   [(equal? (path-extension path) "sls") (list path)]
                   [else '()])))
      (list-sort string<? (directory-list directory)))))

(define (read-forms path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([out '()])
        (let ([form (read port)])
          (if (eof-object? form) (reverse out) (loop (cons form out))))))))

(define (read-annotated path)
  (let* ([text (call-with-input-file path get-string-all)]
         [sfd (source-file-descriptor path 0)]
         [port (open-string-input-port text)])
    (let loop ([bfp 0] [acc '()])
      (let-values ([(form efp) (get-datum/annotations port sfd bfp)])
        (if (eof-object? form) (values text (reverse acc)) (loop efp (cons form acc)))))))

(define (stripped x) (if (annotation? x) (annotation-stripped x) x))
(define (parts x) (if (annotation? x) (annotation-expression x) x))
(define (start x) (source-object-bfp (annotation-source x)))
(define (end x) (source-object-efp (annotation-source x)))

(define (line-of text pos)
  (let loop ([i 0] [line 1])
    (if (>= i pos) line (loop (+ i 1) (if (char=? (string-ref text i) #\newline) (+ line 1) line)))))

(define (exports-of library)
  ;; ((external . internal) ...), including renamed exports.
  (let ([clause (assq 'export (cddr library))])
    (apply append
      (map (lambda (spec)
             (if (symbol? spec) (list (cons spec spec))
                 (map (lambda (r) (cons (cadr r) (car r))) (cdr spec))))
        (if clause (cdr clause) '())))))

(define (import-specs library)
  (let ([clause (assq 'import (cddr library))])
    (if clause (cdr clause) '())))

(define (strip-prefix prefix name)
  (let ([p (symbol->string prefix)] [n (symbol->string name)])
    (and (> (string-length n) (string-length p))
         (string=? p (substring n 0 (string-length p)))
         (string->symbol (substring n (string-length p) (string-length n))))))

(define (source-import spec local lookup)
  ;; lookup receives the original library and external name. Import wrappers
  ;; change spelling/visibility, not the identity used by an analyzer.
  (and (pair? spec)
    (case (car spec)
      [(prefix) (let ([inner (strip-prefix (caddr spec) local)])
                  (and inner (source-import (cadr spec) inner lookup)))]
      [(only) (and (memq local (cddr spec)) (source-import (cadr spec) local lookup))]
      [(except) (and (not (memq local (cddr spec))) (source-import (cadr spec) local lookup))]
      [(rename)
       (let ([renamed (find (lambda (r) (eq? (cadr r) local)) (cddr spec))])
         (cond [renamed (source-import (cadr spec) (car renamed) lookup)]
               [(assq local (cddr spec)) #f]
               [else (source-import (cadr spec) local lookup)]))]
      [(for library) (source-import (cadr spec) local lookup)]
      [else (lookup spec local)])))
