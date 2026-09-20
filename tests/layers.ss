#!/usr/bin/env scheme-script

;; The source tree is the catalog: flat names, kind ranks and runtime
;; alternatives. Read declarations without importing their implementations.
(import (chezscheme))
(include "tests/roots.ss")
(define selected-roots
  (fold-left (lambda (roots runtime)
               (test-roots! runtime)
               (cons (cons runtime (map car (library-directories))) roots))
             '() '(client base)))

(eval
  `(begin
     (import (prefix (test) test:))
     (define roots ',selected-roots)
     (define kinds '(foundation sys core state service head apps modes run))
     (define-record-type source (fields path name kind declaration))
     (define (require! valid? path rule . details)
       (unless valid? (apply error 'layers rule path details))
       #t)

     (define (under? root directory)
       ;; whether directory is root or lies below it
       (or (string=? root directory)
           (and (> (string-length directory) (string-length root))
                (string=? (substring directory 0 (+ (string-length root) 1)) (string-append root "/")))))
     (define (read-sources directory)
       (apply append
         (map
           (lambda (name)
             (let ([path (string-append directory "/" name)])
               (cond
                 [(file-directory? path) (read-sources path)]
                 [(equal? (path-extension name) "sls")
                  (let ([kind (string->symbol (path-last directory))]
                        [stem (string->symbol (path-root name))])
                    (require! (memq kind kinds) path "unknown library kind" kind)
                    (require! (exists (lambda (runtime) (exists (lambda (root) (under? root directory)) (cdr runtime))) roots)
                              path "source is outside the runtime roots")
                    (call-with-input-file path
                      (lambda (port)
                        ;; A documented library imports its form first, then
                        ;; declares itself with elibrary; that import counts
                        ;; among the library's own for the layering rules.
                        (let* ([first (read port)]
                               [declaration
                                (if (and (pair? first) (eq? (car first) 'import))
                                    (let ([second (read port)])
                                      (if (and (list? second) (>= (length second) 4) (eq? (car second) 'elibrary)
                                               (pair? (cadddr second)) (eq? (car (cadddr second)) 'import))
                                          (append (list 'library (cadr second) (caddr second)
                                                        (append (cadddr second) (cdr first)))
                                                  (cddddr second))
                                          second))
                                    first)])
                          (require! (and (list? declaration) (>= (length declaration) 4)
                                         (eq? (car declaration) 'library)
                                         (equal? (cadr declaration) (list kind stem))
                                         (eof-object? (read port)))
                                    path "expected one library named by its kind directory and file stem" (list kind stem))
                          (list (make-source path stem kind declaration))))))]
                 [(member (path-extension name) '("e" "ss" "scm" "sch"))
                  (require! #f path "library sources must use .sls")]
                 [else '()])))
           (list-sort string<? (directory-list directory)))))
     (define sources (read-sources (string-append (current-directory) "/lib")))
     (test:check 'classified-flat-sources (pair? sources) #t)

     (define (clause source name)
       (let ([found (assq name (cddr (source-declaration source)))])
         (require! found (source-path source) "missing library clause" name)
         (cdr found)))
     (define (imports source)
       ;; the library names a source imports, whole: (kind leaf), (rnrs ...)
       (define (unwrap spec)
         (case (car spec)
           [(prefix only except rename for) (unwrap (cadr spec))]
           [else spec]))
       (map unwrap (clause source 'import)))
     (define (exports source)
       (apply append
         (map (lambda (item)
                (cond [(symbol? item) (list item)]
                      [(eq? (car item) 'rename) (map cadr (cdr item))]
                      [else (require! #f (source-path source) "unknown export form" item)]))
              (clause source 'export))))
     (define (rank kind) (- (length kinds) (length (memq kind kinds))))
     (define (runtime-sources runtime)
       ;; the libraries a runtime's roots hold, keyed by their names; stems
       ;; stay unique within a runtime, since a stem is a module's prefix
       (let* ([selected (cdr (assq runtime roots))]
              [foreign (filter (lambda (tree) (not (member tree selected))) (map cadr roots))])
         (fold-left
           (lambda (table source)
             (if (let ([directory (path-parent (source-path source))])
                   (and (exists (lambda (root) (under? root directory)) selected)
                        (not (exists (lambda (tree) (under? tree directory)) foreign))))
                 (let ([old (find (lambda (entry) (eq? (source-name (cdr entry)) (source-name source))) table)])
                   (require! (not old) (source-path source) "duplicate stem in runtime"
                             runtime (source-name source) (and old (source-path (cdr old))))
                   (cons (cons (list (source-kind source) (source-name source)) source) table))
                 table))
           '() sources)))
     (define base (runtime-sources 'base))
     (define client (runtime-sources 'client))

     (for-each
       (lambda (runtime table)
         (for-each
           (lambda (entry)
             (let ([source (cdr entry)])
               (for-each
                 (lambda (name)
                   (unless (memq (car name) '(rnrs chezscheme))
                     (let ([dependency (assoc name table)])
                       (require! dependency (source-path source) "unresolved local import" runtime name)
                       (require! (not (eq? (source-kind (cdr dependency)) 'run))
                                 (source-path source) "library imports a runtime entrypoint" name)
                       (require! (<= (rank (source-kind (cdr dependency))) (rank (source-kind source)))
                                 (source-path source) "upward import" name (source-kind (cdr dependency))))))
                 (imports source))))
           table)
         (test:check (list runtime 'unique-stems-and-imports) (pair? table) #t))
       '(base client) (list base client))

     (define (closure table root)
       (let walk ([pending (list root)] [seen '()])
         (cond [(null? pending) seen]
               [(member (car pending) seen) (walk (cdr pending) seen)]
               [(assoc (car pending) table)
                => (lambda (entry)
                     (walk (append (imports (cdr entry)) (cdr pending)) (cons (car entry) seen)))]
               [else (walk (cdr pending) seen)])))
     (require! (assoc '(run base) base) "lib/base/run/base.sls" "missing daemon entrypoint")
     (test:check 'daemon-has-no-head
       (for-all
         (lambda (name)
           (let ([source (cdr (assoc name base))])
             (require! (not (eq? (source-kind source) 'head)) (source-path source)
                       "daemon transitively imports a head library" 'base name)))
         (closure base '(run base))) #t)

     (test:check 'client-exports-are-subsets
       (for-all
         (lambda (entry)
           (let ([owner (assoc (car entry) base)] [source (cdr entry)])
             (require! owner (source-path source) "client library has no base counterpart" (car entry))
             (or (string=? (source-path source) (source-path (cdr owner)))
                 (let ([base-exports (exports (cdr owner))])
                   (for-all
                     (lambda (name)
                       (require! (memq name base-exports) (source-path source)
                                 "client export is absent in base" name (source-path (cdr owner))))
                     (exports source))))))
         client) #t)
     (test:finish! 'layers)))
