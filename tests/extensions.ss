#!/usr/bin/env scheme-script

;; External checkouts share the ordinary module lifecycle. No live base or TTY.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (core extension) extension:) (prefix (core kernel) kernel:)
             (prefix (foundation string) string:) (prefix (sys sys) sys:)
             (prefix (test) test:))

     (define scratch (format "/tmp/e-extension-test-~a-~a" (get-process-id) (random 1000000)))
     (define (path name) (string-append scratch "/" name))
     (define (put name form)
       (call-with-output-file (path name) (lambda (out) (pretty-print form out)) 'replace))
     (define (remove-tree directory)
       (chmod directory #o700)
       (for-each (lambda (name)
                   (let ([p (string-append directory "/" name)])
                     (if (file-directory? p) (remove-tree p) (delete-file p))))
         (directory-list directory))
       (delete-directory directory))
     (define entries (kernel:make-registry))
     (define starts '())
     (define control
       (kernel:persistent-cell 'extension-test-init
         (lambda ()
           (lambda (name)
             (set! starts (cons name starts))
             (kernel:registry-add! entries name)))))
     (define (entry name dependency)
       `(library (,name) (export init! value)
          (import (chezscheme) (prefix (core kernel) kernel:) (prefix ,dependency helper:))
          (define (value) (helper:value))
          (define (init!)
            ((unbox (kernel:persistent-cell 'extension-test-init (lambda () #f))) ',name))))
     (define (helper value)
       `(library (collection helper) (export value) (import (rnrs)) (define (value) ,value)))

     (mkdir scratch)
     (for-each (lambda (name) (mkdir (path name)))
       '("installation" "objects" "empty" "plugin" "plugin/lib" "deps" "deps/collection" "deps/other"
         "failed" "failed/lib" "conflict" "conflict/lib" "conflict/lib/core" "shadow"))
     (dynamic-wind
       void
       (lambda ()
         (put "deps/collection/helper.sls" (helper 1))
         (put "deps/collection/middle.sls"
           '(library (collection middle) (export value) (import (rnrs) (prefix (collection helper) helper:))
              (define (value) (helper:value))))
         (put "deps/other/helper.sls"
           '(library (other helper) (export value) (import (rnrs)) (define (value) 9)))
         (put "plugin/lib/external-probe.sls" (entry 'external-probe '(collection middle)))
         (put "plugin/lib/external-neighbor.sls" (entry 'external-neighbor '(other helper)))
         (put "plugin/lib/runner-probe.sls"
           '(library (runner-probe) (export init! ready?) (import (chezscheme))
              (define ready #f) (define (ready?) ready) (define (init!) (set! ready #t))))
         (put "runner-test.ss" '(unless (runner-probe:ready?) (error 'runner "entry not initialized")))
         (put "failed/lib/external-retry.sls" (entry 'external-retry '(collection helper)))
         (put "plugin/lib/needs-absent.sls"
           '(library (needs-absent) (export init!) (import (chezscheme) (absent collection)) (define (init!) (void))))
         (put "conflict/lib/core/kernel.sls" '(library (core kernel) (export) (import (rnrs))))
         (put "shadow/external-probe.sls" (entry 'external-probe '(other helper)))
         (chmod (path "plugin/lib") #o555)
         (chmod (path "plugin") #o555)
         (let ([process (sys:open-process
                          (list "scheme" "--script" "tools/test-extension.sps"
                            (path "plugin") "runner-probe" (path "runner-test.ss")))])
           (sys:write-process! process #f)
           (get-bytevector-all (sys:process-input process))
           (let-values ([(code errors) (sys:process-result process)])
             (sys:close-process! process)
             (test:check 'public-test-runner-loads-and-initializes-an-external-entry (list code errors) '(0 ""))))
         (parameterize ([kernel:installation-directory (path "installation")]
                        [library-directories (cons (cons (path "empty") (path "objects")) (library-directories))])
           (extension:load! "../plugin" "external-probe" '("../deps"))
           (let ([roots (library-directories)])
             (extension:load! (path "plugin") "external-probe" '("../deps"))
             ;; one root may be given as a string
             (extension:load! (path "plugin") "external-probe" "../deps")
             (test:check 'one-line-load-is-idempotent-and-keeps-checkout-clean
               (list (eval '(external-probe:value)) starts (equal? roots (library-directories))
                     (list-sort string<? (directory-list (path "plugin"))))
               '(1 (external-probe) #t ("lib"))))
           (test:check 'managed-cache-is-outside-checkout
             (let ([objects (cdr (assoc (path "plugin/lib") (library-directories)))])
               (and (file-exists? (string-append objects "/external-probe.so"))
                    (not (file-exists? (path "plugin/eo"))))) #t)
           (test:check 'conflicts-and-missing-entry-are-refused
             (map test:raises?
               (list (lambda () (extension:load! "../conflict" "kernel"))
                     (lambda () (extension:load! "../plugin" "missing")))) '(#t #t))
           (let ([complaint (lambda (thunk) (guard (ex [else (condition-message ex)]) (thunk) "no error"))]
                 [mentions? (lambda (text . parts)
                              (for-all (lambda (part) (and (string:search text part 0 (string-length text)) #t)) parts))])
             (test:check 'errors-name-the-checkout-the-entry-and-the-missing-library
               (list (mentions? (complaint (lambda () (extension:load! "../plugin" "missing"))) (path "plugin") "missing")
                     (mentions? (complaint (lambda () (extension:load! "../conflict" "kernel"))) "core/kernel.sls" "two roots")
                     (mentions? (complaint (lambda () (extension:load! "../plugin" "needs-absent"))) "(absent collection)" "third argument")
                     (mentions? (complaint (lambda () (extension:load! "../nowhere" "x"))) "repository")
                     (mentions? (complaint (lambda () (extension:load! "../plugin" "external-probe" "../nowhere"))) "library root"))
               '(#t #t #t #t #t)))
           (test:check 'published-module-keeps-its-source
             (parameterize ([library-directories (cons (cons (path "shadow") (path "objects")) (library-directories))])
               (kernel:module-source "external-probe"))
             (path "plugin/lib/external-probe.sls"))
           (test:check 'failed-config-discards-module-and-registrations
             (list
               (test:raises?
                 (lambda ()
                   (kernel:call-with-registration-update
                     (lambda ()
                       (extension:load! "../failed" "external-retry")
                       (error 'config "later failure")))))
               (member "external-retry" (kernel:loaded-modules))
               (kernel:registry-items entries))
             '(#t #f (external-probe)))
           (extension:load! "../failed" "external-retry")
           (test:check 'corrected-config-retries-initialization
             (kernel:registry-items entries) '(external-retry external-probe))

           (extension:load! "../plugin" "external-neighbor")
           (kernel:pin-modules! '("external-neighbor"))
           (set! starts '())
           (put "deps/collection/helper.sls" (helper 2))
           (kernel:reload-module! (kernel:source-library (path "deps/collection/helper.sls")))
           (test:check 'helper-reload-follows-full-library-identities-through-private-importers
             (list (eval '(list (external-probe:value) (external-neighbor:value) (external-retry:value)))
                   (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) starts)
                   (and (member "helper" (kernel:loaded-modules)) #t))
             '((2 9 2) (external-probe external-retry) #f))))
       (lambda () (remove-tree scratch)))
     (test:finish! 'extensions)))
