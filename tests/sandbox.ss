#!/usr/bin/env scheme-script

;; The read-only capability environment: reachability is the whole
;; game -- granted names work, everything else fails to resolve --
;; plus the bounded editor readers over the store.  Run from
;; the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (state store) store:)
             (prefix (core kernel) kernel:)
             (prefix (service log) log:) (prefix (foundation string) string:)
             (prefix (only (service reference) lookup) reference:)
             (prefix (test) test:)
             (only (chezscheme) environment eval format))

     (define check test:check)

     (define (contains? text needle)
       (and (string:search text needle 0 (string-length text)) #t))

     (define (unbound? form env)
       (guard (ex [else (undefined-violation? ex)])
         (eval form env)
         'resolved))

     ;; -- the full tier: computation works, power is unreachable ------

     (define tier (environment '(service sandbox)))

     (check 'pure-computation (eval '(+ 1 2) tier) 3)
     (check 'strings-and-lists
            (eval '(map string-upcase (list "a" "b")) tier)
            '("A" "B"))
     (check 'no-files (unbound? '(open-output-file "x") tier) #t)
     (check 'no-processes (unbound? '(system "true") tier) #t)
     (check 'no-eval (unbound? '(eval '(+ 1 2)) tier) #t)
     (check 'no-environments (unbound? '(environment '(rnrs)) tier) #t)
     (check 'no-store-mutation
            (unbound? '(edit! 'x 1 1 'span '("gone")) tier) #t)

     ;; -- a narrowed grant: (only (service sandbox) ...) subsets it -----------

     (define narrow (environment '(only (service sandbox) + car cons quote)))

     (check 'granted-name-works (eval '(+ 1 2) narrow) 3)
     (check 'ungranted-name-fails
            (unbound? '(string-append "a" "b") narrow) #t)
     (check 'ungranted-reader-fails
            (unbound? '(buffer-names) narrow) #t)

     ;; -- the editor readers: by name, over the store, bounded --------

     (define id (store:create! '(head test) "sandbox-probe"
                               '("alpha" "beta")))

     (check 'buffer-names-sees-the-store
            (and (member "sandbox-probe" (eval '(buffer-names) tier)) #t)
            #t)
     (check 'line-count (eval '(buffer-lines-count "sandbox-probe") tier) 2)
     (check 'line (eval '(buffer-text-line "sandbox-probe" 1) tier) "beta")
     (check 'revision-is-data
            (eval '(buffer-revision "sandbox-probe") tier)
            (store:revision id))
     (check 'read-buffer-numbers-lines
            (contains? (eval '(read-buffer "sandbox-probe") tier)
                       "0: alpha")
            #t)
     (check 'read-buffer-range
            (contains? (eval '(read-buffer "sandbox-probe" 1 1) tier)
                       "1: beta")
            #t)
     (check 'read-buffer-missing
            (contains? (eval '(read-buffer "no-such") tier)
                       "error: no buffer named")
            #t)
     (check 'list-buffers-mentions-it
            (contains? (eval '(list-buffers) tier) "sandbox-probe")
            #t)

     ;; the boundary rule holds mechanically: strings cross it, but no
     ;; string mutator is granted, so sharing the store's immutable
     ;; lines is safe
     (check 'no-string-mutators
            (unbound? '(string-set! (buffer-text-line "sandbox-probe" 0)
                                    0 #\X)
                      tier)
            #t)

     (do ([i 0 (+ i 1)]) ((= i 205)) (log:add! 'tail-probe (number->string i) #f))
     (check 'log-tail-default-zero-count-and-cap
       (map (lambda (args) (eval (cons 'log-tail args) tier)) '(() (0) (1) (200) (201)))
       (map (lambda (n)
              (if (zero? n) "the log is empty"
                  (apply string-append
                    (map (lambda (i) (format "~a\n" (+ i (- 205 n)))) (iota n)))))
            '(20 0 1 200 200)))
     (check 'log-tail-rejects-counts-that-bypass-the-limit-and-extra-arguments
       (map (lambda (args) (test:raises? (lambda () (eval (cons 'log-tail args) tier))))
            '((-1) (1/2) (1.0) (#f) (1 2)))
       '(#t #t #t #t #t))

     ;; A cold corpus read may exhaust its engine fuel. Wind cleanup must
     ;; release its resources and leave no partial index for another reader;
     ;; retain the expired engine through the descriptor check so GC cannot
     ;; hide a leak, then discard it as policy:session-eval! does.
     (let* ([root (format "/tmp/e-sandbox-reference-~a-~a" (get-process-id) (random 1000000))]
            [data (string-append root "/data/describe")]
            [path (string-append data "/describe.sdata")]
            [descriptors (cond [(file-directory? "/proc/self/fd") "/proc/self/fd"]
                               [(file-directory? "/dev/fd") "/dev/fd"] [else #f])]
            [expired #f]
            [describe-text (eval 'describe-text tier)])
       (for-each mkdir (list root (string-append root "/data") data))
       (dynamic-wind
         void
         (lambda ()
           (call-with-output-file path
             (lambda (port)
               (write
                 (map (lambda (i)
                        (list (list (if (zero? i) 'fuel-reference (string->symbol (format "fuel~a" i))))
                              '(("procedure" . "(fuel-reference)"))
                              #f '() 'fixture "Fuel" #f "bounded reference"))
                      (iota 10000)) port)))
           (parameterize ([kernel:installation-directory root])
             (let ([before (and descriptors (length (directory-list descriptors)))])
               ((make-engine (lambda () (describe-text 'fuel-reference))) 10000
                (lambda (ticks value) (void)) (lambda (engine) (set! expired engine)))
               (let* ([finish (test:worker (lambda () (length (reference:lookup 'fuel-reference))))]
                      [count (finish)]
                      [after (and descriptors (length (directory-list descriptors)))])
                 (check 'expired-documentation-read-keeps-corpus-usable
                   (list (procedure? expired) count (equal? before after)) '(#t 1 #t))))))
         (lambda ()
           (set! expired #f)
           (delete-file path)
           (for-each delete-directory
             (list data (string-append root "/data") root)))))
     (store:delete! '(head test) id)
     (test:finish! 'sandbox)))
