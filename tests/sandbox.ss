#!/usr/bin/env scheme-script

;; The read-only capability environment: reachability is the whole
;; game -- granted names work, everything else fails to resolve --
;; plus the bounded editor readers over the state store.  v2 stage 4
;; (dev/DESIGN2.md).  Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (state) state:)
             (only (chezscheme) environment eval format))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'sandbox-test label actual expected)))

     (define (contains? text needle)
       (let ([n (string-length text)] [m (string-length needle)])
         (let scan ([i 0])
           (cond [(> (+ i m) n) #f]
                 [(string=? (substring text i (+ i m)) needle) #t]
                 [else (scan (+ i 1))]))))

     (define (unbound? form env)
       (guard (ex [else (undefined-violation? ex)])
         (eval form env)
         'resolved))

     ;; -- the full tier: computation works, power is unreachable ------

     (define tier (environment '(sandbox)))

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

     ;; -- a narrowed grant: (only (sandbox) ...) subsets it -----------

     (define narrow (environment '(only (sandbox) + car cons quote)))

     (check 'granted-name-works (eval '(+ 1 2) narrow) 3)
     (check 'ungranted-name-fails
            (unbound? '(string-append "a" "b") narrow) #t)
     (check 'ungranted-reader-fails
            (unbound? '(buffer-names) narrow) #t)

     ;; -- the editor readers: by name, over the store, bounded --------

     (define id (state:create! '(head test) "sandbox-probe"
                               '("alpha" "beta")))

     (check 'buffer-names-sees-the-store
            (and (member "sandbox-probe" (eval '(buffer-names) tier)) #t)
            #t)
     (check 'line-count (eval '(buffer-lines-count "sandbox-probe") tier) 2)
     (check 'line (eval '(buffer-text-line "sandbox-probe" 1) tier) "beta")
     (check 'revision-is-data
            (eval '(buffer-revision "sandbox-probe") tier)
            (state:revision id))
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

     (state:delete! '(head test) id)
     (format #t "~a sandbox checks passed\n" checks)))
