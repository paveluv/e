#!/usr/bin/env scheme-script

;; The disk seam: path algebra, the line/trailing-newline algebra,
;; reading and permission-preserving writing, stamps, completion over
;; a directory, and the three-way merge over text.  Works in a
;; scratch directory it creates and removes.  Run from the repository
;; root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (file) file:) (prefix (test) test:)
             (only (chezscheme)
                   format getenv putenv current-directory
                   delete-file delete-directory mkdir chmod get-mode
                   file-exists? time-second current-time
                   random))

     (define check test:check)

     ;; -- the line algebra -----------------------------------------------

     (check 'lines (vector->list (file:lines "a\nb\n")) '("a" "b"))
     (check 'lines-no-trailing (vector->list (file:lines "a\nb")) '("a" "b"))
     (check 'lines-empty (vector->list (file:lines "")) '(""))
     (check 'lines-only-newline (vector->list (file:lines "\n")) '(""))
     (check 'ends-in-newline (file:ends-in-newline? "a\n") #t)
     (check 'ends-in-newline-not (file:ends-in-newline? "a") #f)
     (check 'ends-in-newline-empty (file:ends-in-newline? "") #f)
     (check 'text (file:text (vector "a" "b") #t) "a\nb\n")
     (check 'text-no-trailing (file:text (vector "a" "b") #f) "a\nb")
     (check 'text-round-trip
            (let ([s "one\n\nthree\n"])
              (file:text (file:lines s) (file:ends-in-newline? s)))
            "one\n\nthree\n")

     ;; -- paths --------------------------------------------------------------

     (check 'directory-part (file:directory-part "/a/b/c.e") "/a/b/")
     (check 'directory-part-none (file:directory-part "c.e") #f)
     (check 'base-name (file:base-name "/a/b/c.e") "c.e")
     (check 'base-name-bare (file:base-name "c.e") "c.e")
     (check 'canonical-dots (file:canonical "/a/./b/../c") "/a/c")
     (check 'canonical-empty-segments (file:canonical "//a///b/") "/a/b")
     (check 'canonical-relative
            (file:canonical "x/y")
            (string-append (current-directory) "/x/y"))
     (check 'absolute-keeps (file:absolute "/x") "/x")
     (check 'absolute-keeps-tilde (file:absolute "~/x") "~/x")
     (check 'absolute-relative
            (file:absolute "x") (string-append (current-directory) "/x"))

     (let ([home (getenv "HOME")])
       (check 'expand-tilde (file:expand "~/x") (string-append home "/x"))
       (check 'expand-bare-tilde (file:expand "~") home)
       (check 'expand-plain (file:expand "/x") "/x")
       (check 'abbreviate (file:abbreviate (string-append home "/x")) "~/x")
       (check 'abbreviate-other (file:abbreviate "/nowhere/x") "/nowhere/x"))

     ;; -- a scratch directory -----------------------------------------------

     (define scratch
       (format "~a/e-files-test-~a-~a"
               (or (getenv "TMPDIR") "/tmp")
               (time-second (current-time)) (random 1000000)))
     (mkdir scratch)
     (mkdir (string-append scratch "/dir"))

     (define (path name) (string-append scratch "/" name))

     (file:write! (path "alpha") (vector "one" "two") #t)
     (check 'write-read (file:read (path "alpha")) "one\ntwo\n")
     (file:write! (path "alphabet") (vector "x") #f)
     (check 'write-read-no-trailing (file:read (path "alphabet")) "x")
     (file:write! (path ".hidden") (vector "") #f)
     (check 'write-empty (file:read (path ".hidden")) "")

     (chmod (path "alphabet") #o755)
     (file:write! (path "alphabet") (vector "y") #t)
     (check 'write-keeps-permissions (logand (get-mode (path "alphabet")) #o777) #o755)
     (check 'rewrite-read (file:read (path "alphabet")) "y\n")

     (check 'read-state-pairs-the-content-with-its-observed-stamp
            (file:read-state (path "alpha")) (cons "one\ntwo\n" (file:stamp (path "alpha"))))
     (check 'stamp-absent (file:stamp (path "nope")) #f)
     (check 'read-absent-raises
            (guard (ex [else 'raised]) (file:read (path "nope"))) 'raised)

     (check 'complete
            (file:complete (path "al"))
            (list (path "alpha") (path "alphabet")))
     (check 'complete-hides-dotfiles-and-marks-directories
            (file:complete (string-append scratch "/"))
            (list (path "alpha") (path "alphabet") (path "dir/")))
     (check 'complete-dotfiles-on-request
            (file:complete (path ".")) (list (path ".hidden")))
     (check 'complete-home-root-and-missing-directory
            (list (file:complete "~") (file:complete "/no/such/dir/x")) '(("~/") ()))

     (check 'visit-path-existing
            (file:visit-path (string-append scratch "/./alpha")) (path "alpha"))
     (check 'visit-path-new-file
            (file:visit-path (string-append scratch "/dir/../new.txt")) (path "new.txt"))
     ;; a missing child of the root joins onto realpath's "/" without a
     ;; second separator, so its identity holds once the file exists
     (let ([name (format "e-file-test-~a-~a" (getenv "USER") (random 1000000))])
       (check 'visit-path-new-root-child
              (list (file-exists? (string-append "/" name))
                    (file:visit-path (string-append "/./" name)))
              (list #f (string-append "/" name))))

     ;; One table covers the shared port scope for text and corpus data.
     ;; Keep both the port and any expired engine alive through the check;
     ;; automatic collection must not conceal a missing close.
     (for-each
       (lambda (output?)
         (check (list 'port-lifetime (if output? 'output 'input))
           (map (lambda (exit)
                  (let* ([port #f] [expired #f]
                         [result
                          (guard (ex [(eq? ex 'port-error) 'raised])
                            ((make-engine
                               (lambda ()
                                 (file:call-with-port (path "alpha") output?
                                   (lambda (p)
                                     (set! port p)
                                     (case exit
                                       [(return) (values 'returned 'values)]
                                       [(raise) (raise 'port-error)]
                                       [(fuel) (engine-block)])))))
                             100000 (lambda (ticks . values) values)
                             (lambda (engine) (set! expired engine) 'fuel)))])
                    (list result (port-closed? port) (procedure? expired)
                          (if expired
                              (begin
                                ;; Retrying a closed scope must not reopen or
                                ;; truncate the newer file before it refuses.
                                (file:write! (path "alpha") '#("later") #f)
                                (and (test:raises?
                                       (lambda () (expired 100000 (lambda args (void)) (lambda args (void))))
                                       (lambda (ex)
                                         (and (who-condition? ex) (eq? (condition-who ex) 'file:call-with-port))))
                                     (string=? (file:read (path "alpha")) "later")))
                              #t))))
                '(return raise fuel))
           '(((returned values) #t #f #t) (raised #t #f #t) (fuel #t #t #t))))
       '(#f #t))

     ;; Real helpers must also use that scope. Exhaust fuel during substantial
     ;; I/O, retaining continuations while observing descriptors where available.
     ;; Replacing a file must restore its original mode even after interruption.
     (let ([large (make-vector 100000 "ordinary file data")] [expired '()])
       (for-each
         (lambda (kind)
           (file:write! (path "alpha") large #t)
           (chmod (path "alpha") #o751)
           (let* ([before (test:fd-count)]
                  [result
                   ((make-engine
                      (lambda ()
                        (if (eq? kind 'read) (file:read (path "alpha"))
                            (file:write! (path "alpha") large #t))))
                    10000 (lambda args 'finished)
                    (lambda (engine) (set! expired (cons engine expired)) 'fuel))])
             (check (list kind 'expired-file-io)
               (list result (and (pair? expired) (procedure? (car expired)))
                     (equal? before (test:fd-count)) (logand (get-mode (path "alpha")) #o777))
               '(fuel #t #t #o751))))
         '(read write)))

     ;; Pause a real read with its descriptor open, replace the pathname,
     ;; then finish reading the old inode. A new timestamp cannot certify it.
     (let ([old (apply string-append (make-list 100000 "ordinary line\n"))]
           [handler (timer-interrupt-handler)] [interrupted? #f])
       (file:write! (path "alpha") (file:lines old) #t)
       (let ([before (file:stamp (path "alpha"))])
         (dynamic-wind void
           (lambda ()
             ;; Interrupt in place, keeping the port's ordinary unwind owner.
             (timer-interrupt-handler
               (lambda ()
                 (set! interrupted? #t)
                 ;; Wait for an observable change, even on coarse clocks.
                 (let change ([tries 100])
                   (sleep (make-time 'time-duration 20000000 0))
                   (file:write! (path "alpha") '#("later") #t)
                   (when (equal? before (file:stamp (path "alpha")))
                     (when (zero? tries) (error 'file-test "timestamp did not advance"))
                     (change (- tries 1))))))
             (set-timer 10000)
             (let ([result (file:read-state (path "alpha"))])
               (set-timer 0)
               (check 'changed-during-read-keeps-content-with-an-unknown-stamp
                 (list interrupted? (string=? old (car result)) (cdr result)) '(#t #t #f))))
           (lambda () (set-timer 0) (timer-interrupt-handler handler)))))

     ;; -- merging -------------------------------------------------------------

     (let-values ([(merged trailing conflicts report)
                   (file:merge "f" "a\nb\nc\n" "a\nB\nc\n" "a\nb\nc\nd\n")])
       (check 'merge-clean (vector->list merged) '("a" "B" "c" "d"))
       (check 'merge-clean-trailing trailing #t)
       (check 'merge-clean-conflicts conflicts 0)
       (check 'merge-clean-report (list? report) #t))

     (let-values ([(merged trailing conflicts report)
                   (file:merge "f" "a\nb\n" "a\nX\n" "a\nY\n")])
       (check 'merge-conflict-count conflicts 1)
       (check 'merge-conflict-markers (file:conflict-count merged) 1)
       (check 'merge-conflict-keeps-common (vector-ref merged 0) "a"))

     (let-values ([(merged trailing conflicts report)
                   (file:merge "f" "a\n" "a" "a\nb\n")])
       (check 'merge-trailing-mine-differs trailing #f)
       (check 'merge-trailing-lines (vector->list merged) '("a" "b")))

     (let-values ([(merged trailing conflicts report)
                   (file:merge "f" "a\n" "a\n" "a")])
       (check 'merge-trailing-theirs-differs trailing #f))

     (check 'conflict-count-none (file:conflict-count (vector "a" "<<< not a marker")) 0)

     ;; -- clean up ------------------------------------------------------------

     (for-each (lambda (f) (delete-file (path f))) '("alpha" "alphabet" ".hidden"))
     (delete-directory (path "dir"))
     (delete-directory scratch)
     (check 'scratch-removed (file-exists? scratch) #f)

     (test:finish! 'file)))
