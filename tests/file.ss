#!/usr/bin/env scheme-script

;; The disk seam: path algebra, the line/trailing-newline algebra,
;; reading and permission-preserving writing, stamps, completion over
;; a directory, and the three-way merge over text -- v2 core
;; dissolution (docs/DESIGN2.md).  Works in a scratch directory it
;; creates and removes.  Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (file) file:)
             (only (chezscheme)
                   format getenv putenv current-directory directory-list
                   delete-file delete-directory mkdir chmod get-mode
                   file-exists? file-directory? time-second current-time
                   random))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'file-test label actual expected)))

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

     (check 'stamp-shape (pair? (file:stamp (path "alpha"))) #t)
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
     (check 'complete-nowhere (file:complete "/no/such/dir/x") '())

     (check 'visit-path-existing
            (file:visit-path (string-append scratch "/./alpha")) (path "alpha"))
     (check 'visit-path-new-file
            (file:visit-path (string-append scratch "/dir/../new.txt")) (path "new.txt"))

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

     (format #t "~a file checks passed\n" checks)))
