#!/usr/bin/env scheme-script

;; The disk seam: path algebra, the line/trailing-newline algebra,
;; reading and permission-preserving writing, stamps, completion over
;; a directory.  Works in a
;; scratch directory it creates and removes.  Run from the repository
;; root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (service file) file:) (prefix (service directory) directory:) (prefix (sys sys) sys:) (prefix (test) test:)
             (prefix (service log) log:)
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
            (list (file:absolute "x") (file:absolute "x" "/a") (file:absolute "x" "/a/")
                  (file:absolute "~literal" "/a"))
            (list (string-append (current-directory) "/x") "/a/x" "/a/x" "/a/~literal"))

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
     (check 'complete-relative-to-an-explicit-directory
       (map (lambda (s) (file:complete s scratch)) '("al" "AL" "dir/../al" "nope/../al" ".h" "nope/"))
       '(("alpha" "alphabet") () ("dir/../alpha" "dir/../alphabet") ("nope/../alpha" "nope/../alphabet") (".hidden") ()))

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

     ;; Directory browsing uses metadata without opening files, counts past
     ;; the display threshold, and can be abandoned between filesystem calls.
     ;; One tree covers boundaries, deep/hidden matches, links and races.
     (let ([root (path "browse")])
       (define (child name) (string-append root "/" name))
       (define (remove-tree path)
         (if (file-directory? path #f)
             (begin (for-each (lambda (name) (remove-tree (string-append path "/" name))) (directory-list path))
                    (delete-directory path))
             (delete-file path)))
       (define (scan needle hidden? limit . observe)
         (let ([result #f])
           (directory:scan root needle hidden? limit (lambda () #f)
             (lambda (entries failures done?)
               (when (pair? observe) ((car observe) entries failures done?))
               (when done? (set! result (cons failures entries))))) result))
       (define (entry name result)
         (find (lambda (e) (string=? (directory:entry-path e) (child name))) (cdr result)))
       (define (group name result)
         (let ([e (entry name result)])
           (list (directory:entry-count e) (directory:entry-complete? e)
                 (sort string<? (map (lambda (match) (file:base-name (directory:entry-path match)))
                                  (directory:entry-matches e))))))
       (mkdir root)
       (for-each (lambda (dir) (mkdir (child dir))) '("small" "small/nested" "large" ".private"))
       (for-each (lambda (name) (file:write! (child name) '#("needle in the contents") #f))
         '("small/needle-one" "small/nested/NEEDLE-two" "small/nested/.needle-dot" "small/unrelated"
           "large/needle-a" "large/needle-b" "large/needle-c" ".private/needle-secret" "needle-root"))
       (file:write! (child "needle-root") '#("abc") #f)
       (chmod (child "needle-root") #o640)
       ;; Native test-fixture operations stay in Scheme; (sys) loaded libc.
       (check 'directory-links-fixture
         (list ((foreign-procedure "symlink" (string string) int) root (child "small/loop"))
               ((foreign-procedure "symlink" (string string) int) (child "small") (child "alias"))
               ((foreign-procedure "symlink" (string string) int) (child "absent") (child "dangling"))
               ((foreign-procedure "mkfifo" (string unsigned) int) (child "pipe") #o600)) '(0 0 0 0))
       (let* ([info (sys:file-info (child "needle-root"))] [stamp (file:stamp (child "needle-root"))])
         (check 'directory-metadata-is-typed-and-full-precision
           (list (vector-ref info 0) (vector-ref info 1)
                 (and (memv (vector-ref info 2) '(#f 3)) #t)
                 (= (vector-ref info 3) (+ (* (car stamp) 1000000000) (cdr stamp)))
                 (or (not (vector-ref info 4)) (integer? (vector-ref info 4))))
           '(file 416 #t #t #t)))
       (let ([result (scan "" #f 2)])
         (check 'directory-shallow-counts-and-symlink-boundary
           (list (car result) (group "small" result) (group "large" result)
                 (directory:entry-link? (entry "alias" result))
                 (directory:entry-count (entry "alias" result))
                 (directory:entry-kind (entry "pipe" result))
                 ;; Completion must use the same textual parent as opening,
                 ;; even when a link would traverse to a different OS parent.
                 (file:complete "small/loop/../ne" root))
           '(0 (4 #t ()) (3 #t ()) #t #f special ("small/loop/../needle-one" "small/loop/../nested/"))))
       (let ([result (scan "needle" #f 2)])
         (check 'directory-expands-at-the-limit-and-counts-beyond-it
           (list (car result) (group "small" result) (group "large" result)
                 (entry ".private" result))
           '(0 (2 #t ("NEEDLE-two" "needle-one")) (3 #t ()) #f)))
       (check 'directory-hidden-and-zero-expansion
         (let ([result (scan "needle" #t 0)])
           (list (group ".private" result) (group "small" result)))
         '((1 #t ()) (3 #t ())))
       ;; Reuse the same tree and real shallow publications: narrowing must
       ;; keep valid rows, widening keeps a lower bound, and hidden ancestors
       ;; and leaves must disappear immediately. A completed snapshot wins.
       (let ([before (scan "needle" #t 4)])
         (check 'directory-refilter-keeps-only-valid-evidence
           (map (lambda (step)
                  (let ([result (cons 0 (directory:refilter (cdr before) root "needle" (car step) #t (cadr step) (caddr step)))])
                    (list (group "small" result) (group "large" result)
                          (and (entry ".private" result) #t))))
             '(("NEEDLE-" #f 2) ("ne" #t 4) ("absent" #f 2) ("" #f 2)))
           '(((2 #t ("NEEDLE-two" "needle-one")) (3 #t ()) #f)
             ((3 #f (".needle-dot" "NEEDLE-two" "needle-one")) (3 #f ("needle-a" "needle-b" "needle-c")) #t)
             ((0 #f ()) (0 #f ()) #f)
             ((#f #f ()) (#f #f ()) #f))))
       (check 'directory-publications-preserve-matches-until-authoritative-replacement
         (let* ([before (scan "needle-one" #f 2)]
                [preview (directory:refilter (cdr before) root "needle-one" "needle" #f #f 2)]
                [shallow #f]
                [after (scan "needle" #f 2
                         (lambda (entries failures done?)
                           (unless shallow
                             (set! shallow (cons failures (directory:reconcile entries preview 2 done?))))))]
                [narrowed (directory:refilter (cdr after) root "needle" "needle-o" #f #f 2)])
           (list (group "small" shallow) (group "large" shallow)
                 (group "small" (cons 0 narrowed)) (group "large" (cons 0 narrowed))
                 (group "small" (cons 0 (directory:reconcile (cdr after) preview 2 #t)))
                 (directory:reconcile '() preview 2 #t)))
         '((1 #f ("needle-one")) (0 #f ())
           (1 #t ("needle-one")) (#f #f ())
           (2 #t ("NEEDLE-two" "needle-one")) ()))
       (check 'directory-matches-full-relative-paths-and-directory-slashes
         (let ([result (scan "ALL/NESTED/" #f 2)])
           (list (group "small" result) (group "large" result)
                 (map (lambda (s) (directory:matches? (entry "small" result) root s))
                   '("small/" "ALL/" "small/nope"))))
         '((2 #t ("NEEDLE-two" "nested")) (0 #t ()) (#t #t #f)))
       ;; A filter without a slash names entries, so a directory whose own
       ;; name matches does not claim its whole subtree. Adding a slash
       ;; switches to path matching, and a preview must not carry the
       ;; name-mode count across as exact evidence.
       (check 'directory-name-filters-ignore-matching-ancestors
         (let ([result (scan "small" #f 2)])
           (list (group "small" result) (group "large" result) (car result)
                 (map (lambda (s) (directory:matches? (entry "small" result) root s)) '("MALL" "small/" "all/"))
                 (group "small" (cons 0 (directory:refilter (cdr result) root "small" "small/" #f #f 2)))
                 (group "small" (scan "small/" #f 2))))
         '((0 #t ()) (0 #t ()) 0 (#t #t #t) (0 #f ()) (5 #t ())))
       (check 'directory-cancel-has-no-late-publication
         (let ([cancel? #f] [publications '()])
           (directory:scan root "needle" #f 2 (lambda () cancel?)
             (lambda (entries failures done?) (set! publications (cons done? publications)) (set! cancel? #t)))
           publications) '(#f))
       (check 'directory-vanishing-subtree-stays-explicitly-incomplete
         (let ([changed? #f] [result #f] [pending #f] [preview (cdr (scan "secret" #t 2))])
           (directory:scan root "secret" #t 2 (lambda () #f)
             (lambda (entries failures done?)
               (unless changed? (set! changed? #t) (remove-tree (child ".private")))
               (when done?
                 ;; A failed group's partial zero does not erase known
                 ;; matches; the final snapshot must discard them.
                 (set! pending (cons failures (directory:reconcile entries preview 2 #f)))
                 (set! result (cons failures (directory:reconcile entries preview 2 #t))))))
           (list (car result) (group ".private" pending) (group ".private" result)))
         '(1 (1 #f ("needle-secret")) (0 #f ())))
       (check 'file-read-refuses-special-files-before-opening
         (map (lambda (name) (test:raises? (lambda () (file:read (child name))))) '("pipe" "small"))
         '(#t #t))
       (check 'creation-reuses-directories-and-exclusively-creates-empty-files
         (begin
           (for-each (lambda (name) (file:make-directories! (child name)))
             '("created/parents/" "created/parents" "alias"))
           (file:create! (child "created/parents/empty"))
           (list (file-directory? (child "created/parents"))
                 (file:read (child "created/parents/empty"))
                 (map (lambda (name) (test:raises? (lambda () (file:make-directories! (child name)))))
                   '("needle-root/child" "dangling/child"))
                 (map (lambda (name)
                        (guard (ex [(i/o-file-already-exists-error? ex) #t])
                          (file:create! (child name)) #f))
                   '("needle-root" "alias" "dangling" "pipe" "created/parents/empty" "created/parents/"))
                 (file:read (child "needle-root")) (file-exists? (child "absent"))
                 (map log:datum (reverse (log:entries 'file)))))
         (list #t "" '(#t #t) '(#t #t #t #t #t #t) "abc" #f
               (list (string-append "Created directory " (child "created/"))
                     (string-append "Created directory " (child "created/parents/"))
                     (string-append "Created file " (child "created/parents/empty")))))
       (remove-tree root))

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




     ;; -- clean up ------------------------------------------------------------

     (for-each (lambda (f) (delete-file (path f))) '("alpha" "alphabet" ".hidden"))
     (delete-directory (path "dir"))
     (delete-directory scratch)
     (check 'scratch-removed (file-exists? scratch) #f)

     (test:finish! 'file)))
