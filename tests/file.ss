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
     (import (prefix (service file) file:) (prefix (service directory) directory:) (prefix (foundation path-filter) path-filter:) (prefix (sys sys) sys:) (prefix (test) test:)
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

     ;; Directory browsing caches metadata without opening files and can be
     ;; abandoned between filesystem calls without losing completed reads.
     ;; One tree covers boundaries, deep/hidden matches, links and races.
     (let ([root (path "browse")])
       (define (child name) (string-append root "/" name))
       (define (remove-tree path)
         (if (file-directory? path #f)
             (begin (for-each (lambda (name) (remove-tree (string-append path "/" name))) (directory-list path))
                    (delete-directory path))
             (delete-file path)))
       (define cache (directory:make-cache #f))
       (define (scan needle hidden? . observe)
         (let ([result #f])
           (directory:scan! cache root (if (string=? needle "") '() (cons (string-append root "/") (path-filter:parse needle (getenv "HOME")))) hidden? #t (lambda () #f)
             (lambda (entries failures done?)
               (when (pair? observe) ((car observe) entries failures done?))
               (when done? (set! result (cons failures entries))))) result))
       (define (entry name result)
         (find (lambda (e) (string=? (directory:entry-path e) (child name))) (cdr result)))
       (define (group name result)
         (define (names entries)
           (apply append (map (lambda (e) (cons (file:base-name (directory:entry-path e))
                                            (names (directory:entry-matches e)))) entries)))
         (let ([e (entry name result)])
           (list (directory:entry-count e) (directory:entry-complete? e)
                 (sort string<? (names (directory:entry-matches e))))))
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
               ((foreign-procedure "symlink" (string string) int) "absent" (child "large/broken-link"))
               ((foreign-procedure "symlink" (string string) int) "self-link" (child "large/self-link"))
               ((foreign-procedure "mkfifo" (string unsigned) int) (child "pipe") #o600)) '(0 0 0 0 0 0))
       (let* ([info (sys:file-info (child "needle-root"))] [stamp (file:stamp (child "needle-root"))])
         (check 'directory-metadata-is-typed-and-full-precision
           (list (vector-ref info 0) (vector-ref info 1)
                 (and (memv (vector-ref info 2) '(#f 3)) #t)
                 (= (vector-ref info 3) (+ (* (car stamp) 1000000000) (cdr stamp)))
                 (or (not (vector-ref info 4)) (integer? (vector-ref info 4))))
           '(file 416 #t #t #t)))
       (let ([result (scan "" #f)])
         (check 'directory-shallow-counts-and-symlink-boundary
           (list (car result) (group "small" result) (group "large" result)
                 (directory:entry-link? (entry "alias" result))
                 (directory:entry-count (entry "alias" result))
                 (directory:entry-kind (entry "dangling" result))
                 (directory:entry-kind (entry "pipe" result))
                 ;; Completion must use the same textual parent as opening,
                 ;; even when a link would traverse to a different OS parent.
                 (file:complete "small/loop/../ne" root)
                 (let ([read-directory (sys:directory-reader)] [fds (test:fd-count)])
                   (list (read-directory (child "alias") #f)
                         (length (read-directory (child "alias") #t))
                         (equal? fds (test:fd-count)))))
           '(0 (4 #t ()) (5 #t ()) #t #f link special ("small/loop/../needle-one" "small/loop/../nested/") (#f 4 #t))))
       (check 'directory-unresolved-links-match-by-name-without-making-counts-incomplete
         (map (lambda (query)
                (let ([result (scan query #f)])
                  (list (car result) (group "large" result)))) '("sls" "link"))
         '((0 (0 #t ())) (0 (2 #t ("broken-link" "self-link")))))
       (let ([result (scan "needle" #f)])
         (check 'directory-expands-all-matches-with-their-ancestors
           (list (car result) (group "small" result) (group "large" result)
                 (entry ".private" result))
           '(0 (2 #t ("NEEDLE-two" "needle-one" "nested")) (3 #t ("needle-a" "needle-b" "needle-c")) #f)))
       (check 'directory-hidden-matches-expand-too
         (let ([result (scan "needle" #t)])
           (list (group ".private" result) (group "small" result)))
         '((1 #t ("needle-secret")) (3 #t (".needle-dot" "NEEDLE-two" "needle-one" "nested"))))
       (check 'directory-matches-full-relative-paths-and-directory-slashes
         (let ([result (scan "SMALL/NESTED/" #f)])
           (list (group "small" result) (group "large" result)
                 (map (lambda (s) (directory:matches? (entry "small" result) (list s)))
                   '("small/" "ALL/" "small/nope"))))
         '((1 #t ("nested")) (0 #t ()) (#t #t #f)))
       (check 'directory-matching-stops-at-the-first-satisfying-directory
         (let ([result (scan "small" #f)])
           (list (group "small" result) (group "large" result) (car result)
                 (map (lambda (s) (directory:matches? (entry "small" result) (list s))) '("MALL" "small/" "all/"))))
         '((0 #t ()) (0 #t ()) 0 (#t #t #t)))
       (check 'directory-cancel-has-no-late-publication
         (let ([cancel? #f] [publications '()])
           (directory:scan! (directory:make-cache #f) root '("needle") #f #t (lambda () cancel?)
             (lambda (entries failures done?) (set! publications (cons done? publications)) (set! cancel? #t)))
           publications) '(#f))
       ;; With notifications disabled the exact same inventory must serve
       ;; another filter and another root even when disk is unavailable.
       (let ([before (scan "needle" #t)] [away (string-append root "-away")] [inside #f])
         (rename-file root away)
         (dynamic-wind void
           (lambda ()
             (directory:scan! cache (child "small/nested") '("two") #f #t (lambda () #f)
               (lambda (entries failures done?)
                 (when done? (set! inside (list failures (map directory:entry-path entries))))))
             (check 'directory-cache-reuses-listings-metadata-and-navigation-without-disk
               (list (group "small" (scan "two" #f)) inside
                     (eq? (entry "needle-root" before) (entry "needle-root" (scan "needle" #t))))
               (list '(1 #t ("NEEDLE-two" "nested")) (list 0 (list (child "small/nested/NEEDLE-two"))) #t))
             (directory:clear! cache)
             (check 'directory-refresh-discards-stale-inventory-and-reports-unreadable-root
               (scan "needle" #t) '(1)))
           (lambda () (rename-file away root))))
       ;; One live event stream covers edits, populated directory moves,
       ;; replacement at the same path and resource cleanup. Other entries
       ;; keep their metadata object, proving they were not inspected again.
       (let ([probe (sys:open-directory-watch)])
         (when probe
           (sys:close-directory-watch! probe)
           (let ([before-fds (test:fd-count)] [seen #f])
             (set! cache (directory:make-cache #t))
             (dynamic-wind void
               (lambda ()
                 (let ([stable (entry "needle-root" (scan "needle" #f))])
                   (file:write! (child "small/needle-one") '#("longer updated text") #f)
                   (test:await 'file-event (lambda () (directory:poll! cache)))
                   (let* ([result (scan "needle" #f)]
                          [changed (find (lambda (e) (string=? (directory:entry-path e) (child "small/needle-one")))
                                     (directory:entry-matches (entry "small" result)))])
                     (set! seen (list (= (directory:entry-size changed) 19) (eq? stable (entry "needle-root" result)))))
                   (chmod (child "small/nested") #o700)
                   (rename-file (child "small/nested") (child "small/moved"))
                   (mkdir (child "small/nested"))
                   (call-with-output-file (child "small/nested/NEEDLE-two") (lambda (p) (display "new inode" p)))
                   (test:await 'directory-move (lambda () (directory:poll! cache)))
                   (let* ([result (scan "needle" #f)]
                          [nested (find (lambda (e) (string=? (directory:entry-path e) (child "small/nested")))
                                    (directory:entry-matches (entry "small" result)))])
                     (set! seen (append seen (list (group "small" result)
                                               (directory:entry-size (car (directory:entry-matches nested)))))))
                   (remove-tree (child "small/moved")) (mkdir (child "small/moved"))
                   (do ([i 0 (+ i 1)]) ((= i 25))
                     (call-with-output-file (child (format "small/moved/needle-~a" i)) (lambda (p) (display i p))))
                   (test:await 'directory-replacement (lambda () (directory:poll! cache)))
                   (let ([result (scan "needle" #f)])
                     (set! seen (append seen (list (directory:entry-count (entry "small" result))
                                               (length (caddr (group "small" result)))
                                               (eq? stable (entry "needle-root" result))))))))
               (lambda () (directory:close! cache)))
             (check 'directory-events-invalidate-only-changes-and-expand-past-the-old-cap
               (append seen (list (equal? before-fds (test:fd-count))))
               '(#t #t (3 #t ("NEEDLE-two" "NEEDLE-two" "moved" "needle-one" "nested")) 9 27 29 #t #t)))))
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




     ;; -- checksums ----------------------------------------------------------

     ;; a text's checksum is the digest library's SHA-256 of its UTF-8, tagged
     (check 'a-text-checksum-is-tagged-sha256
       (file:checksum "abc") "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

     ;; -- clean up ------------------------------------------------------------

     (for-each (lambda (f) (delete-file (path f))) '("alpha" "alphabet" ".hidden"))
     (delete-directory (path "dir"))
     (delete-directory scratch)
     (check 'scratch-removed (file-exists? scratch) #f)

     (test:finish! 'file)))
