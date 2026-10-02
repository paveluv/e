#!/usr/bin/env scheme-script

;; Reloading a file that changed on disk: reopening reloads, the disk's
;; text the baseline again and the buffer's entries reapplied on top, an
;; overlapped entry pending as a conflict with the disk's side shown; the
;; conflict type completes and previews by flipping the region in place;
;; the resolution commands settle conflicts, and the browser's conflict
;; rows do by key; a stale save reloads first; reread adopts the disk
;; verbatim. A scratch file, one head over the base.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(test-evaluate!
  '(begin
     (import (prefix (test) test:) (prefix (apps delta-log) delta-log:) (prefix (apps search) search:)
       (prefix (foundation edoc) edoc:) (prefix (foundation string) string:) (prefix (core kernel) kernel:)
       (prefix (head routing) routing:) (prefix (head head) head:) (prefix (head widget) widget:)
       (prefix (head keymap) keymap:) (prefix (head mode) mode:)
       (prefix (service conflict-review) conflict-review:) (prefix (state model) model:)
       (prefix (service document) document:) (prefix (service file) file:) (prefix (service log) log:)
       (prefix (state store) store:) (prefix (foundation text) text:)
       (only (chezscheme) format get-process-id mkdir delete-file delete-directory))
     (define check test:check)
     (include "tests/editor-fixture.sps")
     (define (bound-to context key)
       (let ([hit (keymap:resolved-binding context (list key))])
         (and hit (keymap:binding-action (cdr hit)))))
     (delta-log:init!)
     (define dir (format "/tmp/e-reload-~a" (get-process-id)))
     (mkdir dir)
     (define path (string-append dir "/notes.txt"))
     (define (write-disk! text) (file:write! path (file:lines text) (file:ends-in-newline? text)))
     (define (lines) (vector->list (lines-of b)))
     (define (resolve-all! . side)
       (let* ([id (view:source (interaction:snapshot editing))]
              [draft (conflict-review:create! head:ui-actor (list id))]
              [get (lambda (r k) (cdr (assq k r)))]
              [r (model:snapshot draft)]
              [groups (list (cons id (store:conflicts id)))])
         (conflict-review:choose! head:ui-actor draft (get r 'revision) groups
           (if (null? side) 'disk (car side)))
         (conflict-review:settle! head:ui-actor draft (model:revision draft) (list id))
         (conflict-review:close! head:ui-actor draft (model:revision draft))
         (refresh!)
         (length (cdar groups))))
     (define (contains? s part) (and (string:search s part 0 (string-length s)) #t))
     (write-disk! "alpha\nbeta\ngamma\n")
     (visit! path)
     (define b (view:source (interaction:snapshot editing)))
     (check 'the-file-is-visited (lines) '("alpha" "beta" "gamma"))
     (edit:replace-region-text! editing '(0 . 0) '(0 . 5) "ALPHA")
     (goto! '(2 . 5))
     (edit:insert! editing " tail")
     (write-disk! "omega\nbeta\nGAMMA\n")
     (visit! path)
     (check
       'reopening-reloads-with-the-disks-side-where-they-collide
       (lines)
       '("omega" "beta" "GAMMA tail"))
     (define conflicts (store:conflicts (view:source (interaction:snapshot editing))))
     (check
       'the-conflict-lists-its-region-and-both-sides
       (list
         (length conflicts)
         (cadddr (car conflicts))
         (list-ref (car conflicts) 4)
         (list-ref (car conflicts) 5))
       (list 1 '(0 0 0 5) '("ALPHA") '("omega")))
     (define rev (car (car conflicts)))
     (check
       'a-conflict-completes-with-both-sides-in-its-hint
       (let ([offered (parameterize ([widget:target editing]) (edoc:type-completions 'conflict ""))])
         (list (map car offered) (contains? (caddr (car offered)) "mine \"ALPHA\"")))
       (list (list rev) #t))
     (check
       'settlement-writes-mine-with-an-undoable-conflict-label
       (list
         (delta-log:resolve! (view:source (interaction:snapshot editing)) rev 'mine)
         (lines)
         (store:conflicts (view:source (interaction:snapshot editing)))
         (assq 'conflict (caddr (car (delta-log:log (view:source (interaction:snapshot editing)))))))
       (list 'applied '("ALPHA" "beta" "GAMMA tail") '() (cons 'conflict rev)))
     (edit:save! editing)
     (check 'saving-writes-the-resolved-text (file:read path) "ALPHA\nbeta\nGAMMA tail\n")
     (write-disk! "ALPHA\nBETA\nGAMMA tail\n")
     (goto! '(0 . 5))
     (check
       'an-edit-after-a-disk-change-applies-then-the-buffer-reloads-and-merges-at-once
       (list (begin (edit:insert! editing "!") (lines)) (positive? (store:property b 'conflicts 0)))
       '(("ALPHA!" "BETA" "GAMMA tail") #f))
     (edit:save! editing)
     (check
       'saving-after-the-reload-writes
       (list (lines) (file:read path))
       (list '("ALPHA!" "BETA" "GAMMA tail") "ALPHA!\nBETA\nGAMMA tail\n"))
     (edit:replace-region-text! editing '(0 . 0) '(0 . 6) "OMEGA!")
     (edit:replace-region-text! editing '(2 . 0) '(2 . 5) "Gamma")
     (write-disk! "alpha!\nBETA\ngamma tail\n")
     (check
       'a-save-over-a-changed-disk-reloads-and-refuses-while-conflicts-pend
       (list (guard (ex [(kernel:refusal? ex) (condition-message ex)]) (edit:save! editing)) (lines)
         (length (store:conflicts (view:source (interaction:snapshot editing))))
         (positive? (store:property b 'conflicts 0)) (file:read path))
       (list "Resolve the conflicts first" '("alpha!" "BETA" "gamma tail") 2 #t
         "alpha!\nBETA\ngamma tail\n"))
     (check
       'bulk-disk-resolution-keeps-text-and-history
       (list
         (resolve-all!)
         (store:conflicts (view:source (interaction:snapshot editing)))
         (lines)
         (pair? (delta-log:log (view:source (interaction:snapshot editing)))))
       '(2 () ("alpha!" "BETA" "gamma tail") #t))
     (define path2 (string-append dir "/typed.txt"))
     (file:write! path2 (file:lines "abcdefgh\n") #t)
     (visit! path2)
     (define t (view:source (interaction:snapshot editing)))
     (goto! '(0 . 4))
     (routing:input! editing '(key "BACKSPACE"))
     (routing:input! editing '(text "8" keyboard))
     (file:write! path2 (file:lines "abcDefgh\n") #t)
     (visit! path2)
     (define typed (store:conflicts (view:source (interaction:snapshot editing))))
     (check
       'a-typed-replacement-conflicts-whole-with-the-disks-side-standing
       (list
         (vector->list (lines-of t))
         (map (lambda (c) (list (cadddr c) (list-ref c 4) (list-ref c 5))) typed))
       '(("abcDefgh") (((0 0 0 8) ("abc8efgh") ("abcDefgh")))))
     (check
       'keeping-mine-writes-the-typed-replacement
       (list
         (delta-log:resolve! (view:source (interaction:snapshot editing)) (car (car typed)) 'mine)
         (vector->list (lines-of t)))
       '(applied ("abc8efgh")))
     (delete-file path2)
     (show! b)
     (goto! '(0 . 0))
     (edit:insert! editing "S")
     (check
       'settled-conflicts-make-the-buffer-savable-again
       (list (positive? (store:property b 'conflicts 0)) (edit:save! editing) (file:read path))
       (list #f #t (string-append (string:join (lines) "\n") "\n")))
     (define path3 (string-append dir "/other.txt"))
     (mode:register! "saved-text" '(".txt") '() #f)
     (file:write! path3 (file:lines "keep me\n") #t)
     (define scratch (store:create! head:ui-actor "scratch-save" '("") '((trailing . #t))))
     (show! scratch)
     (goto! '(0 . 0))
     (edit:insert! editing "new text")
     (define (backups-of path) (filter (lambda (entry) (string=? (cadr entry) path)) (edit:backups)))
     (check
       'a-save-as-over-a-file-backs-up-what-it-held
       (let* ([logged (length (log:entries 'document:save-document!))]
              [saved (edit:save-file! editing path3)]
              [on-disk (file:read path3)]
              [entry (car (backups-of path3))]
              [messages (log:entries 'document:save-document!)])
         (list saved on-disk (store:property scratch 'file #f) (car entry) (list-ref entry 4)
           (and (list-ref entry 3) #t)
           (and (find (lambda (t) (string=? (car t) "other.txt.bak")) (edit:trash)) #t)
           (- (length messages) logged) (log:format-entry (car messages))))
       (list #t "new text\n" path3 "other.txt.bak" (file:checksum "keep me\n") #t #f 1
         (format "Wrote ~a; what it held is kept as other.txt.bak" path3)))
     (edit:insert! editing " again")
     (check
       'a-save-backs-up-the-version-it-writes-over
       (let* ([saved (edit:save-file! editing path3)] [names (map car (backups-of path3))])
         (list saved names))
       '(#t ("other.txt.bak<2>" "other.txt.bak")))
     (define (save-as-from! name text)
       (let ([b (store:create! head:ui-actor name '("") '((trailing . #t)))])
         (show! b)
         (goto! '(0 . 0))
         (edit:insert! editing text)
         (edit:save-file! editing path3)
         b))
     (define third (save-as-from! "scratch-third" "keep me"))
     (define fourth (save-as-from! "scratch-fourth" "fourth"))
     (check
       'a-version-the-backups-hold-is-not-kept-twice
       (list (file:read path3) (map car (backups-of path3)))
       '("fourth\n" ("other.txt.bak<3>" "other.txt.bak<2>" "other.txt.bak")))
     (define restored (edit:restore! "other.txt.bak"))
     (check
       'restore-brings-a-backup-back-as-a-buffer
       (list (equal? restored (view:source (interaction:snapshot editing)))
         (let-values ([(lines revision) (store:snapshot restored)]) (vector->list lines))
         (store:property restored 'file #f) (map car (backups-of path3)))
       '(#f ("keep me") #f ("other.txt.bak<3>" "other.txt.bak<2>")))
     (for-each edit:kill-buffer! (list restored fourth third scratch))
     (show! b)
     (delete-file path3)
     (let* ([id (store:create! head:ui-actor "guarded save" '#("replacement"))]
            [held (string-append path3 ".held")]
            [replaced? #f]
            [token (store:subscribe!
                     #f
                     (lambda (event)
                       (when (and (eq? (car event) 'create)
                                  (let ([backup (store:property (cadr event) 'backup #f)])
                                    (and backup (equal? (car backup) path3))))
                         (rename-file path3 held)
                         (file:write! path3 '#("same bytes") #t)
                         (set! replaced? #t))))])
       (dynamic-wind
         (lambda () (file:write! path3 '#("same bytes") #t))
         (lambda ()
           (check
             'save-refuses-stale-mode-and-replaced-disk-witnesses
             (let* ([stale (document:save! head:ui-actor id path3 '("old first line" "saved-text"))]
                    [replaced (document:save! head:ui-actor id path3 '("replacement" "saved-text"))])
               (list (car stale) (car replaced) replaced? (file:read path3) (store:property id 'file #f)))
             '(refused refused #t "same bytes\n" #f)))
         (lambda ()
           (store:unsubscribe! token)
           (store:delete! head:ui-actor id)
           (for-each (lambda (p) (when (file-exists? p) (delete-file p))) (list path3 held)))))
     (goto! '(1 . 2))
     (define first-line (car (lines)))
     (write-disk! (string-append "NEW\n" (string:join (lines) "\n") "\n"))
     (check
       'the-cursor-crosses-a-reread-and-its-undo-on-its-line
       (let* ([after (begin (edit:reread! editing) (list (car (lines)) (point)))]
              [back (begin (edit:undo! editing) (list (car (lines)) (point)))])
         (list after back))
       (list '("NEW" (2 . 2)) (list first-line '(1 . 2))))
     (define before-reread (lines))
     (write-disk! "fresh\n")
     (edit:reread! editing)
     (check 'reread-adopts-the-disk (list (lines) (store:property b 'modified #f)) '(("fresh") #f))
     (edit:undo! editing)
     (check 'undo-brings-the-text-back-after-a-reread (lines) before-reread)
     (define path4 (string-append dir "/positions.txt"))
     (file:write! path4 (file:lines "one\ntwo\nthree\n") #t)
     (visit! path4)
     (define pb (view:source (interaction:snapshot editing)))
     (file:write! path4 (file:lines "zero\none\ntwo\nthree\n") #t)
     (store:set-properties! head:ui-actor pb '((stamp . #f)))
     (goto! '(1 . 2))
     (check
       'the-cursor-crosses-a-reload-on-its-line
       (begin (edit:insert! editing "!") (refresh!) (list (vector->list (lines-of pb)) (point)))
       '(("zero" "one" "tw!o" "three") (2 . 3)))
     (edit:kill-buffer! pb)
     (show! b)
     (delete-file path4)
     (define path5 (string-append dir "/abcd.txt"))
     (file:write! path5 (file:lines "ABCD\n") #t)
     (visit! path5)
     (define ab (view:source (interaction:snapshot editing)))
     (file:write! path5 (file:lines "A2BCD\n") #t)
     (store:set-properties! head:ui-actor ab '((stamp . #f)))
     (goto! '(0 . 1))
     (edit:insert! editing "3")
     (refresh!)
     (check
       'an-insertion-where-the-disk-inserted-conflicts-instead-of-landing-elsewhere
       (list
         (vector->list (lines-of ab))
         (positive? (store:property ab 'conflicts 0))
         (map (lambda (c) (list (cadddr c) (list-ref c 4) (list-ref c 5)))
              (store:conflicts (view:source (interaction:snapshot editing)))))
       '(("A2BCD") #t (((0 0 0 5) ("A3BCD") ("A2BCD")))))
     (check
       'picking-mine-writes-the-typed-side
       (begin (resolve-all! 'mine) (vector->list (lines-of ab)))
       '("A3BCD"))
     (parameterize ([kernel:registering-module 'reload-save-hook])
       (file:add-pre-save-hook!
         (lambda (target editor)
           (edit:undo! editing)
           (edit:move! editing 'finish)
           (edit:insert! editing "!"))))
     (dynamic-wind
       void
       (lambda ()
         (check
           'save-rechecks-conflicts-created-by-a-hook
           (list
             (guard (ex [(kernel:refusal? ex) 'refused]) (edit:save-file! editing path5))
             (file:read path5))
           '(refused "A2BCD\n")))
       (lambda () (kernel:retract-module! 'reload-save-hook)))
     (edit:kill-buffer! ab)
     (show! b)
     (delete-file path5)
     (write-disk! "old old\ntail\n")
     (edit:reread! editing)
     (write-disk! "old old\ntail!\n")
     (store:set-properties! head:ui-actor b '((stamp . #f)))
     (check
       'replacement-finishes-before-automatic-reload
       (list (search:replace! editing "old" "new") (lines))
       '(2 ("new new" "tail!")))
     (let* ([id (store:create!
                  head:ui-actor
                  "lagging conflict"
                  '("alpha tail")
                  '((base . "alpha tail") (trailing . #f)))]
            [trunk #f])
       (store:edit! head:ui-actor id 0 (text:make-span 0 0 0 5) '("mine"))
       (store:reload! head:ui-actor id '("disk tail") '((base . "disk tail") (trailing . #f)))
       (set! trunk id)
       (show! trunk)
       (store:edit! '(head "other") id (store:revision id) (text:make-span 0 0 0 0) '("foreign" ""))
       (let* ([draft (conflict-review:create! head:ui-actor (list id))]
              [r (model:snapshot draft)]
              [rev (cdr (assq 'revision r))])
         (conflict-review:choose! head:ui-actor draft rev (list (cons id (store:conflicts id))) 'mine)
         (check
           'preview-uses-current-text-even-when-the-head-lags
           (list-ref (conflict-review:preview draft id) 3)
           '#("foreign" "mine tail"))
         (conflict-review:close! head:ui-actor draft (model:revision draft)))
       (resolve-all! 'disk)
       (edit:kill-buffer! trunk)
       (show! b))
     (write-disk! "alpha tail\n")
     (edit:reread! editing)
     (edit:replace-region-text! editing '(0 . 0) '(0 . 5) "mine")
     (write-disk! "disk tail\n")
     (edit:reload! editing)
     (edit:replace-region-text! editing '(0 . 0) '(0 . 4) "custom")
     (write-disk! "newdisk tail\n")
     (visit! path)
     (check
       'reopening-preserves-manual-edits-and-older-conflict-alternatives
       (list
         (lines)
         (map (lambda (c) (list-ref c 4)) (store:conflicts (view:source (interaction:snapshot editing)))))
       '(("custom tail") (("mine"))))
     (edit:replace-region-text! editing '(0 . 0) '(0 . 6) "continued")
     (refresh!)
     (check
       'automatic-reload-preserves-continued-typing-in-a-conflict
       (list
         (lines)
         (file:read path)
         (map (lambda (c) (list-ref c 4)) (store:conflicts (view:source (interaction:snapshot editing)))))
       '(("continued tail") "newdisk tail\n" (("mine"))))
     (delete-file path)
     (for-each
       (lambda (how)
         (for-each
           (lambda (scenario)
             (let* ([target (string-append dir "/undo.txt")]
                    [disk (car scenario)]
                    [merged (cadr scenario)]
                    [pending? (caddr scenario)])
               (file:write! target '#("alpha beta") #t)
               (visit! target)
               (let ([b (view:source (interaction:snapshot editing))])
                 (dynamic-wind
                   void
                   (lambda ()
                     (edit:replace-region-text! editing '(0 . 0) '(0 . 5) "ALPHA")
                     (unless (eq? how 'automatic) (edit:replace-region-text! editing '(0 . 0) '(0 . 5) "Mine"))
                     (file:write! target (file:lines disk) #f)
                     (store:set-property! head:ui-actor b 'stamp #f)
                     (case how
                       [(manual) (edit:reload! editing)]
                       [(automatic) (edit:replace-region-text! editing '(0 . 0) '(0 . 5) "Mine")]
                       [(reopen) (visit! target)]
                       [(save) (guard (ex [(kernel:refusal? ex) #f]) (edit:save! editing))])
                     (let* ([after (list (edit:buffer-text b) (positive? (store:property b 'conflicts 0)))]
                            [back (begin
                                    (edit:undo! editing)
                                    (list (edit:buffer-text b) (positive? (store:property b 'conflicts 0))))]
                            [again (begin
                                     (edit:redo! editing)
                                     (list (edit:buffer-text b) (positive? (store:property b 'conflicts 0))))])
                       (edit:undo! editing)
                       (edit:save! editing)
                       (let* ([written (file:read target)]
                              [earlier (begin (edit:undo! editing) (edit:buffer-text b))]
                              [original (begin (edit:undo! editing) (edit:buffer-text b))]
                              [replayed (begin
                                          (edit:redo! editing)
                                          (edit:redo! editing)
                                          (edit:redo! editing)
                                          (list
                                            (edit:buffer-text b)
                                            (positive? (store:property b 'conflicts 0))))])
                         (check
                           (list how disk 'reload-is-one-undoable-action-with-older-history)
                           (list after back again written earlier original replayed)
                           (list (list merged pending?) '("Mine beta\n" #f) (list merged pending?) "Mine beta\n"
                             "ALPHA beta\n" "alpha beta\n" (list merged pending?))))))
                   (lambda ()
                     (store:delete! head:ui-actor b)
                     (widget:unmount! editing)
                     (delete-file target))))))
           '(("alpha BETA" "Mine BETA" #f) ("disk beta" "disk beta" #t) ("alpha beta" "Mine beta" #f))))
       '(manual automatic reopen save))
     (delete-directory dir)
     (test:finish! 'reload)))
