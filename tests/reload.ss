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

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (rename (only (head edit) init!) (init! edit-init!))
             (head literal)
             (prefix (apps delta-log) delta-log:)
             (prefix (apps search) search:)
             (prefix (foundation edoc) edoc:)
             (prefix (foundation string) string:)
             (prefix (core kernel) kernel:)
             (prefix (head dispatch) dispatch:)
             (prefix (head head) head:) (prefix (head window) window:) (prefix (head widget) widget:)
             (prefix (only (head edit) init!) edit:)
             (prefix (head keymap) keymap:)
             (prefix (head mode) mode:)
             (prefix (head paint) paint:)
             (prefix (service conflict-review) conflict-review:)
             (prefix (state model) model:)
             (prefix (service document) document:)
             (prefix (service file) file:)
             (prefix (service log) log:)
             (prefix (state store) store:)
             (prefix (foundation text) text:)
             (only (chezscheme) format get-process-id mkdir delete-file delete-directory))

     (define check test:check)
     (widget:init!) (edit:init!) (window:init!)
     (define (bound-to context key) (let ([hit (keymap:resolved-binding context (list key))]) (and hit (keymap:binding-action (cdr hit)))))
     (delta-log:init!)
     (define dir (format "/tmp/e-reload-~a" (get-process-id)))
     (mkdir dir)
     (define path (string-append dir "/notes.txt"))
     (define (write-disk! text) (file:write! path (file:lines text) (file:ends-in-newline? text)))
     (define (lines) (vector->list (head:buffer-lines b)))
     (define (resolve-all! . side)
       (let* ([id (head:buffer-store-id (head:current-buffer-mirror))] [draft (conflict-review:create! head:ui-actor (list id))]
              [get (lambda (r k) (cdr (assq k r)))] [r (model:snapshot draft)]
              [groups (list (cons id (store:conflicts id)))])
         (conflict-review:choose! head:ui-actor draft (get r 'revision) groups (if (null? side) 'disk (car side)))
         (conflict-review:settle! head:ui-actor draft (model:revision draft) (list id))
         (conflict-review:close! head:ui-actor draft (model:revision draft))
         (head:before-frame!)
         (length (cdar groups))))
     (define (shown) (head:window-buffer (head:current-window)))
     (define (contains? s part) (and (string:search s part 0 (string-length s)) #t))
     (write-disk! "alpha\nbeta\ngamma\n")
     (visit-file! path)
     (define b (head:current-buffer-mirror))
     (check 'the-file-is-visited (lines) '("alpha" "beta" "gamma"))

     ;; the buffer changes line 0 and the end of line 2; the disk changes
     ;; lines 0 and 2 meanwhile, and reopening reloads
     (replace-region-text! '(0 . 0) '(0 . 5) "ALPHA")
     (head:goto! '(2 . 5))
     (insert-text! " tail")
     (write-disk! "omega\nbeta\nGAMMA\n")
     (visit-file! path)
     (check 'reopening-reloads-with-the-disks-side-where-they-collide (lines) '("omega" "beta" "GAMMA tail"))
     (define conflicts (delta-log:conflicts))
     (check 'the-conflict-lists-its-region-and-both-sides
       (list (length conflicts) (cadddr (car conflicts)) (list-ref (car conflicts) 4) (list-ref (car conflicts) 5))
       (list 1 '(0 0 0 5) '("ALPHA") '("omega")))
     (define rev (car (car conflicts)))
     (check 'a-conflict-completes-with-both-sides-in-its-hint
       (let ([offered (edoc:type-completions 'conflict "")])
         (list (map car offered) (contains? (caddr (car offered)) "mine \"ALPHA\"")))
       (list (list rev) #t))

     (check 'settlement-writes-mine-with-an-undoable-conflict-label
       (list (delta-log:resolve! rev 'mine) (lines) (delta-log:conflicts)
         (assq 'conflict (caddr (car (delta-log:log)))))
       (list 'applied '("ALPHA" "beta" "GAMMA tail") '() (cons 'conflict rev)))
     (save!)
     (check 'saving-writes-the-resolved-text (file:read path) "ALPHA\nbeta\nGAMMA tail\n")

     ;; an edit attempt on a file changed on disk reloads it first and refuses
     ;; that once, a refusal the dispatcher runs the key again after, as the
     ;; test does here; the merged text is then edited and saved
     (write-disk! "ALPHA\nBETA\nGAMMA tail\n")
     (head:goto! '(0 . 5))
     (check 'an-edit-after-a-disk-change-applies-then-the-buffer-reloads-and-merges-at-once
       (list (begin (insert-text! "!") (lines)) (head:buffer-conflicted b))
       '(("ALPHA!" "BETA" "GAMMA tail") #f))
     (save!)
     (check 'saving-after-the-reload-writes (list (lines) (file:read path)) (list '("ALPHA!" "BETA" "GAMMA tail") "ALPHA!\nBETA\nGAMMA tail\n"))
     ;; edits made before the disk changed under them: the save reloads, both
     ;; collide, and the save refuses while the conflicts pend, the red !! on
     (replace-region-text! '(0 . 0) '(0 . 6) "OMEGA!")
     (replace-region-text! '(2 . 0) '(2 . 5) "Gamma")
     (write-disk! "alpha!\nBETA\ngamma tail\n")
     (check 'a-save-over-a-changed-disk-reloads-and-refuses-while-conflicts-pend
       (list (guard (ex [(kernel:refusal? ex) (condition-message ex)]) (save!))
             (lines) (length (delta-log:conflicts)) (head:buffer-conflicted b) (file:read path))
       (list "Resolve the conflicts first" '("alpha!" "BETA" "gamma tail") 2 #t "alpha!\nBETA\ngamma tail\n"))

     (check 'bulk-disk-resolution-keeps-text-and-history
       (list (resolve-all!) (delta-log:conflicts) (lines) (pair? (delta-log:log)))
       '(2 () ("alpha!" "BETA" "gamma tail") #t))

     ;; a replacement typed as a backspace and a character is one batch, and
     ;; the reload conflicts it whole: the disk's side stands, both sides are
     ;; listed, and keeping mine writes the typed side
     (define path2 (string-append dir "/typed.txt"))
     (file:write! path2 (file:lines "abcdefgh\n") #t)
     (visit-file! path2)
     (define t (head:current-buffer-mirror))
     (head:goto! '(0 . 4))
     (dispatch:key! "BACKSPACE")
     (dispatch:key! #\8)
     (file:write! path2 (file:lines "abcDefgh\n") #t)
     (visit-file! path2)
     (define typed (delta-log:conflicts))
     (check 'a-typed-replacement-conflicts-whole-with-the-disks-side-standing
       (list (vector->list (head:buffer-lines t)) (map (lambda (c) (list (cadddr c) (list-ref c 4) (list-ref c 5))) typed))
       '(("abcDefgh") (((0 0 0 8) ("abc8efgh") ("abcDefgh")))))
     (check 'keeping-mine-writes-the-typed-replacement
       (list (delta-log:resolve! (car (car typed)) 'mine) (vector->list (head:buffer-lines t))) '(applied ("abc8efgh")))
     (delete-file path2)
     (head:show-buffer-mirror! b)

     ;; the conflicts settled, the buffer's !! is gone and it is savable again
     (head:goto! '(0 . 0))
     (insert-text! "S")
     (check 'settled-conflicts-make-the-buffer-savable-again
       (list (head:buffer-conflicted b) (save!) (file:read path)) (list #f #t (string-append (string:join (lines) "\n") "\n")))

     ;; Saving onto an existing file asks nothing: what the file held is read
     ;; first into a backup, a trashed buffer named after it with .bak that
     ;; backups lists with the path, the stamp and a checksum, and restore!
     ;; brings back; a plain save keeps the previous version too, and a
     ;; version the backups already hold is not kept twice
     (define path3 (string-append dir "/other.txt"))
     (mode:register! "saved-text" '(".txt") '() #f)
     (file:write! path3 (file:lines "keep me\n") #t)
     (define scratch (head:new-buffer! "scratch-save"))
     (head:show-buffer-mirror! scratch)
     (head:goto! '(0 . 0))
     (insert-text! "new text")
     (define (backups-of path) (filter (lambda (entry) (string=? (cadr entry) path)) (backups)))
     (check 'a-save-as-over-a-file-backs-up-what-it-held
       (let* ([logged (length (log:entries 'document:save-document!))]
              [saved (save-file! path3)] [on-disk (file:read path3)] [entry (car (backups-of path3))]
              [messages (log:entries 'document:save-document!)])
         (list saved on-disk (head:buffer-file scratch) (car entry) (list-ref entry 4) (and (list-ref entry 3) #t)
               (and (find (lambda (t) (string=? (car t) "other.txt.bak")) (trash)) #t)
               (- (length messages) logged) (log:format-entry (car messages))))
       (list #t "new text\n" path3 "other.txt.bak" (file:checksum "keep me\n") #t #f
             1 (format "Wrote ~a; what it held is kept as other.txt.bak" path3)))
     (insert-text! " again")
     (check 'a-save-backs-up-the-version-it-writes-over
       (let* ([saved (save-file! path3)] [names (map car (backups-of path3))])
         (list saved names))
       '(#t ("other.txt.bak<2>" "other.txt.bak")))
     (define (save-as-from! name text)
       (let ([b (head:new-buffer! name)])
         (head:show-buffer-mirror! b)
         (head:goto! '(0 . 0))
         (insert-text! text)
         (save-file! path3)
         b))
     (define third (save-as-from! "scratch-third" "keep me"))
     (define fourth (save-as-from! "scratch-fourth" "fourth"))
     (check 'a-version-the-backups-hold-is-not-kept-twice
       (list (file:read path3) (map car (backups-of path3)))
       '("fourth\n" ("other.txt.bak<3>" "other.txt.bak<2>" "other.txt.bak")))
     (check 'restore-brings-a-backup-back-as-a-buffer
       (let* ([restored (head:buffer-of-store-id (restore! "other.txt.bak"))])
         (list (eq? restored (head:current-buffer-mirror)) (vector->list (head:buffer-lines restored)) (head:buffer-file restored)
               (mode:name-of (head:buffer-store-id restored))
               (map car (backups-of path3))))
       '(#t ("keep me") #f "saved-text" ("other.txt.bak<3>" "other.txt.bak<2>")))
     (for-each kill-buffer! (map head:buffer-store-id (list (head:current-buffer-mirror) fourth third scratch)))
     (head:show-buffer-mirror! b)
     (delete-file path3)

     ;; Backup creation is an observable store commit. A subscriber can
     ;; replace the target even with equal bytes; its new identity must win.
     (let* ([id (store:create! head:ui-actor "guarded save" '#("replacement"))]
            [held (string-append path3 ".held")] [replaced? #f]
            [token (store:subscribe! #f
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
           (check 'save-refuses-stale-mode-and-replaced-disk-witnesses
             (let* ([stale (document:save! head:ui-actor id path3 '("old first line" "saved-text"))]
                    [replaced (document:save! head:ui-actor id path3 '("replacement" "saved-text"))])
               (list (car stale) (car replaced) replaced? (file:read path3) (store:property id 'file #f)))
             '(refused refused #t "same bytes\n" #f)))
         (lambda ()
           (store:unsubscribe! token) (store:delete! head:ui-actor id)
           (for-each (lambda (p) (when (file-exists? p) (delete-file p))) (list path3 held)))))

     ;; the cursor crosses a reread on its line, and its undo back: the disk
     ;; adds a line at the top, the cursor moves down a line and returns
     (head:goto! '(1 . 2))
     (define first-line (car (lines)))
     (write-disk! (string-append "NEW\n" (string:join (lines) "\n") "\n"))
     (check 'the-cursor-crosses-a-reread-and-its-undo-on-its-line
       (let* ([after (begin (reread!) (list (car (lines)) (head:point)))]
              [back (begin (undo!) (list (car (lines)) (head:point)))])
         (list after back))
       (list '("NEW" (2 . 2)) (list first-line '(1 . 2))))

     ;; reread adopts the disk as one undoable edit: undo brings the buffer back
     (define before-reread (lines))
     (write-disk! "fresh\n")
     (reread!)
     (check 'reread-adopts-the-disk (list (lines) (head:buffer-modified b)) '(("fresh") #f))
     (undo!)
     (check 'undo-brings-the-text-back-after-a-reread (lines) before-reread)
     ;; a head's positions cross a reload on their text: the disk inserts a
     ;; line above the cursor, and the cursor follows its line down
     (define path4 (string-append dir "/positions.txt"))
     (file:write! path4 (file:lines "one\ntwo\nthree\n") #t)
     (visit-file! path4)
     (define pb (head:current-buffer-mirror))
     (file:write! path4 (file:lines "zero\none\ntwo\nthree\n") #t)
     ;; the write may share the visit's clock tick: the mtime hint is dropped
     ;; so the edit's disk check reads the content
     (head:buffer-facts-set! pb '((stamp . #f)))
     (head:goto! '(1 . 2))
     (check 'the-cursor-crosses-a-reload-on-its-line
       (begin (insert-text! "!") (head:before-frame!)
              (list (vector->list (head:buffer-lines pb)) (head:point)))
       '(("zero" "one" "tw!o" "three") (2 . 3)))
     (kill-buffer! (head:buffer-store-id pb))
     (head:show-buffer-mirror! b)
     (delete-file path4)

     ;; an edit made against the text as it stood before an external change
     ;; is an edit like any other: it applies, the file reloads and merges,
     ;; and where the two met, an insertion each at one place, they conflict
     (define path5 (string-append dir "/abcd.txt"))
     (file:write! path5 (file:lines "ABCD\n") #t)
     (visit-file! path5)
     (define ab (head:current-buffer-mirror))
     (file:write! path5 (file:lines "A2BCD\n") #t)
     (head:buffer-facts-set! ab '((stamp . #f)))
     (head:goto! '(0 . 1))
     (insert-text! "3")
     (head:before-frame!)
     (check 'an-insertion-where-the-disk-inserted-conflicts-instead-of-landing-elsewhere
       (list (vector->list (head:buffer-lines ab)) (head:buffer-conflicted ab)
             (map (lambda (c) (list (cadddr c) (list-ref c 4) (list-ref c 5))) (delta-log:conflicts)))
       '(("A2BCD") #t (((0 0 0 5) ("A3BCD") ("A2BCD")))))
     (check 'picking-mine-writes-the-typed-side
       (begin (resolve-all! 'mine) (vector->list (head:buffer-lines ab))) '("A3BCD"))
     (parameterize ([kernel:registering-module 'reload-save-hook])
       (file:add-pre-save-hook! (lambda (target) (undo!) (end-of-buffer!) (insert-text! "!"))))
     (dynamic-wind void
       (lambda ()
         (check 'save-rechecks-conflicts-created-by-a-hook
           (list (guard (ex [(kernel:refusal? ex) 'refused]) (save-file! path5)) (file:read path5))
           '(refused "A2BCD\n")))
       (lambda () (kernel:retract-module! 'reload-save-hook)))
     (kill-buffer! (head:buffer-store-id ab))
     (head:show-buffer-mirror! b)
     (delete-file path5)

     (write-disk! "old old\ntail\n")
     (reread!)
     (write-disk! "old old\ntail!\n")
     (head:buffer-facts-set! b '((stamp . #f)))
     (check 'replacement-finishes-before-automatic-reload
       (list (search:replace! "old" "new") (lines)) '(2 ("new new" "tail!")))

     ;; The authority can move a conflict before this head consumes its
     ;; notice. Preview from one authoritative text/region snapshot.
     (let* ([id (store:create! head:ui-actor "lagging conflict" '("alpha tail")
                               '((base . "alpha tail") (trailing . #f)))]
            [trunk #f])
       (store:edit! head:ui-actor id 0 (text:make-span 0 0 0 5) '("mine"))
       (store:reload! head:ui-actor id '("disk tail") '((base . "disk tail") (trailing . #f)))
       (set! trunk (head:adopt-store-buffer! id))
       (head:show-buffer-mirror! trunk)
       (store:edit! '(head "other") id (store:revision id) (text:make-span 0 0 0 0) '("foreign" ""))
       (let* ([draft (conflict-review:create! head:ui-actor (list id))]
              [r (model:snapshot draft)] [rev (cdr (assq 'revision r))])
         (conflict-review:choose! head:ui-actor draft rev (list (cons id (store:conflicts id))) 'mine)
         (check 'preview-uses-current-text-even-when-the-head-lags
           (list-ref (conflict-review:preview draft id) 3) '#("foreign" "mine tail"))
         (conflict-review:close! head:ui-actor draft (model:revision draft)))
       (resolve-all! 'disk)
       (kill-buffer! (head:buffer-store-id trunk))
       (head:show-buffer-mirror! b))

     (write-disk! "alpha tail\n")
     (reread!)
     (replace-region-text! '(0 . 0) '(0 . 5) "mine")
     (write-disk! "disk tail\n")
     (reload!)
     (replace-region-text! '(0 . 0) '(0 . 4) "custom")
     (write-disk! "newdisk tail\n")
     (visit-file! path)
     (check 'reopening-preserves-manual-edits-and-older-conflict-alternatives
       (list (lines) (map (lambda (c) (list-ref c 4)) (delta-log:conflicts)))
       '(("custom tail") (("mine"))))
     (replace-region-text! '(0 . 0) '(0 . 6) "continued")
     (head:before-frame!)
     (check 'automatic-reload-preserves-continued-typing-in-a-conflict
       (list (lines) (file:read path) (map (lambda (c) (list-ref c 4)) (delta-log:conflicts)))
       '(("continued tail") "newdisk tail\n" (("mine"))))

     (delete-file path)
     ;; Every file reload path adds one action without replacing earlier
     ;; typing. Its inverse keeps the observed baseline, so save writes Mine
     ;; instead of pulling the same disk changes back in.
     (for-each
       (lambda (how)
         (for-each
           (lambda (scenario)
             (let* ([target (string-append dir "/undo.txt")]
                    [disk (car scenario)] [merged (cadr scenario)] [pending? (caddr scenario)])
               (file:write! target '#("alpha beta") #t)
               (visit-file! target)
               (let ([b (head:current-buffer-mirror)])
                 (dynamic-wind void
                   (lambda ()
                     (replace-region-text! '(0 . 0) '(0 . 5) "ALPHA")
                     (unless (eq? how 'automatic) (replace-region-text! '(0 . 0) '(0 . 5) "Mine"))
                     (file:write! target (file:lines disk) #f)
                     ;; Both writes can share a filesystem clock tick.
                     (head:buffer-fact-set! b 'stamp #f)
                     (case how
                       [(manual) (reload!)]
                       [(automatic) (replace-region-text! '(0 . 0) '(0 . 5) "Mine")]
                       [(reopen) (visit-file! target)]
                       [(save) (guard (ex [(kernel:refusal? ex) #f]) (save!))])
                     (let* ([after (list (buffer-text (head:buffer-store-id b)) (head:buffer-conflicted b))]
                            [back (begin (undo!) (list (buffer-text (head:buffer-store-id b)) (head:buffer-conflicted b)))]
                            [again (begin (redo!) (list (buffer-text (head:buffer-store-id b)) (head:buffer-conflicted b)))])
                       (undo!) (save!)
                       (let* ([written (file:read target)]
                              [earlier (begin (undo!) (buffer-text (head:buffer-store-id b)))]
                              [original (begin (undo!) (buffer-text (head:buffer-store-id b)))]
                              [replayed (begin (redo!) (redo!) (redo!) (list (buffer-text (head:buffer-store-id b)) (head:buffer-conflicted b)))])
                         (check (list how disk 'reload-is-one-undoable-action-with-older-history)
                           (list after back again written earlier original replayed)
                           (list (list merged pending?) '("Mine beta\n" #f)
                             (list merged pending?) "Mine beta\n" "ALPHA beta\n" "alpha beta\n" (list merged pending?))))))
                   (lambda ()
                     (store:delete! head:ui-actor (head:buffer-store-id b))
                     (head:forget-buffer! b) (delete-file target))))))
           '(("alpha BETA" "Mine BETA" #f) ("disk beta" "disk beta" #t)
             ("alpha beta" "Mine beta" #f))))
       '(manual automatic reopen save))
     (delete-directory dir)
     (test:finish! 'reload)))
