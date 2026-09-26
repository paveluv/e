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
             (prefix (head head) head:)
             (prefix (head keymap) keymap:)
             (prefix (head mode) mode:)
             (prefix (head paint) paint:)
             (prefix (service file) file:)
             (prefix (service log) log:)
             (prefix (state store) store:)
             (prefix (foundation text) text:)
             (only (chezscheme) format get-process-id mkdir delete-file delete-directory))

     (define check test:check)
     (define (bound-to context key) (let ([hit (keymap:resolved-binding context (list key))]) (and hit (keymap:binding-action (cdr hit)))))
     (delta-log:init!)
     (define dir (format "/tmp/e-reload-~a" (get-process-id)))
     (mkdir dir)
     (define path (string-append dir "/notes.txt"))
     (define (write-disk! text) (file:write! path (file:lines text) (file:ends-in-newline? text)))
     (define (lines) (vector->list (head:buffer-lines b)))
     (define (shown) (head:window-buffer (head:current-window)))
     (define (contains? s part) (and (string:search s part 0 (string-length s)) #t))
     (write-disk! "alpha\nbeta\ngamma\n")
     (visit-file! path)
     (define b (head:current-buffer))
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
         (list (map car offered) (contains? (cdr (car offered)) "mine \"ALPHA\"")))
       (list (list rev) #t))

     ;; the preview shows the entry's side in place while a prompt has the
     ;; conflict; the thunk puts the picks back; flip! picks the same way
     (check 'a-preview-shows-the-entrys-side-in-place
       (let* ([restore ((edoc:type-preview 'conflict) rev)]
              [during (list (head:buffer-name (shown)) (vector->list (head:buffer-lines (shown))))])
         (restore)
         (list during (eq? (shown) b)))
       (list (list "<preview: notes.txt>" '("ALPHA" "beta" "GAMMA tail")) #t))
     (check 'flip-picks-mine-and-back-nothing-settled
       (let* ([first (begin (delta-log:flip! rev) (list (head:buffer-name (shown)) (delta-log:picks)))]
              [second (begin (delta-log:flip! rev) (list (head:buffer-name (shown)) (delta-log:picks)))])
         (list first second (length (delta-log:conflicts))))
       (list (list "<preview: notes.txt>" (list (cons rev 'mine))) (list "notes.txt" (list (cons rev 'disk))) 1))

     ;; the browser lists the conflicts as its rows, the current row's region
     ;; highlighted in the buffer's window; M-/ flips the row's region and
     ;; back, M-m writes the entry's side, and with the last conflict settled
     ;; the rows return to the log, the resolution its newest entry
     (define (ordinary-windows) (filter (lambda (w) (not (head:popup? w))) (head:windows)))
     (define (window-showing name) (find (lambda (w) (equal? (head:buffer-name (head:window-buffer w)) name)) (ordinary-windows)))
     (define (marked-ranges) (map cdr (filter (lambda (r) (eq? (car r) b)) (paint:highlight-ranges))))
     (define (rows) (cdr (vector->list (head:buffer-lines (head:current-buffer)))))
     (define (range-of c) (let ([r (cadddr c)]) (list (car r) (cadr r) (cadddr r) 'match)))
     (delta-log:conflicts! 0)
     (head:before-frame!)
     (check 'the-conflicts-browser-lists-the-conflicts-with-the-rows-region-highlighted
       (list (head:buffer-name (head:current-buffer)) (head:popup? (head:current-window)) (length (rows))
             (and (contains? (car (rows)) "notes.txt") (contains? (car (rows)) "\"ALPHA\"") (contains? (car (rows)) "\"omega\"")) (marked-ranges))
       (list "<conflicts>" #t 2 #t '((0 0 5 conflict-disk-current))))
     ;; the conflict keys are the conflicts mode's, LEFT and RIGHT picking a side
     (check 'the-conflicts-browsers-keys-are-its-modes
       (list (mode:key-contexts (head:current-buffer))
             (eq? (bound-to 'conflicts "LEFT") delta-log:pick-mine!) (eq? (bound-to 'conflicts "RIGHT") delta-log:pick-disk!)
             (eq? (bound-to 'conflicts " ") delta-log:choose!))
       '((conflicts) #t #t #t))
     ;; C-x C-s in the browser saves the row's buffer: refused while its
     ;; conflicts pend, the pop-up staying current
     (check 'save-row-is-refused-while-the-rows-conflicts-pend
       (list (let ([hit (keymap:resolved-binding 'conflicts '("C-x" "C-s"))]) (and hit (eq? (keymap:binding-action (cdr hit)) delta-log:save-row!)))
             (guard (ex [(kernel:refusal? ex) (contains? (kernel:condition-text ex) "Resolve the conflicts first")]) (delta-log:save-row!) 'saved)
             (eq? (head:current-window) (head:popup)))
       (list #t #t #t))
     (dispatch:key! "RET")
     (head:before-frame!)
     (check 'enter-picks-mine-and-the-preview-shows-it-in-place-highlighted-as-mine
       (let ([w (window-showing "<preview: notes.txt>")])
         (list (and w (vector->list (head:buffer-lines (head:window-buffer w))))
               (map cdr (filter (lambda (r) (and w (eq? (car r) (head:window-buffer w)))) (paint:highlight-ranges)))
               (head:with-buffer b (delta-log:picks)) (length (head:with-buffer b (delta-log:conflicts)))))
       (list '("ALPHA" "beta" "GAMMA tail") '((0 0 5 conflict-mine-current)) (list (cons rev 'mine)) 1))
     (dispatch:key! "M-/")
     (head:before-frame!)
     (check 'm-slash-again-picks-disk-and-the-buffer-is-back (and (window-showing "notes.txt") #t) #t)
     (dispatch:key! "M-m")
     (dispatch:key! "DOWN")
     (head:before-frame!)
     (check 'down-past-the-rows-lands-on-the-settle-row
       (list (head:point) (car (reverse (rows))) (head:buffer-status (head:current-buffer) (head:current-window)))
       '((2 . 0) "Settle all as picked: 1 mine, 0 disk" "settle all as picked"))
     (check 'inspection-and-preview-commands-never-settle-the-review
       (begin (delta-log:show-row!)
              (list (guard (ex [else 'refused]) (delta-log:flip-row!))
                    (length (head:with-buffer b (delta-log:conflicts))) (lines)))
       '(refused 1 ("omega" "beta" "GAMMA tail")))
     (dispatch:key! "RET")
     (head:before-frame!)
     (check 'ret-on-the-settle-row-writes-the-picks-and-the-browser-says-nothing-pends
       (list (lines) (delta-log:conflicts) (head:buffer-modified b) (head:buffer-conflicted b)
             (assq 'conflict (caddr (car (delta-log:log))))
             (rows) (and (window-showing "notes.txt") #t))
       (list '("ALPHA" "beta" "GAMMA tail") '() #t #f (cons 'conflict rev) '("No conflicts pending") #t))
     ;; saving from the browser writes the row's buffer, the pop-up kept current
     (delta-log:save-row!)
     (check 'saving-writes-the-text (list (file:read path) (eq? (head:current-window) (head:popup))) (list "ALPHA\nbeta\nGAMMA tail\n" #t))
     (dispatch:key! "ESC")

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

     ;; two conflict rows in the order of their regions, every region
     ;; highlighted in its side's face, the current one brighter: DOWN moves
     ;; to the second, RIGHT picks disk there, settling nothing, and settling
     ;; as picked keeps both disk sides; the browser returns to the log by itself
     (define pending (delta-log:conflicts))
     (define sorted (list-sort (lambda (x y) (< (car (cadddr x)) (car (cadddr y)))) pending))
     (define (range-of-side c face) (let ([r (cadddr c)]) (list (car r) (cadr r) (cadddr r) face)))
     (delta-log:conflicts! 0)
     (head:before-frame!)
     (check 'two-conflicts-make-two-rows-in-the-order-of-their-regions
       (list (length (rows)) (map car (head:with-buffer b (delta-log:picks))) (marked-ranges))
       (list 3 (map car sorted) (list (range-of-side (car sorted) 'conflict-disk-current) (range-of-side (cadr sorted) 'conflict-disk))))
     (dispatch:key! "DOWN")
     (head:before-frame!)
     (check 'down-moves-to-the-next-conflict-and-the-highlight-follows
       (list (head:point) (marked-ranges))
       (list '(2 . 0) (list (range-of-side (car sorted) 'conflict-disk) (range-of-side (cadr sorted) 'conflict-disk-current))))
     (dispatch:key! "RIGHT")
     (head:before-frame!)
     (check 'right-picks-the-disks-side-settling-nothing
       (list (length (head:with-buffer b (delta-log:conflicts))) (lines) (length (rows)) (map cdr (head:with-buffer b (delta-log:picks))))
       (list 2 '("alpha!" "BETA" "gamma tail") 3 '(disk disk)))
     (check 'settling-as-picked-keeps-both-disk-sides
       (list (head:with-buffer b (delta-log:resolve-all!)) (head:with-buffer b (delta-log:conflicts))) '(2 ()))
     (head:before-frame!)
     (check 'settled-conflicts-leave-the-browser-but-keep-their-history
       (list (rows) (pair? (head:with-buffer b (delta-log:log))) (head:buffer-conflicted b)) '(("No conflicts pending") #t #f))
     (dispatch:key! "ESC")
     (check 'esc-closes-the-browser-and-hides-the-pop-up (list (head:buffer-named "<conflicts>") (head:popup-rows)) '(#f 0))

     ;; a replacement typed as a backspace and a character is one batch, and
     ;; the reload conflicts it whole: the disk's side stands, both sides are
     ;; listed, and keeping mine writes the typed side
     (edit-init!)
     (define path2 (string-append dir "/typed.txt"))
     (file:write! path2 (file:lines "abcdefgh\n") #t)
     (visit-file! path2)
     (define t (head:current-buffer))
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
     (head:show-buffer! b)

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
     (file:write! path3 (file:lines "keep me\n") #t)
     (define scratch (head:new-buffer! "scratch-save"))
     (head:show-buffer! scratch)
     (head:goto! '(0 . 0))
     (insert-text! "new text")
     (define (backups-of path) (filter (lambda (entry) (string=? (cadr entry) path)) (backups)))
     (check 'a-save-as-over-a-file-backs-up-what-it-held
       (let* ([logged (length (log:entries 'save-file!))]
              [saved (save-file! path3)] [on-disk (file:read path3)] [entry (car (backups-of path3))]
              [messages (log:entries 'save-file!)])
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
         (head:show-buffer! b)
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
       (let* ([restored (restore! "other.txt.bak")])
         (list (eq? restored (head:current-buffer)) (vector->list (head:buffer-lines restored)) (head:buffer-file restored)
               (map car (backups-of path3))))
       '(#t ("keep me") #f ("other.txt.bak<3>" "other.txt.bak<2>")))
     (for-each kill-buffer! (list (head:current-buffer) fourth third scratch))
     (head:show-buffer! b)
     (delete-file path3)

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
     (define pb (head:current-buffer))
     (file:write! path4 (file:lines "zero\none\ntwo\nthree\n") #t)
     ;; the write may share the visit's clock tick: the mtime hint is dropped
     ;; so the edit's disk check reads the content
     (head:buffer-facts-set! pb '((stamp . #f)))
     (head:goto! '(1 . 2))
     (check 'the-cursor-crosses-a-reload-on-its-line
       (begin (insert-text! "!") (head:before-frame!)
              (list (vector->list (head:buffer-lines pb)) (head:point)))
       '(("zero" "one" "tw!o" "three") (2 . 3)))
     (kill-buffer! pb)
     (head:show-buffer! b)
     (delete-file path4)

     ;; an edit made against the text as it stood before an external change
     ;; is an edit like any other: it applies, the file reloads and merges,
     ;; and where the two met, an insertion each at one place, they conflict
     (define path5 (string-append dir "/abcd.txt"))
     (file:write! path5 (file:lines "ABCD\n") #t)
     (visit-file! path5)
     (define ab (head:current-buffer))
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
       (begin (delta-log:resolve-all! 'mine) (vector->list (head:buffer-lines ab))) '("A3BCD"))
     (parameterize ([kernel:registering-module 'reload-save-hook])
       (file:add-pre-save-hook! (lambda (target) (undo!) (end-of-buffer!) (insert-text! "!"))))
     (dynamic-wind void
       (lambda ()
         (check 'save-rechecks-conflicts-created-by-a-hook
           (list (guard (ex [(kernel:refusal? ex) 'refused]) (save-file! path5)) (file:read path5))
           '(refused "A2BCD\n")))
       (lambda () (kernel:retract-module! 'reload-save-hook)))
     (kill-buffer! ab)
     (head:show-buffer! b)
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
       (head:show-buffer! trunk)
       (store:edit! '(head "other") id (store:revision id) (text:make-span 0 0 0 0) '("foreign" ""))
       (delta-log:pick! (caar (store:conflicts id)) 'mine)
       (check 'preview-uses-current-text-even-when-the-head-lags
         (head:buffer-lines (shown)) '#("foreign" "mine tail"))
       (delta-log:resolve-all! 'disk)
       (kill-buffer! trunk)
       (head:show-buffer! b))

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
               (let ([b (head:current-buffer)])
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
                     (let* ([after (list (buffer-text b) (head:buffer-conflicted b))]
                            [back (begin (undo!) (list (buffer-text b) (head:buffer-conflicted b)))]
                            [again (begin (redo!) (list (buffer-text b) (head:buffer-conflicted b)))])
                       (undo!) (save!)
                       (let* ([written (file:read target)]
                              [earlier (begin (undo!) (buffer-text b))]
                              [original (begin (undo!) (buffer-text b))]
                              [replayed (begin (redo!) (redo!) (redo!) (list (buffer-text b) (head:buffer-conflicted b)))])
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
