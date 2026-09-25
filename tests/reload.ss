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
             (prefix (foundation edoc) edoc:)
             (prefix (foundation string) string:)
             (prefix (core kernel) kernel:)
             (prefix (head dispatch) dispatch:)
             (prefix (head head) head:)
             (prefix (head keymap) keymap:)
             (prefix (head mode) mode:)
             (prefix (head paint) paint:)
             (prefix (service file) file:)
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

     ;; the preview flips the region to the entry's side in place; the thunk
     ;; puts the buffer back; flip! toggles the same view
     (check 'a-preview-flips-the-region-in-place
       (let* ([restore ((edoc:type-preview 'conflict) rev)]
              [during (list (head:buffer-name (shown)) (vector->list (head:buffer-lines (shown))))])
         (restore)
         (list during (eq? (shown) b)))
       (list (list "<flip: notes.txt>" '("ALPHA" "beta" "GAMMA tail")) #t))
     (check 'flip-shows-the-other-side-and-back
       (let* ([first (begin (delta-log:flip! rev) (head:buffer-name (shown)))]
              [second (begin (delta-log:flip! rev) (head:buffer-name (shown)))])
         (list first second))
       '("<flip: notes.txt>" "notes.txt"))

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
       (list "<conflicts>" #t 1 #t '((0 0 5 match))))
     ;; the conflict keys are the conflicts mode's, LEFT and RIGHT choosing a side
     (check 'the-conflicts-browsers-keys-are-its-modes
       (list (mode:key-contexts (head:current-buffer))
             (eq? (bound-to 'conflicts "LEFT") delta-log:keep-mine!) (eq? (bound-to 'conflicts "RIGHT") delta-log:keep-disk!)
             (eq? (bound-to 'conflicts " ") delta-log:flip-row!))
       '((conflicts) #t #t #t))
     (dispatch:key! "M-/")
     (check 'm-slash-flips-the-rows-region-in-place
       (let ([w (window-showing "<flip: notes.txt>")]) (and w (vector->list (head:buffer-lines (head:window-buffer w)))))
       '("ALPHA" "beta" "GAMMA tail"))
     (dispatch:key! "M-/")
     (check 'm-slash-again-puts-the-buffer-back (and (window-showing "notes.txt") #t) #t)
     (dispatch:key! "M-m")
     (head:before-frame!)
     (check 'm-m-writes-the-entrys-side-and-the-browser-says-nothing-pends
       (list (lines) (delta-log:conflicts) (head:buffer-modified b) (head:buffer-conflicted b)
             (assq 'conflict (caddr (car (delta-log:log))))
             (rows))
       (list '("ALPHA" "beta" "GAMMA tail") '() #t #f (cons 'conflict rev) '("No conflicts pending")))
     (dispatch:key! "ESC")

     ;; saving writes the buffer
     (save!)
     (check 'saving-writes-the-text (file:read path) "ALPHA\nbeta\nGAMMA tail\n")
     ;; an edit attempt on a file changed on disk reloads it first and refuses
     ;; that once, a refusal the dispatcher runs the key again after, as the
     ;; test does here; the merged text is then edited and saved
     (write-disk! "ALPHA\nBETA\nGAMMA tail\n")
     (head:goto! '(0 . 5))
     (check 'an-edit-after-a-disk-change-reloads-first-and-refuses-once
       (list (guard (ex [(kernel:reloaded? ex) 'reloaded]) (insert-text! "!")) (lines) (head:buffer-conflicted b))
       '(reloaded ("ALPHA" "BETA" "GAMMA tail") #f))
     (insert-text! "!")
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

     ;; two conflict rows: M-n moves to the second, the highlight following,
     ;; and M-d keeps the disk's side there; resolve-all settles the rest and
     ;; the browser returns to the log by itself
     (define pending (delta-log:conflicts))
     (delta-log:conflicts! 0)
     (head:before-frame!)
     (check 'two-conflicts-make-two-rows (list (length (rows)) (marked-ranges)) (list 2 (list (range-of (car pending)))))
     (dispatch:key! "DOWN")
     (head:before-frame!)
     (check 'down-moves-to-the-next-conflict-and-the-highlight-follows
       (list (head:point) (marked-ranges)) (list '(2 . 0) (list (range-of (cadr pending)))))
     (dispatch:key! "RIGHT")
     (head:before-frame!)
     (check 'right-keeps-the-disks-side-for-the-rows-conflict
       (list (head:with-buffer b (delta-log:conflicts)) (lines) (length (rows))) (list (list (car pending)) '("alpha!" "BETA" "gamma tail") 1))
     (check 'resolve-all-settles-every-conflict
       (list (head:with-buffer b (delta-log:resolve-all! 'disk)) (head:with-buffer b (delta-log:conflicts))) '(1 ()))
     (head:before-frame!)
     ;; both entries conflicted and were kept only by their conflicts, so the
     ;; log is empty, and the conflicts browser says nothing pends
     (check 'the-browser-says-nothing-pends-and-the-log-is-empty
       (list (rows) (head:with-buffer b (delta-log:log)) (head:buffer-conflicted b)) '(("No conflicts pending") () #f))
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
       (let* ([saved (save-file! path3)] [on-disk (file:read path3)] [entry (car (backups-of path3))])
         (list saved on-disk (head:buffer-file scratch) (car entry) (list-ref entry 4) (and (list-ref entry 3) #t)
               (and (find (lambda (t) (string=? (car t) "other.txt.bak")) (trash)) #t)))
       (list #t "new text\n" path3 "other.txt.bak" (file:checksum "keep me\n") #t #f))
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

     ;; reread adopts the disk as one undoable edit: undo brings the buffer back
     (define before-reread (lines))
     (write-disk! "fresh\n")
     (reread!)
     (check 'reread-adopts-the-disk (list (lines) (head:buffer-modified b)) '(("fresh") #f))
     (undo!)
     (check 'undo-brings-the-text-back-after-a-reread (lines) before-reread)
     (delete-file path)
     (delete-directory dir)
     (test:finish! 'reload)))
