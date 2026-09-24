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
             (head literal)
             (prefix (apps delta-log) delta-log:)
             (prefix (foundation edoc) edoc:)
             (prefix (foundation string) string:)
             (prefix (head dispatch) dispatch:)
             (prefix (head head) head:)
             (prefix (head mode) mode:)
             (prefix (head paint) paint:)
             (prefix (service file) file:)
             (only (chezscheme) format get-process-id mkdir delete-file delete-directory))

     (define check test:check)
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
     (define (marked-ranges) (map (lambda (r) (if (eq? (car r) b) (cdr r) r)) (paint:highlight-ranges)))
     (define (rows) (vector->list (head:buffer-lines (head:current-buffer))))
     (define (range-of c) (let ([r (cadddr c)]) (list (car r) (cadr r) (cadddr r) 'match)))
     (delta-log:conflicts!)
     (head:before-frame!)
     (check 'the-browser-lists-the-conflicts-with-the-rows-region-highlighted
       (list (head:buffer-name (head:current-buffer)) (length (rows))
             (contains? (car (rows)) "mine \"ALPHA\" · disk \"omega\"") (marked-ranges))
       (list "<delta-log>" 1 #t '((0 0 5 match))))
     ;; the conflict keys live in the browser's conflicts context, so the
     ;; keys listing shows them only over conflict rows
     (check 'the-conflict-rows-bring-the-conflicts-context (mode:key-contexts (head:current-buffer)) '(delta-log-conflicts delta-log))
     (dispatch:key! "M-/")
     (check 'm-slash-flips-the-rows-region-in-place
       (let ([w (window-showing "<flip: notes.txt>")]) (and w (vector->list (head:buffer-lines (head:window-buffer w)))))
       '("ALPHA" "beta" "GAMMA tail"))
     (dispatch:key! "M-/")
     (check 'm-slash-again-puts-the-buffer-back (and (window-showing "notes.txt") #t) #t)
     (dispatch:key! "M-m")
     (check 'm-m-writes-the-entrys-side-and-the-rows-return-to-the-log
       (list (lines) (delta-log:conflicts) (head:buffer-modified b)
             (assq 'conflict (caddr (car (delta-log:log))))
             (length (rows)) (contains? (car (rows)) "+\"ALPHA\""))
       (list '("ALPHA" "beta" "GAMMA tail") '() #t (cons 'conflict rev) (length (delta-log:log)) #t))
     (dispatch:key! "ESC")

     ;; saving writes the buffer; a stale save reloads first and writes when
     ;; nothing conflicts, else stops with the conflicts pending
     (save!)
     (check 'saving-writes-the-text (file:read path) "ALPHA\nbeta\nGAMMA tail\n")
     (write-disk! "ALPHA\nBETA\nGAMMA tail\n")
     (head:goto! '(0 . 5))
     (insert-text! "!")
     (save!)
     (check 'a-stale-save-reloads-then-writes (list (lines) (file:read path)) (list '("ALPHA!" "BETA" "GAMMA tail") "ALPHA!\nBETA\nGAMMA tail\n"))
     (write-disk! "alpha!\nBETA\ngamma tail\n")
     (replace-region-text! '(0 . 0) '(0 . 6) "OMEGA!")
     (replace-region-text! '(2 . 0) '(2 . 5) "Gamma")
     (check 'a-conflicting-stale-save-stops-with-the-conflicts-pending
       (list (save!) (lines) (length (delta-log:conflicts)) (file:read path))
       (list #f '("alpha!" "BETA" "gamma tail") 2 "alpha!\nBETA\ngamma tail\n"))

     ;; two conflict rows: M-n moves to the second, the highlight following,
     ;; and M-d keeps the disk's side there; resolve-all settles the rest and
     ;; the browser returns to the log by itself
     (define pending (delta-log:conflicts))
     (delta-log:conflicts!)
     (head:before-frame!)
     (check 'two-conflicts-make-two-rows (list (length (rows)) (marked-ranges)) (list 2 (list (range-of (car pending)))))
     (dispatch:key! "M-n")
     (head:before-frame!)
     (check 'm-n-moves-to-the-next-conflict-and-the-highlight-follows
       (list (head:point) (marked-ranges)) (list '(1 . 0) (list (range-of (cadr pending)))))
     (dispatch:key! "M-d")
     (check 'm-d-keeps-the-disks-side-for-the-rows-conflict
       (list (delta-log:conflicts) (lines) (length (rows))) (list (list (car pending)) '("alpha!" "BETA" "gamma tail") 1))
     (check 'resolve-all-settles-every-conflict (list (delta-log:resolve-all! 'disk) (delta-log:conflicts)) '(1 ()))
     ;; both entries conflicted and were kept only by their conflicts, so the
     ;; log they return to is empty
     (check 'the-browser-returns-to-the-log-with-nothing-pending (list (rows) (delta-log:log)) '(("no entries") ()))
     (check 'conflicts-without-any-pending-shows-the-log (list (delta-log:conflicts!) (rows)) '(0 ("no entries")))
     (dispatch:key! "ESC")
     (check 'esc-closes-the-browser (head:buffer-named "<delta-log>") #f)

     ;; reread adopts the disk verbatim
     (write-disk! "fresh\n")
     (reread!)
     (check 'reread-adopts-the-disk (list (lines) (head:buffer-modified b)) '(("fresh") #f))
     (delete-file path)
     (delete-directory dir)
     (test:finish! 'reload)))
