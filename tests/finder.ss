#!/usr/bin/env scheme-script

;; The <finder> app as a table over a real directory tree: recursive name
;; filters and literal path filters, collapsed and expanded match groups,
;; navigation that preserves each directory's choice, hidden and linked
;; entries, sorting, opening, and identity through kill and reload. Headless:
;; the scan worker publishes to the view, which is refreshed while waiting.
;; Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (head edit) init!) (prefix (head head) head:) (prefix (core kernel) kernel:) (prefix (head keymap) keymap:) (prefix (head dispatch) dispatch:)
             (prefix (foundation string) string:) (prefix (head window) window:) (prefix (test) test:))

     (define check test:check)
     ;; Load through the kernel so the reload below replaces the real module.
     ;; Fresh procedures resolve through the top level after that reload.
     (kernel:load-module! "finder")
     (define (open! . directory)
       (if (null? directory)
           ((top-level-value 'finder:open!))
           ((top-level-value 'finder:open-directory!) (car directory))))
     (define (show-hidden) ((top-level-value 'finder:show-hidden)))

     (define root (format "/tmp/e-files-~a-~a" (get-process-id) (random 1000000)))
     (define (path name) (string-append root "/" name))
     (define names '("apple.txt" "APPLE.txt" "zeta.txt" "a 日本語 long (name).txt" "odd\nname.txt" ".dot"
                     "small/needle-one.txt" "small/nested/needle-only.txt"
                     "large/needle-a.txt" "large/needle-b.txt" "large/needle-c.txt"))
     (define directories '("empty" "large" "small" "small/nested" "small/.日本語\n" "small/.日本語\n/leaf"))
     (mkdir root)
     (for-each (lambda (name) (mkdir (path name))) directories)
     (for-each (lambda (name)
                 (call-with-output-file (path name)
                   (lambda (p) (display (if (string=? name "zeta.txt") "z\n" "one\ntwo\n") p)))) names)

     ;; loading a module through the kernel publishes the literals of the
     ;; completing types it brings: file and directory read paths back
     (check 'loading-a-module-publishes-the-literals-of-its-types
       (list (and (top-level-bound? 'file) (top-level-bound? 'directory))
             ((top-level-value 'file) "notes.txt") ((top-level-value 'directory) root)
             (test:raises? (lambda () ((top-level-value 'directory) "/no/such/directory/here"))))
       (list #t "notes.txt" root #t))
     (define (view) (head:find-tool-buffer "*finder*"))
     (define (lines) (vector->list (head:buffer-lines (view))))
     (define (visible? needle)
       (exists (lambda (line) (and (string:search line needle 0 (string-length line)) #t)) (lines)))
     (define (settle!)
       ;; The scan worker publishes complete snapshots; refreshing the shown
       ;; view collects them until its directory label stops searching.
       (test:await 'scan-settles
         (lambda ()
           (head:refresh-visible-views!)
           (or (not (memq (view) (map head:window-buffer (head:windows))))
               (not (visible? "Searching…"))))))
     (define (press! . events)
       ;; Each key sees a settled scan, as a user's next key would.
       (for-each (lambda (event) (dispatch:key! event) (settle!)) events))
     (define (type! text) (for-each (lambda (c) (dispatch:key! (string c))) (string->list text)) (settle!))
     (define (filter! text) (press! "C-u") (type! text))
     (define (files-open! . directory)
       ((top-level-value 'finder:expansion-limit) 2)
       (apply open! directory)
       (settle!))
     (define (labels)
       (map (lambda (line)
              (substring line 0 (or (string:search line "  " 0 (string-length line)) (string-length line))))
         (list-tail (lines) 3)))
     (define (location)
       (list (head:buffer-fact (view) 'directory #f) (head:buffer-fact (view) 'file-filter #f)))
     (define (chosen-is? prefix) (string:prefix? prefix (head:buffer-line (head:current-buffer) (car (head:point)))))
     (define (group-count name)
       (let ([line (find (lambda (s) (string:prefix? name s)) (lines))])
         (and line (substring line (- (string-length line) 3) (string-length line)))))
     (define (sequence proc items)
       ;; map does not promise effect order; event sequences do.
       (reverse (fold-left (lambda (acc item) (cons (proc item) acc)) '() items)))

     (files-open! root)
     (check 'finder-list-has-only-subdirectories-and-files-with-a-full-directory-path
       (list (list-head (labels) 4) (cadr (lines))
             (and (member "odd\\xA;name.txt" (labels)) #t) (and (member ".dot" (labels)) #t)
             (head:buffer-store-id (view)) (head:app-cursor-visible-in? (head:current-window))
             (head:buffer-selectable? (view))
             (eq? (keymap:binding "C-x C-f") (top-level-value 'finder:open!)))
       (list '("empty/" "large/" "small/" "a 日本語 long (name).txt") (string-append "Directory: " root "/")
             #t #f #f #f #f #t))
     ;; Save As cannot turn the app's projection into a visiting buffer or
     ;; overwrite a file with its table. The next keys must still filter.
     (check 'finder-refuses-save-as-without-changing-the-app-or-disk
       (let* ([before (lines)]
              [results (map (lambda (name)
                              (guard (ex [(kernel:refusal? ex) 'refused]) (save-file! (path name))))
                         '("apple.txt" "new.txt"))])
         (list results
           (equal? before (lines))
           (list (head:buffer-name (view)) (head:buffer-file (view))
                 (head:buffer-fact (view) 'mode #f) (head:buffer-read-only (view)))
           (call-with-input-file (path "apple.txt") get-string-all) (file-exists? (path "new.txt"))))
       '((refused refused) #t ("<finder>" #f "finder" #t) "one\ntwo\n" #f))
     (press! "DOWN" "DOWN") (type! "ONly") ; begin on small/, whose descendant will match
     (check 'finder-single-recursive-match-is-the-default
       (list (visible? "small/nested/needle-") (chosen-is? "small/nested/needle-only.txt")) '(#t #t))
     (press! "RET")
     (define visited
       (begin (head:goto! '(1 . 1)) (insert-text! "!")
              (list (head:buffer-store-id (head:current-buffer)) (head:point) (buffer-text (head:current-buffer)))))
     (files-open! root) (filter! "only") (press! "RET")
     (check 'finder-reopening-reuses-unsaved-buffer-and-window-point
       (list (head:buffer-store-id (head:current-buffer)) (head:point) (buffer-text (head:current-buffer))) visited)

     ;; A filter names entries unless it contains a slash: a directory whose
     ;; name matches does not claim its contents, while its path does.
     (files-open! root) (filter! "small")
     (define named (list (visible? "small/") (visible? "small/needle-one.txt") (group-count "small/")))
     (filter! "small/")
     (check 'finder-name-filters-match-entries-and-slash-filters-match-paths
       (list named (visible? "small/needle-one.txt") (group-count "small/"))
       '((#t #f "  0") #f "  3"))
     (files-open! root) (filter! "needle")
     (check 'finder-groups-remain-stable-through-filter-changes
       (list (and (member "large/" (labels)) #t) (and (member "large/needle-a.txt" (labels)) #t)
             (and (member "small/needle-one.txt" (labels)) #t) (group-count "large/")
             ;; Inspect the handler's immediate rendering before another
             ;; refresh can collect worker results: erase/refill and column
             ;; shifts on each keystroke would show here.
             (let ([header (list-ref (lines) 2)])
               (define (sample event)
                 (dispatch:key! event)
                 (let ([lines (lines)])
                   (define (has? name) (exists (lambda (s) (string:prefix? name s)) lines))
                   (list (for-all has? '("large/" "small/" "small/needle-one.txt" "small/nested/needle-only.txt"))
                         (has? "empty/") (equal? header (list-ref lines 2)))))
               (let ([narrow (sample "-")]) (list narrow (sample "BACKSPACE")))))
       '(#t #f #t "  3" ((#t #f #t) (#t #f #t))))
     (settle!)
     ;; Right enters the chosen directory with the filter; Left returns with
     ;; it selected, and each directory recalls its own choice.
     (press! "HOME" "RIGHT" "DOWN")     ; into large/, then beyond its default row
     (check 'finder-drilldown-and-return-preserve-filter-and-each-directorys-choice
       (cons (location)
         (sequence (lambda (step) (press! (car step)) (list (location) (chosen-is? (cadr step))))
           '(("LEFT" "large/") ("RIGHT" "needle-b.txt") ("LEFT" "large/"))))
       (list (list (path "large") "needle")
         (list (list root "needle") #t) (list (list (path "large") "needle") #t) (list (list root "needle") #t)))
     ;; The filter is never completed: Tab moves the row like Down. The
     ;; empty filter's default row, nested/, still matches and stays chosen.
     (filter! "small/") (press! "RIGHT")
     (define inside (location))
     (filter! "") (type! "ne")
     (define before-tab (chosen-is? "nested/"))
     (press! "TAB")
     (check 'finder-entering-a-typed-directory-path-keeps-the-filter-and-tab-moves-the-row
       (list inside before-tab (location) (chosen-is? "needle-one.txt"))
       (list (list (path "small") "small/") #t (list (path "small") "ne") #t))
     (files-open! root) (filter! "small/.日本語") (press! "DOWN")
     (define shown (location))
     (define escaped (visible? "small/.日本語\\xA;/"))
     (press! "RET")
     (check 'finder-filter-shows-a-safe-label-for-a-hidden-control-character-path-and-enters-it
       (list shown escaped (location))
       (list (list root "small/.日本語") #t (list (path "small/.日本語\n") "small/.日本語")))
     (unless (zero? ((foreign-procedure "symlink" (string string) int) (path "small/nested") (path "linked")))
       (error 'finder "cannot create directory-link fixture"))
     (files-open! root) (filter! "linked/needle")
     (define route (labels))
     (press! "RIGHT")
     (define inside-link (location))
     (filter! "needle-only.txt") (press! "RET")
     (check 'finder-typed-link-path-remains-navigable-without-recursively-following-links
       (list route inside-link (head:buffer-file (head:current-buffer)))
       (list '("linked@/") (list (path "linked") "linked/needle") (path "small/nested/needle-only.txt")))
     (delete-file (path "linked"))
     (files-open! (path "small/nested"))
     (check 'finder-left-right-retraces-three-levels-with-the-return-child-selected
       (sequence (lambda (step) (press! (car step)) (list (car (location)) (chosen-is? (cadr step))))
         `(("LEFT" "nested/") ("LEFT" "small/") ("LEFT" ,(string-append (string:tail root 5) "/"))
           ("RIGHT" "small/") ("RIGHT" "nested/") ("RIGHT" "needle-only.txt")))
       (map (lambda (directory) (list directory #t))
         (list (path "small") root "/tmp" root (path "small") (path "small/nested"))))
     (files-open! (path "small/.日本語\n/leaf"))
     (press! "LEFT")                    ; into the hidden directory, leaf/ selected
     (check 'finder-return-navigation-reveals-a-hidden-child-and-recalls-its-selection
       (sequence (lambda (step) (press! (car step)) (list (car (location)) (show-hidden) (chosen-is? (cadr step))))
         '(("LEFT" ".日本語\\xA;/") ("RIGHT" "leaf/")))
       (list (list (path "small") #t #t) (list (path "small/.日本語\n") #t #t)))
     (press! "M-.")                     ; hide dot entries again
     (files-open! root) (filter! "no-such-match") (press! "RET")
     (check 'finder-empty-filter-result-does-not-navigate
       (list (visible? "No matching files") (car (location))) (list #t root))

     ;; Sort keys cycle ascending, descending and off, renumbering the rest;
     ;; sizes compare as bytes within the file group.
     (filter! "apple") (press! "F2" "F1")
     (define keys (head:buffer-fact (view) 'file-sorts #f))
     (press! "F2")
     (define descending (visible? "Size¹↓"))
     (press! "F2")
     (check 'finder-sort-keys-cycle-and-renumber
       (list keys descending (visible? "Name¹↑") (head:buffer-fact (view) 'file-sorts #f))
       '(((1 . #f) (0 . #f)) #t #t ((0 . #f))))
     (filter! "txt") (press! "F1" "F1" "F2") ; disable Name, enable Size ascending
     (check 'finder-size-sorting-uses-bytes-within-the-file-group
       (find (lambda (name) (string:suffix? ".txt" name)) (labels)) "zeta.txt")
     (filter! "") (press! "M-.")
     (check 'finder-hidden-toggle-reveals-dot-entries (and (member ".dot" (labels)) #t) #t)
     (press! "M-.")
     (check 'finder-mark-command-cannot-enable-selection
       (begin (guard (ex [else #f]) (set-mark-command!)) (head:buffer-marked (view))) #f)
     (filter! "日本語") (press! "RET")
     (check 'finder-unicode-filter-opens-the-complete-path
       (head:buffer-file (head:current-buffer)) (path "a 日本語 long (name).txt"))

     ;; Identity: a killed view's scan is rejected by its recreation, and a
     ;; real reload replaces the worker's owner while restoring the app state.
     (files-open! root)
     (dispatch:key! "n")
     (kill-buffer! (head:current-buffer))
     (files-open! (path "empty"))
     (check 'finder-kill-and-recreate-rejects-the-old-scan
       (list (car (location)) (car (lines)) (head:app-buffer? (view))) (list (path "empty") "Filter: " #t))
     (define same-view
       (let ([before (head:current-buffer)])
         (dispatch:key! "n") (dispatch:key! "M-.")
         (kernel:reload-module! "finder")
         (eq? before (head:current-buffer))))
     (settle!)
     (check 'finder-reload-replaces-worker-ownership-and-restores-app-state
       (list same-view (visible? "No matching files") (car (location)) (car (lines))
             (head:app-buffer? (view)) (show-hidden))
       (list #t #t (path "empty") "Filter: n" #t #t))

     ;; Opening a file from the view without target links shows it in this
     ;; window and puts the view behind in the recency list, so C-x b offers
     ;; the document the view replaced; with target links the file opens in
     ;; every target and this window keeps the view and the focus
     (define w1 (head:current-window))
     (visit-file! (path "zeta.txt"))
     (define b1 (head:current-buffer))
     (files-open! root) (filter! "long") (press! "RET")
     (check 'finder-opening-a-file-puts-the-view-behind-the-document-it-replaced
       (list (head:buffer-name (head:current-buffer)) (eq? (cadr (head:buffers)) b1) (eq? (car (reverse (head:buffers))) (view)))
       (list "a 日本語 long (name).txt" #t #t))
     (define w2 (window:split-below!))
     (window:focus! w1)
     (files-open! root)
     (window:link-target! w2)
     (filter! "zeta") (press! "RET")
     (check 'finder-with-a-target-link-open-the-file-there-and-keep-the-view
       (list (head:buffer-name (head:window-buffer w2)) (eq? (head:current-window) w1) (eq? (head:current-buffer) (view))
             (eq? (head:window-buffer w1) (view)))
       (list "zeta.txt" #t #t #t))
     (window:focus! w2) (window:delete!)

     ;; the app as an API: what an agent asks and does without keys
     (define (api name) (top-level-value name))
     ((api 'finder:filter!) "long") (settle!)
     (check 'the-api-filters-lists-and-locates
       (list ((api 'finder:location)) ((api 'finder:entries))) (list root (list (list 'file (path "a 日本語 long (name).txt")))))
     ((api 'finder:filter!) "") (settle!)
     ((api 'finder:select!) "zeta.txt")
     (check 'the-api-selects-by-path-and-tells-the-choice ((api 'finder:chosen)) (path "zeta.txt"))
     (check 'the-api-sorts-by-column-and-tells-the-order
       (begin ((api 'finder:toggle-sort-column!) 2) (let ([first ((api 'finder:sorts))]) ((api 'finder:toggle-sort-column!) 2) (list first ((api 'finder:sorts)))))
       '(((2 . #f)) ((2 . #t))))
     (check 'the-api-refuses-an-unlisted-path (test:raises? (lambda () ((api 'finder:select!) "nowhere.txt"))) #t)
     (check 'the-create-modes-keys-are-bound-in-its-own-context
       (list (eq? (keymap:binding-action (cdr (keymap:resolved-binding 'finder-create '("C-r")))) (api 'finder:refresh!))
             (keymap:action-text (keymap:binding-action (cdr (keymap:resolved-binding 'finder-create '("F2"))))))
       '(#t "(finder:toggle-sort-column! 2)"))
     (check 'typing-is-a-binding-of-the-finder-context
       (let ([hit (keymap:resolved-binding 'finder '("SELF-INSERT"))])
         (and hit (keymap:action-text (keymap:binding-action (cdr hit)))))
       "(finder:extend-filter! (head:typed-text))")
     (kill-buffer! (view))
     (for-each (lambda (name) (delete-file (path name))) names)
     (for-each (lambda (name) (delete-directory (path name))) (reverse directories))
     (delete-directory root)
     (test:finish! 'finder)))
