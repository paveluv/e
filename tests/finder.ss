#!/usr/bin/env scheme-script

;; The <finder> app as a table over a real directory tree: recursive name
;; filters and literal path filters, fully expanded match trees,
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
             (prefix (foundation string) string:) (prefix (head window) window:) (prefix (head paint) paint:)
             (prefix (foundation path-filter) path-filter:)
             (prefix (service log) log:)
             (prefix (head mode) mode:)
             (prefix (sys sys) sys:) (prefix (test) test:))

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
               (and (not (visible? "Searching…")) (not (visible? "Completing…")))))))
     (define (press! . events)
       ;; Each key sees a settled scan, as a user's next key would.
       (for-each (lambda (event) (dispatch:key! event) (settle!)) events))
     (define (type! text) (for-each (lambda (c) (dispatch:key! (string c))) (string->list text)) (settle!))
     (define (filter-string directory text)
       (string-append (path-filter:format-keys (list (string-append directory "/")))
         (if (string=? text "") "" (string-append " " text))))
     (define (at directory text) (list directory (filter-string directory text)))
     (define (filter! text)
       (let ([text (if (string:prefix? "/" text) text
                       (filter-string (head:buffer-fact (view) 'directory #f) text))])
         ((top-level-value 'finder:filter!) text) (settle!)))
     (define (files-open! . directory)
       (apply open! directory)
       (settle!))
     (define (labels)
       (map (lambda (line)
              (let ([start (let skip ([i 0])
                             (if (and (< i (string-length line)) (char=? (string-ref line i) #\space))
                                 (skip (+ i 1)) i))])
                (substring line 0 (or (string:search line "  " start (string-length line)) (string-length line)))))
         (list-tail (lines) 2)))
     (define (location)
       (list (head:buffer-fact (view) 'directory #f) (head:buffer-fact (view) 'file-filter #f)))
     (define (chosen-is? prefix) (string:prefix? prefix (head:buffer-line (head:current-buffer) (car (head:point)))))
     (define (group-count name)
       (let ([line (find (lambda (s) (string:prefix? name s)) (lines))])
         (and line (substring line (- (string-length line) 3) (string-length line)))))
     (define (sequence proc items)
       ;; map does not promise effect order; event sequences do.
       (reverse (fold-left (lambda (acc item) (cons (proc item) acc)) '() items)))
     (define (underlined row line)
       (styled-text row line '(plain mark)))
     (define (styled-text row line face)
       (let ([styles ((mode:row-styles (mode:of (view))) (view) row line)])
         (list->string
           (filter char?
             (map (lambda (i) (and (equal? (vector-ref styles i) face) (string-ref line i)))
               (iota (string-length line)))))))

     (files-open! root)
     (check 'finder-list-has-only-subdirectories-and-files-with-a-full-directory-path
       (list (list-head (labels) 4) (car (location))
             (and (member "odd\\xA;name.txt" (labels)) #t) (and (member ".dot" (labels)) #t)
             (head:buffer-store-id (view)) (head:app-cursor-visible-in? (head:current-window))
             (head:buffer-selectable? (view))
             (eq? (keymap:binding "C-x C-f") (top-level-value 'finder:open!)))
       (list '("empty/" "large/" "small/" "a 日本語 long (name).txt") root
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
     (press! "DOWN" "DOWN") (type! " ONly") ; begin on small/, whose descendant will match
     (check 'finder-single-recursive-match-is-the-default
       (list (labels) (group-count "small/") (group-count " nested/") (chosen-is? "  needle-only.txt")
         (car (lines))
         (let* ([line (head:buffer-line (view) 0)]
                [styles ((mode:row-styles (mode:of (view))) (view) 0 line)]
                [at (string:search line "[1 match]" 0 (string-length line))])
           (and at (vector-ref styles at))))
       (list '("small/" " nested/" "  needle-only.txt") "  1" "  1" #t
         (string-append "Filter: " root "/ ∧ ONly [1 match]") 'ghost))
     (press! "RET")
     (define visited
       (begin (head:goto! '(1 . 1)) (insert-text! "!")
              (list (head:buffer-store-id (head:current-buffer)) (head:point) (buffer-text (head:current-buffer)))))
     (files-open! root) (filter! "only") (press! "RET")
     (check 'finder-reopening-reuses-unsaved-buffer-and-window-point
       (list (head:buffer-store-id (head:current-buffer)) (head:point) (buffer-text (head:current-buffer))) visited)
     (open!) (settle!)
     (check 'finder-reopens-the-same-directory-and-filter-after-opening-a-file
       (list (location) (labels))
       (list (at root "only") '("small/" " nested/" "  needle-only.txt")))
     (press! "ESC") (open!) (settle!)
     (check 'finder-retains-the-filter-after-cancelling (location) (at root "only"))

     (filter! "SMALL/ne only")
     (check 'finder-underlines-token-parts-in-their-own-path-components
       (map (lambda (i) (underlined i (list-ref (lines) i))) '(2 3 4))
       '("small/" "ne" "only"))
     (filter! "日本語 name")
     (let ([line (list-ref (lines) 2)])
       (check 'finder-token-styling-excludes-elision-and-metadata
         (list (underlined 2 line) (underlined 2 "a 日本語…  name"))
         '("日本語name" "日本語")))
     (filter! "odd")
     (check 'finder-token-styling-keeps-escaped-name-offsets
       (underlined 2 (list-ref (lines) 2)) "odd")

     ;; Path editing changes scope immediately; completing a unique directory
     ;; appends its slash and scans the children absent from the old results.
     (files-open! (path "small")) (press! "BACKSPACE")
     (define path-without-slash (list (location) (labels)))
     (press! "TAB")
     (check 'finder-tab-adds-the-slash-and-lists-the-completed-directorys-children
       (list path-without-slash (location) (labels))
       (list (list (list root (path "small")) '("small/")) (at (path "small") "") '("nested/" "needle-one.txt")))
     (type! " needle") (press! "HOME" "RET")
     (check 'finder-enter-clears-extra-keys-and-opens-a-full-path-overview
       (location) (at (path "small/nested") ""))
     (press! "LEFT") (press! " ") (press! "BACKSPACE")
     (check 'finder-one-backspace-removes-the-spaced-conjunction (location) (at (path "small") ""))
     (press! "C-u" "BACKSPACE")
     (check 'finder-clear-removes-the-whole-filter-and-empty-backspace-is-a-no-op (location) '("/" ""))
     (press! "ESC") (open!) (settle!)
     (check 'finder-reopens-an-intentionally-empty-filter (location) '("/" ""))
     (filter! (string-append root "/absent/deeper/"))
     (check 'finder-missing-path-components-are-italic-in-the-normal-color
       (styled-text 0 (car (lines)) '(plain italic)) "absent/deeper/")
     (mkdir (path "absent")) (mkdir (path "absent/deeper"))
     (press! "C-r")
     (check 'finder-refresh-updates-path-existence-styling
       (styled-text 0 (car (lines)) '(plain italic)) "")
     (delete-directory (path "absent/deeper")) (delete-directory (path "absent"))
     (let ([quoted (string-append root "/missing 日本語/next/")])
       ((top-level-value 'finder:filter!) (format "~s" quoted)) (settle!)
       (let* ([w (head:current-window)] [width (head:window-width w)] [height (head:window-size w)])
         (head:window-width-set! w 52) (head:window-size-set! w 8) (head:refresh-visible-views!)
         (let ([marked (styled-text 0 (head:window-line w 0) '(plain italic))])
           (check 'finder-missing-path-style-excludes-the-clipped-ellipsis-quotes-and-ghost
             (and (positive? (string-length marked)) (string:suffix? marked "missing ∧ 日本語/next/")) #t))
         (head:window-width-set! w width) (head:window-size-set! w height) (head:refresh-visible-views!))
       (check 'finder-missing-path-style-maps-quoted-spaces-and-unicode
         (styled-text 0 (car (lines)) '(plain italic)) "missing ∧ 日本語/next/"))

     (check 'finder-queued-tab-enters-unique-partial-empty-and-quoted-directory-paths
       (sequence (lambda (text)
                   ((top-level-value 'finder:filter!) text) ((top-level-value 'finder:complete!))
                   (settle!) (location))
         (list (path "sm") (path "empty") (format "~s" (path "small/.日本語\n"))))
       (map (lambda (name) (at (path name) "")) '("small" "empty" "small/.日本語\n")))

     ;; A directory match ends its branch until explicitly entered.
     (files-open! root) (filter! "small")
     (define named (list (visible? "small/") (visible? " needle-one.txt") (group-count "small/")))
     (filter! "small/")
     (check 'finder-deep-filters-match-full-relative-paths
       (list named (visible? " needle-one.txt") (group-count "small/"))
       '((#t #f "  0") #f "  0"))
     (files-open! root) (filter! "needle")
     (check 'finder-groups-remain-stable-through-filter-changes
       (list (labels) (group-count "large/") (group-count "small/") (group-count " nested/")
             ;; Inspect the handler's immediate rendering before another
             ;; refresh can collect worker results: erase/refill and column
             ;; shifts on each keystroke would show here.
             (let ([header (list-ref (lines) 1)])
               (define (sample event)
                 (dispatch:key! event)
                 (let ([lines (lines)])
                   (define (has? name) (exists (lambda (s) (string:prefix? name s)) lines))
                   (list (for-all has? '("large/" "small/" " needle-one.txt" " nested/" "  needle-only.txt"))
                         (has? "empty/") (equal? header (list-ref lines 1)))))
               (let ([narrow (sample "-")]) (list narrow (sample "BACKSPACE")))))
       '(("large/" " needle-a.txt" " needle-b.txt" " needle-c.txt" "small/" " nested/" "  needle-only.txt" " needle-one.txt") "  3" "  2" "  1"
         ((#t #f #t) (#t #f #t))))
     (settle!)
     ;; Right enters a fresh directory overview; Left returns with
     ;; it selected, and each directory recalls its own choice.
     (press! "HOME" "RIGHT" "DOWN")     ; into large/, then beyond its default row
     (check 'finder-drilldown-and-return-reset-the-path-and-recall-each-directorys-choice
       (cons (location)
         (sequence (lambda (step) (press! (car step)) (list (location) (chosen-is? (cadr step))))
           '(("LEFT" "large/") ("RIGHT" "needle-b.txt") ("LEFT" "large/"))))
       (list (at (path "large") "")
         (list (at root "") #t) (list (at (path "large") "") #t) (list (at root "") #t)))
     ;; Tab preserves the matches and choice when the filter is already a
     ;; longest common completion. Row navigation remains on the arrows.
     (filter! "small/") (press! "RIGHT")
     (define inside (location))
     (filter! "ne")
     (define before-tab (chosen-is? "nested/"))
     (press! "TAB")
     (check 'finder-entering-a-typed-directory-starts-an-overview-and-tab-preserves-the-choice
       (list inside before-tab (location) (chosen-is? "nested/"))
       (list (at (path "small") "") #t (list (path "small") (string-append (path "small") "/ne ed")) #t))
     (files-open! root) (filter! "small/.日本語") (press! "DOWN")
     (define shown (location))
     (define escaped (visible? " .日本語\\xA;/"))
     (press! "RET")
     (check 'finder-filter-shows-a-safe-label-for-a-hidden-control-character-path-and-enters-it
       (list shown escaped (location))
       (list (at root "small/.日本語") #t (at (path "small/.日本語\n") "")))
     (unless (zero? ((foreign-procedure "symlink" (string string) int) (path "small/nested") (path "linked")))
       (error 'finder "cannot create directory-link fixture"))
     (files-open! root) (press! "C-r") (filter! "linked/needle")
     (define route (labels))
     (press! "RIGHT")
     (define inside-link (location))
     (filter! "needle-only.txt") (press! "RET")
     (check 'finder-typed-link-path-remains-navigable-without-recursively-following-links
       (list route inside-link (head:buffer-file (head:current-buffer)))
       (list '("linked@/") (at (path "linked") "") (path "small/nested/needle-only.txt")))
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
       (find (lambda (name) (and (not (string:prefix? " " name)) (string:suffix? ".txt" name))) (labels)) "zeta.txt")
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
       (list (car (location)) (car (lines)) (head:app-buffer? (view)))
       (list (path "empty") (string-append "Filter: " (path "empty") "/ [0 matches]") #t))
     (define same-view
       (let ([before (head:current-buffer)])
         (dispatch:key! "n") (dispatch:key! "M-.")
         (kernel:reload-module! "finder")
         (eq? before (head:current-buffer))))
     (settle!)
     (check 'finder-reload-replaces-worker-ownership-and-restores-app-state
       (list same-view (visible? "n [create]") (car (location)) (car (lines))
             (head:app-buffer? (view)) (show-hidden))
       (list #t #t (path "empty") (string-append "Filter: " (path "empty") "/n [0 matches] [hidden]") #t #t))

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
     (define (existing-entries)
       (filter (lambda (entry) (file-exists? (cadr entry) #f)) ((api 'finder:entries))))
     ((api 'finder:filter!) (filter-string root "long")) (settle!)
     (check 'the-api-filters-lists-and-locates
       (list ((api 'finder:location)) ((api 'finder:entries))) (list root (list (list 'file (path "a 日本語 long (name).txt")))))
     ((api 'finder:filter!) (filter-string root "")) (settle!)
     ((api 'finder:select!) "zeta.txt")
     (check 'the-api-selects-by-path-and-tells-the-choice ((api 'finder:chosen)) (path "zeta.txt"))
     (check 'the-api-sorts-by-column-and-tells-the-order
       (begin ((api 'finder:toggle-sort-column!) 2) (let ([first ((api 'finder:sorts))]) ((api 'finder:toggle-sort-column!) 2) (list first ((api 'finder:sorts)))))
       '(((2 . #f)) ((2 . #t))))
     (check 'the-api-refuses-an-unlisted-path (test:raises? (lambda () ((api 'finder:select!) "nowhere.txt"))) #t)
     (check 'finder-has-no-separate-create-mode-binding
       (keymap:resolved-binding 'finder '("M-c")) #f)
     (check 'typing-is-a-binding-of-the-finder-context
       (let ([hit (keymap:resolved-binding 'finder '("SELF-INSERT"))])
         (and hit (keymap:action-text (keymap:binding-action (cdr hit)))))
       "(finder:extend-filter! (head:typed-text))")

     ;; The same corpus exercises conjunctive keys, queued completion,
     ;; overlap, visibility, and quoted names without a second matching mode.
     (let ([dir (path "completion")]
           [files '("split-window!" "split-window-right!" "nested/split-window-right!"
                    "alpha.sls" "beta.sls" ".hidden.sls" "a space.txt" "Case/token.dat" "case/token.dat")])
       (mkdir dir)
       (for-each (lambda (name) (mkdir (string-append dir "/" name))) '("nested" "Case" "case"))
       (for-each (lambda (name) (call-with-output-file (string-append dir "/" name) (lambda (p) (display "" p)))) files)
       (files-open! dir) (filter! "window split")
       (let ([before (existing-entries)])
         (press! "TAB")
         (check 'finder-tab-preserves-conjunctive-matches
           (list (cadr (location)) (equal? before (existing-entries)))
           (list (filter-string dir "split-window") #t)))
       ((api 'finder:filter!) (filter-string dir "space txt")) ((api 'finder:complete!))
       (settle!)
       (check 'finder-queued-tab-completes-a-single-path-with-spaces
         (cadr (location)) (format "~s" (string-append dir "/a space.txt")))
       (filter! "window window")
       (check 'finder-repeated-keys-need-separate-occurrences ((api 'finder:entries)) '())
       ((api 'finder:show-hidden) #f) (filter! "sls")
       (let ([before (existing-entries)])
         (press! "TAB")
         (check 'finder-tab-preserves-hidden-entry-visibility
           (list (equal? before (existing-entries)) (show-hidden)) '(#t #f)))
       (files-open! (path "empty"))
       (filter! (string-append dir "/"))
       (press! "C-r" "TAB") ; the new directory was absent from its cached parent
       (check 'finder-rooted-directory-token-shows-its-children
         (list (car (location)) (cadr (location))
           (filter (lambda (entry) (string=? (cadr entry) dir)) ((api 'finder:entries)))
           (car (lines)))
         (list dir (string-append dir "/") '()
           (string-append "Filter: " dir "/ [8 matches]")))
       (files-open! (string-append dir "/Case")) (filter! "token") (press! "TAB")
       (let ([spelled (cadr (location))])
         (filter! (string-append dir "/cas"))
         (press! "TAB")
         (check 'finder-tab-does-not-enter-an-ambiguous-directory-path
           (list (car (location)) ((api 'finder:entries)))
           (list dir (map (lambda (name) (list 'directory (string-append dir "/" name))) '("Case" "case"))))
         (files-open! dir) (filter! "token")
         (let ([before (existing-entries)])
           (press! "TAB")
           (check 'finder-completion-preserves-directory-spelling-and-both-case-variants
             (list spelled (car (location)) (equal? before (existing-entries)))
             (list (string-append dir "/Case/token.dat") dir #t))))
       (files-open! dir)
       (filter! "split") (settle!)
       (let ([before (head:window-lines (head:current-window))])
         (head:refresh-visible-views!)
         (check 'finder-unchanged-refresh-reuses-its-presentation
           (eq? before (head:window-lines (head:current-window))) #t))
       (files-open! root)
       (for-each (lambda (name) (delete-file (string-append dir "/" name))) files)
       (for-each (lambda (name) (delete-directory (string-append dir "/" name))) '("nested" "Case" "case"))
       (delete-directory dir))

     ;; Repeated basenames still identify distinct files, and each branch
     ;; stays together when sorting, regardless of depth.
     (let ([dirs '("tree" "tree/A" "tree/A/B" "tree/A/B/C" "tree/A/D" "tree/Z")]
           [files '("tree/A/B/C/foo.txt" "tree/A/D/foo.txt" "tree/A/foo.txt" "tree/Z/foo.txt" "tree/foo-root.txt")]
           [tree-paths #f])
       (for-each (lambda (name) (mkdir (path name))) dirs)
       (for-each (lambda (name) (call-with-output-file (path name) (lambda (p) (display name p)))) files)
       (files-open! (path "tree"))
       (filter! "foo")
       (set! tree-paths (map cadr ((api 'finder:entries))))
       (check 'finder-tree-keeps-ancestors-counts-and-path-identities
         (list (labels) (map group-count '("A/" " B/" "  C/" " D/" "Z/"))
               tree-paths ((api 'finder:chosen)))
         (list '("A/" " B/" "  C/" "   foo.txt" " D/" "  foo.txt" " foo.txt" "Z/" " foo.txt" "foo-root.txt")
           '("  3" "  1" "  1" "  1" "  1")
           (map path '("tree/A" "tree/A/B" "tree/A/B/C" "tree/A/B/C/foo.txt" "tree/A/D" "tree/A/D/foo.txt"
                       "tree/A/foo.txt" "tree/Z" "tree/Z/foo.txt" "tree/foo-root.txt"))
           (path "tree/A/B/C/foo.txt")))
       (press! "F1" "F1")
       (check 'finder-sorts-siblings-without-detaching-their-descendants
         (labels) '("Z/" " foo.txt" "A/" " D/" "  foo.txt" " B/" "  C/" "   foo.txt" " foo.txt" "foo-root.txt"))
       (press! "F1")

       ;; Filesystem watches are disabled: the cached inventory is reused
       ;; until C-r. Refresh updates counts while preserving path identity.
       (let ([selected ((api 'finder:chosen))] [added (path "tree/A/B/C/foo-added.txt")])
         (call-with-output-file added (lambda (p) (display "new" p)))
         (head:refresh-visible-views!)
         (check 'finder-cache-stays-stable-until-refresh (visible? "foo-added.txt") #f)
         (press! "C-r")
         (check 'finder-refresh-updates-counts-and-retains-selection
           (list (group-count "A/") (group-count " B/") (equal? selected ((api 'finder:chosen))))
           '("  4" "  2" #t))
         (delete-file added) (press! "C-r"))

       (let* ([layout (head:root)] [panel (head:current-window)] [document (head:buffer-named "zeta.txt")]
              [other (head:make-window document 0 0 0 0 0 12 41 40 'default)])
         (head:set-layout-root! (head:make-layout-split 'right panel other 1 1))
         (paint:set-screen-rows! 10) (paint:window-layout) (head:refresh-visible-views!)
         (check 'finder-wheel-scrolls-the-pointed-window-without-opening-or-changing-focus
           (sequence
             (lambda (focus)
               (head:set-current! focus)
               (head:with-window panel ((api 'finder:first-row!)))
               (paint:scroll-window! panel (head:window-size panel))
               (let ([before (head:window-top panel)])
                 (head:with-window panel
                   (parameterize ([head:app-event-focus focus])
                     (head:dispatch-app-event! "WHEEL-DOWN") (head:dispatch-app-event! "S-WHEEL-DOWN")))
                 (head:refresh-visible-views!)
                 (paint:scroll-window! panel (head:window-size panel))
                 (list (> (head:window-top panel) before) (eq? (head:current-window) focus)
                       (eq? (head:window-buffer other) document)
                       (head:with-window panel (and (member ((api 'finder:chosen)) tree-paths) #t)))))
             (list panel other))
           '((#t #t #t #t) (#t #t #t #t)))
         (head:set-current! panel) (head:set-layout-root! layout) (paint:set-screen-rows! 24))

       (check 'finder-enters-intermediate-directories-and-opens-the-picked-duplicate-basename
         (sequence
           (lambda (name)
             (files-open! (path "tree")) (filter! "foo")
             ((api 'finder:select!) (path name)) (press! "RET")
             (if (eq? (head:current-buffer) (view)) (car (location)) (head:buffer-file (head:current-buffer))))
           '("tree/A/B" "tree/A/B/C" "tree/A/D/foo.txt"))
         (map path '("tree/A/B" "tree/A/B/C" "tree/A/D/foo.txt")))
       (for-each (lambda (name) (delete-file (path name))) files)
       (for-each (lambda (name) (delete-directory (path name))) (reverse dirs)))
     ;; Creation rows share ordinary sorting and navigation, but neither
     ;; count as matches nor constrain completion of existing paths.
     (files-open! root)
     (for-each (lambda (key)
                 ((api 'finder:toggle-sort-column!) (car key))
                 (unless (cdr key) ((api 'finder:toggle-sort-column!) (car key))))
       ((api 'finder:sorts)))
     (filter! (string-append (path "apple") " txt"))
     (define creation-first (list (car (labels)) (visible? "[2 matches]") ((api 'finder:chosen))))
     (press! "F1" "F1")
     (check 'finder-creation-row-keeps-name-order-and-survives-additional-keys
       (list creation-first (car (reverse (labels))))
       (list (list "apple [create]" #t (path "APPLE.txt")) "apple [create]"))
     (let ([before (existing-entries)])
       (press! "TAB")
       (check 'finder-creation-suggestion-does-not-constrain-completion
         (existing-entries) before))
     (press! "F1")

     (check 'finder-creation-and-existing-paths-use-the-same-dot-component-resolution
       (sequence (lambda (text) (filter! (path text)) (list (car (location)) (labels)))
         '("small/absent/../../normalized.txt" "small/absent/../needle-one.txt"))
       (list (list root '("normalized.txt [create]")) (list (path "small") '("needle-one.txt"))))

     ((api 'finder:filter!) (string-append (format "~s" (path "new/inner 日本語/note.txt")) " unrelated")) (settle!)
     (check 'finder-missing-hierarchy-is-rooted-in-the-existing-prefix-and-styled
       (list (car (location)) (labels) (visible? "[0 matches]")
         (map (lambda (i)
                (let* ([line (list-ref (lines) i)] [styles ((mode:row-styles (mode:of (view))) (view) i line)]
                       [face (vector-ref styles (- i 2))])
                  (list (and (pair? face) (memq 'italic face) #t) (styled-text i line 'ghost)))) '(2 3 4)))
       (list root '("new/ [create]" " inner 日本語/ [create]" "  note.txt [create]") #t
         '((#t " [create]") (#t " [create]") (#t " [create]"))))
     (let* ([w (head:current-window)] [width (head:window-width w)])
       (head:window-width-set! w 20) (head:refresh-visible-views!)
       (check 'finder-narrow-rows-retain-the-create-ghost
         (map (lambda (row) (styled-text row (head:window-line w row) 'ghost)) '(2 3 4))
         '(" [create]" " [create]" " [create]"))
       (head:window-width-set! w width) (head:refresh-visible-views!))
     (press! "RIGHT")
     (check 'finder-right-on-a-proposed-file-does-not-create-its-parents (file-exists? (path "new")) #f)
     (press! "RET")
     (check 'finder-creates-and-opens-the-file-with-parent-first-logging
       (list (head:buffer-file (head:current-buffer))
         (eof-object? (call-with-input-file (path "new/inner 日本語/note.txt") get-string-all))
         (map log:datum (reverse (list-head (log:entries 'file:create!) 3))))
       (list (path "new/inner 日本語/note.txt") #t
         (list (string-append "Created directory " (path "new/"))
               (string-append "Created directory " (path "new/inner 日本語/"))
               (string-append "Created file " (path "new/inner 日本語/note.txt")))))
     (open!) (settle!)
     (check 'finder-creation-invalidates-the-missing-path-and-inventory-caches
       (list (visible? "[create]") (styled-text 0 (car (lines)) '(plain italic))) '(#f ""))

     (filter! (path "only/inner/"))
     (parameterize ([head:app-event-buffer-position '(2 . 1)]) (head:dispatch-app-event! "MOUSE-CLICK")) (settle!)
     (define created-parent (list (location) (file-directory? (path "only")) (file-exists? (path "only/inner"))))
     (filter! (path "only/inner/")) (press! "RIGHT")
     (check 'finder-click-creates-only-the-chosen-directory-and-right-creates-the-leaf
       (list created-parent (location) (directory-list (path "only/inner")))
       (list (list (at (path "only") "") #t #f) (at (path "only/inner") "") '()))
     (check 'visit-file-creates-directories-and-opens-finder-directly
       (begin (visit-file! (path "only/direct/")) (settle!)
              (list (location) (file-directory? (path "only/direct"))))
       (list (at (path "only/direct") "") #t))

     (filter! (path "race"))
     (define creation-logs (length (log:entries 'file:create!)))
     (call-with-output-file (path "race") (lambda (p) (display "another process" p)))
     (press! "RET")
     (check 'finder-creation-race-opens-the-existing-file-without-replacing-it
       (list (buffer-text (head:current-buffer)) (length (log:entries 'file:create!)))
       (list "another process" creation-logs))
     (files-open! root) (filter! (path "race/child")) (press! "RET")
     (check 'finder-refuses-a-file-in-the-parent-path-without-leaving-the-app
       (list (eq? (head:current-buffer) (view))
         (call-with-input-file (path "race") get-string-all) (length (log:entries 'file:create!)))
       (list #t "another process" creation-logs))
     (delete-file (path "race")) (delete-file (path "new/inner 日本語/note.txt"))
     (for-each (lambda (name) (delete-directory (path name))) '("new/inner 日本語" "new" "only/inner" "only/direct" "only"))
     (kill-buffer! (view))
     (for-each (lambda (name) (delete-file (path name))) names)
     (for-each (lambda (name) (delete-directory (path name))) (reverse directories))
     (delete-directory root)
     (test:finish! 'finder)))
