#!/usr/bin/env scheme-script

;; The <buffet> app as a table: one live model behind switching, filtering
;; by name or path, ordered ascending/descending/off sorts with compound
;; priorities, modification clocks, and identity through renames, kills and
;; recreation. Headless: the app handler receives the events the dispatcher
;; would send. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (head edit) init!) (head literal) (prefix (head head) head:) (prefix (head mode) mode:) (prefix (core kernel) kernel:) (prefix (head dispatch) dispatch:)
             (prefix (apps buffet) buffet:) (prefix (head paint) paint:) (prefix (head keymap) keymap:) (prefix (foundation edoc) edoc:)
             (prefix (foundation string) string:) (prefix (foundation text) text:) (prefix (state store) store:) (prefix (test) test:))

     (define check test:check)
     ;; the app's keys are bound in its mode's context by its install
     (buffet:init!)
     (putenv "TZ" "UTC")               ; deterministic clock cells
     (define (press! . events) (for-each dispatch:key! events))
     (define (type! text) (for-each (lambda (c) (dispatch:key! (string c))) (string->list text)))
     (define (sequence proc items)
       ;; map does not promise effect order; event sequences do.
       (reverse (fold-left (lambda (acc item) (cons (proc item) acc)) '() items)))
     (define (rows) (cddr (vector->list (head:buffer-lines (head:current-buffer)))))
     (define (heading) (vector-ref (head:buffer-lines (head:current-buffer)) 1))
     (define (name-in line)
       (let ([start (string:search line "<" 0 (string-length line))])
         (and start (substring line start (+ 1 (string:search line ">" start (string-length line)))))))
     (define (names) (map name-in (rows)))
     (define (selected) (name-in (head:buffer-line (head:current-buffer) (car (head:point)))))
     (define (state) (list (head:buffer-name (head:current-buffer)) (head:point)))
     (define (order tags) (map (lambda (tag) (format "<picker-~a>" tag)) tags))
     (define picker-all (order '(alpha beta gamma)))
     (define (row-of name)
       (find (lambda (line) (string:search line name 0 (string-length line))) (rows)))

     (define origin (head:current-buffer))
     (parameterize ([kernel:registering-module 'picker-fixture])
       (for-each (lambda (name) (mode:register! name '() '() (lambda (line) #f))) '("pick-a" "pick-z")))
     (for-each
       (lambda (entry)
         (let ([b (head:new-local-buffer! (car entry))])
           (head:buffer-lines-set! b (make-vector (cadr entry) (car entry)))
           (head:buffer-file-set! b (caddr entry))
           (head:with-buffer b (mode:choose! (cadddr entry)))
           (head:add-buffer! b)))
       '(("picker-alpha" 20 "/project/zebra/long/日本語 alpha.txt" "pick-z")
         ("picker-beta" 9 "/project/alpha/src/beta.ss" "pick-a")
         ("picker-gamma" 100 #f #f)))
     (head:show-buffer! (buffer "<picker-alpha>")) (head:goto! '(3 . 2))
     (head:show-buffer! (buffer "<picker-beta>")) (head:goto! '(4 . 1))

     ;; Enter initially selects the previous document; the app itself never
     ;; becomes the default, even after repeated quick switches.
     (check 'switching-defaults-to-the-previous-document
       (sequence (lambda (i) (buffet:open!) (press! "RET") (state)) (iota 4))
       '(("<picker-alpha>" (3 . 2)) ("<picker-beta>" (4 . 1)) ("<picker-alpha>" (3 . 2)) ("<picker-beta>" (4 . 1))))

     (buffet:open!)
     (check 'filter-matches-names-and-full-paths-without-case-or-prefix-restrictions
       (sequence (lambda (needle) (press! "C-u") (type! needle) (list (length (rows)) (name-in (car (rows)))))
         '("kEr-BeTa" "project/alpha/src" "日本語"))
       '((1 "<picker-beta>") (1 "<picker-beta>") (1 "<picker-alpha>")))
     (press! "C-u") (type! "no-such-buffer") (press! "RET")
     (check 'empty-results-never-create-a-buffer-or-leave-the-list
       (list (rows) (head:buffer-name (head:current-buffer)) (head:buffer-named "no-such-buffer"))
       '(("No matching buffers") "<buffet>" #f))
     (press! "C-g")
     (check 'cancel-restores-the-origin-and-its-point (state) '("<picker-beta>" (4 . 1)))

     ;; Sorting cycles ascending, descending and off on real values without
     ;; moving the keyboard candidate.
     (for-each (lambda (name) (head:buffer-modified-set! (buffer name) #t)) '("<picker-alpha>" "<picker-gamma>"))
     (head:buffer-fact-set! (buffer "<picker-alpha>") 'modified-at 1704164645000000900)
     (head:buffer-fact-set! (buffer "<picker-gamma>") 'modified-at 1704078245000000900)
     (for-each (lambda (name) (head:buffer-read-only-set! (buffer name) #t)) '("<picker-beta>" "<picker-gamma>"))
     (buffet:open!) (type! "picker-")
     (define (sort! key labels)
       ;; One snapshot covers the heading marks, row order and selection.
       (press! (format "F~a" key))
       (let ([heading (heading)])
         (list (for-all (lambda (label) (and (string:search heading label 0 (string-length heading)) #t)) labels)
               (= (length labels) (length (filter (lambda (c) (memv c '(#\↑ #\↓))) (string->list heading))))
               (names) (selected))))
     (define sort-cases
       '((1 "Modified" (beta gamma alpha) (alpha gamma beta))
         (2 "Flags" (alpha beta gamma) (beta gamma alpha))
         (3 "Buffer" (alpha beta gamma) (gamma beta alpha))
         (4 "Lines" (beta alpha gamma) (gamma alpha beta))
         (5 "Mode" (gamma beta alpha) (alpha beta gamma))
         (6 "File" (gamma beta alpha) (alpha beta gamma))
         ;; Same visible second, but gamma is 800 nanoseconds earlier.
         (1 "Modified" (beta gamma alpha) (alpha gamma beta) 1704164645000000100)))
     (check 'column-keys-cycle-real-values-without-changing-selection
       (sequence
         (lambda (entry)
           (when (pair? (cddddr entry))
             (head:buffer-fact-set! (buffer "<picker-gamma>") 'modified-at (car (cddddr entry))))
           (sequence (lambda (marks) (sort! (car entry) (if marks (list (string-append (cadr entry) marks)) '())))
             '("¹↑" "¹↓" #f)))
         sort-cases)
       (map (lambda (entry)
              (map (lambda (tags) (list #t #t (order tags) "<picker-alpha>"))
                (list (caddr entry) (cadddr entry) '(alpha beta gamma))))
         sort-cases))
     ;; Equal first and second keys force comparison through the third key.
     (head:buffer-read-only-set! (buffer "<picker-alpha>") #t)
     (head:buffer-fact-set! (buffer "<picker-gamma>") 'modified-at (head:buffer-modified-at (buffer "<picker-alpha>")))
     (define compound-cases
       '((1 ("Modified¹↑") (beta alpha gamma))
         (2 ("Modified¹↑" "Flags²↑") (beta alpha gamma))
         (4 ("Modified¹↑" "Flags²↑" "Lines³↑") (beta alpha gamma))
         (4 ("Modified¹↑" "Flags²↑" "Lines³↓") (beta gamma alpha))
         (1 ("Modified¹↓" "Flags²↑" "Lines³↓") (gamma alpha beta))
         (1 ("Flags¹↑" "Lines²↓") (gamma alpha beta))
         (1 ("Flags¹↑" "Lines²↓" "Modified³↑") (gamma alpha beta))
         (4 ("Flags¹↑" "Modified²↑") (beta alpha gamma))
         (2 ("Flags¹↓" "Modified²↑") (beta alpha gamma))
         (2 ("Modified¹↑") (beta alpha gamma))
         (1 ("Modified¹↓") (alpha gamma beta))
         (1 () (alpha beta gamma))))
     (check 'compound-sort-retains-priority-renumbers-and-appends-reenabled-keys
       (sequence (lambda (entry) (sort! (car entry) (cadr entry))) compound-cases)
       (map (lambda (entry) (list #t #t (order (caddr entry)) "<picker-alpha>")) compound-cases))

     ;; Unsaved rows carry their clock and sort first; saving clears both.
     (head:buffer-read-only-set! (buffer "<picker-alpha>") #f)
     (press! "F1" "F1")                ; Modified descending
     (define modified-rows (names))
     (define clocks (map (lambda (name) (string:prefix? "03:04:05" (row-of name))) picker-all))
     (head:buffer-modified-set! (buffer "<picker-alpha>") #f) (head:refresh-visible-views!)
     (define saved-rows (names))
     (head:buffer-modified-set! (buffer "<picker-gamma>") #f) (head:refresh-visible-views!)
     (define clean-rows (list (names) (for-all (lambda (name) (string:prefix? "  " (row-of name))) picker-all)))
     (press! "F1")                     ; off
     (check 'modified-times-sort-unsaved-rows-first-and-clear-on-save
       (list clocks modified-rows saved-rows clean-rows (names))
       (list '(#t #f #t) (order '(alpha gamma beta)) (order '(gamma alpha beta)) (list picker-all #t) picker-all))

     ;; Global switching shares the table comparator, including compound
     ;; sorts, but does not get trapped by a panel filter. Reverse steps
     ;; retrace exactly; archived entries are never destinations.
     (check 'global-switching-follows-numeric-compound-order-in-both-directions
       (sequence
         (lambda (keys)
           (for-each buffet:toggle-sort-column! keys)
           (buffet:filter! "picker-gamma")
           (head:show-buffer! (buffer "<picker-beta>"))
           (let ([seen (sequence (lambda (step) (step) (head:buffer-name (head:current-buffer)))
                         (list buffet:next! buffet:next! buffet:previous! buffet:previous!))])
             (buffet:open!)
             (for-each (lambda (column)
                         (buffet:toggle-sort-column! column) (buffet:toggle-sort-column! column)) keys)
             seen))
         '((4) (4 2)))
       (make-list 2 (order '(alpha gamma alpha beta))))
     (buffet:filter! "picker-")
     (buffet:select! (buffer "<picker-alpha>"))

     ;; Identity: a rename keeps the selection, a kill leaves a live candidate.
     (head:buffer-name-set! (buffer "<picker-alpha>") "picker-delta") (head:refresh-visible-views!)
     (check 'renaming-the-selected-buffer-does-not-move-selection (selected) "<picker-delta>")
     (kill-buffer! (buffer "<picker-delta>")) (head:refresh-visible-views!)
     (check 'deleting-a-selected-buffer-leaves-a-live-candidate (and (member (selected) (names)) #t) #t)
     (press! "C-g")

     ;; A recreated switcher lists itself with current metadata; visiting a
     ;; row and retiring that buffer does not return to the switcher.
     (kill-buffer! (head:find-tool-buffer "*buffet*"))
     (buffet:open!)
     (check 'recreated-switcher-lists-itself-with-current-metadata
       (let* ([b (head:current-buffer)] [lines (head:buffer-lines b)]
              [needle (format "<buffet>  ~a" (vector-length lines))])
         (list (head:app-buffer? b) (head:buffer-selectable? b) (head:app-cursor-visible-in? (head:current-window))
               (= (vector-length lines) (+ 2 (length (head:buffers))))
               (exists (lambda (line) (and (string:search line needle 0 (string-length line)) #t))
                 (vector->list lines))))
       '(#t #f #f #t #t))

     ;; The trash: a killed shared buffer sits below the live rows under its
     ;; own heading, dimmed, and Enter on it restores.
     (define doomed (head:new-buffer! "picker-doomed"))
     (let* ([root (head:root)] [w (head:current-window)]
            [left (head:make-window doomed 0 0 0 0 0 8 0 30 'default)]
            [right (head:make-window doomed 0 0 0 0 0 8 31 30 'default)])
       (head:set-layout-root! (head:make-layout-split 'below w (head:make-layout-split 'right left right 1 1) 1 1))
       (buffet:open!) (buffet:select! doomed) (press! "C-k")
       (check 'c-k-trashes-the-candidate-and-retires-it-from-every-window
         (list (and (assoc "picker-doomed" (trash)) #t)
               (eq? (head:current-window) w) (head:app-buffer? (head:current-buffer))
               (exists (lambda (window) (eq? (head:window-buffer window) doomed)) (head:windows))
               (and (buffet:chosen) #t))
         '(#t #t #t #f #t))
       (head:set-layout-root! root))
     (check 'the-trash-lists-killed-shared-buffers-below-the-live-rows
       (let ([lines (rows)])
         (list (and (member "Trash" lines) #t)
               (and (row-of "picker-doomed") (string:search (row-of "picker-doomed") "trash" 0 (string-length (row-of "picker-doomed"))) #t)
               (< (length (memp (lambda (line) (string=? line "Trash")) lines)) (length lines))
               (head:buffer-named "picker-doomed")))
       '(#t #t #t #f))
     (press! "END" "RET")
     (check 'enter-on-a-trash-row-restores-the-buffer
       (list (head:buffer-name (head:current-buffer)) (and (head:buffer-named "picker-doomed") #t))
       '("picker-doomed" #t))
     (buffet:open!)
     (check 'a-restored-buffer-leaves-the-trash-section
       (and (member "Trash" (rows)) #t) #f)
     (press! "C-g")
     (type! "picker-beta") (press! "RET")
     (kill-buffer! (head:current-buffer))
     (check 'retiring-a-visited-buffer-does-not-return-to-the-switcher
       (list (eq? (head:current-buffer) (head:find-tool-buffer "*buffet*")) (and (memq (head:current-buffer) (head:buffers)) #t))
       '(#f #t))

     (head:show-buffer! origin)
     (kill-buffer! (buffer "<picker-gamma>"))
     (kernel:retract-module! 'picker-fixture)

     ;; The backups: a version a save wrote over sits under its own heading,
     ;; with the file it came from, and Enter restores it too
     (store:create! head:ui-actor "notes.txt.bak" '("old notes")
       `((trashed ,(time-second (current-time 'time-utc)) ,head:ui-actor) (backup "/tmp/notes.txt" #f "sha256:0")))
     (buffet:open!)
     (buffet:filter! "notes.txt.bak")
     (check 'the-backups-list-under-their-own-heading-with-the-file
       (let ([lines (rows)] [line (row-of "notes.txt.bak")])
         (list (and (member "Backups" lines) #t)
               (and line (string:search line "backup" 0 (string-length line)) #t)
               (and line (string:search line "/tmp/notes.txt" 0 (string-length line)) #t)
               (and (member "Trash" lines) #t)))
       '(#t #t #t #f))
     (press! "END" "RET")
     (check 'enter-on-a-backup-row-restores-it
       (list (head:buffer-name (head:current-buffer)) (vector->list (head:buffer-lines (head:current-buffer))))
       '("notes.txt.bak" ("old notes")))
     (buffet:open!)
     (check 'a-restored-backup-leaves-the-backups-section (and (member "Backups" (rows)) #t) #f)
     (press! "C-g")
     (kill-buffer! (buffer "notes.txt.bak"))
     ;; the app as an API: the filter set, a buffer chosen, the choice told
     (buffet:open!)
     (buffet:filter! "picker-b")
     (buffet:select! (buffer "<picker-beta>"))
     (check 'the-api-filters-selects-and-tells-the-choice
       (list (eq? (buffet:chosen) (buffer "<picker-beta>")) (test:raises? (lambda () (buffet:select! (buffer "<picker-gamma>")))))
       '(#t #t))
     ;; Flags have one typed vocabulary in the buffer and app APIs. Their
     ;; rendered strings sort lexicographically, without masking clocks.
     (define flag-cases
       '(("flag-none" () "") ("flag-conflict" (conflicted) "!!")
         ("flag-both" (conflicted read-only) "!! %") ("flag-read" (read-only) "%")))
     (for-each
       (lambda (entry)
         (let ([id (store:create! head:ui-actor (car entry) '("abc") '((base . "abc") (trailing . #f)))])
           (when (memq 'conflicted (cadr entry))
             (store:edit! head:ui-actor id 0 (text:make-span 0 0 0 3) '("mine"))
             (store:reload! head:ui-actor id '("disk") '((base . "disk") (trailing . #f))))
           (when (memq 'read-only (cadr entry)) (store:set-property! head:ui-actor id 'read-only #t)))) flag-cases)
     (head:before-frame!) (buffet:filter! "flag-")
     (check 'flags-enumerate-canonically-and-keep-the-modification-clock
       (sequence (lambda (entry)
                   (let ([b (head:buffer-named (car entry))])
                     (buffet:select! b)
                     (let ([cells (string-append (if (head:buffer-modified b)
                                                   (substring (row-of (car entry)) 0 8) "")
                                                 "  " (caddr entry) "  " (car entry))])
                       (list (head:buffer-flags b) (buffet:flags)
                             (string:prefix? cells (row-of (car entry)))
                             (edoc:type-accepts? '(list-of buffer-flag) (buffet:flags)))))) flag-cases)
       (map (lambda (entry) (list (cadr entry) (cadr entry) #t #t)) flag-cases))
     (check 'flags-sort-by-complete-marker-text
       (sequence (lambda (i)
                   (buffet:toggle-sort-column! 2)
                   (map (lambda (line) (car (find (lambda (entry) (string:search line (car entry) 0 (string-length line))) flag-cases))) (rows)))
         '(0 1 2))
       (list (map car flag-cases) (reverse (map car flag-cases)) '("flag-both" "flag-conflict" "flag-none" "flag-read")))

     (buffet:select! (head:buffer-named "flag-both"))
     (let* ([w (head:current-window)] [row (car (head:point))] [width (head:window-width w)])
       (define (marker)
         (find (lambda (range)
                 (and (eq? (car range) w) (= (cadr range) row)
                      (let ([face (list-ref range 4)]) (and (list? face) (memq 'error face)))))
           (paint:highlight-ranges)))
       (head:window-width-set! w 100) (head:refresh-visible-views!)
       (check 'conflict-marker-keeps-its-color-in-selection-and-hover-and-hides-with-flags
         (let ([selected (marker)])
           (parameterize ([head:app-event-buffer-position (cons row 0)]) (head:dispatch-app-event! "MOUSE-MOVE"))
           (head:set-mouse-position! '(1 . 1))
           (let ([hovered (marker)])
             (head:window-width-set! w 12) (head:refresh-visible-views!)
             (list (and selected (list-ref selected 4)) (and hovered (list-ref hovered 4))
                   (and selected hovered (= (caddr selected) (caddr hovered)) (> (caddr selected) 8))
                   (marker))))
         '((candidate error) (candidate-hover error) #t #f))
       (head:set-mouse-position! #f) (head:dispatch-app-event! "MOUSE-LEAVE")
       (head:window-width-set! w width) (head:refresh-visible-views!))

     ;; Wheel ticks scroll the viewport, even through section headings. The
     ;; focused document stays put when the panel receives an inactive tick;
     ;; a subsequent refresh must not pull the viewport to its old candidate.
     (let* ([root (head:root)] [panel (head:current-window)]
            [other (head:make-window origin 0 0 0 0 0 12 41 40 'default)])
       (do ([i 0 (+ i 1)]) ((= i 24))
         (store:create! head:ui-actor (format "scroll-trash-~2,'0d" i) '("")
           `((trashed ,(time-second (current-time 'time-utc)) ,head:ui-actor))))
       (head:set-layout-root! (head:make-layout-split 'right panel other 1 1))
       (buffet:filter! "scroll-trash-")
       (paint:set-screen-rows! 16) (paint:window-layout) (head:refresh-visible-views!)
       (check 'wheel-scrolls-active-and-inactive-panels-through-trash
         (sequence
           (lambda (focus)
             (head:set-current! focus)
             (let ([before (head:window-top panel)])
               (head:with-window panel
                 (parameterize ([head:app-event-focus focus])
                   (head:dispatch-app-event! "WHEEL-DOWN") (head:dispatch-app-event! "WHEEL-DOWN")))
               (head:refresh-visible-views!)
               (paint:scroll-window! panel (head:window-size panel))
               (list (> (head:window-top panel) before) (eq? (head:current-window) focus)
                     (eq? (head:window-buffer other) origin)
                     (head:with-window panel (string? (buffet:chosen))))))
           (list panel other))
         '((#t #t #t #t) (#t #t #t #t)))
       (head:set-current! panel) (head:set-layout-root! root) (paint:set-screen-rows! 24))

     ;; Permanent deletion is deliberately unavailable on live rows. The
     ;; same command removes either archived kind and keeps a valid choice.
     (store:create! head:ui-actor "delete-backup" '("old notes")
       `((trashed ,(time-second (current-time 'time-utc)) ,head:ui-actor) (backup "/tmp/notes.txt" #f "sha256:0")))
     (check 'permanent-delete-is-a-distinct-shifted-key
       (keymap:spec "C-x D") '("C-x" "D"))
     (check 'permanent-delete-removes-only-archived-candidates
       (sequence
         (lambda (name)
           (buffet:filter! name) (press! "END")
           (let ([id (store:find-named name)])
             ((keymap:binding 'buffet "C-x D")) (head:refresh-visible-views!)
             (list (store:exists? id) (buffet:chosen) (rows))))
         '("delete-backup" "scroll-trash-00"))
       '((#f #f ("No matching buffers")) (#f #f ("No matching buffers"))))
     (buffet:filter! "flag-none")
     (check 'permanent-delete-refuses-a-live-buffer
       (list (test:raises? buffet:delete!) (and (head:buffer-named "flag-none") #t)) '(#t #t))
     (test:finish! 'buffet)))
