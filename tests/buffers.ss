#!/usr/bin/env scheme-script

;; The <buffers> app as a table: one live model behind switching, filtering
;; by name or path, ordered ascending/descending/off sorts with compound
;; priorities, modification clocks, and identity through renames, kills and
;; recreation. Headless: the app handler receives the events the dispatcher
;; would send. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (edit) init!) (literal) (prefix (head) head:) (prefix (mode) mode:) (prefix (kernel) kernel:)
             (prefix (string) string:) (prefix (test) test:))

     (define check test:check)
     (putenv "TZ" "UTC")               ; deterministic clock cells
     (define (press! . events) (for-each head:dispatch-app-event! events))
     (define (type! text) (for-each (lambda (c) (head:dispatch-app-event! (string c))) (string->list text)))
     (define (sequence proc items)
       ;; map does not promise effect order; event sequences do.
       (reverse (fold-left (lambda (acc item) (cons (proc item) acc)) '() items)))
     (define (rows) (cddr (vector->list (head:buffer-lines (current-buffer)))))
     (define (heading) (vector-ref (head:buffer-lines (current-buffer)) 1))
     (define (name-in line)
       (let ([start (string:search line "<" 0 (string-length line))])
         (and start (substring line start (+ 1 (string:search line ">" start (string-length line)))))))
     (define (names) (map name-in (rows)))
     (define (selected) (name-in (buffer-line (current-buffer) (car (point)))))
     (define (state) (list (head:buffer-name (current-buffer)) (point)))
     (define (order tags) (map (lambda (tag) (format "<picker-~a>" tag)) tags))
     (define picker-all (order '(alpha beta gamma)))
     (define (row-of name)
       (find (lambda (line) (string:search line name 0 (string-length line))) (rows)))

     (define origin (current-buffer))
     (parameterize ([kernel:registering-module 'picker-fixture])
       (for-each (lambda (name) (mode:register! name '() '() (lambda (line) #f))) '("pick-a" "pick-z")))
     (for-each
       (lambda (entry)
         (let ([b (head:new-local-buffer (car entry))])
           (head:buffer-lines-set! b (make-vector (cadr entry) (car entry)))
           (head:buffer-file-set! b (caddr entry))
           (mode:choose! b (cadddr entry))
           (head:add-buffer! b)))
       '(("picker-alpha" 20 "/project/zebra/long/日本語 alpha.txt" "pick-z")
         ("picker-beta" 9 "/project/alpha/src/beta.ss" "pick-a")
         ("picker-gamma" 100 #f #f)))
     (show-buffer! (buffer "<picker-alpha>")) (goto-point! '(3 . 2))
     (show-buffer! (buffer "<picker-beta>")) (goto-point! '(4 . 1))

     ;; Enter initially selects the previous document; the app itself never
     ;; becomes the default, even after repeated quick switches.
     (check 'switching-defaults-to-the-previous-document
       (sequence (lambda (i) (list-buffers!) (press! "RET") (state)) (iota 4))
       '(("<picker-alpha>" (3 . 2)) ("<picker-beta>" (4 . 1)) ("<picker-alpha>" (3 . 2)) ("<picker-beta>" (4 . 1))))

     (list-buffers!)
     (check 'filter-matches-names-and-full-paths-without-case-or-prefix-restrictions
       (sequence (lambda (needle) (press! "C-u") (type! needle) (list (length (rows)) (name-in (car (rows)))))
         '("kEr-BeTa" "project/alpha/src" "日本語"))
       '((1 "<picker-beta>") (1 "<picker-beta>") (1 "<picker-alpha>")))
     (press! "C-u") (type! "no-such-buffer") (press! "RET")
     (check 'empty-results-never-create-a-buffer-or-leave-the-list
       (list (rows) (head:buffer-name (current-buffer)) (head:buffer-named "no-such-buffer"))
       '(("No matching buffers") "<buffers>" #f))
     (press! "C-g")
     (check 'cancel-restores-the-origin-and-its-point (state) '("<picker-beta>" (4 . 1)))

     ;; Sorting cycles ascending, descending and off on real values without
     ;; moving the keyboard candidate.
     (for-each (lambda (name) (head:buffer-modified-set! (buffer name) #t)) '("<picker-alpha>" "<picker-gamma>"))
     (head:buffer-fact-set! (buffer "<picker-alpha>") 'modified-at 1704164645000000900)
     (head:buffer-fact-set! (buffer "<picker-gamma>") 'modified-at 1704078245000000900)
     (for-each (lambda (name) (head:buffer-read-only-set! (buffer name) #t)) '("<picker-beta>" "<picker-gamma>"))
     (list-buffers!) (type! "picker-")
     (define (sort! key labels)
       ;; One snapshot covers the heading marks, row order and selection.
       (press! (format "F~a" key))
       (let ([heading (heading)])
         (list (for-all (lambda (label) (and (string:search heading label 0 (string-length heading)) #t)) labels)
               (= (length labels) (length (filter (lambda (c) (memv c '(#\↑ #\↓))) (string->list heading))))
               (names) (selected))))
     (define sort-cases
       '((1 "Modified" (beta gamma alpha) (alpha gamma beta))
         (2 "RO" (alpha beta gamma) (beta gamma alpha))
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
         (2 ("Modified¹↑" "RO²↑") (beta alpha gamma))
         (4 ("Modified¹↑" "RO²↑" "Lines³↑") (beta alpha gamma))
         (4 ("Modified¹↑" "RO²↑" "Lines³↓") (beta gamma alpha))
         (1 ("Modified¹↓" "RO²↑" "Lines³↓") (gamma alpha beta))
         (1 ("RO¹↑" "Lines²↓") (gamma alpha beta))
         (1 ("RO¹↑" "Lines²↓" "Modified³↑") (gamma alpha beta))
         (4 ("RO¹↑" "Modified²↑") (beta alpha gamma))
         (2 ("RO¹↓" "Modified²↑") (beta alpha gamma))
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

     ;; Identity: a rename keeps the selection, a kill leaves a live candidate.
     (set-buffer-name! (buffer "<picker-alpha>") "picker-delta") (head:refresh-visible-views!)
     (check 'renaming-the-selected-buffer-does-not-move-selection (selected) "<picker-delta>")
     (kill-buffer! (buffer "<picker-delta>")) (head:refresh-visible-views!)
     (check 'deleting-a-selected-buffer-leaves-a-live-candidate (and (member (selected) (names)) #t) #t)
     (press! "C-g")

     ;; A recreated switcher lists itself with current metadata; visiting a
     ;; row and retiring that buffer does not return to the switcher.
     (kill-buffer! (head:find-tool-buffer "*buffers*"))
     (list-buffers!)
     (check 'recreated-switcher-lists-itself-with-current-metadata
       (let* ([b (current-buffer)] [lines (head:buffer-lines b)]
              [needle (format "<buffers>  ~a" (vector-length lines))])
         (list (head:app-buffer? b) (head:buffer-selectable? b) (head:app-cursor-visible-in? (selected-window))
               (= (vector-length lines) (+ 2 (length (buffer-list))))
               (exists (lambda (line) (and (string:search line needle 0 (string-length line)) #t))
                 (vector->list lines))))
       '(#t #f #f #t #t))
     (type! "picker-beta") (press! "RET")
     (kill-buffer! (current-buffer))
     (check 'retiring-a-visited-buffer-does-not-return-to-the-switcher
       (list (eq? (current-buffer) (head:find-tool-buffer "*buffers*")) (and (memq (current-buffer) (buffer-list)) #t))
       '(#f #t))

     (show-buffer! origin)
     (kill-buffer! (buffer "<picker-gamma>"))
     (kernel:retract-module! 'picker-fixture)
     (test:finish! 'buffers)))
