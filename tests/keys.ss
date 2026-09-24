#!/usr/bin/env scheme-script

;; The keys helper: C-x TAB shows the active window's keys in the pop-up as
;; the read-only buffer <keys>, the app's declared keys first, then its mode
;; contexts' bindings, then the global ones, keys running one command
;; sharing a row, descriptions wrapped, titles bold; again pages down and
;; wraps to the top; the listing follows the active window; the pop-up is
;; selectable and read-only while shown and gives focus back when hidden.
;; Headless, the editor's key table installed.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (rename (head edit) (init! edit-init!))
             (head literal)
             (prefix (apps keys) keys:)
             (prefix (foundation string) string:)
             (prefix (head head) head:)
             (prefix (head keymap) keymap:)
             (prefix (head mode) mode:)
             (prefix (head window) window:))

     (define check test:check)
     (edit-init!)
     (keys:init!)
     (define popup (head:popup))
     (define (contains? s part) (and (string:search s part 0 (string-length s)) #t))
     (define (view) (head:window-buffer popup))
     (define (lines) (vector->list (head:buffer-lines (view))))
     (define (index-of part) (let loop ([ls (lines)] [i 0]) (cond [(null? ls) #f] [(contains? (car ls) part) i] [else (loop (cdr ls) (+ i 1))])))
     (define (line-at i) (list-ref (lines) i))
     (define (heading-or-key? line) (or (not (char=? (string-ref line 0) #\space)) (not (char=? (string-ref line 2) #\space))))
     (define w1 (head:current-window))
     (define b (head:new-buffer! "keyed"))
     (head:show-buffer! b)
     (mode:register! "keys-test" '() '() (lambda (line) #f))
     (head:with-buffer b (mode:choose! "keys-test"))
     (keymap:bind-default! 'keys-test "M-q" kill-line!)
     (keymap:bind-default! 'keys-test "M-w" kill-line!)
     (keymap:bind-default! 'keys-test "M-z" (lambda () #f))
     (keymap:bind-default! 'keys-test "RET" beginning-of-line!)

     (check 'c-x-tab-is-bound-to-the-helper (eq? (keymap:binding "C-x TAB") keys:show!) #t)
     (keys:show!)
     (head:before-frame!)
     (check 'the-listing-is-a-read-only-keys-buffer-in-the-pop-up-with-the-mode-section-first
       (list (head:buffer-name (view)) (head:buffer-read-only (view)) (mode:name-of (view)) (> (head:popup-rows) 0)
             (index-of "keys-test keys") (< 0 (index-of "Global keys"))
             (contains? (line-at 1) "M-q") (contains? (line-at 1) "kill-line!") (contains? (line-at 1) "Kill from point"))
       (list "<keys>" #t "keys" #t 0 #t #t #t #t))
     (check 'a-long-description-wraps-in-its-column-and-keys-running-one-command-share-the-row
       (let ([at (index-of "M-q")])
         ;; the wrapped description runs on below the first line, however narrow the column
         (list (contains? (line-at (+ at 1)) "M-w") (not (contains? (line-at (+ at 1)) "kill-line!"))
               (exists (lambda (i) (contains? (line-at i) "accumulate")) (map (lambda (k) (+ at k)) (iota 8)))
               (string:prefix? "  " (line-at (+ at 1)))))
       '(#t #t #t #t))
     (check 'a-row-with-a-short-description-takes-one-line
       (let ([at (index-of "RET")]) (list (contains? (line-at at) "beginning-of-line!") (heading-or-key? (line-at (+ at 1)))))
       '(#t #t))
     (check 'a-key-bound-to-an-anonymous-command-is-left-out (index-of "M-z") #f)
     (check 'section-titles-are-bold-and-rows-plain
       (let ([styles (mode:line-styles (view))])
         (list (vector-ref (styles (line-at 0)) 0) (vector-ref (styles (line-at 1)) 0)))
       '(bold plain))
     (check 'the-whole-listing-is-in-the-buffer (> (length (lines)) (head:popup-rows)) #t)

     ;; C-x TAB pages the pop-up down from anywhere, back to the top past the end
     (define size (head:popup-rows))
     (keys:show!)
     (check 'c-x-tab-again-pages-the-listing-down (list (head:window-top popup) (eq? (head:current-window) w1)) (list size #t))
     (let loop ([n 0]) (when (and (> (head:window-top popup) 0) (< n (+ 2 (quotient (length (lines)) size)))) (keys:show!) (loop (+ n 1))))
     (check 'past-the-end-it-returns-to-the-top (head:window-top popup) 0)

     ;; the listing follows the active window
     (define other (head:new-buffer! "other"))
     (mode:register! "keys-other" '() '() (lambda (line) #f))
     (head:with-buffer other (mode:choose! "keys-other"))
     (keymap:bind-default! 'keys-other "F9" kill-line!)
     (head:show-buffer! other)
     (head:before-frame!)
     (check 'the-listing-follows-the-active-window (list (line-at 0) (contains? (line-at 1) "F9")) '("keys-other keys" #t))
     ;; a capturing context counts for an app buffer only, and lists what it takes
     (define captured (head:register-view! (head:new-local-buffer! "captured") void))
     (mode:register! "keys-cap" '() '() (lambda (line) #f))
     (mode:choose! "keys-cap" captured)
     (keymap:set-context-capture! 'keys-cap "C-]" beginning-of-line! '("C-x" "M-x"))
     (head:show-buffer! captured)
     (head:before-frame!)
     (check 'a-capturing-context-says-what-it-takes
       (let ([at (index-of "other keys")])
         (list (and at #t) (contains? (line-at at) "to the app") (exists (lambda (i) (contains? (line-at i) "C-x and M-x")) (map (lambda (k) (+ at k)) (iota 4)))))
       '(#t #t #t))

     ;; the shown pop-up is selectable, read-only, and gives focus back when hidden
     (check 'the-shown-pop-up-can-be-selected-and-is-read-only
       (list (window:focus! popup) (eq? (head:current-window) popup) (guard (ex [else 'refused]) (insert-text! "x"))
             (eq? (window:focus-next!) w1) (begin (window:focus! popup) (eq? (head:current-window) popup)))
       (list #t #t 'refused #t #t))
     (keys:hide!)
     (head:before-frame!)
     (check 'hiding-gives-focus-back-and-drops-the-view
       (list (head:popup-rows) (head:buffer-name (view)) (head:buffer-named "<keys>") (eq? (head:current-window) w1))
       '(0 "<pop-up>" #f #t))
     (check 'the-hidden-pop-up-is-not-selectable (list (window:focus! popup) (eq? (head:current-window) w1)) '(#f #t))
     (keys:show!)
     (window:clear-pop-up!)
     (head:before-frame!)
     (check 'clearing-the-pop-up-drops-the-view-too (list (head:popup-rows) (head:buffer-named "<keys>")) '(0 #f))

     ;; the long key names show short and bind under either spelling
     (check 'long-key-names-show-short
       (list (keymap:sequence-text (keymap:spec "M-BACKSPACE")) (keymap:sequence-text (keymap:spec "BS")) (keymap:spec "BS")
             (keymap:sequence-text (keymap:spec "PGDN")) (keymap:spec "PGUP") (keymap:sequence-text (keymap:spec "DELETE"))
             (keymap:sequence-text (keymap:spec "C-M-SPC")) (keymap:sequence-text (keymap:spec "SPC")))
       '("M-BS" "BS" ("BACKSPACE") "PGDN" ("PAGEUP") "DEL" "C-M-SPC" "SPC"))
     (test:finish! 'keys)))
