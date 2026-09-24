#!/usr/bin/env scheme-script

;; The keys helper: C-x TAB lists the current buffer's keys in the pop-up,
;; the app's declared keys first, then its mode contexts' bindings, then
;; the global ones, with the command and its documentation; again turns
;; the page, past the last page hides; the ↓ clears it too. Headless, the
;; editor's key table installed.

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
     (define (contains? s part) (and (string:search s part 0 (string-length s)) #t))
     (define (popup-name) (head:buffer-name (head:window-buffer (head:popup))))
     (define (popup-lines) (vector->list (head:buffer-lines (head:window-buffer (head:popup)))))
     (define (row-with part) (find (lambda (l) (contains? l part)) (popup-lines)))
     (define (index-of part) (let loop ([ls (popup-lines)] [i 0]) (cond [(null? ls) #f] [(contains? (car ls) part) i] [else (loop (cdr ls) (+ i 1))])))
     (define b (head:new-buffer! "keyed"))
     (head:show-buffer! b)
     (head:buffer-fact-set! b 'keys '(("RET" "open" "Open the row")))
     (mode:register! "keys-test" '() '() (lambda (line) #f))
     (head:with-buffer b (mode:choose! "keys-test"))
     (keymap:bind-default! 'keys-test "M-q" kill-line!)

     (check 'c-x-tab-is-bound-to-the-helper (eq? (keymap:binding "C-x TAB") keys:show!) #t)
     (keys:show!)
     (head:before-frame!)
     (check 'the-listing-shows-app-keys-then-the-modes-then-the-global-ones
       (list (popup-name) (> (head:popup-rows) 0)
             (index-of "keyed keys") (contains? (list-ref (popup-lines) 1) "RET") (contains? (list-ref (popup-lines) 1) "open  Open the row")
             ;; headless the command is also bound unprefixed, and that name is found first
             (index-of "keys-test keys") (contains? (list-ref (popup-lines) 3) "M-q") (contains? (list-ref (popup-lines) 3) "kill-line!")
             (contains? (list-ref (popup-lines) 3) "Kill from point to the end of the line") (index-of "Global keys"))
       '("<keys>" #t 0 #t #t 2 #t #t #t 4))
     (define first-page (popup-lines))
     (keys:show!)
     (check 'c-x-tab-again-turns-the-page (list (popup-name) (equal? (popup-lines) first-page)) '("<keys>" #f))
     (keys:hide!)
     (head:before-frame!)
     (check 'hiding-puts-the-pop-up-away-and-drops-the-view
       (list (head:popup-rows) (popup-name) (head:buffer-named "<keys>")) '(0 "<pop-up>" #f))
     (keys:show!)
     (window:clear-pop-up!)
     (head:before-frame!)
     (check 'clearing-the-pop-up-drops-the-view-too (list (head:popup-rows) (head:buffer-named "<keys>")) '(0 #f))
     (test:finish! 'keys)))
