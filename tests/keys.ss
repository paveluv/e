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
             (prefix (head paint) paint:)
             (prefix (head prompt) prompt:)
             (prefix (head window) window:)
             (prefix (state actor) actor:))

     (define check test:check)
     (define (bound-to context key) (let ([hit (keymap:resolved-binding context (list key))]) (and hit (keymap:binding-action (cdr hit)))))
     (edit-init!)
     (keys:init!)
     ;; a listing an older checkpoint brought back as a plain local buffer
     (define stale (head:new-local-buffer! "keys"))
     (head:add-buffer! stale)
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
     (keymap:bind-default! 'keys-test "C-k" end-of-line!)

     (check 'c-x-tab-and-c-x-s-tab-are-bound-to-the-helper
       (list (eq? (keymap:binding "C-x TAB") keys:show!) (eq? (keymap:binding "C-x S-TAB") keys:page-up!)) '(#t #t))
     (check 'the-prompts-keys-are-its-commands-and-c-x-tab-is-allowed-there
       (list (eq? (keymap:binding-action (cdr (keymap:resolved-binding 'prompt '("RET")))) prompt:accept!)
             (eq? (keymap:binding-action (cdr (keymap:resolved-binding 'prompt '("C-g")))) prompt:cancel!)
             (keymap:action-text (keymap:binding-action (cdr (keymap:resolved-binding 'prompt '("SELF-INSERT")))))
             (prompt:allowed? keys:show!))
       (list #t #t "(prompt:type! (head:typed-text))" #t))
     (keys:show!)
     (paint:window-layout) ; tiled, the pop-up has its geometry for the painter's clamp below
     (head:before-frame!)
     (check 'a-stale-listing-is-dropped-and-the-fresh-one-is-named-plainly-and-kept-out-of-checkpoints
       (list (memq stale (head:buffers)) (head:buffer-name (view)) (head:buffer-fact (view) 'resume-kind #f)
             (head:window-scrollbar? (head:popup)) (string:prefix? "page 1 of " (head:buffer-status (view) popup))
             ;; the bar names no key: the listing is the reference
             (not (string:search (head:buffer-status (view) popup) "C-x" 0 (string-length (head:buffer-status (view) popup))))
             (begin (head:checkpoint!)
                    (exists (lambda (entry) (let ([r (car entry)]) (and (pair? r) (eq? (car r) 'local) (equal? (cadr r) "<keys>"))))
                            (list-ref (actor:checkpoint head:ui-actor) 4))))
       '(#f "<keys>" keys right #t #t #f))
     (check 'the-listing-is-a-read-only-keys-buffer-in-the-pop-up-with-the-mode-section-first
       (let ([at (index-of "M-q")])
         (list (head:buffer-name (view)) (head:buffer-read-only (view)) (mode:name-of (view)) (> (head:popup-rows) 0)
               (index-of "keys-test keys") (< 0 at (index-of "Global keys"))
               (contains? (line-at at) "kill-line!") (contains? (line-at at) "Kill from point")))
       (list "<keys>" #t "keys" #t 0 #t #t #t))
     (check 'a-long-description-wraps-in-its-column-and-keys-running-one-command-share-the-row
       (let ([at (index-of "M-q")])
         ;; the wrapped description runs on below the first line, however narrow the column
         (list (contains? (line-at (+ at 1)) "M-w") (not (contains? (line-at (+ at 1)) "kill-line!"))
               (exists (lambda (i) (contains? (line-at i) "accumulate")) (map (lambda (k) (+ at k)) (iota 8)))
               (char=? (string-ref (line-at (+ at 1)) 0) #\space)))
       '(#t #t #t #t))
     (check 'the-keys-of-a-group-are-joined-by-a-line-in-the-margin
       (let* ([at (index-of "M-q")] [styles (mode:line-styles (view))])
         (list (substring (line-at at) 0 2) (substring (line-at (+ at 1)) 0 2) (vector-ref (styles (line-at at)) 1)
               (substring (line-at (index-of "RET")) 0 2)))
       '(" ╷" " ╵" chrome "  "))
     (check 'a-global-key-the-mode-takes-is-left-out-of-the-global-section
       (let ([global (index-of "Global keys")])
         (list (and (index-of "C-k") (< (index-of "C-k") global))
               (exists (lambda (l) (contains? l "  C-k ")) (list-tail (lines) global))
               (exists (lambda (l) (contains? l "  C-_ ")) (list-tail (lines) global))))
       '(#t #f #t))
     (check 'a-row-with-a-short-description-takes-one-line
       (let ([at (index-of "RET")]) (list (contains? (line-at at) "beginning-of-line!") (heading-or-key? (line-at (+ at 1)))))
       '(#t #t))
     (check 'a-key-bound-to-a-lambda-shows-as-the-anonymous-command-it-is
       (let ([at (index-of "M-z")]) (and at (contains? (line-at at) "anonymous command"))) #t)
     (check 'typing-lists-as-any-character-running-type-with-the-typed-text
       (let ([at (index-of "any character")])
         (and at (list (contains? (line-at at) "type!") (contains? (line-at at) "(head:typed-text)") (contains? (line-at at) "Type text"))))
       '(#t #t #t))
     (check 'a-call-with-a-constant-argument-reads-as-the-call
       (let ([text (keymap:action-text (keymap:call beginning-of-line! 3))])
         (list (contains? text "beginning-of-line!") (string:suffix? " 3)" text)))
       '(#t #t))
     (check 'section-titles-are-bold-and-rows-plain
       (let ([styles (mode:line-styles (view))])
         (list (vector-ref (styles (line-at 0)) 0) (vector-ref (styles (line-at 1)) 0)))
       '(bold plain))
     (check 'the-whole-listing-is-in-the-buffer (> (length (lines)) (head:popup-rows)) #t)

     ;; a narrower terminal lays the listing out again for the new width
     ;; before the frame paints, the pane's place kept
     (define wide (length (lines)))
     (paint:set-screen-cols! 60)
     (paint:window-layout)
     (head:before-frame!)
     (check 'a-resize-lays-the-listing-out-again-for-the-new-width
       (list (for-all (lambda (l) (< (string-length l) (head:window-content-width popup))) (lines)) (> (length (lines)) wide))
       '(#t #t))
     (paint:set-screen-cols! 80)
     (paint:window-layout)
     (head:before-frame!)
     (check 'and-back-again-when-it-widens (length (lines)) wide)

     ;; C-x TAB pages the pop-up down from anywhere, back to the top past the end
     (define size (head:popup-rows))
     (define (page-of)
       (let* ([s (head:buffer-status (view) popup)] [from (+ 5 (string:search s "page " 0 (string-length s)))])
         (substring s from (string-length s))))
     (define pages (string->number (list-ref (let loop ([s (page-of)] [out '()]) (cond [(string:search s " " 0 (string-length s)) => (lambda (i) (loop (substring s (+ i 1) (string-length s)) (cons (substring s 0 i) out)))] [else (reverse (cons s out))])) 2)))
     (check 'the-bar-counts-the-pages-of-the-listing (list (> pages 1) (page-of)) (list #t (format "1 of ~a" pages)))
     ;; the painter's clamp keeps point a margin from the edges: paging must
     ;; leave the top where it put it once a frame has clamped
     (define (clamp!) (paint:scroll-window! popup (head:popup-rows)))
     (keys:page-up!) (clamp!)
     (check 'c-x-s-tab-at-the-top-shows-the-last-page (page-of) (format "~a of ~a" pages pages))
     (keys:show!) (clamp!)
     (check 'and-c-x-tab-then-shows-the-first (page-of) (format "1 of ~a" pages))
     (keys:show!) (clamp!)
     (check 'the-next-page-down-is-the-second (list (page-of) (head:window-top popup)) (list (format "2 of ~a" pages) size))
     (keys:show!) (clamp!)
     (check 'and-the-one-after-is-the-third (page-of) (format "3 of ~a" pages))
     (keys:page-up!) (clamp!)
     (keys:page-up!) ; back to the top
     (keys:page-up!)
     (check 'c-x-s-tab-at-the-top-goes-to-the-last-page
       (list (> (head:window-top popup) 0) (= 0 (mod (head:window-top popup) size))) '(#t #t))
     (keys:show!)
     (check 'and-c-x-tab-past-the-end-returns-to-the-top (head:window-top popup) 0)
     (keys:show!)
     (check 'c-x-tab-again-pages-the-listing-down (list (head:window-top popup) (eq? (head:current-window) w1)) (list size #t))
     (let loop ([n 0]) (when (and (> (head:window-top popup) 0) (< n (+ 2 (quotient (length (lines)) size)))) (keys:show!) (loop (+ n 1))))
     (check 'past-the-end-it-returns-to-the-top (head:window-top popup) 0)

     ;; where the text is read-only the editing commands are left out
     (head:buffer-read-only-set! b #t)
     (head:show-buffer! b)
     (head:before-frame!)
     (check 'a-read-only-buffer-lists-no-editing-command
       (let ([global (list-tail (lines) (index-of "Global keys"))])
         (list (exists (lambda (l) (contains? l "  C-_ ")) global) (exists (lambda (l) (contains? l "kill-line!")) global)
               (exists (lambda (l) (contains? l "beginning-of-buffer!")) global)))
       '(#f #f #t))
     (head:buffer-read-only-set! b #f)

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

     ;; the listing opens in an ordinary window too, for the buffer shown there,
     ;; C-x TAB pages it there, and hiding takes it away
     (window:focus! w1)
     (head:show-buffer! b)
     (keys:open!)
     (paint:window-layout) ; tiled, the window reports its width
     (check 'the-listing-opens-in-the-current-window-for-its-buffer
       (list (head:buffer-name (head:current-buffer)) (head:buffer-line (head:current-buffer) 0) (head:popup-rows) (head:window-top w1)
             ;; every line fits the window that shows it
             (for-all (lambda (l) (< (string-length l) (head:window-content-width w1))) (vector->list (head:buffer-lines (head:current-buffer)))))
       '("<keys>" "keys-test keys" 0 0 #t))
     (keys:show!)
     (check 'c-x-tab-pages-the-listing-in-its-window (list (> (head:window-top w1) 0) (head:popup-rows)) '(#t 0))
     (keys:hide!)
     (check 'hiding-takes-the-listing-out-of-the-window
       (list (head:buffer-named "<keys>") (head:buffer-name (head:window-buffer w1))) '(#f "keyed"))
     ;; ESC and C-g in the listing return the window to what it showed: the
     ;; pop-up over a buffer shows that buffer again, over nothing it hides
     (head:set-window-buffer! (head:popup) b)
     (head:show-popup! (head:popup-default-rows))
     (window:focus! (head:popup))
     (keys:show!)
     (check 'esc-in-the-listing-returns-the-pop-up-to-what-it-showed
       (list (head:buffer-name (head:current-buffer)) (eq? (bound-to 'keys "ESC") keys:return!)
             (begin (keys:return!) (head:buffer-name (head:window-buffer (head:popup)))) (> (head:popup-rows) 0))
       '("<keys>" #t "keyed" #t))
     (window:clear-pop-up!)
     (window:focus! w1)
     (keys:show!)
     (check 'esc-in-the-listing-over-nothing-hides-the-pop-up
       (begin (window:focus! (head:popup)) (keys:return!) (list (head:popup-rows) (head:buffer-named "<keys>"))) '(0 #f))
     (window:focus! w1)

     ;; any buffer may carry its own status text, with the window when the
     ;; provider takes it, and taken away with #f
     (check 'a-buffer-carries-its-own-status-text-for-the-window-painted
       (let ([w1 (head:current-window)])
         (list (begin (head:set-buffer-status! b (lambda (b w) (format "w~a" (head:window-index w)))) (head:buffer-status b w1))
               (begin (head:set-buffer-status! b head:buffer-name) (head:buffer-status b w1))
               (begin (head:set-buffer-status! b #f) (head:buffer-status b w1))))
       (list (format "w~a" (head:window-index (head:current-window))) "keyed" #f))

     ;; the long key names show short and bind under either spelling
     (check 'long-key-names-show-short
       (list (keymap:sequence-text (keymap:spec "M-BACKSPACE")) (keymap:sequence-text (keymap:spec "BS")) (keymap:spec "BS")
             (keymap:sequence-text (keymap:spec "PGDN")) (keymap:spec "PGUP") (keymap:sequence-text (keymap:spec "DELETE"))
             (keymap:sequence-text (keymap:spec "C-M-SPC")) (keymap:sequence-text (keymap:spec "SPC")))
       '("M-BS" "BS" ("BACKSPACE") "PGDN" ("PAGEUP") "DEL" "C-M-SPC" "SPC"))
     ;; an app in the pop-up, the user in it: C-x TAB lists that app's keys,
     ;; not the keys of the window selected before, and keeps listing them
     ;; while the pop-up stays current
     (let ([k (head:buffer-named "<keys>")]) (when k (kill-buffer! k)))
     (define app (head:new-local-buffer! "popped app"))
     (head:with-buffer app (mode:choose! "keys-test"))
     (head:set-window-buffer! popup app)
     (head:show-popup! 8)
     (head:set-current! popup)
     (keys:show!)
     (head:before-frame!)
     (check 'c-x-tab-in-the-pop-up-lists-the-pop-ups-apps-keys
       (list (eq? (head:window-buffer popup) (view)) (index-of "keys-test keys") (< 0 (index-of "Global keys")) (eq? (head:current-window) popup))
       '(#t 0 #t #t))
     (head:set-current! w1)

     (test:finish! 'keys)))
