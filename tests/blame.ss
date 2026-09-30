#!/usr/bin/env scheme-script

;; Blame: another actor's fresh ink is tinted in that actor's face while its
;; text survives, tints stay bounded and expire on their deadline, and the
;; author at point is reported from the store's delta log. Headless: the
;; two nested editors consume the same acquired source at different widths.
;; Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (head edit) init!) (prefix (head head) head:) (prefix (head window) window:) (prefix (head widget) widget:)
             (prefix (only (head edit) init!) edit:) (prefix (state store) store:) (prefix (foundation text) text:)
             (prefix (head layout) layout:) (prefix (state view) view:) (prefix (core kernel) kernel:) (prefix (service log) log:)
             (prefix (foundation string) string:) (prefix (test) test:))

     (define check test:check)
     (widget:init!) (edit:init!) (window:init!)
     ;; Load through the kernel so a reload below replaces the real module.
     ;; Fresh procedures resolve through the top level after that reload.
     (kernel:load-module! "blame")
     (define (tint-seconds! seconds) ((top-level-value 'blame:tint-seconds) seconds))
     (define (at-point!) ((top-level-value 'blame:at-point!) a))

     (define id (store:create! head:ui-actor "blame-naming" '("")))
     (define a (create-view! head:ui-actor id '()))
     (define b (create-view! head:ui-actor id '()))
     (define root (view:create! head:ui-actor #f 'blame-fixture 1 '() '()))
     (define faces '(blame-1 blame-2 blame-3 blame-4 blame-5 blame-6))
     (widget:register! 'blame-fixture 1 (layout:container 'x))
     (view:arrange! head:ui-actor (list (list root 0 (list (list 'a a '(grow 1)) (list 'b b '(grow 2))) '())) '())
     (widget:mount! root 'nested-blame)
     (define (pump!)
       (widget:pump!)
       (widget:present! (list (list (widget:prepare! root 120 8) 0 0))))
     (define (ranges . target)
       (pump!)
       (let ([f (widget:prepared (if (null? target) a (car target)))])
         (apply append
           (map (lambda (line row)
                  (let ([styles (widget:frame-styles f row line)])
                    (let loop ([i 0] [start #f] [out '()])
                      (let ([ink? (and (< i (vector-length styles))
                                       (let ([s (vector-ref styles i)])
                                         (or (memq s faces) (and (list? s) (exists (lambda (face) (memq face s)) faces)))) )])
                        (cond [(= i (vector-length styles)) (reverse (if start (cons (list row start i) out) out))]
                          [ink? (loop (+ i 1) (or start i) out)]
                          [else (loop (+ i 1) #f (if start (cons (list row start i) out) out))])))))
             (widget:frame-lines f) (iota (length (widget:frame-lines f)))))))
     (pump!)

     ;; Tints follow surviving content. An overlap drops only that tint;
     ;; own/app edits add none, while new collaborator ink gets its own range.
     (check 'blame-keeps-surviving-ink-and-all-authors
       (map
         (lambda (example)
           (store:reset! head:ui-actor id '("abcd" "base"))
           (pump!)
           (store:edit! '(agent seed) id (store:revision id) (text:make-span 0 1 0 1) '("INK"))
           (store:edit! '(agent seed) id (store:revision id) (text:make-span 1 0 1 0) '("KEPT"))
           (pump!)
           (store:edit! (car example) id (store:revision id) (apply text:make-span (cadr example)) (caddr example))
           (pump!)
           (list (ranges) (equal? (cadar (store:blame id 1)) (car example))))
         `((,head:ui-actor (0 2 0 2) ("X"))       ; inside
           (,head:ui-actor (0 1 0 1) ("X"))       ; left boundary
           (,head:ui-actor (0 4 0 4) ("X"))       ; right boundary
           (,head:ui-actor (0 0 0 0) ("" "x"))   ; before, changing rows/columns
           (,head:ui-actor (1 7 1 7) ("X"))       ; after both ranges
           (,head:ui-actor (0 0 0 2) ("X"))       ; partial replacement
           (,head:ui-actor (0 2 0 3) (""))        ; deletion inside
           ((app producer) (0 2 0 2) ("X"))
           ((head "rival") (0 2 0 2) ("X"))
           ((agent rival) (0 2 0 2) ("X"))))
       '((((1 0 4)) #t) (((0 2 5) (1 0 4)) #t) (((0 1 4) (1 0 4)) #t)
         (((1 2 5) (2 0 4)) #t) (((0 1 4) (1 0 4)) #t) (((1 0 4)) #t)
         (((1 0 4)) #t) (((1 0 4)) #t) (((0 2 3) (1 0 4)) #t) (((0 2 3) (1 0 4)) #t)))

     ;; The overlay cap must bound all fade work, including after reset,
     ;; a real reload and retirement. Avoid counting Chez's GC helpers as
     ;; editor workers during this small burst; count native tasks on Linux.
     (check 'blame-burst-keeps-only-newest-ink-without-workers
       (map
         (lambda (ending)
           (tint-seconds! 8)
           (store:reset! head:ui-actor id '(""))
           (pump!)
           (parameterize ([collect-trip-bytes (* 128 1024 1024)])
             (let* ([count (lambda () (and (file-directory? "/proc/self/task")
                                        (length (directory-list "/proc/self/task"))))]
                    [before (count)])
               (do ([i 0 (+ i 1)]) ((= i 160))
                 (store:edit! '(agent burst) id (store:revision id) (text:make-span 0 0 0 0) '("x")))
               (pump!)
               (let ([ink (ranges)] [bounded? (or (not before) (<= (count) before))])
                 (case ending
                   [(reset) (store:reset! '(agent burst) id '(""))]
                   [(reload) (kernel:reload-module! "blame")]
                   [(retire) (store:set-property! head:ui-actor id 'audience '())])
                 (pump!)
                 (let ([cleared (ranges)])
                   (when (eq? ending 'retire)
                     (store:set-property! head:ui-actor id 'audience 'all)
                     (pump!)
                     (void))
                   (list ink bounded? cleared))))))
         '(reset reload retire))
       (make-list 3 '(((0 0 8)) #t ())))

     ;; A fractional duration expires: the painter drops the ink once its
     ;; deadline passes, without input or another edit.
     (tint-seconds! 1/10)
     (store:reset! head:ui-actor id '("base"))
     (pump!)
     (store:edit! '(agent fade) id (store:revision id) (text:make-span 0 0 0 4) '("ink!"))
     (pump!)
     (let ([tinted (ranges)])
       (test:await 'tint-expires (lambda () (null? (ranges))))
       (check 'blame-fades-after-its-deadline (list tinted (ranges)) '(((0 0 4)) ())))

     ;; A rival's edit is attributed at point from the store's delta log.
     ;; The report is a stamped message: a log entry, shown by the echo area.
     (tint-seconds! 8)
     (store:edit! '(agent rival) id (store:revision id) (text:make-span 0 0 0 2) '("BL"))
     (pump!)
     (move! a '(0 . 0))
     (at-point!)
     (check 'blame-names-the-rival-at-point
       (list (equal? (ranges a) (ranges b)) (not (head:buffer-of-store-id id))
         (exists (lambda (entry)
                   (let ([text (log:format-entry entry)])
                     (and (string:search text "(agent rival) wrote this at revision" 0 (string-length text)) #t)))
           (log:entries)))
       '(#t #t #t))
     (widget:unmount! root)

     (test:finish! 'blame)))
