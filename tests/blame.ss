#!/usr/bin/env scheme-script

;; Blame: another actor's fresh ink is tinted in that actor's face while its
;; text survives, tints stay bounded and expire on their deadline, and the
;; author at point is reported from the store's delta log. Headless: the
;; head's frame preparation and the painter's highlight ranges observe the
;; module. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (edit) init!) (prefix (head) head:) (prefix (store) store:) (prefix (text) text:)
             (prefix (paint) paint:) (prefix (kernel) kernel:) (prefix (log) log:)
             (prefix (string) string:) (prefix (test) test:))

     (define check test:check)
     ;; Load through the kernel so a reload below replaces the real module.
     ;; Fresh procedures resolve through the top level after that reload.
     (kernel:load-module! "blame")
     (define (tint-seconds! seconds) ((top-level-value 'blame:tint-seconds) seconds))
     (define (at-point!) ((top-level-value 'blame:at-point!)))

     (define id
       (let ([b (head:new-buffer! "blame-naming")])
         (head:add-buffer! b)
         (show-buffer! b)
         (head:buffer-store-id b)))
     (define (target) (head:buffer-named "blame-naming"))
     (define faces '(blame-1 blame-2 blame-3 blame-4 blame-5 blame-6))
     (define (ranges)
       (map (lambda (range) (list (cadr range) (caddr range) (cadddr range)))
         (filter (lambda (range)
                   (and (= (length range) 5) (eq? (car range) (target)) (memq (list-ref range 4) faces)))
           (paint:highlight-ranges))))

     ;; Tints follow surviving content. An overlap drops only that tint;
     ;; own/app edits add none, while new collaborator ink gets its own range.
     (check 'blame-keeps-surviving-ink-and-all-authors
       (map
         (lambda (example)
           (head:store-reset! (target) '("abcd" "base"))
           (head:before-frame!)
           (store:edit! '(agent seed) id (store:revision id) (text:make-span 0 1 0 1) '("INK"))
           (store:edit! '(agent seed) id (store:revision id) (text:make-span 1 0 1 0) '("KEPT"))
           (head:before-frame!)
           (store:edit! (car example) id (store:revision id) (apply text:make-span (cadr example)) (caddr example))
           (head:before-frame!)
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
         (((1 0 4)) #t) (((1 0 4)) #t) (((1 0 4) (0 2 3)) #t) (((1 0 4) (0 2 3)) #t)))

     ;; The overlay cap must bound all fade work, including after reset,
     ;; a real reload and retirement. Avoid counting Chez's GC helpers as
     ;; editor workers during this small burst; count native tasks on Linux.
     (check 'blame-burst-keeps-only-newest-ink-without-workers
       (map
         (lambda (ending)
           (tint-seconds! 8)
           (head:store-reset! (target) '(""))
           (head:before-frame!)
           (parameterize ([collect-trip-bytes (* 128 1024 1024)])
             (let* ([count (lambda () (and (file-directory? "/proc/self/task")
                                        (length (directory-list "/proc/self/task"))))]
                    [before (count)])
               (do ([i 0 (+ i 1)]) ((= i 160))
                 (store:edit! '(agent burst) id (store:revision id) (text:make-span 0 0 0 0) '("x")))
               (head:before-frame!)
               (let ([ink (ranges)] [bounded? (or (not before) (<= (count) before))])
                 (case ending
                   [(reset) (store:reset! '(agent burst) id '(""))]
                   [(reload) (kernel:reload-module! "blame")]
                   [(retire) (store:set-property! head:ui-actor id 'audience '())])
                 (head:before-frame!)
                 (let ([cleared (ranges)])
                   (when (eq? ending 'retire)
                     (store:set-property! head:ui-actor id 'audience 'all)
                     (head:before-frame!)
                     (show-buffer! (target)))
                   (list ink bounded? cleared))))))
         '(reset reload retire))
       (make-list 3 '(((0 7 8) (0 6 7) (0 5 6) (0 4 5) (0 3 4) (0 2 3) (0 1 2) (0 0 1)) #t ())))

     ;; A fractional duration expires: the painter drops the ink once its
     ;; deadline passes, without input or another edit.
     (tint-seconds! 1/10)
     (head:store-reset! (target) '("base"))
     (head:before-frame!)
     (store:edit! '(agent fade) id (store:revision id) (text:make-span 0 0 0 4) '("ink!"))
     (head:before-frame!)
     (let ([tinted (ranges)])
       (test:await 'tint-expires (lambda () (null? (ranges))))
       (check 'blame-fades-after-its-deadline (list tinted (ranges)) '(((0 0 4)) ())))

     ;; A rival's edit is attributed at point from the store's delta log.
     ;; The report is a stamped message: a log entry, shown by the echo area.
     (tint-seconds! 8)
     (store:edit! '(agent rival) id (store:revision id) (text:make-span 0 0 0 2) '("BL"))
     (head:before-frame!)
     (goto-point! '(0 . 0))
     (at-point!)
     (check 'blame-names-the-rival-at-point
       (exists (lambda (entry)
                 (let ([text (log:format-entry entry)])
                   (and (string:search text "(agent rival) wrote this at revision" 0 (string-length text)) #t)))
         (log:entries))
       #t)

     (test:finish! 'blame)))
