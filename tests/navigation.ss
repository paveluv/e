#!/usr/bin/env scheme-script

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(test-evaluate!
  '(begin
     (import (prefix (core kernel) kernel:) (prefix (head edit) edit:)
             (prefix (head head) head:) (prefix (head interaction) interaction:)
             (prefix (head widget) widget:) (prefix (state store) store:)
             (prefix (state view) view:) (prefix (test) test:))
     (kernel:load-module! "widget") (kernel:load-module! "edit")
     (for-each
       (lambda (lines options width steps expected)
         (let* ([source (store:create! head:ui-actor "navigation" lines)]
                [editor (edit:create-view! head:ui-actor source options)])
           (widget:mount! editor 'navigation)
           (widget:present! (list (list (widget:prepare! editor width 8) 0 0)))
           (edit:select! editor '(0 . 1) '(0 . 1))
           (test:check 'vertical-goal-survives-short-rows-and-wide-glyphs
             (reverse (fold-left
                        (lambda (out direction)
                          (edit:move! editor direction)
                          (cons (car (view:state (interaction:snapshot editor))) out))
                        '() steps)) expected)
           (widget:unmount! editor)))
       '(("界ab" "ábcde" "x" "界ab") ("ab界éxy"))
       '(((wrap . #f)) ((wrap . #t))) '(12 4)
       '((down down down up up up) (down down up up))
       '(((1 . 3) (2 . 1) (3 . 1) (2 . 1) (1 . 3) (0 . 1))
         ((0 . 2) (0 . 6) (0 . 2) (0 . 1))))
     (test:finish! 'navigation)))
