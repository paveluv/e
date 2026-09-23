#!/usr/bin/env scheme-script

;; Links between windows: directed and tagged, many to many, made from the
;; current window, the same link once, pruned when a window closes; the
;; target tag registered, other tags registered by code, and the
;; window-link-tag type completing them. Headless.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (head literal)
             (prefix (foundation edoc) edoc:)
             (prefix (head head) head:)
             (prefix (head window) window:))

     (define check test:check)
     (define (refused? thunk) (guard (ex [else #t]) (thunk) #f))
     (define w1 (head:current-window))
     (define w2 (window:split-below!))
     (window:focus! w1)
     (define w3 (window:split-right!))
     (window:focus! w1)
     (define (indexes ws) (map head:window-index ws))

     (check 'a-target-link-goes-from-the-current-window
       (list (window:link-target! w2) (indexes (window:linked 'target)) (indexes (window:linked 'target w1)))
       (list (list (head:window-index w1) (head:window-index w2) 'target) (indexes (list w2)) (indexes (list w2))))
     (check 'a-window-links-out-many-times-and-the-same-link-is-one
       (begin (window:link-target! w3) (window:link-target! w2)
              (list (indexes (window:linked 'target)) (length (window:links))))
       (list (indexes (list w2 w3)) 2))
     (window:focus! w3)
     (check 'a-window-is-linked-to-from-many
       (begin (window:link-target! w2)
              (map car (filter (lambda (l) (= (cadr l) (head:window-index w2))) (window:links))))
       (indexes (list w1 w3)))
     (window:focus! w1)
     (check 'a-self-link-and-an-unregistered-tag-are-refused
       (list (refused? (lambda () (window:link-target! w1))) (refused? (lambda () (window:link! w2 'mirror))))
       '(#t #t))
     (check 'a-registered-tag-links-and-completes-with-its-description
       (begin (window:register-link-tag! 'mirror "a window showing the same text")
              (window:link! w3 'mirror)
              (list (indexes (window:linked 'mirror)) (edoc:type-completions 'window-link-tag "")
                    (edoc:type-value 'window-link-tag 'mirror)))
       (list (indexes (list w3))
             '((target . "the window a chooser in the linked window opens its pick in") (mirror . "a window showing the same text"))
             'mirror))
     (check 'unlinking-removes-the-links-to-a-window-under-a-tag-or-all
       (begin (window:unlink! w3 'mirror)
              (let ([after-one (indexes (window:linked 'mirror))])
                (window:unlink! w3)
                (list after-one (indexes (window:linked 'target)))))
       (list '() (indexes (list w2))))
     (window:focus! w2)
     (window:delete!)
     (check 'closing-a-window-drops-the-links-to-and-from-it (window:links) '())
     (test:finish! 'window-link)))
