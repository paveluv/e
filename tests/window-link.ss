#!/usr/bin/env scheme-script

;; Links between windows: directed and tagged, many to many, made from the
;; current window, the same link once, pruned when a window closes; the
;; target tag registered, other tags registered by code, and the
;; window-link-tag type completing them. Headless.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (head literal)
             (prefix (foundation edoc) edoc:)
             (prefix (head head) head:) (prefix (head seat) seat:)
             (prefix (head window-host) window-host:))

     (define check test:check)
     (define (refused? thunk) (guard (ex [else #t]) (thunk) #f))
     (define w1 (seat:current-window))
     (define w2 (window-host:split-below!))
     (window-host:focus! w1)
     (define w3 (window-host:split-right!))
     (window-host:focus! w1)
     (define (indexes ws) (map seat:window-index ws))

     (check 'a-target-link-goes-from-the-current-window
       (list (window-host:link-target! w2) (indexes (window-host:linked 'target)) (indexes (window-host:linked 'target w1)))
       (list (list (seat:window-index w1) (seat:window-index w2) 'target) (indexes (list w2)) (indexes (list w2))))
     (check 'a-window-links-out-many-times-and-the-same-link-is-one
       (begin (window-host:link-target! w3) (window-host:link-target! w2)
              (list (indexes (window-host:linked 'target)) (length (window-host:links))))
       (list (indexes (list w2 w3)) 2))
     (window-host:focus! w3)
     (check 'a-window-is-linked-to-from-many
       (begin (window-host:link-target! w2)
              (map car (filter (lambda (l) (= (cadr l) (seat:window-index w2))) (window-host:links))))
       (indexes (list w1 w3)))
     (window-host:focus! w1)
     (check 'a-self-link-and-an-unregistered-tag-are-refused
       (list (refused? (lambda () (window-host:link-target! w1))) (refused? (lambda () (window-host:link! w2 'mirror))))
       '(#t #t))
     (check 'a-registered-tag-links-and-completes-with-its-description
       (begin (window-host:register-link-tag! 'mirror "a window showing the same text")
              (window-host:link! w3 'mirror)
              (list (indexes (window-host:linked 'mirror)) (edoc:type-completions 'window-link-tag "")
                    (edoc:type-value 'window-link-tag 'mirror)))
       (list (indexes (list w3))
             '((target #f "the window a chooser in the linked window opens its pick in") (mirror #f "a window showing the same text"))
             'mirror))
     (check 'unlinking-removes-the-links-to-a-window-under-a-tag-or-all
       (begin (window-host:unlink! w3 'mirror)
              (let ([after-one (indexes (window-host:linked 'mirror))])
                (window-host:unlink! w3)
                (list after-one (indexes (window-host:linked 'target)))))
       (list '() (indexes (list w2))))
     (window-host:focus! w2)
     (window-host:delete!)
     (check 'closing-a-window-drops-the-links-to-and-from-it (window-host:links) '())
     (test:finish! 'window-link)))
