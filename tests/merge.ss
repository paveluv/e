#!/usr/bin/env scheme-script

;; The merge app: a buffer whose text holds conflict markers has the merge
;; context, where M-n, M-m and M-d run next!, keep-mine! and keep-disk!;
;; the commands hop to and resolve the conflict at point; with the markers
;; gone the context is gone, and the keys are nobody's globally. Headless.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (head literal)
             (prefix (apps merge) merge:)
             (prefix (head head) head:)
             (prefix (head keymap) keymap:)
             (prefix (head mode) mode:))

     (define check test:check)
     (merge:init!)
     (define b (head:new-buffer! "merged"))
     (head:show-buffer! b)
     (head:goto! '(0 . 0))
     (insert-text! "one\n<<<<<<< buffer\nmine\n=======\ntheirs\n>>>>>>> disk\ntwo")
     (define (text) (vector->list (head:buffer-lines b)))
     (define (bound-to context key) (let ([hit (keymap:resolved-binding context (list key))]) (and hit (keymap:binding-action (cdr hit)))))
     (check 'a-buffer-with-conflict-markers-has-the-merge-context (mode:key-contexts b) '(merge))
     (check 'the-merge-keys-are-the-contexts-not-the-global-maps
       (list (eq? (bound-to 'merge "M-n") merge:next!) (eq? (bound-to 'merge "M-m") merge:keep-mine!)
             (eq? (bound-to 'merge "M-d") merge:keep-disk!) (eq? (bound-to 'global "M-n") merge:next!))
       '(#t #t #t #f))
     (head:goto! '(0 . 0))
     (merge:next!)
     (check 'next-hops-to-the-conflict (head:point) '(1 . 0))
     (head:goto! '(2 . 0))
     (merge:keep-mine!)
     (check 'keeping-mine-leaves-the-buffers-side (text) '("one" "mine" "two"))
     (check 'with-the-markers-gone-the-context-is-gone (mode:key-contexts b) '())
     (undo!)
     (check 'undo-brings-the-conflict-and-the-context-back (list (length (text)) (mode:key-contexts b)) '(7 (merge)))
     (head:goto! '(3 . 0))
     (merge:keep-disk!)
     (check 'keeping-disk-leaves-the-disks-side (text) '("one" "theirs" "two"))
     (test:finish! 'merge)))
