#!/usr/bin/env scheme-script

;; Local buffers own their text and facts without crossing the store
;; seam.  Exercise the normal head helpers, including frame-time marks
;; and lifecycle cleanup.  Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)

(eval
  '(begin
     (import (prefix (head head) head:) (prefix (head seat) seat:)
             (prefix (head suspension) suspension:) (prefix (head text-source) text-source:)
             (prefix (state store) store:)
             (prefix (foundation text) text:) (prefix (test) test:))

     (define check test:check)

     (include "tests/suspension.sps")

     (define scratch (seat:window-buffer (seat:current-window)))
     (define scratch-id (seat:buffer-store-id scratch))
     (define initial-store (list-sort (lambda (a b) (< (cadr a) (cadr b))) (store:buffer-list)))
     (define events '())
     (define subscription
       (store:subscribe! #f (lambda (event) (set! events (cons event events)))))

     (define local (seat:new-local-buffer! "*local-test*"))
     (define other (seat:new-local-buffer! "*other-local*"))
     (seat:set-buffers! (append (seat:buffers) (list local other)))
     (seat:set-window-buffer! (seat:current-window) local)

     (check 'local-label (seat:buffer-name local) "<local-test>")
     (check 'plain-local-label
            (seat:buffer-name (seat:new-local-buffer! "plain name")) "<plain name>")
     (check 'bracketed-local-label-is-idempotent
            (seat:buffer-name (seat:new-local-buffer! "<ready>")) "<ready>")
     (check 'no-twin (seat:buffer-store-id local) #f)
     (check 'no-false-id-lookup (seat:buffer-of-store-id #f) #f)
     (check 'initial-text (seat:buffer-lines local) '#(""))
     (check 'trailing-default (seat:buffer-trailing local) #t)
     (check 'mode-auto-default (seat:buffer-mode-auto local) #t)
     (check 'wrap-default (seat:buffer-fact local 'wrap #f) 'default)
     (check 'missing-fact (seat:buffer-fact local 'custom 'absent) 'absent)
     (seat:buffer-fact-set! local 'custom #f)
     (check 'explicit-false (seat:buffer-fact local 'custom 'absent) #f)
     (seat:buffer-fact-set! local 'custom '(one two))
     (check 'fact-replaced (seat:buffer-fact local 'custom #f) '(one two))
     (check 'facts-are-per-buffer
            (seat:buffer-fact other 'custom 'absent) 'absent)
     (seat:buffer-read-only-set! local #t)
     (seat:buffer-trailing-set! local #f)
     (check 'read-only-fact (seat:buffer-read-only local) #t)
     (check 'false-managed-fact (seat:buffer-trailing local) #f)

     ;; The public record constructor still accepts its original args.
     (define bare
       (seat:make-buffer "bare" (vector "") 0
                         0 0 #f 0 0 0 #f 0))
     (seat:buffer-fact-set! bare 'custom 'bare)
     (check 'constructed-record-has-facts
            (seat:buffer-fact bare 'custom #f) 'bare)
     (check 'constructor-facts-isolated
            (seat:buffer-fact local 'custom #f) '(one two))

     (define lines (vector "alpha" "bravo"))
     (seat:buffer-lines-set! local lines)
     (check 'local-replacement-owns-vector (eq? (seat:buffer-lines local) lines) #f)
     (seat:store-edit! local (text:make-span 0 1 0 4) '("L"))
     (check 'local-edit (seat:buffer-lines local) '#("aLa" "bravo"))
     (check 'old-text-stays-unchanged lines '#("alpha" "bravo"))
     (seat:window-prow-set! (seat:current-window) 1)
     (seat:window-pcol-set! (seat:current-window) 5)
     (seat:buffer-lines-set! local '#("x"))
     (check 'replacement-clamps-point
            (cons (seat:window-prow (seat:current-window))
                  (seat:window-pcol (seat:current-window)))
            '(0 . 1))

     (seat:buffer-name-set! local "*renamed-local*")
     (check 'local-rename (seat:buffer-name local) "<renamed-local>")
     (seat:buffer-marked-set! local #t)
     (head:before-frame!)
     (check 'local-frame-publishes-no-marks
            (store:marks head:ui-actor scratch-id) '())
     (seat:forget-buffer! local)
     (check 'forgotten-from-list (memq local (seat:buffers)) #f)
     (check 'window-falls-back (eq? (seat:window-buffer (seat:current-window)) scratch) #t)
     (seat:forget-buffer! other)
     (check 'store-list-unchanged (list-sort (lambda (a b) (< (cadr a) (cadr b))) (store:buffer-list)) initial-store)
     (check 'local-lifecycle-emits-no-store-events events '())
     (store:unsubscribe! subscription)

     ;; Shared buffers continue to use the store as their only fact owner.
     (define shared (seat:new-buffer! "shared-test"))
     (check 'ordinary-buffer-has-twin (store:exists? (seat:buffer-store-id shared)) #t)
     (seat:buffer-fact-set! shared 'custom 'head)
     (check 'shared-write (store:property (seat:buffer-store-id shared) 'custom) 'head)
     (store:set-property! '(agent test) (seat:buffer-store-id shared) 'custom 'foreign)
     (check 'shared-read (seat:buffer-fact shared 'custom #f) 'foreign)
     (check 'shared-absence-uses-declared-fallback
            (seat:buffer-fact shared 'missing 'fallback) 'fallback)

     (test:finish! 'local)))
