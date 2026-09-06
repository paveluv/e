#!/usr/bin/env scheme-script

;; Local buffers own their text and facts without crossing the store
;; seam.  Exercise the normal head helpers, including frame-time marks
;; and lifecycle cleanup.  Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (head) head:)
             (prefix (store) store:)
             (prefix (text) text:))

     (define checks 0)
     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'local-test label actual expected)))

     (define scratch (head:window-buffer (head:current)))
     (define scratch-id (head:buffer-store-id scratch))
     (define initial-store (list-sort < (store:buffer-list)))
     (define events '())
     (define subscription
       (store:subscribe! #f (lambda (event) (set! events (cons event events)))))

     (define local (head:new-local-buffer "*local-test*"))
     (define other (head:new-local-buffer "*other-local*"))
     (head:set-buffers! (append (head:buffers) (list local other)))
     (head:set-window-buffer! (head:current) local)

     (check 'local-label (head:buffer-name local) "<local-test>")
     (check 'plain-local-label
            (head:buffer-name (head:new-local-buffer "plain name")) "<plain name>")
     (check 'bracketed-local-label-is-idempotent
            (head:buffer-name (head:new-local-buffer "<ready>")) "<ready>")
     (check 'no-twin (head:buffer-store-id local) #f)
     (check 'no-false-id-lookup (head:buffer-of-store-id #f) #f)
     (check 'initial-text (head:buffer-lines local) '#(""))
     (check 'trailing-default (head:buffer-trailing local) #t)
     (check 'mode-auto-default (head:buffer-mode-auto local) #t)
     (check 'wrap-default (head:buffer-fact local 'wrap #f) 'default)
     (check 'missing-fact (head:buffer-fact local 'custom 'absent) 'absent)
     (head:buffer-fact-set! local 'custom #f)
     (check 'explicit-false (head:buffer-fact local 'custom 'absent) #f)
     (head:buffer-fact-set! local 'custom '(one two))
     (check 'fact-replaced (head:buffer-fact local 'custom #f) '(one two))
     (check 'facts-are-per-buffer
            (head:buffer-fact other 'custom 'absent) 'absent)
     (head:buffer-read-only-set! local #t)
     (head:buffer-trailing-set! local #f)
     (check 'read-only-fact (head:buffer-read-only local) #t)
     (check 'false-managed-fact (head:buffer-trailing local) #f)

     ;; The public record constructor still accepts its original args.
     (define bare
       (head:make-buffer "bare" (vector "") 0 (vector '() '())
                         0 0 #f 0 0 0 'default #f 0))
     (head:buffer-fact-set! bare 'custom 'bare)
     (check 'constructed-record-has-facts
            (head:buffer-fact bare 'custom #f) 'bare)
     (check 'constructor-facts-isolated
            (head:buffer-fact local 'custom #f) '(one two))

     (define lines (vector "alpha" "bravo"))
     (head:buffer-lines-set! local lines)
     (check 'local-replacement-owns-vector (eq? (head:buffer-lines local) lines) #f)
     (head:store-edit! local (text:make-span 0 1 0 4) '("L"))
     (check 'local-edit (head:buffer-lines local) '#("aLa" "bravo"))
     (check 'old-text-stays-unchanged lines '#("alpha" "bravo"))
     (head:window-prow-set! (head:current) 1)
     (head:window-pcol-set! (head:current) 5)
     (head:view-append! local '("charlie"))
     (check 'appended-lines (head:buffer-lines local) '#("aLa" "bravo" "charlie"))
     (check 'append-follows-tail
            (cons (head:window-prow (head:current))
                  (head:window-pcol (head:current)))
            '(2 . 7))
     (head:view-replace! local '("x"))
     (check 'replacement-clamps-point
            (cons (head:window-prow (head:current))
                  (head:window-pcol (head:current)))
            '(0 . 1))
     (define revision (head:buffer-revision local))
     (head:view-replace! local '("x"))
     (check 'unchanged-view-keeps-revision (head:buffer-revision local) revision)

     (head:buffer-name-set! local "*renamed-local*")
     (check 'local-rename (head:buffer-name local) "<renamed-local>")
     (head:buffer-marked-set! local #t)
     (head:before-frame!)
     (check 'local-frame-publishes-no-marks
            (store:marks head:ui-actor scratch-id) '())
     (head:forget-buffer! local)
     (check 'forgotten-from-list (memq local (head:buffers)) #f)
     (check 'window-falls-back (eq? (head:window-buffer (head:current)) scratch) #t)
     (head:forget-buffer! other)
     (check 'store-list-unchanged (list-sort < (store:buffer-list)) initial-store)
     (check 'local-lifecycle-emits-no-store-events events '())
     (store:unsubscribe! subscription)

     ;; Shared buffers continue to use the store as their only fact owner.
     (define shared (head:new-buffer "shared-test"))
     (check 'ordinary-buffer-has-twin (store:exists? (head:buffer-store-id shared)) #t)
     (head:buffer-fact-set! shared 'custom 'head)
     (check 'shared-write (store:property (head:buffer-store-id shared) 'custom) 'head)
     (store:set-property! '(agent test) (head:buffer-store-id shared) 'custom 'foreign)
     (check 'shared-read (head:buffer-fact shared 'custom #f) 'foreign)
     (check 'shared-absence-uses-declared-fallback
            (head:buffer-fact shared 'missing 'fallback) 'fallback)

     (format #t "~a local checks passed\n" checks)))
