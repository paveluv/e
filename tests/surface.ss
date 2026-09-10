#!/usr/bin/env scheme-script

;; Surface frames are coherent, owned, guarded, and cheap to read by range.
;; Shared test helpers coordinate races; no timing-dependent assertions.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (surface) surface:) (prefix (store) store:)
             (prefix (kernel) kernel:) (prefix (text) text:)
             (prefix (datum) datum:) (prefix (test) test:))

     (define author '(app surface-test))
     (define (buffer) (store:create! author "surface" '("ab" "cd")))
     (define (row n face)
       (list n (vector face 'plain)
             (vector (list (string-copy "https://example.test") (string-copy "link")) #f)
             (list (cons 'width 1))))
     (define (publish id basis revision changes cursor size)
       (call-with-values (lambda () (surface:publish! id basis revision changes cursor size)) list))
     (define (generation id) (car (surface:snapshot id)))
     (define (change id changes)
       (publish id (generation id) (store:revision id) changes '(0 0 #t) '(2 2)))

     (define id (buffer))
     (define initial (list (row 0 (string-copy "31"))))
     (define expected (datum:copy initial))
     (define cursor (list 0 1 #t))
     (define size (list 2 2))
     (define observed (test:recorder))
     (define observer
       (surface:subscribe! id
         (lambda (event)
           (observed (list event (surface:snapshot id)
                           (surface:rows id (caddr event) 0 2))))))
     (define first (publish id #f 0 initial cursor size))
     (define first-generation (cadr first))
     (test:check 'one-complete-frame-before-notification
       (list (car first) (observed))
       (list 'applied
             (list (list (list 'surface id first-generation 0 'all '(0 1 #t) '(2 2))
                         (list first-generation 0 '(0 1 #t) '(2 2))
                         (append expected '((1 . #f)))))))
     (surface:unsubscribe! observer)
     (string-set! (vector-ref (cadar initial) 0) 0 #\X)
     (string-set! (car (vector-ref (caddar initial) 0)) 0 #\X)
     (set-cdr! (car (list-ref (car initial) 3)) 9)
     (set-car! cursor 99)
     (set-car! size 99)
     (let ([data (surface:rows id first-generation 0 1)] [header (surface:snapshot id)])
       (string-set! (vector-ref (cadar data) 0) 0 #\Y)
       (string-set! (cadr (vector-ref (caddar data) 0)) 0 #\Y)
       (set-car! (caddr header) 99)
       (set-car! (cadddr header) 99))
     (test:check 'admission-and-reads-own-all-mutable-data
       (list (surface:rows id first-generation 0 1) (surface:snapshot id))
       (list expected (list first-generation 0 '(0 1 #t) '(2 2))))

     ;; Every bad frame includes a valid row prefix. None can install that
     ;; prefix, advance the generation, or notify a subscriber.
     (define events (test:recorder))
     (define token (surface:subscribe! id events))
     (define cycle (list 'cyclic))
     (set-cdr! cycle cycle)
     (define bad-row-cases
       (list (list (row 0 'duplicate)) '((1 #(plain) #() ()))
             '((1 #(42) #(#f) ())) '((1 #(plain) #(("" #f)) ()))
             (list (row 2 'outside)) (list (list 1 '#(plain) '#(#f) cycle))
             (list (list 1 '#(plain) '#(#f) void))))
     (test:check 'invalid-row-batches-are-atomic
       (for-all
         (lambda (bad)
           (test:raises?
             (lambda () (publish id first-generation 0 (cons (row 0 'changed) bad) '(0 0 #t) '(2 2)))))
         bad-row-cases) #t)
     (test:check 'invalid-frame-headers-are-atomic
       (for-all
         (lambda (header)
           (test:raises? (lambda () (publish id first-generation 0 (list (row 0 'changed))
                                             (car header) (cadr header)))))
         '(((0 0 #t) (0 2)) ((0 0 #t) (2 2.0)) ((0 2 #t) (2 2))
           ((2 0 #t) (2 2)) ((0 0 maybe) (2 2)) ((-1 0 #t) (2 2)))) #t)
     (test:check 'invalid-frames-leave-state-and-events-unchanged
       (list (surface:snapshot id) (surface:rows id first-generation 0 1) (events))
       (list (list first-generation 0 '(0 1 #t) '(2 2)) expected '()))

     ;; A style-only race shares the text revision: the surface generation
     ;; is the needed precondition. Exactly one candidate can be accepted.
     (define racers
       (test:parallel 4
         (lambda (n) (publish id first-generation 0 (list (row 1 (number->string n))) '(0 0 #t) '(2 2)))))
     (test:check 'one-frame-wins-a-generation-race
       (list (length (filter (lambda (result) (eq? (car result) 'applied)) racers))
             (filter (lambda (result) (eq? (car result) 'stale)) racers)
             (length (events)))
       '(1 ((stale frame-changed) (stale frame-changed) (stale frame-changed)) 1))
     (define previous (generation id))
     (test:check 'identical-publication-does-not-notify-or-advance
       (list (change id '()) (length (events))) (list (list 'applied previous) 1))
     (test:check 'superseded-ranges-refuse (surface:rows id first-generation 0 1) #f)
     (store:edit! author id 0 (text:make-span 0 0 0 2) '("AB"))
     (test:check 'past-and-future-text-revisions-refuse
       (map (lambda (revision) (publish id previous revision '() #f '(2 2))) '(0 2))
       '((stale text-changed) (stale text-changed)))
     (test:check 'unpublished-text-is-distinguishable-from-the-frame
       (list (store:revision id) (cadr (surface:snapshot id))) '(1 0))
     (change id (list (row 0 'new-text)))
     (test:check 'text-advance-invalidates-render-caches
       (list (list-ref (car (reverse (events))) 4) (cadr (surface:snapshot id))) '(all 1))
     (define before-shrink (generation id))
     (store:reset! author id '("x"))
     (test:check 'truncation-requires-dropping-outside-rendition
       (test:raises? (lambda () (change id '()))) #t)
     (change id '((1 . #f)))
     (test:check 'old-generation-refuses-before-new-range-bounds
       (surface:rows id before-shrink 0 2) #f)
     (test:check 'range-boundaries
       (list (surface:rows id (generation id) 1 1)
             (test:raises? (lambda () (surface:rows id (generation id) 0 2)))) '(() #t))
     (surface:unsubscribe! token)

     ;; One blocked receiver leaves later receivers pending. Repeated writes
     ;; merge their dirty rows, keep the latest header, and bound each queue
     ;; to one notice per buffer. Withdrawal/readmission must invalidate all.
     (for-each
       (lambda (retire?)
         (let* ([id (buffer)] [seed (publish id #f 0 (list (row 0 'seed) (row 1 'seed)) #f '(2 2))]
                [seen (test:recorder)] [revoked (test:recorder)] [late (test:recorder)]
                [entered (test:gate)] [release (test:gate)] [armed? #t]
                [observer (surface:subscribe! #f seen)]
                [removed (surface:subscribe! id revoked)]
                [blocker
                 (surface:subscribe! id
                   (lambda (event)
                     (set-car! event 'damaged)
                     (when armed?
                       (set! armed? #f) (entered #t) (test:await 'release release))))]
                [writer (test:worker (lambda () (change id (list (row 0 'first)))))])
           (test:await 'entered entered)
           (surface:unsubscribe! removed)
           (if retire?
               (begin
                 (surface:withdraw! id (generation id))
                 (publish id #f 0 (list (row 1 'new-life)) #f '(2 2)))
               (begin (change id (list (row 1 'second))) (change id (list (row 0 'third)))))
           (let* ([new-token (surface:subscribe! id late)]
                  [other (buffer)]
                  [other-frame (publish other #f 0 '() #f '(2 2))]
                  [last (publish id (generation id) 0 '() '(1 2 #t) '(2 3))])
             (release #t)
             (writer)
             (test:check (list 'coalesced retire?)
               (list (filter (lambda (event) (= (cadr event) id)) (seen))
                     (filter (lambda (event) (= (cadr event) other)) (seen))
                     (length (seen)) (revoked) (late))
               (list (list (list 'surface id (cadr last) 0 (if retire? 'all '(0 1)) '(1 2 #t) '(2 3)))
                     (list (list 'surface other (cadr other-frame) 0 'all #f '(2 2)))
                     2 '() (list (list 'surface id (cadr last) 0 '() '(1 2 #t) '(2 3)))))
             (for-each surface:unsubscribe! (list observer blocker new-token)))))
       '(#f #t))

     ;; Callback reentry reads a complete frame, writes another, then throws.
     ;; Taking the pending notice before calling out keeps the stream live.
     (define reentrant (buffer))
     (define seen (test:recorder))
     (define observing (surface:subscribe! reentrant seen))
     (define armed? #t)
     (define changing
       (surface:subscribe! reentrant
         (lambda (event)
           (when armed?
             (set! armed? #f)
             (change reentrant (list (row 0 'reentrant)))
             (error 'surface-test "intentional callback failure")))))
     (define reentry
       (test:worker (lambda () (publish reentrant #f 0 (list (row 0 'first)) '(0 0 #t) '(2 2)))))
     (define reentry-receipt (reentry))
     (test:check 'reentrant-publication-keeps-the-first-receipt
       (list (car reentry-receipt) (< (cadr reentry-receipt) (generation reentrant))
             (length (seen)) (caddr (car (seen))))
       (list 'applied #t 1 (generation reentrant)))
     (change reentrant (list (row 1 'after-error)))
     (test:check 'callback-failure-does-not-strand-future-notices (length (seen)) 2)
     (for-each surface:unsubscribe! (list observing changing))

     ;; Registration lifetime follows the kernel; aborted staging is never
     ;; heard, and a module's explicit retraction removes its callbacks.
     (define owned (test:recorder))
     (test:raises?
       (lambda ()
         (kernel:call-with-registration-update
           (lambda ()
             (surface:subscribe! id owned)
             (change id (list (row 0 'during-staging)))
             (error 'surface-test "abort staged subscription")))))
     (parameterize ([kernel:registering-module 'surface-fixture]) (surface:subscribe! id owned))
     (change id (list (row 0 'owned)))
     (kernel:retract-module! 'surface-fixture)
     (change id (list (row 0 'after-retraction)))
     (test:check 'staged-and-retracted-subscribers-stay-silent (length (owned)) 1)

     (define before-init (surface:snapshot id))
     (test:raises?
       (lambda ()
         (kernel:call-with-registration-update
           (lambda () (surface:init!) (error 'surface-test "abort importing initializer")))))
     (test:check 'initialization-preserves-live-frames (surface:snapshot id) before-init)
     (define deaths (test:recorder))
     (define death-token (surface:subscribe! id deaths))
     (define during-delete #t)
     (define delete-token
       (store:subscribe! id
         (lambda (event) (when (eq? (car event) 'delete) (set! during-delete (surface:snapshot id))))))
     (store:delete! author id)
     (test:check 'deletion-is-invisible-before-its-cleanup-delivers
       (list during-delete (surface:snapshot id) (surface:rows id (car before-init) 0 1)) '(#f #f #f))
     (surface:init!)
     (surface:withdraw! id (car before-init))
     (test:check 'deletion-cleans-up-once-despite-initializer-abort
       (list (length (deaths)) (list-tail (car (deaths)) 3)) '(1 (#f all #f #f)))
     (surface:unsubscribe! death-token)
     (store:unsubscribe! delete-token)
     (define retired (generation reentrant))
     (surface:withdraw! reentrant retired)
     (publish reentrant #f 0 '() #f '(2 2))
     (test:check 'withdrawal-and-readmission-do-not-reuse-generations
       (list (> (generation reentrant) retired)
             (surface:rows reentrant retired 0 1)
             (map (lambda (basis)
                    (call-with-values (lambda () (surface:withdraw! reentrant basis)) list))
                  (list (cadr reentry-receipt) retired)))
       '(#t #f ((stale frame-changed) (stale frame-changed))))

     ;; Redefine only this library, independently of the editor's pinned
     ;; import graph. Capture old exports before Chez rebinds the library;
     ;; old callers and new code share frames, subscribers, and cleanup.
     (define old-snapshot surface:snapshot)
     (define old-rows surface:rows)
     (define old-unsubscribe! surface:unsubscribe!)
     (define before-reload (surface:snapshot reentrant))
     (define reload-events (test:recorder))
     (define reload-token (surface:subscribe! reentrant reload-events))
     (load "lib/surface.e")
     (define refreshed
       (eval '(begin (import (prefix (surface) refreshed:))
                     (list refreshed:snapshot refreshed:publish!))))
     (define reloaded-header ((car refreshed) reentrant))
     ((cadr refreshed) reentrant (car reloaded-header) 0 (list (row 0 'reloaded)) #f '(2 2))
     (define after-reload (old-snapshot reentrant))
     (define reloaded-rows (old-rows reentrant (car after-reload) 0 1))
     (store:delete! author reentrant)
     (test:check 'reload-shares-state-subscriptions-and-deletion-cleanup
       (list (eq? (car refreshed) old-snapshot) reloaded-header
             (> (car after-reload) (car before-reload)) reloaded-rows
             (old-snapshot reentrant) (length (reload-events))
             (list-tail (cadr (reload-events)) 3))
       (list #f before-reload #t (list (row 0 'reloaded)) #f 2 '(#f all #f #f)))
     (old-unsubscribe! reload-token)
     (test:finish! 'surface)))
