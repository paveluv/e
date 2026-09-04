#!/usr/bin/env scheme-script

;; The buffer store: transactions and rebasing, actor-owned marks,
;; attributed undo, subscriptions, and reload persistence.  Run from
;; the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (store) store:)
             (prefix (text) text:)
             (prefix (kernel) kernel:)
             (only (kernel) persistent-cell)
             (only (chezscheme)
                   box unbox set-box! parameterize fork-thread
                   make-time sleep))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'store-test label actual expected)))

     (define alice '(human alice))
     (define bot '(agent claude 1))

     (define (span sl sc el ec) (text:make-span sl sc el ec))
     (define (edit! actor id basis sl sc el ec replacement)
       (let-values ([(status info)
                     (store:edit! actor id basis (span sl sc el ec)
                                  replacement)])
         (list status info)))

     ;; -- lifecycle --------------------------------------------------------

     (define b (store:create! alice "notes" '("alpha" "bravo" "charlie")))

     (check 'created (store:exists? b) #t)
     (check 'named (store:buffer-name b) "notes")
     (check 'found (store:find-named "notes") b)
     (check 'listed (and (memv b (store:buffer-list)) #t) #t)
     (check 'content (map (lambda (n) (store:line b n)) '(0 1 2))
            '("alpha" "bravo" "charlie"))
     (check 'fresh-revision (store:revision b) 0)

     ;; -- transactions -----------------------------------------------------

     (check 'edit-applies
            (edit! alice b 0 1 0 1 5 '("BRAVO"))
            '(applied 1))
     (check 'edit-took (store:line b 1) "BRAVO")

     ;; a snapshot taken before an edit stays coherent
     (let-values ([(text revision) (store:snapshot b)])
       (edit! alice b 1 0 0 0 0 '("x "))
       (check 'snapshot-immutable (vector-ref text 0) "alpha")
       (check 'snapshot-revision revision 1)
       (check 'live-moved-on (store:line b 0) "x alpha"))

     ;; a stale basis with a disjoint span rebases and applies:
     ;; the bot appends to charlie, unaware alice edited line 0
     (check 'disjoint-stale-basis-rebases
            (car (edit! bot b 1 2 7 2 7 '("!")))
            'applied)
     (check 'rebased-edit-landed (store:line b 2) "charlie!")

     ;; a stale basis over content someone replaced refuses: alice's
     ;; first edit rewrote exactly (1,0)-(1,5)
     (check 'overlapping-stale-basis-refuses
            (edit! bot b 0 1 0 1 5 '("nope"))
            '(stale overlap))

     ;; an unknown basis refuses
     (check 'ancient-basis-refuses
            (edit! bot b 999 0 0 0 0 '("y"))
            '(stale basis-too-old))

     ;; -- marks ------------------------------------------------------------

     (define m (store:create! alice "marked" '("one two" "three")))
     (store:set-mark! alice m 'cursor '(1 . 3))
     (store:set-mark! bot m 'anchor '(0 . 4))

     ;; an insertion above shifts alice's cursor a line down
     (store:edit! bot m (store:revision m) (span 0 0 0 0) '("new" ""))
     (check 'mark-rebases (store:mark alice m 'cursor) '(2 . 3))
     (check 'other-actors-marks-too (store:mark bot m 'anchor) '(1 . 4))

     ;; deleting around a mark collapses it to the edit's end
     (store:edit! alice m (store:revision m) (span 1 0 2 5) '(""))
     (check 'mark-collapses (store:mark alice m 'cursor) '(1 . 0))

     (check 'marks-listed (store:marks bot m) '((anchor . (1 . 0))))
     (store:drop-mark! bot m 'anchor)
     (check 'mark-dropped (store:mark bot m 'anchor) #f)

     ;; -- span marks: published selections ----------------------------------

     (define (span-ends s)
       (list (text:span-start s) (text:span-end s)))

     (define r (store:create! alice "selected" '("abcdef" "ghijkl")))
     (store:set-mark! alice r 'region (span 0 1 0 4))

     ;; an edit before the selection shifts it whole
     (store:edit! bot r (store:revision r) (span 0 0 0 0) '("XX"))
     (check 'span-mark-rebases
            (span-ends (store:mark alice r 'region))
            '((0 . 3) (0 . 6)))

     ;; an overlapping edit degrades to endpoint rebasing, never #f
     (store:edit! bot r (store:revision r) (span 0 4 0 5) '("YYY"))
     (check 'span-mark-survives-overlap
            (let ([s (store:mark alice r 'region)])
              (and (text:span? s)
                   (text:position<=? (text:span-start s)
                                     (text:span-end s))))
            #t)

     ;; a reset clamps both endpoints into the new text
     (store:set-mark! alice r 'region (span 0 2 1 4))
     (store:reset! bot r '("ab"))
     (check 'span-mark-clamps-on-reset
            (span-ends (store:mark alice r 'region))
            '((0 . 2) (0 . 2)))

     ;; -- blame: attribution with geometry -----------------------------------

     (define bl (store:create! alice "blamed" '("aaa bbb")))
     (store:edit! alice bl (store:revision bl) (span 0 0 0 3) '("AAA"))
     (store:edit! bot bl (store:revision bl) (span 0 4 0 7) '("Z"))

     ;; newest first, spans in current coordinates
     (check 'blame-attributes
            (map (lambda (entry)
                   (cons (cadr entry) (span-ends (car entry))))
                 (store:blame bl))
            (list (cons bot '((0 . 4) (0 . 5)))
                  (cons alice '((0 . 0) (0 . 3)))))

     ;; an insertion up front shifts every older span
     (store:edit! bot bl (store:revision bl) (span 0 0 0 0) '("> "))
     (check 'blame-rebases-older-spans
            (span-ends (car (list-ref (store:blame bl) 2)))
            '((0 . 2) (0 . 5)))

     ;; a reset is a new baseline: blame starts over
     (store:reset! alice bl '("fresh"))
     (check 'blame-cleared-by-reset (store:blame bl) '())

     ;; -- attributed undo ---------------------------------------------------

     (define u (store:create! alice "undoable" '("aaa" "bbb" "ccc")))
     (store:edit! alice u 0 (span 0 0 0 3) '("ALICE"))
     (store:edit! bot u 1 (span 2 0 2 3) '("BOT"))

     ;; alice undoes her edit although the bot edited later, elsewhere
     (let-values ([(status info) (store:undo! alice u)])
       (check 'undo-rebases-past-others status 'applied))
     (check 'undo-restored (store:line u 0) "aaa")
     (check 'undo-kept-others (store:line u 2) "BOT")

     ;; nothing of alice's is left to undo
     (let-values ([(status info) (store:undo! alice u)])
       (check 'undo-exhausted status 'nothing))

     ;; the bot cannot undo through alice's overlapping later edit
     (store:edit! bot u (store:revision u) (span 1 0 1 3) '("BOT2"))
     (store:edit! alice u (store:revision u) (span 1 0 1 4) '("OVER"))
     (let-values ([(status info) (store:undo! bot u)])
       (check 'undo-blocked-by-overlap status 'blocked))

     ;; -- attribution --------------------------------------------------------

     (define h (store:create! alice "blame" '("one" "two")))
     (store:edit! alice h 0 (span 0 0 0 3) '("ONE"))
     (store:edit! bot h 1 (span 1 0 1 0) '("> "))
     (check 'history-attributes-newest-first
            (map (lambda (entry) (list (car entry) (cadr entry)))
                 (store:history h))
            (list (list 2 bot) (list 1 alice)))
     (check 'history-carries-positions
            (cddr (car (store:history h)))
            '((1 . 0) (1 . 0) (1 . 2)))
     (store:reset! alice h '("fresh"))
     (check 'reset-clears-history (store:history h) '())

     ;; -- subscriptions ------------------------------------------------------

     (define events (box '()))
     (define token
       (store:subscribe! b (lambda (event)
                             (set-box! events
                                       (cons event (unbox events))))))

     (store:edit! bot b (store:revision b) (span 0 0 0 1) '("X"))
     (let ([event (car (unbox events))])
       (check 'event-names-the-edit
              (list (car event) (cadr event) (cadddr event))
              (list 'edit b bot)))

     ;; other buffers stay silent for a scoped subscriber
     (store:edit! alice m (store:revision m) (span 0 0 0 1) '("Y"))
     (check 'scoped-subscription (length (unbox events)) 1)

     (store:unsubscribe! token)
     (store:edit! bot b (store:revision b) (span 0 0 0 1) '("Z"))
     (check 'unsubscribed (length (unbox events)) 1)

     ;; -- concurrent writers -------------------------------------------------

     ;; two threads race blind appends through the retry loop; every
     ;; edit must land exactly once
     (define race (store:create! alice "race" '("start")))
     ;; the correct racing-writer pattern: compute the span against a
     ;; snapshot and pass that snapshot's revision as the basis --
     ;; separate reads could straddle another actor's edit
     (define (append-line! actor tag)
       (let retry ()
         (let*-values ([(text basis) (store:snapshot race)])
           (let* ([last (- (vector-length text) 1)]
                  [column (string-length (vector-ref text last))]
                  [s (span last column last column)])
             (let-values ([(status info)
                           (store:edit! actor race basis s
                                        (list "" tag))])
               (unless (eq? status 'applied) (retry)))))))
     (define finished (list (box #f) (box #f)))
     (for-each
       (lambda (actor flag)
         (fork-thread
           (lambda ()
             (do ([i 0 (+ i 1)]) ((= i 25))
               (append-line! actor (format "~a-~a" (cadr actor) i)))
             (set-box! flag #t))))
       (list alice bot) finished)
     (let wait ([tries 400])
       (unless (for-all unbox finished)
         (when (zero? tries) (error 'store-test "race did not finish"))
         (sleep (make-time 'time-duration 25000000 0))
         (wait (- tries 1))))
     (check 'all-racing-appends-landed (store:line-count race) 51)

     ;; -- persistence across reload -------------------------------------------

     ;; the store cell survives: asking again returns the same box
     (check 'persistent-cell-persists
            (eq? (persistent-cell 'store (lambda () 'fresh))
                 (persistent-cell 'store (lambda () 'fresh)))
            #t)
     (check 'persistent-cell-kept-the-store
            (not (eq? (unbox (persistent-cell 'store
                                              (lambda () 'fresh)))
                      'fresh))
            #t)

     ;; -- deletion --------------------------------------------------------------

     (store:delete! alice m)
     (check 'deleted (store:exists? m) #f)
     (check 'delete-raises-for-the-gone
            (guard (ex [else 'rejected]) (store:line m 0))
            'rejected)

     ;; -- properties: buffer-level facts shared by every head ------------------

     (define pb (store:create! alice "propped" '("x")))
     (store:set-property! alice pb 'file "/tmp/a.txt")
     (store:set-property! bot pb 'read-only #t)

     (check 'property-read (store:property pb 'file) "/tmp/a.txt")
     (check 'property-cross-actor (store:property pb 'read-only) #t)
     (check 'property-absent-is-false (store:property pb 'mode) #f)
     (check 'properties-listed
            (list (assq 'file (store:properties pb))
                  (assq 'read-only (store:properties pb)))
            '((file . "/tmp/a.txt") (read-only . #t)))

     ;; #f is a value (a fact explicitly off); drop-property! forgets;
     ;; facts survive a reset (they are not text)
     (store:set-property! bot pb 'read-only #f)
     (check 'property-off-is-listed
            (assq 'read-only (store:properties pb)) '(read-only . #f))
     (store:drop-property! bot pb 'read-only)
     (check 'property-dropped
            (assq 'read-only (store:properties pb)) #f)
     (store:reset! bot pb '("fresh"))
     (check 'property-survives-reset (store:property pb 'file) "/tmp/a.txt")

     ;; subscribers hear fact changes
     (define prop-events (box '()))
     (define prop-token
       (store:subscribe!
         pb (lambda (event)
              (when (eq? (car event) 'property)
                (set-box! prop-events
                          (cons event (unbox prop-events)))))))
     (store:set-property! alice pb 'mode "scheme")
     (check 'property-event
            (car (unbox prop-events))
            (list 'property pb 'mode alice))
     (store:unsubscribe! prop-token)
     (store:delete! alice pb)

     ;; -- the buffer lifecycle is an event stream too ------------------------

     (define life-events (box '()))
     (define life-token
       (store:subscribe!
         #f (lambda (event)
              (when (memq (car event) '(create rename delete))
                (set-box! life-events
                          (cons event (unbox life-events)))))))
     (define lb (store:create! bot "agent-notes" '("n")))
     (store:rename! bot lb "agent-log")
     (store:delete! bot lb)
     (check 'lifecycle-events
            (reverse (unbox life-events))
            (list (list 'create lb "agent-notes" bot)
                  (list 'rename lb "agent-log" bot)
                  (list 'delete lb bot)))
     (store:unsubscribe! life-token)

     ;; -- subscriptions are registry-owned ------------------------------------

     (define sub-events (box '()))
     (define owned-buffer (store:create! alice "owned" '("x")))
     (parameterize ([kernel:registering-module 'testmod])
       (store:subscribe!
         #f (lambda (event)
              (set-box! sub-events (cons event (unbox sub-events))))))
     (store:edit! alice owned-buffer (store:revision owned-buffer)
                  (span 0 0 0 0) '("a"))
     (check 'owned-subscription-hears (length (unbox sub-events)) 1)
     (kernel:retract-module! 'testmod)
     (store:edit! alice owned-buffer (store:revision owned-buffer)
                  (span 0 0 0 0) '("b"))
     (check 'retracted-subscription-is-silent
            (length (unbox sub-events)) 1)
     (store:delete! alice owned-buffer)

     ;; -- the ui's whole-line splices, as edit's splice-lines! builds them ----

     ;; edit.e's splice-lines! turns "replace lines [from, to)" into a
     ;; span with three cases; these drive the same spans through
     ;; edit! and pin the resulting text
     (define (spliced from to inserted)
       (let* ([id (store:create! alice "spliced" '("aaa" "bbb" "ccc"))]
              [count 3]
              [last-len 3]
              [sp (cond
                    [(< to count) (span from 0 to 0)]
                    [(> from 0) (span (- from 1) 3 (- count 1) last-len)]
                    [else (span 0 0 (- count 1) last-len)])]
              [replacement
               (cond
                 [(< to count) (append inserted '(""))]
                 [(> from 0) (cons "" inserted)]
                 [(null? inserted) '("")]
                 [else inserted])])
         (store:edit! alice id (store:revision id) sp replacement)
         (let-values ([(text revision) (store:snapshot id)])
           (store:delete! alice id)
           (vector->list text))))

     (check 'splice-interior (spliced 0 1 '("XX")) '("XX" "bbb" "ccc"))
     (check 'splice-interior-delete (spliced 1 2 '()) '("aaa" "ccc"))
     (check 'splice-interior-grow
            (spliced 1 2 '("p" "q")) '("aaa" "p" "q" "ccc"))
     (check 'splice-through-the-end (spliced 1 3 '("YY")) '("aaa" "YY"))
     (check 'splice-delete-tail (spliced 2 3 '()) '("aaa" "bbb"))
     (check 'splice-whole-buffer (spliced 0 3 '("Z")) '("Z"))
     (check 'splice-empty-buffer (spliced 0 3 '()) '(""))

     (format #t "~a store checks passed\n" checks)))
