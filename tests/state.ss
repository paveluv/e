#!/usr/bin/env scheme-script

;; The buffer store: transactions and rebasing, actor-owned marks,
;; attributed undo, subscriptions, and reload persistence -- v2 stage
;; 1 (docs/DESIGN2.md).  Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (state) state:)
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
         (error 'state-test label actual expected)))

     (define alice '(human alice))
     (define bot '(agent claude 1))

     (define (span sl sc el ec) (text:make-span sl sc el ec))
     (define (edit! actor id basis sl sc el ec replacement)
       (let-values ([(status info)
                     (state:edit! actor id basis (span sl sc el ec)
                                  replacement)])
         (list status info)))

     ;; -- lifecycle --------------------------------------------------------

     (define b (state:create! alice "notes" '("alpha" "bravo" "charlie")))

     (check 'created (state:exists? b) #t)
     (check 'named (state:buffer-name b) "notes")
     (check 'found (state:find-named "notes") b)
     (check 'listed (and (memv b (state:buffer-list)) #t) #t)
     (check 'content (map (lambda (n) (state:line b n)) '(0 1 2))
            '("alpha" "bravo" "charlie"))
     (check 'fresh-revision (state:revision b) 0)

     ;; -- transactions -----------------------------------------------------

     (check 'edit-applies
            (edit! alice b 0 1 0 1 5 '("BRAVO"))
            '(applied 1))
     (check 'edit-took (state:line b 1) "BRAVO")

     ;; a snapshot taken before an edit stays coherent
     (let-values ([(text revision) (state:snapshot b)])
       (edit! alice b 1 0 0 0 0 '("x "))
       (check 'snapshot-immutable (vector-ref text 0) "alpha")
       (check 'snapshot-revision revision 1)
       (check 'live-moved-on (state:line b 0) "x alpha"))

     ;; a stale basis with a disjoint span rebases and applies:
     ;; the bot appends to charlie, unaware alice edited line 0
     (check 'disjoint-stale-basis-rebases
            (car (edit! bot b 1 2 7 2 7 '("!")))
            'applied)
     (check 'rebased-edit-landed (state:line b 2) "charlie!")

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

     (define m (state:create! alice "marked" '("one two" "three")))
     (state:set-mark! alice m 'cursor '(1 . 3))
     (state:set-mark! bot m 'anchor '(0 . 4))

     ;; an insertion above shifts alice's cursor a line down
     (state:edit! bot m (state:revision m) (span 0 0 0 0) '("new" ""))
     (check 'mark-rebases (state:mark alice m 'cursor) '(2 . 3))
     (check 'other-actors-marks-too (state:mark bot m 'anchor) '(1 . 4))

     ;; deleting around a mark collapses it to the edit's end
     (state:edit! alice m (state:revision m) (span 1 0 2 5) '(""))
     (check 'mark-collapses (state:mark alice m 'cursor) '(1 . 0))

     (check 'marks-listed (state:marks bot m) '((anchor . (1 . 0))))
     (state:drop-mark! bot m 'anchor)
     (check 'mark-dropped (state:mark bot m 'anchor) #f)

     ;; -- span marks: published selections ----------------------------------

     (define (span-ends s)
       (list (text:span-start s) (text:span-end s)))

     (define r (state:create! alice "selected" '("abcdef" "ghijkl")))
     (state:set-mark! alice r 'region (span 0 1 0 4))

     ;; an edit before the selection shifts it whole
     (state:edit! bot r (state:revision r) (span 0 0 0 0) '("XX"))
     (check 'span-mark-rebases
            (span-ends (state:mark alice r 'region))
            '((0 . 3) (0 . 6)))

     ;; an overlapping edit degrades to endpoint rebasing, never #f
     (state:edit! bot r (state:revision r) (span 0 4 0 5) '("YYY"))
     (check 'span-mark-survives-overlap
            (let ([s (state:mark alice r 'region)])
              (and (text:span? s)
                   (text:position<=? (text:span-start s)
                                     (text:span-end s))))
            #t)

     ;; a reset clamps both endpoints into the new text
     (state:set-mark! alice r 'region (span 0 2 1 4))
     (state:reset! bot r '("ab"))
     (check 'span-mark-clamps-on-reset
            (span-ends (state:mark alice r 'region))
            '((0 . 2) (0 . 2)))

     ;; -- blame: attribution with geometry -----------------------------------

     (define bl (state:create! alice "blamed" '("aaa bbb")))
     (state:edit! alice bl (state:revision bl) (span 0 0 0 3) '("AAA"))
     (state:edit! bot bl (state:revision bl) (span 0 4 0 7) '("Z"))

     ;; newest first, spans in current coordinates
     (check 'blame-attributes
            (map (lambda (entry)
                   (cons (cadr entry) (span-ends (car entry))))
                 (state:blame bl))
            (list (cons bot '((0 . 4) (0 . 5)))
                  (cons alice '((0 . 0) (0 . 3)))))

     ;; an insertion up front shifts every older span
     (state:edit! bot bl (state:revision bl) (span 0 0 0 0) '("> "))
     (check 'blame-rebases-older-spans
            (span-ends (car (list-ref (state:blame bl) 2)))
            '((0 . 2) (0 . 5)))

     ;; a reset is a new baseline: blame starts over
     (state:reset! alice bl '("fresh"))
     (check 'blame-cleared-by-reset (state:blame bl) '())

     ;; -- attributed undo ---------------------------------------------------

     (define u (state:create! alice "undoable" '("aaa" "bbb" "ccc")))
     (state:edit! alice u 0 (span 0 0 0 3) '("ALICE"))
     (state:edit! bot u 1 (span 2 0 2 3) '("BOT"))

     ;; alice undoes her edit although the bot edited later, elsewhere
     (let-values ([(status info) (state:undo! alice u)])
       (check 'undo-rebases-past-others status 'applied))
     (check 'undo-restored (state:line u 0) "aaa")
     (check 'undo-kept-others (state:line u 2) "BOT")

     ;; nothing of alice's is left to undo
     (let-values ([(status info) (state:undo! alice u)])
       (check 'undo-exhausted status 'nothing))

     ;; the bot cannot undo through alice's overlapping later edit
     (state:edit! bot u (state:revision u) (span 1 0 1 3) '("BOT2"))
     (state:edit! alice u (state:revision u) (span 1 0 1 4) '("OVER"))
     (let-values ([(status info) (state:undo! bot u)])
       (check 'undo-blocked-by-overlap status 'blocked))

     ;; -- attribution --------------------------------------------------------

     (define h (state:create! alice "blame" '("one" "two")))
     (state:edit! alice h 0 (span 0 0 0 3) '("ONE"))
     (state:edit! bot h 1 (span 1 0 1 0) '("> "))
     (check 'history-attributes-newest-first
            (map (lambda (entry) (list (car entry) (cadr entry)))
                 (state:history h))
            (list (list 2 bot) (list 1 alice)))
     (check 'history-carries-positions
            (cddr (car (state:history h)))
            '((1 . 0) (1 . 0) (1 . 2)))
     (state:reset! alice h '("fresh"))
     (check 'reset-clears-history (state:history h) '())

     ;; -- subscriptions ------------------------------------------------------

     (define events (box '()))
     (define token
       (state:subscribe! b (lambda (event)
                             (set-box! events
                                       (cons event (unbox events))))))

     (state:edit! bot b (state:revision b) (span 0 0 0 1) '("X"))
     (let ([event (car (unbox events))])
       (check 'event-names-the-edit
              (list (car event) (cadr event) (cadddr event))
              (list 'edit b bot)))

     ;; other buffers stay silent for a scoped subscriber
     (state:edit! alice m (state:revision m) (span 0 0 0 1) '("Y"))
     (check 'scoped-subscription (length (unbox events)) 1)

     (state:unsubscribe! token)
     (state:edit! bot b (state:revision b) (span 0 0 0 1) '("Z"))
     (check 'unsubscribed (length (unbox events)) 1)

     ;; -- concurrent writers -------------------------------------------------

     ;; two threads race blind appends through the retry loop; every
     ;; edit must land exactly once
     (define race (state:create! alice "race" '("start")))
     ;; the correct racing-writer pattern: compute the span against a
     ;; snapshot and pass that snapshot's revision as the basis --
     ;; separate reads could straddle another actor's edit
     (define (append-line! actor tag)
       (let retry ()
         (let*-values ([(text basis) (state:snapshot race)])
           (let* ([last (- (vector-length text) 1)]
                  [column (string-length (vector-ref text last))]
                  [s (span last column last column)])
             (let-values ([(status info)
                           (state:edit! actor race basis s
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
         (when (zero? tries) (error 'state-test "race did not finish"))
         (sleep (make-time 'time-duration 25000000 0))
         (wait (- tries 1))))
     (check 'all-racing-appends-landed (state:line-count race) 51)

     ;; -- persistence across reload -------------------------------------------

     ;; the store cell survives: asking again returns the same box
     (check 'persistent-cell-persists
            (eq? (persistent-cell 'state-store (lambda () 'fresh))
                 (persistent-cell 'state-store (lambda () 'fresh)))
            #t)
     (check 'persistent-cell-kept-the-store
            (not (eq? (unbox (persistent-cell 'state-store
                                              (lambda () 'fresh)))
                      'fresh))
            #t)

     ;; -- deletion --------------------------------------------------------------

     (state:delete! alice m)
     (check 'deleted (state:exists? m) #f)
     (check 'delete-raises-for-the-gone
            (guard (ex [else 'rejected]) (state:line m 0))
            'rejected)

     ;; -- properties: buffer-level facts shared by every head ------------------

     (define pb (state:create! alice "propped" '("x")))
     (state:set-property! alice pb 'file "/tmp/a.txt")
     (state:set-property! bot pb 'read-only #t)

     (check 'property-read (state:property pb 'file) "/tmp/a.txt")
     (check 'property-cross-actor (state:property pb 'read-only) #t)
     (check 'property-absent-is-false (state:property pb 'mode) #f)
     (check 'properties-listed
            (list (assq 'file (state:properties pb))
                  (assq 'read-only (state:properties pb)))
            '((file . "/tmp/a.txt") (read-only . #t)))

     ;; #f removes; facts survive a reset (they are not text)
     (state:set-property! bot pb 'read-only #f)
     (check 'property-removed (state:property pb 'read-only) #f)
     (state:reset! bot pb '("fresh"))
     (check 'property-survives-reset (state:property pb 'file) "/tmp/a.txt")

     ;; subscribers hear fact changes
     (define prop-events (box '()))
     (define prop-token
       (state:subscribe!
         pb (lambda (event)
              (when (eq? (car event) 'property)
                (set-box! prop-events
                          (cons event (unbox prop-events)))))))
     (state:set-property! alice pb 'mode "scheme")
     (check 'property-event
            (car (unbox prop-events))
            (list 'property pb 'mode alice))
     (state:unsubscribe! prop-token)
     (state:delete! alice pb)

     ;; -- the buffer lifecycle is an event stream too ------------------------

     (define life-events (box '()))
     (define life-token
       (state:subscribe!
         #f (lambda (event)
              (when (memq (car event) '(create rename delete))
                (set-box! life-events
                          (cons event (unbox life-events)))))))
     (define lb (state:create! bot "agent-notes" '("n")))
     (state:rename! bot lb "agent-log")
     (state:delete! bot lb)
     (check 'lifecycle-events
            (reverse (unbox life-events))
            (list (list 'create lb "agent-notes" bot)
                  (list 'rename lb "agent-log" bot)
                  (list 'delete lb bot)))
     (state:unsubscribe! life-token)

     ;; -- subscriptions are registry-owned ------------------------------------

     (define sub-events (box '()))
     (define owned-buffer (state:create! alice "owned" '("x")))
     (parameterize ([kernel:registering-module 'testmod])
       (state:subscribe!
         #f (lambda (event)
              (set-box! sub-events (cons event (unbox sub-events))))))
     (state:edit! alice owned-buffer (state:revision owned-buffer)
                  (span 0 0 0 0) '("a"))
     (check 'owned-subscription-hears (length (unbox sub-events)) 1)
     (kernel:retract-module! 'testmod)
     (state:edit! alice owned-buffer (state:revision owned-buffer)
                  (span 0 0 0 0) '("b"))
     (check 'retracted-subscription-is-silent
            (length (unbox sub-events)) 1)
     (state:delete! alice owned-buffer)

     ;; -- the ui's whole-line splices, as core's splice-lines! builds them ----

     ;; core.e splice-lines! turns "replace lines [from, to)" into a
     ;; span with three cases; these drive the same spans through
     ;; edit! and pin the resulting text
     (define (spliced from to inserted)
       (let* ([id (state:create! alice "spliced" '("aaa" "bbb" "ccc"))]
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
         (state:edit! alice id (state:revision id) sp replacement)
         (let-values ([(text revision) (state:snapshot id)])
           (state:delete! alice id)
           (vector->list text))))

     (check 'splice-interior (spliced 0 1 '("XX")) '("XX" "bbb" "ccc"))
     (check 'splice-interior-delete (spliced 1 2 '()) '("aaa" "ccc"))
     (check 'splice-interior-grow
            (spliced 1 2 '("p" "q")) '("aaa" "p" "q" "ccc"))
     (check 'splice-through-the-end (spliced 1 3 '("YY")) '("aaa" "YY"))
     (check 'splice-delete-tail (spliced 2 3 '()) '("aaa" "bbb"))
     (check 'splice-whole-buffer (spliced 0 3 '("Z")) '("Z"))
     (check 'splice-empty-buffer (spliced 0 3 '()) '(""))

     (format #t "~a state checks passed\n" checks)))
