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
                   make-time sleep make-mutex with-mutex))

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

     ;; Scope changes selection, never the original authorship or the
     ;; overlap rule.  Redo follows the requester of an undo, even when
     ;; that requester was undoing another actor's work.
     (define reviewer '(head reviewer))
     (define scoped (store:create! alice "undo-scopes" '("abc" "def")))
     (store:edit! alice scoped 0 (span 0 0 0 3) '("ALICE"))
     (store:edit! bot scoped 1 (span 1 0 1 3) '("BOT"))
     (check 'undo-authors-newest-first (store:undo-authors scoped) (list bot alice))
     (check 'undo-mine-is-the-default
            (call-with-values (lambda () (store:undo! reviewer scoped)) list)
            '(nothing #f))
     (define scoped-events '())
     (define scoped-token
       (store:subscribe! scoped (lambda (event) (set! scoped-events (cons event scoped-events)))))
     (check 'undo-another-actor
            (call-with-values (lambda () (store:undo! reviewer scoped (list 'actor bot))) list)
            '(applied 3))
     (check 'targeted-undo-only-changes-that-author
            (list (store:line scoped 0) (store:line scoped 1)) '("ALICE" "def"))
     (check 'undo-event-preserves-author-and-requester
            (list (cadddr (car scoped-events)) (list-ref (car scoped-events) 5))
            (list reviewer (list 'undo bot 2 2)))
     (check 'undo-history-preserves-author-and-requester
            (list (cadr (car (store:history scoped))) (list-ref (car (store:history scoped)) 5))
            (list reviewer (list 'undo bot 2 2)))
     (check 'undo-removes-author-from-candidates (store:undo-authors scoped) (list alice))
     (check 'other-actor-cannot-take-requesters-redo
            (call-with-values (lambda () (store:redo! bot scoped)) list) '(nothing #f))
     (check 'requester-can-redo-another-authors-change
            (call-with-values (lambda () (store:redo! reviewer scoped)) list) '(applied 4))
     (check 'redo-restores-selected-change (store:line scoped 1) "BOT")
     (check 'redo-records-original-author
            (list-ref (car (store:history scoped)) 5) (list 'redo bot 2 3))
     (store:undo! reviewer scoped 'all)
     (store:undo! reviewer scoped 'all)
     (check 'all-scope-walks-back-through-authors
            (list (store:line scoped 0) (store:line scoped 1)) '("abc" "def"))
     (store:redo! reviewer scoped)
     (store:redo! reviewer scoped)
     (check 'redo-walks-forward-through-authors
            (list (store:line scoped 0) (store:line scoped 1)) '("ALICE" "BOT"))
     (let-values ([(text revision changes) (store:snapshot-since scoped 0)])
       (check 'history-metadata-does-not-change-snapshot-chain
              (for-all (lambda (entry) (= (length entry) 3)) changes) #t))
     (store:unsubscribe! scoped-token)

     ;; Once an overlapping change has itself been undone, it must no
     ;; longer obstruct older history.  The actual provenance stays.
     (define overlap (store:create! alice "undo-overlap" '("abc")))
     (store:edit! alice overlap 0 (span 0 0 0 3) '("ALICE"))
     (store:edit! bot overlap 1 (span 0 0 0 5) '("BOT"))
     (check 'targeted-undo-respects-live-overlap
            (call-with-values (lambda () (store:undo! reviewer overlap (list 'actor alice))) list)
            '(blocked overlap))
     (store:undo! reviewer overlap 'all)
     (check 'undo-can-cross-a-compensated-overlap
            (call-with-values (lambda () (store:undo! alice overlap)) list) '(applied 4))
     (check 'compensated-overlap-restores-original (store:line overlap 0) "abc")
     (check 'cancellation-retains-the-audit-log (length (store:history overlap)) 4)

     ;; A user-level group may have a foreign edit between its parts.
     ;; All parts undo atomically, preserving that intervening work.
     (define grouped (store:create! alice "undo-group" '("base" "other")))
     (define typing '(typing 1))
     (store:edit! alice grouped 0 (span 0 0 0 4) '("Abase") (list typing "insert AB"))
     (store:edit! bot grouped 1 (span 1 0 1 0) '("G"))
     (store:edit! alice grouped 2 (span 0 0 0 5) '("ABbase") (list typing "insert AB"))
     (define group-observations '())
     (define group-token
       (store:subscribe! grouped
         (lambda (event)
           (set! group-observations
             (cons (list (store:line grouped 0) (store:line grouped 1)) group-observations)))))
     (check 'group-undo-has-an-atomic-receipt
            (call-with-values (lambda () (store:history-step! alice grouped 'undo 'mine)) list)
            (list 'applied (list 5 1 alice typing "insert AB")))
     (check 'group-notifications-see-the-complete-result
            group-observations '(("base" "Gother") ("base" "Gother")))
     (store:unsubscribe! group-token)
     (store:redo! alice grouped)
     (check 'group-redo-preserves-the-intervening-actor
            (list (store:line grouped 0) (store:line grouped 1)) '("ABbase" "Gother"))
     (store:undo! reviewer grouped 'all)
     (store:undo! reviewer grouped 'all)
     (check 'all-scope-treats-each-group-as-one-action
            (list (store:line grouped 0) (store:line grouped 1)) '("base" "other"))

     ;; A conflict in the oldest part is discovered before the newer
     ;; part commits, including notifications and redo eligibility.
     (define atomic (store:create! alice "undo-atomic" '("one" "two")))
     (store:edit! alice atomic 0 (span 0 0 0 3) '("ONE") '(group "two lines"))
     (store:edit! alice atomic 1 (span 1 0 1 3) '("TWO") '(group "two lines"))
     (store:edit! bot atomic 2 (span 0 0 0 3) '("BOT"))
     (define atomic-events '())
     (define atomic-token
       (store:subscribe! atomic (lambda (event) (set! atomic-events (cons event atomic-events)))))
     (check 'conflicted-group-refuses-before-any-commit
            (call-with-values (lambda () (store:undo! alice atomic)) list) '(blocked overlap))
     (check 'conflicted-group-keeps-text-revision-and-events
            (list (store:line atomic 0) (store:line atomic 1) (store:revision atomic) atomic-events)
            '("BOT" "TWO" 3 ()))
     (check 'refused-undo-has-no-redo
            (call-with-values (lambda () (store:redo! alice atomic)) list) '(nothing #f))
     (store:unsubscribe! atomic-token)
     (store:undo! reviewer atomic (list 'actor bot))
     (store:undo! alice atomic)
     (check 'group-can-be-retried-after-conflict-is-undone
            (list (store:line atomic 0) (store:line atomic 1)) '("one" "two"))
     (store:edit! bot atomic (store:revision atomic) (span 0 0 0 3) '("changed"))
     (check 'redo-refuses-a-new-overlap
            (call-with-values (lambda () (store:redo! alice atomic)) list) '(blocked overlap))
     (store:edit! alice atomic (store:revision atomic) (span 1 0 1 0) '("new "))
     (check 'new-edit-invalidates-requesters-redo
            (call-with-values (lambda () (store:redo! alice atomic)) list) '(nothing #f))

     ;; Retention cannot turn an incomplete action into a partial undo.
     (define aged (store:create! alice "undo-retention" '("old" "tail")))
     (store:edit! alice aged 0 (span 0 0 0 3) '("OLD"))
     (do ([i 0 (+ i 1)]) ((= i 257))
       (store:edit! bot aged (store:revision aged) (span 1 0 1 0) '("x") '(long "long group")))
     (define aged-revision (store:revision aged))
     (check 'missing-provenance-refuses-history
            (call-with-values (lambda () (store:undo! alice aged)) list) '(blocked basis-too-old))
     (check 'truncated-group-refuses-as-a-whole
            (call-with-values (lambda () (store:undo! reviewer aged 'all)) list) '(blocked basis-too-old))
     (check 'retention-refusals-do-not-commit (store:revision aged) aged-revision)
     (store:reset! bot aged '("new baseline"))
     (check 'reset-does-not-resurrect-history
            (call-with-values (lambda () (store:undo! reviewer aged 'all)) list) '(nothing #f))

     ;; Text-related facts belong to the same transaction and inverse,
     ;; so another head can undo exact file contents without UI snapshots.
     (define factful (store:create! alice "undo-facts" '("base")))
     (store:set-property! alice factful 'trailing #f)
     (define fact-observations '())
     (define fact-token
       (store:subscribe! factful
         (lambda (event)
           (set! fact-observations
             (cons (list (store:line factful 0) (store:property factful 'trailing))
                   fact-observations)))))
     (store:edit! alice factful 0 (span 0 0 0 4) '("BASE")
                  '(format "format" ((trailing . #t))))
     (check 'text-and-facts-commit-together fact-observations '(("BASE" #t) ("BASE" #t)))
     (store:unsubscribe! fact-token)
     (store:undo! reviewer factful 'all)
     (check 'another-actor-undoes-text-and-facts
            (list (store:line factful 0) (store:property factful 'trailing)) '("base" #f))
     (store:redo! reviewer factful)
     (check 'another-actor-redoes-text-and-facts
            (list (store:line factful 0) (store:property factful 'trailing)) '("BASE" #t))
     (store:set-property! bot factful 'trailing #f)
     (store:set-property! bot factful 'trailing #t)
     (define fact-revision (store:revision factful))
     (check 'property-write-and-return-still-blocks-undo
            (call-with-values (lambda () (store:undo! reviewer factful 'all)) list)
            '(blocked property-changed))
     (check 'fact-conflict-keeps-text-and-revision
            (list (store:line factful 0) (store:revision factful)) (list "BASE" fact-revision))

     ;; Multiple changes of one property in a group restore the right
     ;; version in each inverse.  Even an absent property has a version.
     (define absent (store:create! alice "undo-absent-fact" '("a" "b")))
     (store:edit! alice absent 0 (span 0 0 0 1) '("A") '(g "group" ((trailing . #f))))
     (store:edit! alice absent 1 (span 1 0 1 1) '("B") '(g "group" ((trailing . #t))))
     (store:undo! reviewer absent 'all)
     (check 'grouped-fact-undo-restores-absence (store:properties absent) '())
     (store:redo! reviewer absent)
     (check 'grouped-fact-redo-restores-value (store:property absent 'trailing) #t)
     (store:undo! reviewer absent 'all)
     (store:drop-property! bot absent 'trailing)
     (check 'another-drop-invalidates-absent-fact-redo
            (call-with-values (lambda () (store:redo! reviewer absent)) list)
            '(blocked property-changed))
     (check 'property-tombstones-stay-private (store:properties absent) '())
     (define invalid-context-revision (store:revision absent))
     (check 'duplicate-transaction-properties-refuse
            (guard (ex [else #t])
              (store:edit! alice absent invalid-context-revision (span 0 0 0 1) '("bad")
                           '(g "bad" ((trailing . #t) (trailing . #f))))
              #f)
            #t)
     (check 'invalid-context-never-commits (store:revision absent) invalid-context-revision)

     ;; Generated chronological histories must walk exactly back and
     ;; forward, including overlapping replacements and line changes.
     ;; Each expected state was captured before undo, not computed by
     ;; the history implementation or its cancellation algorithm.
     (define history-seed 731)
     (define (choose n)
       (set! history-seed (mod (+ (* history-seed 25173) 13849) 65536))
       (mod history-seed n))
     (define (snapshot-text id)
       (let-values ([(text revision) (store:snapshot id)]) text))
     (define (text-positions text)
       (let rows ([r 0] [out '()])
         (if (= r (vector-length text)) out
             (let cols ([c 0] [out out])
               (if (> c (string-length (vector-ref text r)))
                   (rows (+ r 1) out)
                   (cols (+ c 1) (cons (cons r c) out)))))))
     (check 'generated-histories-round-trip-through-all-actors
            (let cases ([left 40])
              (or (zero? left)
                  (let* ([id (store:create! alice "generated-history" '("abc" "def"))]
                         [states
                          (let edits ([left 8] [states (list (snapshot-text id))])
                            (if (zero? left) states
                                (let* ([positions (text-positions (car states))]
                                       [start (list-ref positions (choose (length positions)))]
                                       [end (list-ref positions (choose (length positions)))]
                                       [actor (list-ref (list alice bot reviewer) (choose 3))]
                                       [replacement (list-ref '(("") ("X") ("Y" "Z") ("" "")) (choose 4))])
                                  (store:edit! actor id (store:revision id)
                                               (span (car start) (cdr start) (car end) (cdr end)) replacement)
                                  (edits (- left 1) (cons (snapshot-text id) states)))))])
                    (and
                      (for-all
                        (lambda (expected)
                          (let-values ([(status detail) (store:undo! reviewer id 'all)])
                            (and (eq? status 'applied) (equal? expected (snapshot-text id)))))
                        (cdr states))
                      (for-all
                        (lambda (expected)
                          (let-values ([(status detail) (store:redo! reviewer id)])
                            (and (eq? status 'applied) (equal? expected (snapshot-text id)))))
                        (cdr (reverse states)))
                      (begin (store:delete! alice id) (cases (- left 1)))))))
            #t)

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

     ;; Hold the first event before another subscriber sees it.  A
     ;; second writer must commit without entering callbacks in parallel
     ;; or overtaking that event.  Gates control ordering, not sleeps.
     (define gate-lock (make-mutex))
     (define (gate-set! gate value)
       (with-mutex gate-lock (set-box! gate value)))
     (define (gate-read gate)
       (with-mutex gate-lock (unbox gate)))
     (define (await-gate gate)
       (let wait ([tries 400])
         (or (gate-read gate)
             (begin
               (when (zero? tries)
                 (error 'store-test "notification gate timed out"))
               (sleep (make-time 'time-duration 10000000 0))
               (wait (- tries 1))))))

     (define ordered (store:create! alice "ordered" '("abcdef")))
     (define delivered (box '()))
     (define entered (box #f))
     (define release (box #f))
     (define completed (box #f))
     (define observer
       (store:subscribe! ordered
         (lambda (event)
           (gate-set! delivered
                      (cons (caddr event) (gate-read delivered))))))
     ;; Registrations run newest first: the blocker precedes observer.
     (define blocker
       (store:subscribe! ordered
         (lambda (event)
           (when (= (caddr event) 1)
             (gate-set! entered #t)
             (await-gate release)))))
     (fork-thread
       (lambda ()
         (gate-set! completed
           (guard (ex [else 'failed])
             (edit! alice ordered 0 0 0 0 2 '("x"))))))
     (await-gate entered)
     (define second-result (edit! bot ordered 1 0 0 0 2 '("y")))
     (define while-blocked (gate-read delivered))
     (gate-set! release #t)
     (await-gate completed)
     (check 'concurrent-writer-commits second-result '(applied 2))
     (check 'blocked-writer-finishes (gate-read completed) '(applied 1))
     (check 'callbacks-do-not-race while-blocked '())
     (check 'events-follow-commit-order (reverse (gate-read delivered)) '(1 2))
     (store:unsubscribe! blocker)
     (store:unsubscribe! observer)

     ;; A subscriber may itself write.  Every observer finishes the
     ;; parent event before hearing that nested edit; there is no lock
     ;; held across either callback or a wait for nested delivery.
     (define nested (store:create! alice "nested" '("abc")))
     (define nested-events '())
     (define nested-observer
       (store:subscribe! nested
         (lambda (event)
           (set! nested-events (cons (caddr event) nested-events)))))
     (define nested-writer
       (store:subscribe! nested
         (lambda (event)
           (when (= (caddr event) 1)
             (store:edit! bot nested 1 (span 0 0 0 0) '("y"))))))
     (store:edit! alice nested 0 (span 0 0 0 0) '("x"))
     (check 'reentrant-events-follow-commit-order (reverse nested-events) '(1 2))
     (check 'reentrant-write-landed (store:line nested 0) "yxabc")
     (store:unsubscribe! nested-writer)
     (store:unsubscribe! nested-observer)

     ;; A coherent snapshot includes every retained intervening delta,
     ;; even before a writer's notifications have finished delivery.
     (let-values ([(text revision changes) (store:snapshot-since nested 0)])
       (check 'incremental-snapshot-text text '#("yxabc"))
       (check 'incremental-snapshot-revision revision 2)
       (check 'incremental-snapshot-order (map car changes) '(1 2))
       (check 'incremental-snapshot-attribution (map cadr changes) (list alice bot))
       (check 'incremental-snapshot-deltas (for-all text:delta? (map caddr changes)) #t))
     (let-values ([(text revision changes) (store:snapshot-since nested 2)])
       (check 'incremental-snapshot-already-current changes '()))
     (let-values ([(text revision changes) (store:snapshot-since nested 3)])
       (check 'incremental-snapshot-future-basis changes #f))
     (store:reset! bot nested '("fresh"))
     (let-values ([(text revision changes) (store:snapshot-since nested 2)])
       (check 'incremental-snapshot-reset-gap (list text revision changes)
              '(#("fresh") 3 #f)))
     (do ([i 0 (+ i 1)]) ((= i 257))
       (store:edit! bot nested (+ 3 i) (span 0 0 0 0) '("x")))
     (let-values ([(text revision changes) (store:snapshot-since nested 3)])
       (check 'incremental-snapshot-truncated-gap changes #f))
     (let-values ([(text revision changes) (store:snapshot-since nested 4)])
       (check 'incremental-snapshot-retained-boundary
              (list (length changes) (caar changes) (car (car (reverse changes))) revision)
              '(256 5 260 260)))

     ;; Registration applies to future commits; revocation also removes
     ;; callbacks queued behind a subscriber that is currently running.
     (define revoked (store:create! alice "revoked" '("abc")))
     (define revoked-events '())
     (define late-events '())
     (define late-token #f)
     (define revoked-token
       (store:subscribe! revoked
         (lambda (event) (set! revoked-events (cons event revoked-events)))))
     (define revoker
       (store:subscribe! revoked
         (lambda (event)
           (when (= (caddr event) 1)
             (store:edit! bot revoked 1 (span 0 0 0 0) '("y"))
             (set! late-token
               (store:subscribe! revoked
                 (lambda (event) (set! late-events (cons (caddr event) late-events)))))
             (store:unsubscribe! revoked-token)))))
     (store:edit! alice revoked 0 (span 0 0 0 0) '("x"))
     (check 'revocation-skips-queued-callbacks revoked-events '())
     (check 'subscription-skips-earlier-commits late-events '())
     (store:edit! alice revoked 2 (span 0 0 0 0) '("z"))
     (check 'subscription-hears-later-commits late-events '(3))
     (store:unsubscribe! revoker)
     (store:unsubscribe! late-token)

     ;; One failing or escaping subscriber must not wedge the stream or
     ;; discard the callbacks and events queued behind it.
     (define escaping (store:create! alice "escaping" '("abc")))
     (define escape-events '())
     (define escape-observer
       (store:subscribe! escaping
         (lambda (event) (set! escape-events (cons (caddr event) escape-events)))))
     (define escape-token #f)
     (check 'subscriber-can-escape
       (call/cc
         (lambda (escape)
           (set! escape-token
             (store:subscribe! escaping
               (lambda (event)
                 (when (= (caddr event) 1)
                   (store:edit! bot escaping 1 (span 0 0 0 0) '("y"))
                   (escape 'escaped)))))
           (store:edit! alice escaping 0 (span 0 0 0 0) '("x"))
           'returned))
       'escaped)
     (check 'escape-drains-remaining-callbacks (reverse escape-events) '(1 2))
     (store:unsubscribe! escape-token)
     (define failing-token
       (store:subscribe! escaping
         (lambda (event) (error 'subscriber "intentional failure"))))
     (store:edit! alice escaping 2 (span 0 0 0 0) '("z"))
     (check 'subscriber-failure-keeps-stream-live (reverse escape-events) '(1 2 3))
     (store:unsubscribe! failing-token)
     (store:unsubscribe! escape-observer)

     (define saved-delivery #f)
     (define capture-token
       (store:subscribe! escaping
         (lambda (event)
           (call/cc (lambda (resume) (set! saved-delivery resume))))))
     (check 'completed-delivery-cannot-be-resumed
            (let ([attempted? #f])
              (guard (ex [else 'refused])
                (store:edit! alice escaping 3 (span 0 0 0 0) '("q"))
                (if attempted?
                    'resumed
                    (begin
                      (set! attempted? #t)
                      (saved-delivery 'again)))))
            'refused)
     (store:unsubscribe! capture-token)
     (check 'delivery-remains-live-after-refused-resume
            (edit! alice escaping 4 0 0 0 0 '("r")) '(applied 5))

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
