#!/usr/bin/env scheme-script

;; Command history selects mine, all, or a named actor through the
;; store journal, while local buffers keep snapshot history.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (prefix (head head) head:)
             (prefix (state store) store:)
             (prefix (foundation text) text:)
             (prefix (foundation string) string:)
             (prefix (head mode) mode:)
             (prefix (core kernel) kernel:))

     (define check test:check)
     (store:log-retention 256)   ; the bound these checks exercise
     (define bot '(agent undo-test))
     (define (fresh name shared?)
       (let ([b ((if shared? head:new-buffer! head:new-local-buffer!) name)])
         (head:buffer-lines-set! b '#("base" "other"))
         (head:show-buffer! b)
         (head:goto! '(0 . 0))
         b))
     (define (text-of b) (vector->list (head:buffer-lines b)))
     (define (blocked? report) (and (string:search report "blocked" 0 (string-length report)) #t))
     (define (nothing? report) (and (string:search report "No further" 0 (string-length report)) #t))

     ;; UI edit, foreign edit, UI edit, then two undos.  The first undo
     ;; must not erase the foreign provenance.  Both own edits can be
     ;; undone because their inverses are disjoint from the agent's edit.
     (define b (fresh "undo-foreign" #t))
     (define id (head:buffer-store-id b))
     (insert-text! "A")
     (store:edit! bot id (store:revision id) (text:make-span 1 0 1 0) '("G"))
     (head:before-frame!)
     (head:goto! '(0 . 5))
     (insert-text! "B")
     (undo!)
     (check 'first-undo-preserves-foreign-text (text-of b) '("Abase" "Gother"))
     (check 'undo-retains-foreign-provenance
            (and (exists (lambda (entry) (equal? (cadr entry) bot)) (store:history id 256)) #t)
            #t)
     (check 'second-undo-applies (blocked? (undo!)) #f)
     (check 'second-undo-keeps-foreign-text (text-of b) '("base" "Gother"))
     (check 'mine-does-not-fall-through-to-other-actors (nothing? (undo!)) #t)
     (redo!)
     (redo!)
     (check 'redo-keeps-foreign-text (text-of b) '("AbaseB" "Gother"))
     (undo!)
     (check 'undo-after-redo-keeps-foreign-text (text-of b) '("Abase" "Gother"))

     ;; Ordinary undo/redo can traverse several entries without clearing
     ;; the store's delta history.  Unchanged content keeps others' marks.
     (define plain (fresh "undo-plain" #t))
     (define plain-id (head:buffer-store-id plain))
     (store:set-mark! bot plain-id 'anchor '(1 . 2))
     (insert-text! "A")
     (insert-text! "B")
     (undo!)
     (undo!)
     (check 'repeated-ordinary-undo (text-of plain) '("base" "other"))
     (check 'undo-keeps-unrelated-mark (store:mark bot plain-id 'anchor) '(1 . 2))
     (redo!)
     (redo!)
     (check 'repeated-ordinary-redo (text-of plain) '("ABbase" "other"))

     ;; A reset or truncation cannot authorize restoring a head snapshot.
     (define reset-buffer (fresh "undo-reset" #t))
     (define reset-id (head:buffer-store-id reset-buffer))
     (insert-text! "A")
     (store:reset! bot reset-id '("foreign reset"))
     (head:before-frame!)
     (check 'reset-gap-has-no-undo (nothing? (undo!)) #t)
     (check 'reset-gap-keeps-text (text-of reset-buffer) '("foreign reset"))

     (define truncated (fresh "undo-truncated" #t))
     (define truncated-id (head:buffer-store-id truncated))
     (insert-text! "A")
     (store:edit! bot truncated-id (store:revision truncated-id)
                  (text:make-span 1 0 1 0) '("G"))
     (do ([i 0 (+ i 1)]) ((= i 257))
       (store:edit! bot truncated-id (store:revision truncated-id)
                    (text:make-span 1 0 1 0) '("x") '(long "long agent action")))
     (head:before-frame!)
     (define truncated-text (text-of truncated))
     (check 'truncated-provenance-refuses-undo (blocked? (undo!)) #t)
     (check 'truncated-provenance-keeps-text (text-of truncated) truncated-text)

     ;; A foreign edit made by a subscriber after undo commits still
     ;; reaches the cache.  Redo rebases its inverse, preserving that edit.
     (define raced (fresh "undo-callback" #t))
     (define raced-id (head:buffer-store-id raced))
     (insert-text! "A")
     (define token
       (store:subscribe! raced-id
         (lambda (event)
           (when (and (eq? (car event) 'edit)
                      (equal? (cadddr event) head:ui-actor))
             (store:edit! bot raced-id (store:revision raced-id)
                          (text:make-span 1 0 1 0) '("G"))))))
     (undo!)
     (store:unsubscribe! token)
     (check 'foreign-edit-during-undo-is-kept (text-of raced) '("base" "Gother"))
     (check 'redo-after-disjoint-racing-foreign-edit-applies (blocked? (redo!)) #f)
     (check 'redo-keeps-racing-foreign-edit (text-of raced) '("Abase" "Gother"))

     ;; A live overlap refuses, with no history movement or side effects.
     (define overlap (fresh "undo-overlap" #t))
     (define overlap-id (head:buffer-store-id overlap))
     (insert-text! "A")
     (store:edit! bot overlap-id (store:revision overlap-id) (text:make-span 0 0 0 5) '("BOT"))
     (head:before-frame!)
     (define before-refusal (head:buffer-history overlap))
     (define pending-undo (vector-ref before-refusal 0))
     (define pending-redo (vector-ref before-refusal 1))
     (check 'overlap-refuses (blocked? (undo!)) #t)
     (check 'overlap-keeps-foreign-text (text-of overlap) '("BOT" "other"))
     (check 'refusal-keeps-undo-stack (eq? pending-undo (vector-ref before-refusal 0)) #t)
     (check 'refusal-keeps-redo-stack (eq? pending-redo (vector-ref before-refusal 1)) #t)
     (undo-actor! bot)
     (check 'targeted-undo-restores-covered-own-edit (text-of overlap) '("Abase" "other"))
     (undo!)
     (check 'mine-can-follow-another-actors-undo (text-of overlap) '("base" "other"))

     ;; Scope is a validated head preference.  A per-call choice does
     ;; not change it, and redo follows the requester in either mode.
     (check 'default-scope (undo-scope) 'mine)
     (check 'invalid-scope-refuses (guard (ex [else #t]) (undo-scope 'everyone) #f) #t)
     (check 'invalid-scope-keeps-default (undo-scope) 'mine)
     (define scoped (fresh "undo-scoped" #t))
     (define scoped-id (head:buffer-store-id scoped))
     (insert-text! "A")
     (store:edit! bot scoped-id (store:revision scoped-id) (text:make-span 1 0 1 0) '("G"))
     (head:before-frame!)
     (parameterize ([undo-scope 'all])
       (undo!)
       (check 'all-selects-latest-foreign-action (text-of scoped) '("Abase" "other"))
       (undo!)
       (check 'all-continues-to-own-action (text-of scoped) '("base" "other"))
       (redo!)
       (redo!)
       (check 'all-redo-restores-both-authors (text-of scoped) '("Abase" "Gother"))
       (parameterize ([undo-scope 'mine]) (undo!))
       (check 'one-off-mine-preserves-other-author (text-of scoped) '("base" "Gother"))
       (check 'one-off-scope-keeps-preference (undo-scope) 'all))
     (check 'scope-is-head-local-configuration (store:property scoped-id 'undo-scope) #f)
     (parameterize ([undo-scope 'all]) (undo!))
     (check 'one-off-all-undo-with-no-own-history (text-of scoped) '("base" "other"))
     (redo!)
     (check 'redo-a-foreign-action-while-default-is-mine (text-of scoped) '("base" "Gother"))
     (check 'scope-restores-after-parameterize (undo-scope) 'mine)

     ;; Groups remain one action even with multiple line transactions.
     (define grouped (fresh "undo-grouped" #t))
     (call-as-one-edit! "two lines"
       (lambda () (insert-text! "A") (newline!) (insert-text! "B")))
     (define grouped-text (text-of grouped))
     (undo!)
     (check 'grouped-command-undo (text-of grouped) '("base" "other"))
     (redo!)
     (check 'grouped-command-redo (text-of grouped) grouped-text)

     ;; Whole-vector command results are edits, so both formatting and
     ;; indentation retain undo.  Formatting also restores the local
     ;; command's final-newline choice when that command is undone.
     (define formatted (fresh "undo-formatted" #t))
     (mode:register! "undo-format" '() '() (lambda (line) #f))
     (head:with-buffer formatted (mode:choose! "undo-format"))
     (head:buffer-trailing-set! formatted #f)
     (mode:register-formatter! "undo-format" (lambda (b from to) '("BASE" "OTHER")))
     (format-buffer!)
     (check 'format-applies (text-of formatted) '("BASE" "OTHER"))
     (undo!)
     (check 'format-undo (text-of formatted) '("base" "other"))
     (check 'format-undo-restores-final-newline (head:buffer-trailing formatted) #f)
     (redo!)
     (check 'format-redo (text-of formatted) '("BASE" "OTHER"))
     (check 'format-redo-restores-final-newline (head:buffer-trailing formatted) #t)
     (head:buffer-trailing-set! formatted #f)
     (format-buffer!)
     (check 'format-can-change-only-final-newline (head:buffer-trailing formatted) #t)
     (undo!)
     (check 'metadata-only-format-undo (buffer-text formatted) "BASE\nOTHER")
     (redo!)
     (check 'metadata-only-format-redo (buffer-text formatted) "BASE\nOTHER\n")
     (mode:register-indenter! "undo-format" (lambda (b from to) '(2 4)))
     (indent-buffer!)
     (check 'indent-applies (text-of formatted) '("  BASE" "    OTHER"))
     (undo!)
     (check 'indent-undo (text-of formatted) '("BASE" "OTHER"))

     ;; Ordinary undo must not restore an old snapshot's unrelated
     ;; final-newline flag.  Formatting does own that flag, and refuses
     ;; its entire inverse when another actor has subsequently changed it.
     (define foreign-fact (fresh "undo-foreign-fact" #t))
     (define foreign-fact-id (head:buffer-store-id foreign-fact))
     (insert-text! "A")
     (store:set-property! bot foreign-fact-id 'trailing #f)
     (undo!)
     (check 'ordinary-undo-preserves-foreign-final-newline (buffer-text foreign-fact) "base\nother")
     (head:with-buffer foreign-fact (mode:choose! "undo-format"))
     (format-buffer!)
     (store:set-property! bot foreign-fact-id 'trailing #f)
     (check 'format-undo-refuses-foreign-fact-change (blocked? (undo!)) #t)
     (check 'fact-conflict-keeps-entire-formatted-result (buffer-text foreign-fact) "BASE\nOTHER")

     ;; The same editing guard protects history actions and fresh edits.
     (head:show-buffer! plain)
     (head:buffer-read-only-set! plain #t)
     (define protected-text (text-of plain))
     (check 'read-only-history-commands-refuse
       (map (lambda (command)
              (guard (ex [(kernel:read-only-error? ex) #t] [else (raise ex)]) (command) #f))
            (list undo! (lambda () (parameterize ([undo-scope 'all]) (undo!))) (lambda () (undo-actor! bot)) redo!))
       '(#t #t #t #t))
     ;; The store transaction still protects the buffer if its flag changed
     ;; after the command preflight, or a caller enters the head seam directly.
     (check 'read-only-shared-entrypoints-refuse
       (cons (guard (ex [(kernel:refusal? ex) (condition-message ex)] [else (raise ex)])
               (head:store-edit! plain (text:make-span 0 0 0 0) '("bad")) #f)
             (map (lambda (direction)
                    (call-with-values (lambda () (head:store-history! plain direction 'mine)) list))
                  '(undo redo)))
       '("Edit not applied: the buffer is read-only" (refused read-only) (refused read-only)))
     (check 'read-only-keeps-text (text-of plain) protected-text)
     (head:buffer-read-only-set! plain #f)

     ;; Local history requires no store provenance and emits no events.
     (define local (fresh "undo-local" #f))
     (define local-events '())
     (define local-token
       (store:subscribe! #f (lambda (event) (set! local-events (cons event local-events)))))
     (insert-text! "A")
     (undo!)
     (check 'local-undo (text-of local) '("base" "other"))
     (redo!)
     (check 'local-redo (text-of local) '("Abase" "other"))
     (parameterize ([undo-scope 'all]) (undo!))
     (check 'all-scope-on-local-buffer-stays-local (text-of local) '("base" "other"))
     (undo-actor! bot)
     (check 'foreign-actor-choice-does-not-change-local-buffer (text-of local) '("base" "other"))
     (check 'local-history-stays-local local-events '())
     (store:unsubscribe! local-token)
     (head:with-buffer local (mode:choose! "undo-format"))
     (head:buffer-trailing-set! local #f)
     (format-buffer!)
     (check 'local-format-sets-final-newline (buffer-text local) "BASE\nOTHER\n")
     (undo!)
     (check 'local-format-undo-restores-final-newline (buffer-text local) "base\nother")

     (test:finish! 'undo)))
