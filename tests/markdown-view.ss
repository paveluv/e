#!/usr/bin/env scheme-script

;; Markdown is a local presentation of live source, never a replacement
;; for it.  Exercise ownership, two windows, links, reload, and lifecycle.
;; Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (head edit) init!)
             (prefix (head head) head:)
             (prefix (state store) store:)
             (prefix (foundation text) text:)
             (prefix (head mode) mode:)
             (prefix (modes md-mode) md-mode:)
             (prefix (apps markdown) markdown:)
             (prefix (core kernel) kernel:)
             (prefix (head keymap) keymap:)
             (prefix (service file) file:)
             (prefix (head paint) paint:)
             (prefix (test) test:))

     (define check test:check)
     (define refused? test:raises?)
     (define (find-row b text)
       (let loop ([r 0])
         (cond [(= r (head:buffer-line-count b)) (error 'find-row "missing line" text)]
               [(string=? (head:buffer-line b r) text) r]
               [else (loop (+ r 1))])))
     (define (make-source name lines)
       (let ([b (head:new-buffer! name)])
         (head:buffer-lines-set! b (list->vector lines))
         (mode:choose! b "markdown")
         (head:add-buffer! b)
         b))

     (md-mode:init!)
     (parameterize ([kernel:registering-module 'markdown-test]) (markdown:init!))

     (define source (make-source "notes.md" '("# Heading" "" "one" "two")))
     (define id (head:buffer-store-id source))
     (head:buffer-wrap-set! source #t)
     (define history (vector '(saved-undo) '(saved-redo)))
     (head:buffer-history-set! source history)
     (head:show-buffer! source)
     (head:goto! '(2 . 1))
     (define w1 (head:current-window))
     (head:window-width-set! w1 80)
     (head:window-size-set! w1 12)
     (define w2 (head:make-window source 0 0 0 0 0 12 80 80 'default))
     (head:set-layout-root! (head:make-layout-split 'right w1 w2 1 1))

     (define original-text (head:buffer-lines source))
     (define original-revision (store:revision id))
     (define original-facts (store:properties id))
     (define original-buffers (list-sort < (store:buffer-list)))
     (define events '())
     (define subscription
       (store:subscribe! #f (lambda (event) (set! events (cons event events)))))
     (define companion (markdown:companion! source))
     (define companion-keeps-focus (and (eq? (head:current-buffer) source) (equal? (head:point) '(2 . 1))))
     (markdown:view!)
     (define view (head:current-buffer))
     (check 'companion-is-independent-of-window-selection
            (list (eq? view source) (eq? view companion) companion-keeps-focus
                  (eq? (markdown:companion source) view)) '(#f #t #t #t))
     (check 'view-is-local (head:buffer-store-id view) #f)
     (check 'view-label-is-local (head:buffer-name view) "<markdown notes.md>")
     (check 'view-is-an-app (head:app-buffer? view) #t)
     (check 'view-is-read-only (head:buffer-read-only view) #t)
     (check 'view-has-no-file (head:buffer-file view) #f)
     (check 'view-is-formatted (head:buffer-lines view) '#("Heading" "" "one two"))
     (check 'other-window-keeps-source (eq? (head:window-buffer w2) source) #t)
     (check 'source-vector-is-untouched (eq? (head:buffer-lines source) original-text) #t)
     (check 'source-revision-is-untouched (store:revision id) original-revision)
     (check 'source-facts-are-untouched (store:properties id) original-facts)
     (check 'source-history-is-untouched (eq? (head:buffer-history source) history) #t)
     (check 'source-mode-stays-markdown (mode:name-of source) "markdown")
     (check 'viewing-keeps-store-buffers (list-sort < (store:buffer-list)) original-buffers)
     (check 'viewing-emits-no-store-events events '())
     (markdown:edit!)
     (check 'toggle-returns-source (eq? (head:current-buffer) source) #t)
     (check 'toggle-keeps-source-row (head:point) '(2 . 0))
     (check 'toggle-does-not-restore-text (store:revision id) original-revision)
     (check 'toggle-does-not-write-facts (store:properties id) original-facts)
     (check 'toggle-is-local events '())
     (store:unsubscribe! subscription)

     (head:buffer-name-set! source "renamed.md")
     (head:buffer-read-only-set! source #t)
     (markdown:view!)
     (check 'source-rename-keeps-companion (eq? (head:current-buffer) view) #t)
     (markdown:edit!)
     (check 'source-read-only-is-preserved (head:buffer-read-only source) #t)
     (head:buffer-read-only-set! source #f)
     (markdown:view!)
     (store:edit! '(agent markdown-test) id (store:revision id)
                  (text:make-span 2 0 2 0) '("foreign "))
     (head:before-frame!)
     (check 'foreign-source-edit-reaches-view
            (head:buffer-lines view) '#("Heading" "" "foreign one two"))
     (markdown:edit!)
     (check 'return-keeps-foreign-edit (store:line id 2) "foreign one")
     (check 'return-keeps-source-history (eq? (head:buffer-history source) history) #t)

     ;; Resize a shared presentation with independent window positions.
     (define table
       (make-source "table.md"
         '("|alpha beta gamma delta epsilon|x|" "|-|-|" "|long entry|y|"
           "" "After table")))
     (head:show-buffer! table)
     (markdown:view!)
     (define table-view (head:current-buffer))
     (head:set-window-buffer! w2 table-view)
     (define after (find-row table-view "After table"))
     (head:window-prow-set! w2 after)
     (head:window-top-set! w2 after)
     (head:buffer-spot-row-set! table-view after)
     (head:buffer-spot-col-set! table-view 3)
     (head:buffer-spot-top-set! table-view after)
     (head:buffer-mark-row-set! table-view after)
     (head:buffer-mark-col-set! table-view 3)
     (head:buffer-marked-set! table-view #t)
     (head:goto! '(0 . 0))
     (check 'app-point-is-per-window (head:window-prow w2) after)
     (head:window-width-set! w1 24)
     (head:before-frame!)
     (check 'table-refit-changes-row-count
            (> (find-row table-view "After table") after) #t)
     (check 'refit-keeps-other-window-content
            (head:buffer-line table-view (head:window-prow w2)) "After table")
     (check 'refit-keeps-other-window-top
            (head:buffer-line table-view (head:window-top w2)) "After table")
     (check 'refit-keeps-selected-window-row (head:window-prow w1) 0)
     (check 'refit-keeps-selection-mark
            (head:buffer-line table-view (head:buffer-mark-row table-view)) "After table")
     (check 'refit-keeps-saved-position
            (head:buffer-line table-view (head:buffer-spot-row table-view)) "After table")
     (check 'refit-keeps-saved-viewport
            (head:buffer-line table-view (head:buffer-spot-top table-view)) "After table")

     ;; Rebind an already registered runtime companion from its input.
     (define callback (head:app-refresh! (head:app-of table-view)))
     (kernel:retract-module! 'markdown-test)
     (parameterize ([kernel:registering-module 'markdown-test]) (markdown:init!))
     (check 'reload-rebinds-existing-companion
            (eq? callback (head:app-refresh! (head:app-of table-view))) #f)
     (check 'reload-keeps-source-reference
            (eq? (head:buffer-fact table-view 'markdown-input #f) table) #t)
     (check 'reload-keeps-rendered-text
            (head:buffer-line table-view (head:window-prow w2)) "After table")

     ;; Literal markdown belongs to local app input, never a shared target.
     (define literal (head:new-local-buffer! "literal markdown"))
     (head:add-buffer! literal)
     (markdown:view-install! literal '("**literal**"))
     (check 'literal-render (head:buffer-lines literal) '#("literal"))
     (check 'literal-is-read-only (head:buffer-read-only literal) #t)
     (check 'literal-has-no-editable-source
            (refused? (lambda () (markdown:edit! literal))) #t)
     (define before-refused (store:revision id))
     (check 'rendering-into-store-refused
            (refused? (lambda () (markdown:view-install! source '("bad")))) #t)
     (check 'refused-render-preserves-source (store:revision id) before-refused)

     ;; Relative links use the source's directory; absolute paths stay so.
     (define directory
       (format "/tmp/e-markdown-view-~a-~a"
               (time-second (current-time)) (random 1000000000)))
     (mkdir directory)
     (mkdir (string-append directory "/docs"))
     (define child-path (string-append directory "/docs/child.md"))
     (file:write! child-path (vector "# Child") #t)
     (head:buffer-file-set! source (string-append directory "/notes.md"))
     (head:buffer-lines-set! source
       (vector "[child](docs/child.md)"))
     (head:show-buffer! source)
     (markdown:view!)
     (head:goto! '(0 . 0))
     (head:tile! 80 12)
     (let* ([w (head:current-window)] [cell (paint:window-screen-position w 0 1)])
       (head:set-mouse-position! (cons (cdr cell) (car cell)))
       (check 'markdown-link-hover-uses-the-link-label-without-moving-point
         (list (filter (lambda (range) (eq? (car range) w)) (paint:highlight-ranges)) (head:point))
         (list (list (list w 0 0 5 'hover)) '(0 . 0)))
       (head:set-mouse-position! #f))
     ((keymap:binding 'markdown-view "RET"))
     (define child-view (head:current-buffer))
     (define child (head:buffer-fact child-view 'markdown-input #f))
     (check 'relative-link-visits-source-directory (head:buffer-file child) child-path)
     (check 'relative-link-opens-local-view (head:buffer-store-id child-view) #f)
     (check 'relative-link-shows-child (head:buffer-lines child-view) '#("Child"))
     (head:buffer-lines-set! source (vector (format "[child](~a)" child-path)))
     (head:show-buffer! source)
     (markdown:view!)
     (head:goto! '(0 . 0))
     ((keymap:binding 'markdown-view "RET"))
     (check 'absolute-link-reuses-child-source
            (eq? (head:buffer-fact (head:current-buffer) 'markdown-input #f) child) #t)
     (delete-file child-path)
     (delete-directory (string-append directory "/docs"))
     (delete-directory directory)

     ;; Killing only the presentation preserves source; the inverse
     ;; closes all its dependent local presentations without resurrection.
     (kill-buffer! view)
     (check 'killing-view-preserves-source (store:exists? id) #t)
     (head:show-buffer! source)
     (markdown:view!)
     (define replacement-view (head:current-buffer))
     (check 'killed-view-is-recreated (eq? replacement-view view) #f)
     ;; Hiding and deleting share local retirement. A source in one window
     ;; and its companion in another disappear before any repaint callback.
     (define retiring #f)
     (define cleanup-count 0)
     (head:add-buffer-kill-hook!
       (lambda (b) (when (eq? b retiring) (set! cleanup-count (+ cleanup-count 1)))))
     (for-each
       (lambda (entry)
         (let* ([source (car entry)] [action (cadr entry)] [id (head:buffer-store-id source)]
                [hidden? (memq action '(hide-own hide-foreign))])
           (head:show-buffer! source)
           (markdown:view!)
           (let ([view (head:current-buffer)] [observations '()])
             (set! retiring source)
             (set! cleanup-count 0)
             (head:set-window-buffer! w2 source)
             (head:before-frame!)
             (head:set-repaint-hook!
               (lambda ()
                 (set! observations
                   (cons (list (memq source (head:buffers)) (memq view (head:buffers))
                               (head:app-of view)
                               (exists (lambda (w) (memq (head:window-buffer w) (list source view)))
                                       (head:windows)))
                         observations))))
             (case action
               [(kill) (kill-buffer! source)]
               [(delete) (store:delete! '(agent markdown-test) id)]
               [else (store:set-property! (if (eq? action 'hide-own) head:ui-actor '(agent markdown-test))
                                          id 'audience '())])
             (head:before-frame!)
             (head:forget-buffer! source) ; repeated/reentrant retirement is inert
             (head:set-repaint-hook! void)
             (check 'source-retirement-is-coherent
                    (list observations cleanup-count (store:exists? id)
                          (refused? (lambda () (markdown:edit! view)))
                          (refused? (lambda () (head:add-buffer! source)))
                          (and hidden? (store:mark head:ui-actor id 'point)))
                    ;; a killed document is trashed, not deleted: it stays in the store, hidden
                    (list '((#f #f #f #f)) 1 (and (or hidden? (eq? action 'kill)) #t) #t #t #f)))))
       (list (list source 'kill) (list table 'delete)
             (list (make-source "private-own.md" '("# Own")) 'hide-own)
             (list (make-source "private-foreign.md" '("# Foreign")) 'hide-foreign)))

     ;; The same companion relation works for a local markdown source.
     (define local-source (head:new-local-buffer! "local source"))
     (head:buffer-lines-set! local-source (vector "# Local"))
     (mode:choose! local-source "markdown")
     (head:show-buffer! local-source)
     (markdown:view!)
     (check 'local-source-gets-local-view (head:buffer-store-id (head:current-buffer)) #f)
     (check 'local-source-stays-source (head:buffer-lines local-source) '#("# Local"))
     (markdown:edit!)
     (check 'return-to-local-source (eq? (head:current-buffer) local-source) #t)

     (test:finish! 'markdown-view)))
