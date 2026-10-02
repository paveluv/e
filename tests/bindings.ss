#!/usr/bin/env scheme-script
;; Binding facts, per-view fitting and demand share one compact fixture.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(test-evaluate!
  '(begin
     (import (prefix (test) test:) (prefix (apps bindings) bindings:)
             (prefix (head binding-list) listing:)
             (prefix (head edit) edit:) (prefix (head head) head:) (prefix (service window) window:) (prefix (head keymap) keymap:)
             (prefix (head widget) widget:) (prefix (head window-control) window-control:)
             (prefix (core kernel) kernel:) (prefix (state store) store:) (prefix (head interaction) interaction:)
             (prefix (head mouse) mouse:)  (prefix (head routing) routing:)
             (prefix (foundation string) string:) (prefix (state model) model:) (prefix (state view) view:)
             (prefix (state actor) actor:))
     (define check test:check)
     (define (get r k) (cdr (assq k r)))
     (define (contains? s needle) (and (string:search s needle 0 (string-length s)) #t))
     (define (text rows) (string:join (map (lambda (r) (format "~s" r)) rows) "\n"))
     (for-each kernel:load-module! '("window-control" "edit" "bindings"))
     (keymap:bind-default! 'inspection-test "M-q" edit:kill-line!)
     (keymap:bind-default! 'inspection-test "M-w" edit:kill-line!)
     (keymap:bind-default! 'inspection-test "C-k" (keymap:call edit:move! widget:target 'end))
     (keymap:bind-default! 'inspection-test "M-z" (lambda () #f))
     (let* ([capture (listing:capture (listing:basis #f '(inspection-test global) #f '()) '())]
            [rows (car capture)] [s (text rows)]
            [readonly (car (listing:capture (listing:basis #f '(inspection-test global) #t '()) '()))])
       (check 'binding-facts-group-keys-and-retain-anonymous-and-named-actions
         (list (cadar rows) (contains? s "anonymous command")
           (exists (lambda (r) (and (member "M-q" (caddr r)) (member "M-w" (caddr r)) #t)) rows)
           (exists (lambda (r) (and (member "C-k" (caddr r)) (contains? (cadddr r) "move!"))) rows)
           (contains? (text readonly) "kill-line!") (cadr capture)) '("Mouse bindings" #t #t #t #f #f))
       (check 'fitting-preserves-logical-anchors-across-widths
         (let* ([wide (listing:fit rows 220)] [narrow (listing:fit rows 60)]
                [anchor (listing:anchor wide 5)] [at (listing:locate narrow anchor)])
           (list (> (vector-length narrow) (vector-length wide))
             (equal? (car anchor) (car (listing:anchor narrow at))))) '(#t #t)))
     (let* ([calls 0] [producer (lambda () (set! calls (+ calls 1)) 2)]
            [action (keymap:call list (keymap:call + producer 3) '(a b))]
            [s (keymap:action-text action (list (cons producer 2)))])
       (check 'tracing-never-runs-producers-and-spelled-calls-run-in-eval
         (list (begin (keymap:action-trace action) calls) (keymap:run! action) calls (eval (read (open-input-string s))))
         '(0 (5 (a b)) 1 (5 (a b)))))
     (check 'long-key-names-share-the-canonical-spelling
       (list (keymap:sequence-text (keymap:spec "M-BACKSPACE")) (keymap:spec "BS")
         (keymap:sequence-text (keymap:spec "PGDN")) (keymap:spec "PGUP")
         (keymap:sequence-text (keymap:spec "DELETE")) (keymap:sequence-text (keymap:spec "C-M-SPC")))
       '("M-BS" ("BACKSPACE") "PGDN" ("PAGEUP") "DEL" "C-M-SPC"))
     (define source (view:create! head:ui-actor #f 'text 2 '() '(0)))
     (widget:mount! source 'inspection-subject)
     (define a (bindings:create! #f '() source))
     (define query (view:source (view:snapshot a)))
     (define root (view:create! head:ui-actor #f 'row 1 '() '()))
     (define b (view:fork! head:ui-actor a))
     (view:arrange! head:ui-actor (list (list root 0 (list (list 'a a '(grow 1)) (list 'b b '(grow 3))) '())) '())
     (define (pump!)
       (widget:pump!) (widget:present! (list (list (widget:prepare! root 240 12) 0 0))))
     (widget:mount! root 'bindings-fixture)
     (test:await 'inspection-ready
       (lambda () (pump!) (eq? 'ready (get (get (model:snapshot query) 'value) 'status))))
     (define (viewport id) (widget:descendant id 'viewport))
     (define (anchor id) (view:state (interaction:snapshot (viewport id))))
     (check 'inspection-is-a-shared-base-snapshot-with-independent-viewports
       (let* ([ra (widget:prepared (widget:descendant a 'viewport 'content 'listing))]
              [rb (widget:prepared (widget:descendant b 'viewport 'content 'listing))])
         (list (equal? query (view:source (interaction:snapshot b)))
           (< (caddr (widget:frame-rect ra)) (caddr (widget:frame-rect rb)))
           (contains? (text (get (model:snapshot (cdr (assq 'listing (get (get (model:snapshot query) 'value) 'parts)))) 'value)) "Composition"))) '(#t #t #t))
     ;; Hold a base publication: preparing another frame must still return,
     ;; and a newer subject must supersede the queued intermediate capture.
     (let* ([captured (test:gate)] [release (test:gate)]
            [watch (model:subscribe! (list query)
                     (lambda (notice) (unless (captured) (captured #t) (test:await 'release-inspection release))))])
       (dynamic-wind void
         (lambda ()
           (bindings:inspect! a root) (pump!)
           (test:await 'inspection-publication-held captured)
           (bindings:inspect! a source) (pump!)
           (check 'inspection-publication-never-blocks-preparing-a-newer-subject
             (and (widget:prepared root) #t) #t))
         (lambda () (release #t) (model:unsubscribe! watch)))
       (test:await 'inspection-latest-subject
         (lambda ()
           (pump!)
           (let-values ([(shown d inputs) (widget:context (widget:descendant a 'viewport 'content 'listing))])
             (and (equal? source (car (get (get (model:snapshot query) 'value) 'subject)))
               (= (get shown 'revision) (model:revision (view:source d))))))))
     (let* ([leaf (widget:descendant a 'viewport 'content 'listing)] [d (interaction:snapshot leaf)]
            [r (model:snapshot (view:source d))] [heading (car (get r 'value))] [label (cadr heading)]
            [start (list (car heading) 0 0)] [end (list (car heading) 0 (string-length label))])
       (bindings:select! leaf end start (get r 'revision))
       (widget:prepare! root 80 12) (bindings:copy! leaf)
       (check 'copy-and-selection-survive-reflow-without-changing-the-fork
         (list (edit:copy-text) (view:state (interaction:snapshot (widget:descendant b 'viewport 'content 'listing)))) (list label '()))
       (pump!))
     (bindings:page! a 'down) (pump!)
     (check 'paging-moves-only-the-selected-inspection-viewport (list (and (anchor a) #t) (anchor b)) '(#t #f))
     (let ([before (model:revision query)] [at (anchor a)])
       (mouse:input! #t #\M 35 10 3) (pump!)
       (widget:pointer! '(scroll 0 2 lines) 10 3) (pump!)
       (mouse:cancel!) (head:set-mouse-position! #f) (pump!)
       (check 'self-hover-and-wheel-preserve-subject-and-do-not-republish
         (list (= before (model:revision query)) (not (equal? at (anchor a))) (mouse:position)) '(#t #t (10 . 3))))
     (for-each (lambda (size) (widget:prepare! root (car size) (cadr size))) '((0 0) (1 1) (5 2)))
     (define executed 0)
     (keymap:bind-default! 'global "C-c F11" (lambda () (set! executed (+ executed 1))))
     (pump!) (bindings:capture-key! a) (pump!)
     (routing:input! root '(key "C-c" #f)) (pump!)
     (check 'key-inspector-keeps-a-prefix-in-ordinary-view-state
       (view:state (interaction:snapshot (widget:descendant a 'reader))) '("C-c"))
     (routing:input! root '(key "F11" #f)) (pump!)
     (test:await 'captured-key-published
       (lambda () (pump!)
         (let ([result (text (get (model:snapshot (cdr (assq 'listing (get (get (model:snapshot query) 'value) 'parts)))) 'value))])
           (and (contains? result "C-c F11") (contains? result "Resolved in global")))))
     (let ([rows (get (model:snapshot (cdr (assq 'listing (get (get (model:snapshot query) 'value) 'parts)))) 'value)])
       (check 'key-inspector-reports-resolution-without-executing-it
         (list executed (not (assq 'reader (view:children (interaction:snapshot a))))
           (contains? (text rows) "C-c F11") (contains? (text rows) "Resolved in global")) '(0 #t #t #t)))
     (bindings:capture-key! a) (pump!) (routing:input! root '(key "C-g" #f)) (pump!)
     (check 'key-capture-cancel-removes-only-the-reader
       (list executed (not (assq 'reader (view:children (interaction:snapshot a))))) '(0 #t))
     (widget:unmount! root)
     (let ([before (model:revision query)])
       (keymap:bind-default! 'inspection-test "F12" edit:kill-line!) (head:before-frame!)
       (check 'hidden-inspectors-release-demand-and-do-not-publish
         (list (model:demanded? query) (= before (model:revision query))) '(#f #t)))
     ;; A persistent window outlives its transient inspection query. Cover
     ;; both departure (unavailable) and recovery (absent), without a second
     ;; copy of the screen's auxiliary-placement tests.
     (define manager (window:create-manager! #f))
     (define window (window:current manager))
     (widget:mount! manager 'inspection-lifetime)
     (window-control:open-document! window (store:create! head:ui-actor "inspection subject" '("")))
     (define (show!)
       (widget:pump!) (widget:present! (list (list (widget:prepare! manager 80 12) 0 0))))
     (show!)
     (for-each
       (lambda (reason)
         (let* ([shown (bindings:open! window manager)] [query (view:source (interaction:snapshot shown))])
           (test:await 'inspection-ready (lambda () (show!) (eq? 'ready (get (get (model:snapshot query) 'value) 'status))))
           (case reason
             [(departed)
              (let ([r (model:snapshot query)])
                (model:commit! head:ui-actor
                  (list (list query (get r 'revision) (get r 'references)
                          (map (lambda (p) (if (eq? (car p) 'status) '(status . unavailable) p)) (get r 'value))))))]
             [(restored) (model:retire! head:ui-actor query (model:revision query))])
           (show!)
           (let* ([fresh (bindings:open! window manager)] [source (view:source (interaction:snapshot fresh))])
             (test:await 'reopened-inspection-ready
               (lambda () (show!) (eq? 'ready (get (get (model:snapshot source) 'value) 'status))))
             (check (list 'inspection-is-rebuilt reason)
               (list (not (equal? shown fresh)) (model:snapshot shown)
                 (window:document manager window) (and (model:snapshot query) #t))
               (list #t #f fresh (eq? reason 'departed))))))
       '(restored departed))
     (include "tests/inspection.sps")
     (test:finish! 'bindings)))
