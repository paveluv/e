#!/usr/bin/env scheme-script

;; Widget compositions and the default window host. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(test-evaluate!
  '(begin
     (import (prefix (head edit) edit:)
             (prefix (head head) head:)
             (prefix (state store) store:)
             (prefix (state model) model:)
             (prefix (state connection) connection:) (prefix (core port) port:)
             (prefix (state catalogue) catalogue:) (prefix (state collection) collection:) (prefix (head range) range:)
             (prefix (state view) view:)
             (prefix (head interaction) interaction:)
             (prefix (head widget) widget:)
             (prefix (head entry) entry:) (prefix (head text-source) text-source:) (prefix (foundation text) text:)
             (prefix (head control) control:) (prefix (core descriptor) descriptor:)
             (prefix (head table) table:)
             (prefix (foundation string) string:)
             (prefix (sys glyph) glyph:)
             (prefix (core kernel) kernel:)
             (prefix (service log) log:)
             (prefix (test) test:)
             (prefix (state actor) actor:) (prefix (state surface) surface:) (prefix (head render) render:)
             (prefix (head text-layout) text-layout:) (prefix (head mode) mode:) (prefix (head keymap) keymap:)
             (prefix (head routing) routing:)
             (prefix (head prompt) prompt:) (prefix (service prompt-request) prompt-request:)
             (prefix (head search-control) search-control:)
             (prefix (head suspension) suspension:)
             (prefix (head layout) layout:) (prefix (head completion) completion:)
             (prefix (apps paren) paren:)
             (prefix (apps terminal) terminal:)
             (prefix (apps pretty-scheme) pretty-scheme:)
             (prefix (apps git-view) git-view:)
             (prefix (apps log-view) log-view:))

     (define check test:check)
     (kernel:load-module! "widget") (edit:init!)
     (define refused? test:raises?)
     (include "tests/journal-widget.sps")
     (entry:init!)
     (define model-checks 0)
     (model:register-kind! 'widget-test 1 (lambda (value) (set! model-checks (+ model-checks 1)) (string? value)))
     ;; Independent mounts share a source while caching geometry and renderer lifetime.
     (let* ([owner "widget-test-renderer"] [calls 0]
            [data (model:create! head:ui-actor 'widget-test 1 'session 'persistent '() "original")]
            [first (view:create! head:ui-actor data 'probe 1 '() 0)]
            [second (view:create! head:ui-actor data 'probe 1 '() 0)])
       (define (install!)
         (parameterize ([kernel:registering-module owner])
           (widget:register! 'probe 1
             (list (cons 'render (lambda (model d width height range)
                                   (set! calls (+ calls 1))
                                   (make-list (+ height 2) (format "~a ~a 界界界界界界界界" (cdr (assq 'value model)) (view:state d)))))
               (cons 'actions (list (cons 'choose (lambda (id)
                                                    (let-values ([(model d inputs) (widget:context id)])
                                                      (values (cdr (assq 'revision model)) (view:state d)))))))))))
       (define (frames width) (widget:pump!) (list (widget:prepare! first 12 4) (widget:prepare! second width 2)))
       (install!) (widget:mount! first 'first) (widget:mount! second 'second)
       (let* ([shown (frames 24)] [before model-checks])
         (frames 24)
         (check 'widget-warm-frame-reuses-owned-snapshot
           (list model-checks calls (map (lambda (f) (map glyph:cells (widget:frame-lines f))) shown))
           (list before 2 '((12 12 12 12) (24 24)))))
       (interaction:set-state! head:ui-actor first 0 9)
       (check 'widget-action-uses-provisional-target-before-ack
         (list (call-with-values (lambda () (widget:act! first 'choose)) list)
           (view:state (view:snapshot first)) (view:state (interaction:snapshot second))) '((0 9) 0 0))
       (interaction:flush!)
       (check 'widget-fence-publishes-state (view:state (view:snapshot first)) 9)
       (model:commit! '(base test) (list (list data 0 '() "new"))) (frames 24)
       (let ([before calls]) (frames 8) (check 'resize-only-renders-affected-view (- calls before) 1))
       (kernel:retract-module! owner) (frames 8)
       (check 'unavailable-renderer-keeps-mount-without-actions (widget:actions first) '())
       (install!) (frames 8) (check 'late-renderer-reclaims-view (widget:actions first) '(choose))
       (widget:unmount! first) (widget:unmount! second)
       (check 'unmount-retains-data-and-descriptors
         (list (map (lambda (id) (view:owner (view:snapshot id))) (list first second))
           (cdr (assq 'value (model:snapshot data)))) '((#f #f) "new"))
       (kernel:retract-module! owner))

     ;; Tree mounts allocate no buffers. Reorder retains identity and state;
     ;; failed ownership changes and duplicate hosts cannot release the tree.
     (let* ([data (model:create! head:ui-actor 'widget-test 1 'session 'persistent '() "one\ntwo")]
            [parent (view:create! head:ui-actor #f 'column 1 '() '())]
            [a (view:create! head:ui-actor data 'text 2 '() 0)]
            [b (view:create! head:ui-actor data 'text 2 '() 0)]
            [count (length (store:buffer-list))])
       (view:arrange! head:ui-actor (list (list parent 0 (list (list 'a a 'fit) (list 'b b '(grow 1))) '())) '())
       (let ([m (widget:mount! parent 'slot)])
         (check 'recursive-mount-idempotent-with-no-adapter-buffers
           (list (eq? m (widget:mount! parent 'slot)) (= count (length (store:buffer-list)))
             (refused? (lambda () (widget:mount! parent 'another)))
             (refused? (lambda () (widget:mount! a 'nested)))) '(#t #t #t #t))
         (widget:prepare! parent 20 5) (widget:focus! parent b)
         (widget:prepare! a 10 2)
         (check 'subtree-projection-preserves-root-focus (widget:focused parent) b)
         (interaction:flush!)
         (widget:act! a 'move 1)
         (let-values ([(status rows) (widget:arrange! (list (list parent (cdr (assq 'revision (model:snapshot parent)))
                                                              (list (list 'b b '(grow 1)) (list 'a a 'fit)) '())))])
           (check 'reordered-child-keeps-local-state-and-parent
             (list status (view:state (interaction:snapshot a)) (view:parent (interaction:snapshot a)))
             (list 'applied 1 parent)))
         (let-values ([(status rows) (widget:arrange! (list (list parent (cdr (assq 'revision (model:snapshot parent))) (list (list 'a a 'fit)) '())))])
           (check 'removed-child-releases-runtime-and-owner
             (list status (refused? (lambda () (widget:actions b))) (view:owner (view:snapshot b))) '(applied #t #f)))
         (widget:unmount! parent)
         (check 'recursive-release (map (lambda (id) (view:owner (view:snapshot id))) (list parent a)) '(#f #f))))

     ;; A bare composition exercises the same routing used by window hosts.
     (let* ([events '()] [owner 'routing-fixture] [capturing? #f]
            [a (view:create! head:ui-actor #f 'route-leaf 1 '() '())]
            [b (view:create! head:ui-actor #f 'route-leaf 1 '() '())]
            [row (view:create! head:ui-actor #f 'route-row 1 '() '())]
            [root (view:create! head:ui-actor #f 'overlay 1 '() '())]
            [barrier (view:create! head:ui-actor #f 'overlay 1 '((modal . #t)) '())])
       (define (record! id tag) (set! events (cons (list tag id) events)))
       (define (install-leaf! full?)
         (parameterize ([kernel:registering-module owner])
           (widget:register! 'route-leaf 1
             (list (cons 'focus #t) (cons 'contexts '(route-leaf)) (cons 'capture (if full? 'full 'partial))
               (cons 'actions (list (cons 'record record!)))
               (cons 'pointer-bindings
                 (lambda (f x y) (map (lambda (button) (list (list 'click button '()) (keymap:call widget:act! (widget:frame-id f) 'record 'press))) '(primary secondary))))
               (cons 'event (lambda (id source d event)
                              (case (car event)
                                [(text) (record! id (cadr event)) #t]
                                [(pointer) (and (not (eq? (caddr event) 'middle))
                                             (begin (when (eq? (cadr event) 'press) (widget:capture! id)) (record! id (cadr event)) #t))]
                                [(cancel) (record! id 'cancel) #t]
                                [else #f])))))))
       (define (show!) (widget:present! (list (list (widget:prepare! root 10 2) 0 0))))
       (define (key! key) (routing:input! root (list 'key key (and (= 1 (string-length key)) key))))
       (define (take) (let ([out (reverse events)]) (set! events '()) out))
       (install-leaf! #f)
       (widget:register! 'route-row 1
         (list (cons 'contexts '(route-parent)) (cons 'capture-contexts (lambda (id d) '(route-capture)))
           (cons 'capture (lambda (id d) (if capturing? 'full 'partial)))
           (cons 'yield (lambda (id d) (if capturing? '("C-x") '())))
           (cons 'capture-event (lambda (id source d event)
                                  (and capturing? (begin (record! id (list 'captured (car event))) #t))))
           (cons 'capture-pointer-bindings
             (lambda (f x y) (if capturing?
                               (list (list '(click primary ()) (keymap:call widget:act! (widget:frame-id f) 'record 'captured))) '())))
           (cons 'actions (list (cons 'record record!)))
           (cons 'pointer-bindings
             (lambda (f x y) (map (lambda (button) (list (list 'click button '()) (keymap:call widget:act! (widget:frame-id f) 'record 'bubbled))) '(primary middle secondary))))
           (cons 'event (lambda (id source d event) (and (eq? (car event) 'pointer) (begin (record! id 'bubbled) #t))))
           (cons 'layout (lambda (d width height measure locate)
                           (map (lambda (child x) (list (cadr child) (list x 0 5 height))) (view:children d) '(0 5))))))
       (for-each (lambda (entry)
                   (keymap:bind-default! (car entry) (cadr entry) (keymap:call widget:act! widget:target 'record (caddr entry))))
         '((route-leaf "C-x a" leaf) (route-leaf "F2" shadowed) (route-leaf "z" shortcut)
           (route-parent "F1" parent) (route-parent "C-x b" parent-chord) (route-capture "F2" capture)))
       (view:arrange! head:ui-actor
         (list (list root 0 (list (list 'body row '(grow 1))) '())
               (list row 0 (list (list 'a a '(grow 1)) (list 'b b '(grow 1))) '())) '())
       (widget:mount! root 'routing-test) (show!) (widget:focus! root a)
       (check 'mouse-discovery-shadows-per-gesture-and-is-read-only
         (list (map (lambda (binding) (car (keymap:call-action-arguments (cadr binding)))) (widget:pointer-bindings 1 0)) (take))
         (list (list a a row) '()))
       (key! "C-x")
       (check 'chord-start-returns-without-reading-input (routing:pending?) #t)
       ;; A sibling's background publication renews the tree's lease, but
       ;; leaves the focused receiver path and its pending chord unchanged.
       (interaction:flush!)
       (widget:arrange! (list (list b (model:revision b) '() '((text . "updated sibling")))))
       (show!)
       (key! "a") (key! "C-x") (key! "b") (key! "F1") (key! "F2") (key! "z")
       (routing:input! root '(text "z z" paste))
       (check 'explicit-receivers-capture-phase-and-text-not-as-keys (take)
         (list (list 'leaf a) (list 'parent-chord row) (list 'parent row) (list 'capture row) (list 'shortcut a) (list "z z" a)))
       (set! capturing? #t)
       (check 'capture-discovery-precedes-child-bindings-without-dispatch
         (list (keymap:call-action-arguments (cadr (assoc '(click primary ()) (widget:pointer-bindings 1 0)))) (take))
         (list (list row 'record 'captured) '()))
       (key! "z") (routing:input! root '(text "paste" paste))
       (key! "C-x") (key! "a")
       (widget:pointer! '(pointer press primary ()) 1 0)
       (widget:pointer! '(scroll 0 3 cells) 1 0)
       (check 'parent-capture-precedes-children-and-yielded-chords-keep-their-route (take)
         (list (list '(captured key) row) (list '(captured text) row) (list 'leaf a)
           (list '(captured pointer) row) (list '(captured scroll) row)))
       (set! capturing? #f)
       (key! "C-x") (widget:focus! root b) (key! "a")
       (key! "C-x") (keymap:bind-default! 'unrelated "F9" void) (key! "a")
       (check 'focus-and-binding-change-do-not-replay-chord-suffix (take) '())
       (widget:focus! root a) (show!)
       (widget:pointer! '(pointer press primary ()) 1 0)
       (widget:pointer! '(pointer move primary ()) 99 99)
       (widget:pointer! '(pointer release primary ()) 99 99)
       (widget:pointer! '(pointer move none ()) 99 99)
       (check 'pointer-capture-survives-outside-and-ends-on-release (take)
         (list (list 'press a) (list 'move a) (list 'release a) (list 'leave a)))
       (widget:pointer! '(pointer press middle ()) 1 0)
       (check 'unhandled-pointer-bubbles-to-its-own-parent (take) (list (list 'bubbled row)))
       (widget:pointer! '(pointer press primary ()) 1 0)
       (widget:present! (list (list (widget:prepare! root 0 0) 0 0)))
       (widget:pointer! '(pointer release primary ()) 1 0)
       (check 'hidden-capture-cancels-without-retargeting-or-forgetting-focus
         (list (take) (view:focus (interaction:snapshot root)))
         (list (list (list 'press a) (list 'cancel a)) a))
       (show!)
       (key! "C-x") (kernel:retract-module! owner) (install-leaf! #t) (show!) (key! "a")
       (key! "F1") (key! "x") (key! "F2")
       (check 'reload-cancels-chord-full-capture-still-types-and-ancestor-capture-wins (take)
         (list (list "x" a) (list 'capture row)))
       (interaction:flush!)
       (let-values ([(status rows) (widget:arrange! (list (list root (cdr (assq 'revision (model:snapshot root)))
                                                            (list (list 'body row '(grow 1)) (list 'modal barrier '(grow 1))) '())))
                    ])
         (check 'modal-attachment-commits status 'applied))
       (show!) (key! "x") (widget:pointer! '(pointer press primary ()) 1 0)
       (check 'empty-modal-has-no-key-or-pointer-click-through
         (list (take) (widget:pointer-bindings 1 0)) '(() ()))
       (widget:unmount! root) (kernel:retract-module! owner)
       (widget:invalidate!))

     ;; Reflow keeps focus in its app when a new section replaces the old
     ;; visible section, even if that successor was absent from the old order.
     (let* ([swap? #f] [root (view:create! head:ui-actor #f 'row 1 '() '())]
            [outside (view:create! head:ui-actor #f 'focus-leaf 1 '() '() root)]
            [panel (view:create! head:ui-actor #f 'focus-panel 1 '() '() root)]
            [a (view:create! head:ui-actor #f 'focus-leaf 1 '() '() panel)]
            [b (view:create! head:ui-actor #f 'focus-leaf 1 '() '() panel)])
       (define (show!) (widget:present! (list (list (widget:prepare! root 20 2) 0 0))))
       (widget:register! 'focus-leaf 1 '((focus . #t)))
       (widget:register! 'focus-panel 1
         (list '(focus . fallback)
           (cons 'layout (lambda (d w h measure locate)
                           (list (list a (list 0 0 w (if swap? 0 h))) (list b (list 0 0 w (if swap? h 0))))))))
       (view:arrange! head:ui-actor
         (list (list root 0 (list (list 'outside outside '(grow 1)) (list 'panel panel '(grow 1))) '())
           (list panel 0 (list (list 'a a 'fit) (list 'b b 'fit)) '())) '())
       (widget:mount! root 'focus-reflow) (show!) (widget:focus! root a) (show!)
       (set! swap? #t) (show!)
       (check 'focus-reflow-stays-in-the-nearest-surviving-container (widget:focused root) b)
       (widget:unmount! root) (view:retire! head:ui-actor root (model:revision root)))

     ;; Entries share authored text and its journal, but never cursor state.
     (let* ([source (store:create! head:ui-actor "widget entry" '("a界éz"))]
            [a (view:create! head:ui-actor source 'entry 1 '() '((0 . 0) (0 . 0)))]
            [b (view:create! head:ui-actor source 'entry 1 '() '((0 . 0) (0 . 0)))]
            [root (view:create! head:ui-actor #f 'row 1 '() '())]
           )
       (define (line) (let-values ([(text rev) (store:snapshot source)]) (vector-ref text 0)))
       (define (show!) (widget:present! (list (list (widget:prepare! root 20 1) 0 0))))
       (define (foreign! start end replacement)
         (let-values ([(text rev) (store:snapshot source)])
           (store:edit! '(agent "entry-test") source rev (text:make-span 0 start 0 end) replacement))
         (text-source:open! head:ui-actor source))
       (view:arrange! head:ui-actor (list (list root 0 (list (list 'a a '(grow 1)) (list 'b b '(grow 1))) '())) '())
       (widget:mount! root 'entry-test) (show!)
       (entry:move! a 'right) (entry:move! a 'right) (entry:move! a 'right)
       (check 'entry-moves-by-graphemes-and-keeps-independent-selection
         (list (view:state (interaction:snapshot a)) (view:state (interaction:snapshot b)))
         '(((0 . 4) (0 . 4)) ((0 . 0) (0 . 0))))
       (entry:move! a 'left #t) (show!)
       (check 'entry-caret-and-selection-share-cell-geometry
         (list (widget:caret (widget:prepared root))
               (vector-ref (widget:frame-styles (widget:prepared root) 0 (car (widget:frame-lines (widget:prepared root)))) 2))
         '((3 . 0) selection))
       (widget:focus! root b) (show!)
       (check 'entry-caret-translates-into-the-second-child (widget:caret (widget:prepared root)) '(10 . 0))
       (let ([bindings (widget:pointer-bindings 14 0)])
         (check 'entry-pointer-bindings-use-grapheme-positions-and-do-not-select
           (list (keymap:call-action-arguments (cadr (assoc '(click primary ()) bindings)))
             (keymap:call-action-arguments (cadr (assoc '(click primary (shift)) bindings)))
             (view:state (interaction:snapshot b)))
           (list (list b 4 4) (list b 4 0) '((0 . 0) (0 . 0)))))
       (widget:pointer! '(pointer move primary ()) 14 0)
       (check 'entry-does-not-start-a-drag-from-another-widget (view:state (interaction:snapshot b)) '((0 . 0) (0 . 0)))
       (widget:pointer! '(pointer press primary ()) 11 0)
       (widget:pointer! '(pointer move primary ()) 14 0)
       (widget:pointer! '(pointer release primary ()) 14 0)
       (check 'entry-drag-selects-whole-wide-and-combining-graphemes
         (view:state (interaction:snapshot b)) '((0 . 4) (0 . 1)))
       (widget:focus! root a) (show!)
       (routing:input! root '(key "TAB" #f))
       (check 'entry-tab-uses-host-traversal (view:focus (interaction:snapshot root)) b)
       (routing:input! root '(key "S-TAB" #f))
       (routing:input! root '(text "Q" paste))
       (check 'entry-paste-replaces-selection-once (line) "a界Qz")
       (entry:undo! a) (entry:redo! a)
       (check 'entry-uses-existing-undo-redo (line) "a界Qz")
       (widget:act! a 'delete 'backward)
       (check 'entry-delete-action-keeps-the-original-edit-basis (line) "a界z")
       (entry:undo! a)
       (check 'entry-refuses-multiline-paste-without-mutation
         (list (refused? (lambda () (entry:insert! a "bad\npaste"))) (line)) '(#t "a界Qz"))
       (entry:select! a 1 1) (show!)
       (widget:pointer! '(pointer press primary ()) 1 0)
       (widget:pointer! '(pointer release primary ()) 1 0)
       (foreign! 0 0 '("R"))
       (entry:insert! a "X")
       (check 'entry-click-intent-rebases-through-remote-insertion
         (list (line) (view:state (interaction:snapshot a))) '("RaX界Qz" ((0 . 3) (0 . 3))))
       (entry:select! a 5 3)
       (foreign! 4 4 '("remote"))
       (let ([before (line)])
         (check 'entry-overlap-refuses-without-losing-foreign-text
           (list (refused? (lambda () (entry:insert! a "lost"))) (string=? before (line))) '(#t #t)))
       (entry:select! a (string-length (line)) (string-length (line)))
       (widget:unmount! root)
       (foreign! 0 (string-length (line)) '("restoredA"))
       (text-source:forget! source)
       (widget:mount! root 'entry-test) (show!)
       (check 'remounted-entry-deletes-at-its-displayed-caret-after-hidden-edits
         (list (refused? (lambda () (entry:delete! a 'backward))) (line)) '(#f "restored"))
       (foreign! 0 0 '("first" "second")) (show!)
       (check 'entry-external-multiline-is-an-inert-field-not-a-readonly-source
         (list (widget:caret (widget:prepared root)) (store:property source 'read-only #f)
               (refused? (lambda () (entry:insert! a "no")))) '(#f #f #t))
       (widget:unmount! root) (widget:invalidate!))

     (let* ([root (view:create! head:ui-actor #f 'row 1 '() '())]
            [producer (view:create! head:ui-actor #f 'connected-producer 1 '() '((choice . "first")))]
            [consumer (view:create! head:ui-actor #f 'connected-consumer 1 '((choice . "default")) '())])
       (port:register! '(view connected-producer 1) '((output choice string (state choice))))
       (port:register! '(view connected-consumer 1) '((input choice string (options choice))))
       (widget:register! 'connected-producer 1 '())
       (widget:register! 'connected-consumer 1
         (list (cons 'prepare (lambda (id source inputs) (caddr (assq 'choice inputs))))
           (cons 'render (lambda (data descriptor width height range) (list data)))
           (cons 'actions (list (cons 'inspect (lambda (id)
                                                 (let-values ([(source descriptor inputs) (widget:context id)])
                                                   (caddr (assq 'choice inputs)))))))))
       (view:arrange! head:ui-actor (list (list root 0 (list (list 'producer producer 'fit) (list 'consumer consumer '(grow 1))) '())) '())
       (connection:bind! head:ui-actor root (list (list consumer 'choice #f (list producer 'choice))))
       (widget:mount! root 'connection-fixture)
       (let* ([frame (widget:prepare! root 30 2)] [shown (cadr (widget:frame-children frame))])
         (interaction:set-state! head:ui-actor producer #f '((choice . "next")))
         (check 'widget-connected-input-uses-provisional-state-and-pins-shown-bundle
           (list (widget:act! consumer 'inspect)
             (parameterize ([widget:event-frame shown]) (widget:act! consumer 'inspect))
             (cadr (connection:read consumer 'choice)))
           '("next" "first" "first"))
         (interaction:flush!)
         (check 'widget-published-input-is-visible-at-base (cadr (connection:read consumer 'choice)) "next")
         (interaction:bind! head:ui-actor root (list (list consumer 'choice (list producer 'choice) #f)))
         (check 'widget-dependency-bundle-refreshes-on-rewiring-without-changing-shown-input
           (list (widget:act! consumer 'inspect)
             (parameterize ([widget:event-frame shown]) (widget:act! consumer 'inspect))
             (string:prefix? "default" (car (widget:frame-lines (cadr (widget:frame-children (widget:prepare! root 30 2)))))))
           '("default" "first" #t)))
       (widget:unmount! root) (widget:invalidate!))

     (model:register-kind! 'widget-view 4 string?)
     (let ([id (model:create! head:ui-actor 'widget-view 4 'session 'persistent '() "future descriptor")])
       (widget:mount! id 'future) (widget:prepare! id 20 4)
       (check 'unknown-descriptor-is-inspectable-without-claiming-owner
         (list (widget:actions id) (interaction:snapshot id) (cdr (assq 'value (model:snapshot id))))
         '(() #f "future descriptor"))
       (widget:unmount! id))

     (include "tests/control.sps")
     (include "tests/editor-widget.sps")
     (include "tests/window-control.sps")
     (include "tests/terminal-widget.sps")
     (include "tests/prompt-widget.sps")
     (include "tests/search-control.sps")
     (include "tests/table-widget.sps")
     (include "tests/git-widget.sps")
     (include "tests/range.sps")
     (include "tests/document.sps")
     (include "tests/tetris.sps")
     (include "tests/modal.sps")
     (include "tests/screen.sps")
     (test:finish! 'app))
)
