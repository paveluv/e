#!/usr/bin/env scheme-script

;; Widget compositions and the default window host. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)

(define evaluate! (eval '(let () (import (prefix (core kernel) kernel:)) kernel:evaluate!)))

(evaluate!
  '(begin
     (import (except (head edit) init!)
             (prefix (only (head edit) init!) edit:)
             (prefix (head head) head:) (prefix (head seat) seat:)
             (prefix (head catalogue-host) catalogue-host:)
             (prefix (state store) store:)
             (prefix (state model) model:)
             (prefix (state connection) connection:) (prefix (core port) port:)
             (prefix (state catalogue) catalogue:) (prefix (state collection) collection:) (prefix (head range) range:)
             (prefix (state view) view:)
             (prefix (head interaction) interaction:)
             (prefix (head widget) widget:) (prefix (head window) window:)
             (prefix (head entry) entry:) (prefix (head text-source) text-source:) (prefix (foundation text) text:)
             (prefix (head control) control:) (prefix (core descriptor) descriptor:)
             (prefix (head table) table:)
             (prefix (foundation string) string:)
             (prefix (sys glyph) glyph:)
             (prefix (core kernel) kernel:)
             (prefix (service log) log:)
             (prefix (test) test:)
             (prefix (state actor) actor:) (prefix (state surface) surface:) (prefix (head render) render:)
             (prefix (head paint) paint:) (prefix (head mode) mode:) (prefix (head keymap) keymap:)
             (prefix (head dispatch) dispatch:) (prefix (head routing) routing:)
             (prefix (head prompt) prompt:) (prefix (service prompt-request) prompt-request:)
             (prefix (head search-control) search-control:)
             (prefix (head prompt-host) prompt-host:) (prefix (head suspension) suspension:)
             (prefix (head layout) layout:) (prefix (head completion) completion:)
             (prefix (apps paren) paren:)
             (prefix (apps terminal) terminal:)
             (prefix (apps pretty-scheme) pretty-scheme:)
             (prefix (apps git-view) git-view:)
             (prefix (apps log-view) log-view:))

     (define check test:check)
     (widget:init!) (edit:init!) (window:init!)
     (define refused? test:raises?)
     ;; Host placement must not consume a shared document with the same label.
     (let* ([ordinary (seat:new-buffer! "<app-collision>")]
            [id (view:create! head:ui-actor #f 'row 1 '((name . "<app-collision>")) '())]
            [host (window:show-widget! (seat:current-window) id)])
       (check 'widget-placement-preserves-shared-label-and-identity
         (list (eq? host ordinary) (seat:buffer-name host) (seat:buffer-store-id host)
           (seat:buffer-read-only ordinary)) '(#f "<app-collision 2>" #f #f))
       (seat:forget-buffer! host)
       (view:retire! head:ui-actor id (model:revision id)))

     (include "tests/journal-widget.sps")

     ;; The ordinary buffer path validates generated hyperlink ranges too.
     (let ([b (seat:new-buffer! "*hyperlink-test*")])
       (seat:with-buffer-mirror b (insert-text! "https://example.com/path"))
       (check 'buffer-link-ranges (paint:buffer-line-hyperlinks b 0)
         '((0 24 "https://example.com/path")))
       (kill-buffer! (seat:buffer-store-id b)))

     ;; Two view identities share data, while geometry, selection and renderer
     ;; lifetime remain independent. Reuse the app fixture and its windows.
     (entry:init!)
     ;; Widget host hooks must leave shared buffers alone after their store
     ;; records disappear, whether hidden or still shown in a window.
     (let* ([was (seat:current-buffer-mirror)]
            [shown (seat:new-buffer! "deleted while shown")]
            [hidden (seat:new-buffer! "deleted while hidden")]
            [ids (map seat:buffer-store-id (list shown hidden))]
            [errors (log:entries 'seat:forget-buffer!)])
       (seat:show-buffer-mirror! shown)
       (for-each (lambda (id) (store:delete! '(base test) id)) ids)
       (seat:sync-foreign-edits!)
       (check 'widget-host-retirement-never-reads-deleted-shared-facts
         (list (map seat:buffer-of-store-id ids)
               (and (not (memq (seat:current-buffer-mirror) (list shown hidden))) #t)
               (equal? errors (log:entries 'seat:forget-buffer!)))
         '((#f #f) #t #t))
       (seat:show-buffer-mirror! was))
     (define model-checks 0)
     (model:register-kind! 'widget-test 1 (lambda (value) (set! model-checks (+ model-checks 1)) (string? value)))
     (let* ([root (seat:root)] [w (seat:current-window)] [was (seat:current-buffer-mirror)]
            [owner (string-copy "widget-test-renderer")] [calls 0]
            [data (model:create! head:ui-actor 'widget-test 1 'session 'persistent '() "original")]
            [first (view:create! head:ui-actor data 'probe 1 '() 0)]
            [second (view:create! head:ui-actor data 'probe 1 '() 0)]
            [other (seat:make-window was 0 0 0 0 0 2 7 24 'default)]
            [a (window:show-widget! w first)] [b (window:show-widget! other second)])
       (define (install!)
         (parameterize ([kernel:registering-module owner])
           (widget:register! (quote probe) 1 (list (cons (quote render) (lambda (model descriptor width height range) (define state (view:state descriptor)) (set! calls (+ calls 1)) (make-list (+ height 2) (format "~a ~a 界界界界界界界界" (cdr (assq (quote value) model)) state)))) (cons (quote actions) (list (cons (quote choose) (lambda (id) (let-values ([(model descriptor inputs) (widget:context id)]) (values (cdr (assq (quote revision) model)) (view:state descriptor)))))))))))
       (define (refresh!) (for-each (lambda (buffer) ((seat:app-refresh! (seat:app-of buffer)))) (list a b)))
       (install!)
       (seat:show-buffer-mirror! a)
       (seat:set-layout-root! (seat:make-layout-split 'right w other 1 2))
       (seat:window-width-set! w 12) (seat:window-size-set! w 4)
       (seat:window-width-set! other 24) (seat:window-size-set! other 2)
       (refresh!)
       (let ([before model-checks])
         (refresh!)
         (check 'widget-warm-frame-reuses-its-owned-model-snapshot model-checks before))
       (check 'widget-host-bounds-and-cached-rendering
         (list calls (map (lambda (window) (map glyph:cells (vector->list (seat:window-lines window)))) (list w other)))
         '(2 ((12 12 12 12) (24 24))))
       (interaction:set-state! head:ui-actor first 0 9)
       (check 'widget-action-uses-provisional-target-before-ack
         (list (call-with-values (lambda () (widget:act! first 'choose)) list)
               (view:state (view:snapshot first)) (view:state (interaction:snapshot second))) '((0 9) 0 0))
       (seat:checkpoint!)
       (check 'widget-lifecycle-checkpoint-fences-state (view:state (view:snapshot first)) 9)
       (model:commit! '(base test) (list (list data 0 '() "new")))
       (refresh!)
       (let ([before calls])
         (seat:window-width-set! other 8) (refresh!)
         (check 'widget-resize-only-rerenders-affected-view (- calls before) 1))
       (kernel:retract-module! owner) (refresh!)
       (check 'widget-unavailable-renderer-keeps-mount-without-actions
         (list (widget:actions first) (seat:app-refresh-error (seat:app-of a))) '(() #f))
       (install!) (refresh!)
       (check 'widget-late-renderer-reclaims-view (widget:actions first) '(choose))
       (seat:forget-buffer! a) (seat:forget-buffer! b)
       (check 'widget-unmount-and-buffer-kill-retain-model-and-descriptors
         (list (map (lambda (id) (view:owner (view:snapshot id))) (list first second))
               (cdr (assq 'value (model:snapshot data))) (seat:app-of a) (seat:app-of b)) '((#f #f) "new" #f #f))
       (seat:set-layout-root! root) (seat:show-buffer-mirror! was)
       (kernel:retract-module! owner))

     ;; Tree mounts allocate no buffers. Reorder retains identity and state;
     ;; failed ownership changes and duplicate hosts cannot release the tree.
     (let* ([data (model:create! head:ui-actor 'widget-test 1 'session 'persistent '() "one\ntwo")]
            [parent (view:create! head:ui-actor #f 'column 1 '() '())]
            [a (view:create! head:ui-actor data 'text 2 '() 0)]
            [b (view:create! head:ui-actor data 'text 2 '() 0)]
            [count (length (seat:buffers))])
       (view:arrange! head:ui-actor (list (list parent 0 (list (list 'a a 'fit) (list 'b b '(grow 1))) '())) '())
       (let ([m (widget:mount! parent 'slot)])
         (check 'recursive-mount-idempotent-with-no-adapter-buffers
           (list (eq? m (widget:mount! parent 'slot)) (= count (length (seat:buffers)))
             (refused? (lambda () (widget:mount! parent 'another)))
             (refused? (lambda () (widget:mount! a 'nested)))) '(#t #t #t #t))
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

     ;; Additional window placement forks descriptors; reopening hidden roots
     ;; reuses their adapter and remembered interaction.
     (let* ([w (seat:current-window)] [was (seat:current-buffer-mirror)]
            [data (model:create! head:ui-actor 'widget-test 1 'session 'persistent '() "shared")]
            [id (view:create! head:ui-actor data 'text 2 '() 0)]
            [b (window:show-widget! w id)]
            [other (seat:make-window b 0 0 0 0 0 2 0 12 'default)]
            [copy (seat:window-buffer other)] [fork (seat:buffer-fact copy 'widget-id #f)])
       (check 'placement-forks-only-views (list (equal? id fork) (view:source (view:snapshot fork))) (list #f data))
       (seat:show-buffer-mirror! was)
       (check 'hidden-root-reuses-buffer (eq? b (window:show-widget! w id)) #t)
       (widget:unmount! id)
       (check 'explicitly-unmounted-adapter-remounts-on-show
         (begin (window:show-widget! w id) (widget:actions id)) '(move select choose))
       (actor:checkpoint! head:ui-actor
         `(screen 4 1 (split right 1 1 (window 1 0 0 0 default #t #f default) (window 2 0 0 0 default #t #f default))
            (((widget ,id) #f ()))))
       (seat:resume!)
       (let* ([windows (filter (lambda (w) (not (seat:popup? w))) (seat:windows))]
              [ids (map (lambda (w) (seat:buffer-fact (seat:window-buffer w) 'widget-id #f)) windows)])
         (check 'resume-resolves-duplicate-widget-placements-before-installing-layout
           (list (length ids) (equal? (car ids) (cadr ids))
                 (map (lambda (id) (view:source (view:snapshot id))) ids))
           (list 2 #f (list data data))))
       (window:delete-others!) (seat:show-buffer-mirror! was)
       (for-each (lambda (b) (when (seat:buffer-fact b 'widget-id #f) (seat:forget-buffer! b))) (seat:buffers)))

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

     ;; Entries share authored text and its journal, but never cursor state.
     (let* ([source (store:create! head:ui-actor "widget entry" '("a界éz"))]
            [a (view:create! head:ui-actor source 'entry 1 '() '((0 . 0) (0 . 0)))]
            [b (view:create! head:ui-actor source 'entry 1 '() '((0 . 0) (0 . 0)))]
            [root (view:create! head:ui-actor #f 'row 1 '() '())]
            [ambient (seat:current-buffer-mirror)])
       (define (line) (let-values ([(text rev) (store:snapshot source)]) (vector-ref text 0)))
       (define (show!) (widget:present! (list (list (widget:prepare! root 20 1) 0 0))))
       (define (foreign! start end replacement)
         (let-values ([(text rev) (store:snapshot source)])
           (store:edit! '(agent "entry-test") source rev (text:make-span 0 start 0 end) replacement))
         (seat:sync-foreign-edits! source))
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
               (refused? (lambda () (entry:insert! a "no"))) (eq? ambient (seat:current-buffer-mirror))) '(#f #f #t #t))
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
     (let* ([id (model:create! head:ui-actor 'widget-view 4 'session 'persistent '() "future descriptor")]
            [previous (seat:current-buffer-mirror)] [b (window:show-widget! (seat:current-window) id)])
       (seat:show-buffer-mirror! b)
       ((seat:app-refresh! (seat:app-of b)))
       (check 'widget-unknown-descriptor-is-inspectable-without-claiming-an-owner
         (list (widget:actions id) (interaction:snapshot id) (cdr (assq 'value (model:snapshot id))))
         '(() #f "future descriptor"))
       (seat:forget-buffer! b) (seat:show-buffer-mirror! previous))

     (include "tests/control.sps")
     (include "tests/editor-widget.sps")
     (include "tests/terminal-widget.sps")
     (include "tests/prompt-widget.sps")
     (include "tests/search-control.sps")
     (include "tests/table-widget.sps")
     (include "tests/git-widget.sps")
     (include "tests/range.sps")
     (include "tests/document.sps")
     (include "tests/tetris.sps")
     (test:finish! 'app))
  (interaction-environment))
