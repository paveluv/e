;; The shipped editor's outer policy. Construction and wiring live in start.e.
(import (only (foundation edoc) elibrary))
(elibrary (apps screen)
  (export auxiliary! hide-auxiliary! init! open-document! open-file! quit! review!)
  (import (chezscheme) (prefix (apps buffet) buffet:) (prefix (apps finder) finder:)
          (prefix (head edit) edit:) (prefix (head head) head:)
          (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head layout) layout:) (prefix (head lifecycle) lifecycle:) (prefix (head widget) widget:)
          (prefix (head window-control) window-control:) (prefix (service window) window:)
          (prefix (state model) model:) (prefix (state view) view:))

  (define (get xs key) (cdr (assq key xs)))
  (define (within? id parent)
    (and id (or (equal? id parent)
              (let ([d (interaction:snapshot id)]) (and d (within? (view:parent d) parent))))))
  (define (place-auxiliary! id show?)
    (let* ([area (widget:descendant id 'content)] [d (interaction:snapshot area)]
           [aux (car (view:owned d))] [children (view:children d)]
           [next (if show? (append (list-head children 1) (list (list 'auxiliary aux '(grow 1)))) (list-head children 1))])
      (unless (equal? next children)
        (interaction:flush!)
        ;; Flushing acknowledges logical split state before its mirror mail
        ;; arrives. Read that revision at this structural command boundary.
        (let* ([r (caddar (cadr (model:snapshots (list area))))] [d (get r 'value)])
          (unless (equal? children (view:children d)) (error 'place-auxiliary! "auxiliary placement changed"))
          (let-values ([(status rows) (widget:arrange!
                                        (list (list area (get r 'revision) next (view:options d))))])
            (unless (eq? status 'applied) (error 'place-auxiliary! "auxiliary placement changed" status)))))
      aux))

  (edoc "Reveal this screen's auxiliary manager and return its current window without moving focus. Its ordinary split shares the main manager's resize mechanism. Hidden presentations retain logical state without source demand."
    (receiver id (view screen)) (id model "editor screen") (returns model))
  (define (auxiliary! id)
    (let* ([area (widget:descendant id 'content)] [d (interaction:snapshot area)]
           [aux (car (view:owned d))] [focus (widget:focused id)])
      (unless (within? focus aux)
        (interaction:set-state! head:ui-actor id #f (list (cons 'return-focus focus))))
      (window:current (place-auxiliary! id #t))))

  (edoc "Hide this screen's auxiliary manager, retaining its windows and presentations. If focus was inside it, restore the surviving origin or the main manager's current window."
    (receiver id (view screen)) (id model "editor screen"))
  (define (hide-auxiliary! id)
    (let* ([d (interaction:snapshot id)] [area (widget:descendant id 'content)]
           [aux (car (view:owned (interaction:snapshot area)))]
           [restore? (within? (widget:focused id) aux)]
           [saved (cond [(assq 'return-focus (view:state d)) => cdr] [else #f])])
      (place-auxiliary! id #f)
      (when restore?
        (if (and saved (within? saved id)) (interaction:focus! id saved)
          (window-control:select! (window:current (widget:descendant id 'content 'windows)))))))

  (edoc "Visit a file in this screen's selected window, or navigate its Finder for a directory. File acquisition returns canonical document references. The manager owns placement and retained app history."
        (receiver id (view screen)) (id model "editor screen") (path file "file or directory to visit") (returns boolean))
  (define (open-file! id path)
    (let* ([manager (widget:descendant id 'content 'windows)] [window (window:current manager)])
      (edit:visit-file! path
        (lambda (kind value)
          (case kind
            [(directory) (finder:open-directory! value window)]
            [(buffer) (window-control:open-document! window value)])))))

  (edoc "Open a canonical document through this screen's main manager, independently of an auxiliary pane's focus. Presentation preferences pass to the ordinary window opener."
    (receiver id (view screen)) (id model "editor screen") (document (or buffer model) "document")
    (preferences (list-of list) "optional point/presentation preferences") (returns model))
  (define (open-document! id document . preferences)
    (let ([manager (widget:descendant id 'content 'windows)])
      (apply window-control:open-document! (window:current manager) document preferences)))

  (edoc "Detach this editor screen after flushing its logical interaction. Its documents and persistent view graph remain in the base."
        (receiver id (view screen)) (id model "editor screen") (prompts))
  (define (quit! id)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (lifecycle:quit!)))

  (edoc "Show this screen's document catalogue in its auxiliary manager and focus it for review."
    (receiver id (view screen)) (id model "editor screen"))
  (define (review! id)
    (let ([window (auxiliary! id)])
      (buffet:open! window) (window-control:select! window)))

  (edoc "Register the default editor screen's ordinary vertical container and outer commands. Loading allocates no view, manager or document." (public))
  (define (init!)
    (widget:register! 'screen 1
      (append (layout:container 'y)
        (list '(contexts screen global) (cons 'actions (list (cons 'open-file open-file!) (cons 'open-document open-document!) (cons 'quit quit!) (cons 'review review!)
                                                         (cons 'auxiliary auxiliary!) (cons 'hide-auxiliary hide-auxiliary!))))))
    (keymap:bind-default! 'screen "C-x C-c" (keymap:call quit! widget:target)))
)
