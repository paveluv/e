;; Git's domain queries live in the base; these views compose ordinary controls.
(import (only (foundation edoc) elibrary))
(elibrary (apps git-view)
  (export choose! create! init! log! log-of! refresh!)
  (import (chezscheme) (prefix (foundation string) string:)
          (prefix (head head) head:) (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:) (prefix (head layout) layout:) (prefix (head mode) mode:)
          (prefix (head table) table:) (prefix (head widget) widget:) (prefix (head window) window:)
          (prefix (service file) file:) (prefix (service git-source) git-source:)
          (prefix (state model) model:) (prefix (state view) view:))
  (define (child id name) (cadr (assq name (view:children (interaction:snapshot id)))))
  (define (get r k fallback) (cond [(assq k r) => cdr] [else fallback]))

  (edoc "Create an unmounted Git browser with a history table and an independent read-only patch editor. Enter or click a commit to expand its files, then select a file to inspect its patch. No window is created."
        (path file "path inside the repository") (returns model) (public))
  (define (create! path)
    (let* ([query (git-source:create! head:ui-actor path)]
           [root (view:create! head:ui-actor query 'git-history 1 '() '() query)]
           [patch (git-source:create-patch! head:ui-actor root)]
           [table (table:create! head:ui-actor query '(status commit date author subject) '((identity . subject) (presentation git 1)))]
           [heading (view:create! head:ui-actor #f 'row 1 '((spacing . normal)) '())]
           [label (view:create! head:ui-actor #f 'label 1 (list (cons 'text (string-append "Git: " (file:abbreviate path)))) '())]
           [refresh (view:create! head:ui-actor #f 'action-text 1
                      (list '(text . "[refresh]") '(enabled . #t) (list 'commands (list 'activate root 'refresh '()))) '())]
           [preview (view:create! head:ui-actor (car patch) 'git-patch 1 '() '() (car patch))]
           [editor (view:create! head:ui-actor (cadr patch) 'editor 1
                     '((read-only . #t) (wrap . #f) (annotations)) '((0 . 0) (0 . 0) (0 . 0) #f) (car patch))]
           [d (view:snapshot table)])
      (view:arrange! head:ui-actor
        (list (list root 0 (list (list 'heading heading 'fit) (list 'table table '(grow 1)) (list 'patch preview '(grow 1))) (list (cons 'owned (list (car patch)))))
          (list heading 0 (list (list 'label label '(grow 1)) (list 'refresh refresh 'fit)) '((spacing . normal)))
          (list table 1 (view:children d) (cons (list 'commands (list 'activate root 'choose '())) (view:options d)))
          (list preview 0 (list (list 'text editor '(grow 1))) '())) '()) root))

  (edoc "Expand a displayed commit or select a displayed file into this browser's patch request. The base validates the shown result before changing domain state."
        (receiver id (view git-history)) (id model "Git browser") (selection row-selection "shown query, generation and key") (basis datum "shown result basis"))
  (define (choose! id selection basis)
    (unless (and (list? selection) (= (length selection) 3) (equal? (car selection) (view:source (interaction:snapshot id))))
      (error 'choose! "selection does not belong to this browser"))
    (case (car (caddr selection))
      [(commit) (git-source:expand! head:ui-actor selection basis)]
      [(file) (git-source:select-patch! head:ui-actor (view:source (interaction:snapshot (child id 'patch))) selection basis)]
      [else (error 'choose! "expected a commit or file")]))

  (edoc "Refresh this browser's history and selected patch asynchronously. Keyboard and the refresh control use the same operation."
        (receiver id (view git-history)) (id model "Git browser"))
  (define (refresh! id)
    (git-source:refresh! head:ui-actor (view:source (interaction:snapshot id)))
    (git-source:refresh! head:ui-actor (view:source (interaction:snapshot (child id 'patch)))))
  (define (busy? id d)
    (let* ([r (model:snapshot (view:source d))] [v (and r (get r 'value '()))])
      (and v (eq? (get v 'status #f) 'pending))))
  (define (present proc)
    (lambda (cell cells attributes) (list (if (eq? (car cell) 'ready) (proc (cadr cell) attributes) ""))))
  (define (date seconds attributes)
    (let ([d (time-utc->date (make-time 'time-utc 0 seconds))])
      (format "~4,'0d-~2,'0d-~2,'0d" (date-year d) (date-month d) (date-day d))))
  (define (diff-styles line)
    (make-vector (string-length line)
      (cond [(exists (lambda (prefix) (string:prefix? prefix line)) '("@@" "+++ " "--- ")) 'keyword]
        [(string:prefix? "+" line) 'string] [(string:prefix? "-" line) 'rainbow1]
        [(exists (lambda (prefix) (string:prefix? prefix line)) '("diff --git " "index ")) 'comment]
        [(string:prefix? "[" line) 'ghost] [else 'plain])))

  (edoc "Open the retained Git browser for a path in the current window. Different paths retain independent queries and selections."
        (path file "path inside the repository") (returns model))
  (define (log-of! path)
    (let* ([path (file:expand path)] [root (window:tool! (string-append "git " (file:abbreviate path))
                                             (lambda (commands) (create! path)) (string-append "git:" path))])
      (window:show-widget! (head:current-window) root)
      (let ([app (child root 'app)]) (widget:focus! root (child (child (child app 'table) 'body) 'rows)) app)))

  (edoc "Open Git history for the current file, or the working directory. Repository work remains asynchronous.")
  (define (log!) (log-of! (or (head:buffer-file (head:current-buffer-mirror)) ".")))

  (edoc "Register the Git composition, patch highlighting and named bindings without opening a repository or tool." (public))
  (define (init!)
    (widget:register! 'git-history 1
      (append (layout:container 'y)
        (list '(receivers (table table)) (cons 'capture-contexts '(git-history))
          (cons 'actions (list (cons 'choose choose!) (cons 'refresh refresh!))))))
    (widget:register! 'git-patch 1 (append (layout:container 'y) (list (cons 'busy? busy?))))
    (table:register-presentation! 'git 1
      (list (list 'status 1 'text '() (present (lambda (value attrs)
                                                 (case value [(added) "A"] [(modified) "M"] [(deleted) "D"] [(renamed) "R"] [(copied) "C"] [else "?"]))))
        (list 'commit 10 'text '() (present (lambda (value attrs) (if (> (get attrs 'depth 0) 0) "" (substring value 0 (min 10 (string-length value)))))))
        (list 'date 10 'text '() (present date))
        (list 'author 12 'text '() (present (lambda (value attrs) value)))
        (list 'subject 16 'text '() (present (lambda (value attrs) value)))))
    (mode:register! "git:diff" '() '() diff-styles)
    (for-each (lambda (key) (keymap:bind-default! 'git-history key (keymap:call refresh! widget:target))) '("r" "C-r"))
    (keymap:bind-default! "C-x g" log!)))
