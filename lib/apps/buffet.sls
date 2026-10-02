;; Buffer catalogue composition. Hosts place documents; the base owns queries.
(import (only (foundation edoc) elibrary))
(elibrary (apps buffet)
  (export choose! create! delete! init! kill! next! open! previous!)
  (import (chezscheme)
          (prefix (foundation string) string:)


          (prefix (head control) control:)
          (prefix (head entry) entry:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:)
          (prefix (head layout) layout:)

          (prefix (head table) table:)
          (prefix (head widget) widget:)
          (prefix (head window-control) window-control:)

          (prefix (service file) file:)
          (prefix (service window) window:)
          (prefix (state catalogue) catalogue:)
          (prefix (state collection) collection:)
          (prefix (state construction) construction:)
          (prefix (state model) model:)
          (prefix (state store) store:)
          (prefix (state view) view:))

  (define (get xs key fallback) (cond [(assq key xs) => cdr] [else fallback]))
  (define (child id name) (cadr (assq name (view:children (interaction:snapshot id)))))
  (define target-table (keymap:call widget:descendant widget:target 'table))
  (define target-entry (keymap:call widget:descendant widget:target 'table 'filter 'entry))
  (define (event! id source descriptor event)
    (and (eq? (car event) 'text)
      (let ([entry (child (child (child id 'table) 'filter) 'entry)]
            [root (let loop ([id id])
                    (let ([parent (view:parent (interaction:snapshot id))]) (if parent (loop parent) id)))])
        (widget:focus! root entry) (entry:insert! entry (cadr event)) #t)))

  (edoc "Create an unmounted Buffet composition. Commands explicitly bind open (document reference) and return; no current-window fallback is used. An optional existing query shares filter and sort; selection and geometry always belong to this view."
        (owner (or model #f) "lifetime owner, false for a session root") (commands list "host command bindings") (shared (list-of row-source) "optional shared catalogue query") (returns model "app view"))
  (define (create! owner commands . shared)
    (construction:call! head:ui-actor
      (lambda (remember!)
        (unless (<= (length shared) 1) (error 'create! "expected an optional shared query"))
        (let* ([query (if (pair? shared) (car shared)
                        (remember! (car (catalogue:create-query! head:ui-actor (remember! (catalogue:create-source! head:ui-actor (file:expand "~/") 'persistent))))))]
               [r (collection:summary query)]
               [filter (and r (find (lambda (ref) (eq? (car ref) 'buffer)) (get (get r 'value '()) 'owned '())))])
          (unless filter (error 'create! "expected a catalogue query with an editable filter" query))
          (let* ([root (remember! (view:create! head:ui-actor #f 'buffet 1 '() '() owner))]
                 [table (table:create! head:ui-actor root query '(modified flags name lines mode file) '((identity . name) (presentation buffet 1)))]
                 [filter (control:create-filter! head:ui-actor root filter "Filter:" "")]
                 [d (view:snapshot table)])
            (view:arrange! head:ui-actor
              (list (list table 1 (cons (list 'filter filter 'fit) (view:children d))
                      (cons* '(empty-text . "No matching buffers")
                        (list 'commands (list 'activate root 'choose '()) (list 'trash root 'kill '()) (list 'delete root 'delete '())) (view:options d)))
                (list root 0 (list (list 'table table '(grow 1)))
                  (list (cons 'commands (cons (list 'current table 'emphasize '()) commands))))) '())
            root)))))

  (define (selected-row id selection basis)
    (let* ([table (child id 'table)] [d (interaction:snapshot table)])
      (unless (and (list? selection) (= (length selection) 3) (equal? (view:source d) (car selection)))
        (error 'buffet "selection does not belong to this catalogue"))
      (let ([rows (collection:lookup (car selection) (cadr selection) (caddr selection) '(version archive))])
        (unless (and (eq? (car rows) 'ready) (equal? (caddr rows) basis) (pair? (list-ref rows 4)))
          (error 'buffet "the selected result changed"))
        (let* ([row (car (list-ref rows 4))] [cells (caddr row)]
               [version (assq 'version cells)] [archive (assq 'archive cells)])
          (unless (and (equal? (cadr row) (caddr selection)) version archive
                       (eq? (cadr version) 'ready) (eq? (cadr archive) 'ready))
            (error 'buffet "choose a document row"))
          (list (cadr row) (caddr version) (caddr archive))))))

  (define (archive! row action)
    (let-values ([(status metadata) (store:archive! head:ui-actor (car row) (cadr row) action)])
      (unless (eq? status 'applied) (error 'buffet "document changed; choose it again" status))))

  (edoc "Open the exact selected document through the host; archive selections restore that ID against its shown version first."
        (receiver id (view buffet)) (id model "Buffet view") (selection row-selection "shown query, generation and key") (basis datum "shown result basis"))
  (define (choose! id selection basis)
    (unless (assq 'open (widget:commands id)) (error 'choose! "no open command is connected"))
    (let ([row (selected-row id selection basis)])
      (unless (eq? (caddr row) 'live) (archive! row 'restore))
      (widget:invoke! id 'open (car row))))

  (edoc "Trash a selected live shared document, delete disposable output, or retire a listed view; stale versions refuse. Every displaying window gets the ordinary fallback."
        (receiver id (view buffet)) (id model "Buffet view") (selection row-selection "shown selection") (basis datum "shown result basis"))
  (define (kill! id selection basis)
    (let* ([row (selected-row id selection basis)] [ref (car row)])
      (unless (eq? (caddr row) 'live) (error 'kill! "choose a live document"))
      (if (eq? (car ref) 'buffer)
        (archive! row 'trash)
        (let* ([r (caddar (cadr (model:snapshots (list ref))))] [d (and r (get r 'value #f))])
          (unless (and d (equal? (cadr row)
                                 (list (view:generation d) (get (view:options d) 'name #f)
                                   (get (view:options d) 'audience 'all)))
                    (get (view:options d) 'catalogue #f))
            (error 'kill! "document changed; choose it again"))
          (let-values ([(status current) (view:retire! head:ui-actor ref (get r 'revision #f))])
            (unless (eq? status 'applied) (error 'kill! "document changed; choose it again" status)))))))

  (edoc "Permanently delete a selected Trash or Backups item against its shown version. Live documents and files on disk are never deleted."
        (receiver id (view buffet)) (id model "Buffet view") (selection row-selection "shown selection") (basis datum "shown result basis"))
  (define (delete! id selection basis)
    (let ([row (selected-row id selection basis)])
      (when (eq? (caddr row) 'live) (error 'delete! "choose a Trash or Backups item"))
      (archive! row 'delete)))

  (edoc "Open Buffet in this window, clearing its filter and selecting the previous retained document. Other panes share query preferences but keep independent selection and scrolling."
        (receiver window (view window)) (window model "destination window")
        (returns model "Buffet view"))
  (define (open! window)
    (let* ([manager (window-control:manager window)]
           [found (window:find-app manager window "buffet")]
           [recent (window:documents manager window)]
           [previous (find (lambda (ref) (not (and found (equal? ref (cadr found)))))
                           (if (pair? recent) (cdr recent) '()))]
           [fallback (window:document manager window)]
           [app (window-control:open-app! window "buffet" create!)] )
      (widget:pump!)
      (let* ([table (child app 'table)] [entry (child (child table 'filter) 'entry)])
        (entry:delete! entry 'all)
        (widget:focus! app entry)
        (table:select! table (or previous fallback))) app))

  (define (switch-window! window direction)
    (let* ([manager (window-control:manager window)] [found (window:find-app manager window "buffet")]
           [query (and found (view:source (view:snapshot (cadr (assq 'table (view:children (view:snapshot (cadr found))))))))]
           [ref (catalogue:neighbor head:ui-actor query (window:document manager window) direction)])
      (when ref (window-control:open-document! window ref))))

  (edoc "Switch this window to the next live document in Buffet's unfiltered compound order, wrapping at the end. Before Buffet exists, use the default name order without creating a table."
        (receiver window (view window)) (window model "destination window"))
  (define (next! window) (switch-window! window 'next))

  (edoc "Switch this window to the previous live document in Buffet's unfiltered compound order, wrapping at the beginning. Before Buffet exists, use the default name order without creating a table."
        (receiver window (view window)) (window model "destination window"))
  (define (previous! window) (switch-window! window 'previous))

  (define (ready-value cells name)
    (let ([p (assq name cells)]) (and p (eq? (cadr p) 'ready) (caddr p))))
  (define (age seconds)
    (let ([s (max 0 seconds)])
      (cond [(< s 60) (format "~a s" s)] [(< s 3600) (format "~a min" (div s 60))]
        [(< s 86400) (format "~a h" (div s 3600))] [else (format "~a d" (div s 86400))])))
  (define (clock cell cells attributes)
    (let ([archived (ready-value cells 'archived-at)])
      (cond [archived (list (string-append (age (- (time-second (current-time 'time-utc)) archived)) " ago"))]
        [(eq? (car cell) 'ready)
         (let* ([value (cadr cell)] [date (time-utc->date (make-time 'time-utc (mod value 1000000000) (div value 1000000000)))])
           (list (format "~2,'0d:~2,'0d:~2,'0d" (date-hour date) (date-minute date) (date-second date))))]
        [else '("")])))
  (define (flags value attributes)
    (let ([s (string:join (filter values (list (and (memq 'conflicted value) "!!") (and (memq 'read-only value) "%"))) " ")])
      (cons s (if (memq 'conflicted value) '((0 2 error)) '()))))
  (define (present format)
    (lambda (cell cells attributes)
      (case (car cell)
        [(ready) (format (cadr cell) attributes)] [(absent) '("")]
        [else (let ([text (if (eq? (car cell) 'pending) "[Pending]" "[Unavailable]")])
                (list text (list 0 (string-length text) 'ghost)))])))
  (define (location cell cells attributes)
    (let ([expires (ready-value cells 'expires-at)])
      (if expires
        (let ([left (- expires (time-second (current-time 'time-utc)))])
          (list (if (positive? left) (string-append (age left) " left") "expiring")))
        (if (eq? (car cell) 'ready) (list (file:abbreviate (cadr cell))) '("")))))

  (edoc "Install Buffet's composition, column presentation, domain actions and global switching shortcuts. Entry, table and scroll widgets own editing, sorting, selection and pointer behavior." (public))
  (define (init!)
    (widget:register! 'buffet 1
      (append (layout:container 'y)
        (list '(receivers (table table)) (cons 'capture-contexts '(buffet)) (cons 'event event!)
          (cons 'actions (list (cons 'choose choose!) (cons 'kill kill!) (cons 'delete delete!))))))
    (table:register-presentation! 'buffet 1
      (list (list 'modified 10 'text '(archived-at) clock) (list 'flags 7 'text '() (present flags))
        (list 'name 9 'text '() (present (lambda (v a) (list v))))
        (list 'lines 8 'right '() (present (lambda (v a) (list (number->string v)))))
        (list 'mode 7 'text '(archive)
          (lambda (cell cells attrs)
            (let ([archive (ready-value cells 'archive)])
              (if (memq archive '(trash backup)) (list (symbol->string archive))
                (if (eq? (car cell) 'ready) (list (cadr cell)) '(""))))))
        (list 'file 10 'tail '(expires-at) location)))
    (for-each (lambda (p) (keymap:bind-default! 'buffet (car p) (keymap:call table:move! target-table (cdr p))))
      '(("DOWN" . next) ("C-n" . next) ("TAB" . next) ("UP" . previous) ("C-p" . previous) ("S-TAB" . previous)
        ("HOME" . first) ("C-a" . first) ("M-<" . first) ("END" . last) ("C-e" . last) ("M->" . last)
        ("PGDN" . page-next) ("C-v" . page-next) ("PGUP" . page-previous) ("M-v" . page-previous)))
    (for-each (lambda (p) (keymap:bind-default! 'buffet (car p) (keymap:call table:invoke! target-table (cdr p))))
      '(("RET" . activate) ("C-k" . trash) ("C-x D" . delete)))
    (keymap:bind-default! 'buffet "C-u" (keymap:call entry:delete! target-entry 'all))
    (for-each (lambda (n column) (keymap:bind-default! 'buffet (format "F~a" n) (keymap:call table:toggle-sort! target-table column)))
      '(1 2 3 4 5 6) '(modified flags name lines mode file))
    (for-each (lambda (key) (keymap:bind-default! 'buffet key (keymap:call widget:invoke! widget:target 'return))) '("ESC" "C-g"))
    (for-each (lambda (key) (keymap:bind-default! 'composed-window key (keymap:call open! widget:target))) '("C-x b" "C-x C-b"))
    (keymap:bind-default! 'composed-window "M-S-UP" (keymap:call previous! widget:target))
    (keymap:bind-default! 'composed-window "M-S-DOWN" (keymap:call next! widget:target))))
