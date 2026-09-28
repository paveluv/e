;; Buffer catalogue composition. Hosts place documents; the base owns queries.
(import (only (foundation edoc) elibrary))
(elibrary (apps buffet)
  (export choose! create! delete! init! kill! next! open! previous!)
  (import (chezscheme) (prefix (foundation string) string:)
          (prefix (head control) control:) (prefix (head document) document:)
          (prefix (head entry) entry:) (prefix (head head) head:)
          (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head layout) layout:) (prefix (head table) table:)
          (prefix (head widget) widget:) (prefix (head window) window:)
          (prefix (service file) file:) (prefix (state catalogue) catalogue:)
          (prefix (state collection) collection:) (prefix (state store) store:)
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
        (commands list "host command bindings") (shared (list-of row-source) "optional shared catalogue query") (returns model "app view"))
  (define (create! commands . shared)
    (unless (<= (length shared) 1) (error 'create! "expected an optional shared query"))
    (let* ([query (if (pair? shared) (car shared)
                    (car (catalogue:create-query! head:ui-actor (document:create-source! 'persistent))))]
           [r (collection:summary query)]
           [filter (and r (find (lambda (ref) (eq? (car ref) 'buffer)) (get (get r 'value '()) 'owned '())))])
      (unless filter (error 'create! "expected a catalogue query with an editable filter" query))
      (let* ([root (view:create! head:ui-actor #f 'buffet 1 '() '())]
             [table (table:create! head:ui-actor query '(modified flags name lines mode file) '((identity . name) (presentation buffet 1)))]
             [filter (control:create-filter! head:ui-actor filter "Filter:" "")]
             [d (view:snapshot table)])
        (view:arrange! head:ui-actor
          (list (list table 1 (cons (list 'filter filter 'fit) (view:children d))
                  (cons* '(empty-text . "No matching buffers")
                    (list 'commands (list 'activate root 'choose '()) (list 'trash root 'kill '()) (list 'delete root 'delete '())) (view:options d)))
            (list root 0 (list (list 'table table '(grow 1)))
              (list (cons 'commands (cons (list 'current table 'emphasize '()) commands))))) '())
        root)))

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
    (let-values ([(status metadata) (store:archive! head:ui-actor (cadar row) (cadr row) action)])
      (unless (eq? status 'applied) (error 'buffet "document changed; choose it again" status))))

  (edoc "Open the exact selected document through the host; archive selections restore that ID against its shown version first."
        (id model "Buffet view") (selection row-selection "shown query, generation and key") (basis datum "shown result basis"))
  (define (choose! id selection basis)
    (unless (assq 'open (widget:commands id)) (error 'choose! "no open command is connected"))
    (let ([row (selected-row id selection basis)])
      (unless (eq? (caddr row) 'live) (archive! row 'restore))
      (widget:invoke! id 'open (car row))))

  (edoc "Trash a selected live shared document, delete disposable output, or retire an attachment-local app; stale versions refuse. Every displaying window gets the ordinary fallback."
        (id model "Buffet view") (selection row-selection "shown selection") (basis datum "shown result basis"))
  (define (kill! id selection basis)
    (let* ([row (selected-row id selection basis)] [ref (car row)])
      (unless (eq? (caddr row) 'live) (error 'kill! "choose a live document"))
      (if (eq? (car ref) 'buffer)
        (let ([b (document:resolve! ref)])
          (archive! row 'trash)
          (when b (head:forget-buffer! b)))
        (unless (document:retire! ref (cadr row)) (error 'kill! "document changed; choose it again")))))

  (edoc "Permanently delete a selected Trash or Backups item against its shown version. Live documents and files on disk are never deleted."
        (id model "Buffet view") (selection row-selection "shown selection") (basis datum "shown result basis"))
  (define (delete! id selection basis)
    (let ([row (selected-row id selection basis)])
      (when (eq? (caddr row) 'live) (error 'delete! "choose a Trash or Backups item"))
      (archive! row 'delete)))

  (define (default!) (window:tool! "buffet" create!))

  (edoc "Open the default Buffet in this window, with a clear filter and the previous document selected. The retained window host owns origin and MRU policy."
        (returns model "Buffet view"))
  (define (open!)
    (let* ([was (head:current-buffer)] [host (default!)]
           [previous (or (find (lambda (b) (and (not (eq? b was))
                                                (not (equal? (head:buffer-fact b 'tool-key #f) "*buffet*")))) (head:buffers)) was)]
           [b (window:show-widget! (head:current-window) host)]
           [host (head:buffer-fact b 'widget-id #f)] [app (child host 'app)]
           [table (child app 'table)] [entry (child (child table 'filter) 'entry)])
      (head:show-buffer! b)
      (entry:delete! entry 'all)
      (widget:focus! host entry)
      (widget:pump!)
      (table:select! table (document:reference previous))
      app))

  (define (switch! direction)
    (let* ([host (default!)] [app (child host 'app)] [table (child app 'table)]
           [ref (catalogue:neighbor head:ui-actor (view:source (interaction:snapshot table)) (document:reference (head:current-buffer)) direction)]
           [b (and ref (document:resolve! ref))])
      (when b
        (if (equal? (head:buffer-fact b 'tool-key #f) "*buffet*") (open!) (head:show-buffer! b)))))

  (edoc "Switch to the next live document in Buffet's unfiltered compound order, wrapping at the end.")
  (define (next!) (switch! 'next))

  (edoc "Switch to the previous live document in Buffet's unfiltered compound order, wrapping at the beginning.")
  (define (previous!) (switch! 'previous))

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

  (edoc "Install Buffet's composition, column presentation, domain actions and global switching shortcuts. Entry, table and scroll widgets own editing, sorting, selection and pointer behavior.")
  (define (init!)
    (widget:register! 'buffet 1
      (append (layout:container 'y)
        (list (cons 'capture-contexts '(buffet)) (cons 'event event!)
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
    (for-each (lambda (p) (keymap:bind-default! 'buffet (car p) (keymap:call table:activate! target-table (cdr p))))
      '(("RET" . activate) ("C-k" . trash) ("C-x D" . delete)))
    (keymap:bind-default! 'buffet "C-u" (keymap:call entry:delete! target-entry 'all))
    (for-each (lambda (n column) (keymap:bind-default! 'buffet (format "F~a" n) (keymap:call table:toggle-sort! target-table column)))
      '(1 2 3 4 5 6) '(modified flags name lines mode file))
    (for-each (lambda (key) (keymap:bind-default! 'buffet key (keymap:call widget:invoke! widget:target 'return))) '("ESC" "C-g"))
    (keymap:bind-default! "C-x b" open!)
    (keymap:bind-default! "C-x C-b" open!)
    (keymap:bind-default! "M-S-UP" previous!)
    (keymap:bind-default! "M-S-DOWN" next!)))
