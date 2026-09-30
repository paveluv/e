;; Filesystem composition: base queries, shared controls, explicit host placement.
(import (only (foundation edoc) elibrary))
(elibrary (apps finder)
  (export choose! complete! create! enter! init! navigate! open! open-directory! parent! show-hidden toggle-hidden!)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (foundation string) string:)
          (prefix (head catalogue-host) catalogue-host:) (prefix (head edit) edit:)
          (prefix (head entry) entry:) (prefix (head head) head:)
          (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head layout) layout:) (prefix (head table) table:)
          (prefix (head widget) widget:) (prefix (head window) window:)
          (prefix (service directory) directory:) (prefix (service file) file:)
          (prefix (service file-query) file-query:) (prefix (service filesystem) filesystem:)
          (prefix (state collection) collection:) (prefix (state connection) connection:)
          (prefix (state model) model:) (prefix (state view) view:) (prefix (sys glyph) glyph:))

  (define (get r k fallback) (cond [(and r (assq k r)) => cdr] [else fallback]))
  (define (child id name) (cadr (assq name (view:children (interaction:snapshot id)))))
  (define (table id) (child id 'table))
  (define (entry id) (child (child (table id) 'filter) 'entry))
  (define (query id) (view:source (interaction:snapshot id)))
  (define (top id)
    (let ([p (view:parent (interaction:snapshot id))]) (if p (top p) id)))
  (define (focus-entry! id) (widget:focus! (top id) (entry id)))
  (define target-table (keymap:call widget:descendant widget:target 'table))
  (define target-entry (keymap:call widget:descendant widget:target 'table 'filter 'entry))
  (define completing (make-hashtable equal-hash equal?))
  (define (read-model id) (caddar (cadr (model:snapshots (list id)))))

  (edoc "Whether newly created Finder queries include dot entries. Existing queries have their own source option; M-. toggles it."
        (value boolean))
  (define show-hidden (make-parameter #f (lambda (v) (unless (boolean? v) (error 'show-hidden "expected boolean")) v)))

  (edoc "Create an unmounted Finder with explicit open (document reference) and return host commands. An optional existing filesystem query shares filter, hidden policy and sort; selection, navigation history and geometry belong to each view."
        (commands list "host command bindings") (directory directory "initial directory")
        (shared (list-of row-source) "optional existing filesystem query") (returns model))
  (define (create! commands directory . shared)
    (unless (<= (length shared) 1) (error 'create! "expected at most one query"))
    (let* ([q (if (pair? shared) (car shared)
                (car (filesystem:create-query! head:ui-actor
                       (filesystem:create-source! head:ui-actor (file:expand "~") (show-hidden) 'persistent)
                       (file-query:directory-filter (file:canonical (file:expand directory))))))]
           [r (collection:summary q)] [v (get r 'value '())]
           [filter (find (lambda (ref) (eq? (car ref) 'buffer)) (get v 'owned '()))]
           [source (read-model (get v 'source #f))])
      (unless (and filter (eq? (get source 'kind #f) 'filesystem-source))
        (error 'create! "expected a filesystem query with an editable filter" q))
      (let* ([root (view:create! head:ui-actor q 'finder 1 (list (cons 'commands commands)) '((history)))]
             [table (table:create! head:ui-actor q '(name size modified created permissions count) '((identity . name) (presentation finder 1) (selection-policy . suggest)))]
             [row (view:create! head:ui-actor filter 'filter 1 '((spacing . normal)) '())]
             [label (view:create! head:ui-actor #f 'label 1 '((text . "Filter:")) '())]
             [entry (view:create! head:ui-actor filter 'entry 1 '((presentation finder 1) (policy rooted-path 1) (context))
                      (make-list 2 (cons 0 (string-length (get v 'input-filter (get v 'filter ""))))))]
             [status (view:create! head:ui-actor q 'finder-status 1 '() '())]
             [d (view:snapshot table)])
        (view:arrange! head:ui-actor
          (list (list row 0 (list (list 'label label 'fit) (list 'entry entry 'fit) (list 'status status 'fit)) '((spacing . normal)))
            (list table 1 (cons (list 'filter row 'fit) (view:children d))
              (cons* '(empty-text . "No matching paths")
                (list 'commands (list 'activate root 'choose '(#f)) (list 'enter root 'choose '(#t))) (view:options d)))
            (list root 0 (list (list 'table table '(grow 1))) (list (cons 'commands commands)))) '())
        (connection:bind! head:ui-actor root (list (list entry 'context #f (list q 'summary))))
        root)))

  (define (summary id)
    (let-values ([(source d inputs) (widget:context id 'current)]) (get source 'value '())))
  (define (entry-source id)
    (let-values ([(source d inputs) (widget:context (entry id) 'current)]) source))
  (define (text id) (vector-ref (get (entry-source id) 'value '#("")) 0))
  (define (history id) (get (view:state (interaction:snapshot id)) 'history '()))

  (edoc "Navigate one Finder query to an absolute directory, clearing other filter keys. Restore this view's last child choice when returning."
        (receiver id (view finder)) (id model "Finder view") (path directory "directory to list"))
  (define (navigate! id path)
    (let* ([path (file:canonical (file:expand path))] [filter (file-query:directory-filter path)]
           [selected (cond [(assoc path (history id)) => cdr] [else #f])])
      (hashtable-delete! completing id)
      (entry:set-text! (entry id) filter)
      (interaction:set-state! head:ui-actor id #f
        (list (cons 'history (history id)) (list 'returning filter selected)))
      (focus-entry! id)
      (when selected (table:select! (table id) (list 'path selected 'directory)))))

  (edoc "Navigate to the parent directory, remembering the departed child so repeated Left/Right retraces the route, including during a pending listing."
        (receiver id (view finder)) (id model "Finder view"))
  (define (parent! id)
    (let* ([keys (file-query:keys (text id) (file:expand "~"))]
           [v (summary id)]
           [path (if (and (equal? (text id) (get v 'input-filter #f)) (eq? (get v 'status #f) 'ready))
                   (get (get v 'details '()) 'root (file-query:root keys)) (file-query:root keys))]
           [parent (directory:parent path)]
           [h (cons (cons parent path) (filter (lambda (p) (not (string=? (car p) parent))) (history id)))])
      (interaction:set-state! head:ui-actor id #f (list (cons 'history h)))
      (when (string:prefix? "." (file:base-name path))
        (let* ([v (get (collection:summary (query id)) 'value '())] [r (read-model (get v 'source #f))])
          (unless (get (get r 'value '()) 'hidden #f)
            (filesystem:configure! head:ui-actor (get r 'id #f) (get r 'revision #f) #t))))
      (navigate! id parent)))

  (edoc "Enter the selected directory. While a parent listing is pending, follow its remembered return child so rapid Left/Right navigation preserves every step. Files remain untouched."
        (receiver id (view finder)) (id model "Finder view"))
  (define (enter! id)
    (let* ([v (summary id)] [r (get (view:state (interaction:snapshot id)) 'returning #f)]
           [selection (get (view:state (interaction:snapshot (table id))) 'selection #f)])
      (if (and r (cadr r) (equal? (car r) (text id))
            (or (not (equal? (get v 'input-filter #f) (text id))) (not (eq? (get v 'status #f) 'ready))
              (not selection) (not (= (cadr selection) (get v 'generation 0)))))
        (navigate! id (cadr r)) (table:invoke! (table id) 'enter))))

  (define (selected-row id selection basis)
    (unless (and (list? selection) (= (length selection) 3) (equal? (car selection) (query id)))
      (error 'choose! "selection belongs to another query"))
    (let ([r (collection:lookup (car selection) (cadr selection) (caddr selection) '(path kind proposal))])
      (unless (and (eq? (car r) 'ready) (equal? (caddr r) basis) (pair? (list-ref r 4)))
        (error 'choose! "the selected result changed"))
      (car (list-ref r 4))))
  (define (ready cells key)
    (let ([p (assq key cells)]) (and p (eq? (cadr p) 'ready) (caddr p))))

  (edoc "Visit the exact shown path through edit:visit-file!. Directories navigate this query; files go to its explicit host. Directory-only activation leaves files untouched. Pending or stale selections refuse."
        (receiver id (view finder)) (id model "Finder view") (directory-only? boolean "Right rather than Enter")
        (selection row-selection "shown query, generation and key") (basis datum "shown result basis"))
  (define (choose! id directory-only? selection basis)
    (let* ([row (selected-row id selection basis)] [cells (caddr row)]
           [path (ready cells 'path)] [directory? (eq? (ready cells 'kind) 'directory)]
           [proposed? (eq? (car (cadr row)) 'proposal)])
      (unless (and directory-only? (not directory?))
        (widget:keep-host-focus!)
        (unless (memq (ready cells 'kind) '(file directory)) (error 'choose! "choose a regular file or directory" path))
        (unless (or directory? (assq 'open (widget:commands id))) (error 'choose! "no open command is connected"))
        (if (and directory? (not proposed?)) (navigate! id path)
          (edit:visit-file! (if directory? (string-append path "/") path)
            (lambda (kind value)
              (case kind
                [(directory) (navigate! id value)]
                [(buffer) (widget:invoke! id 'open (catalogue-host:reference value))]))
            (and proposed? (ready cells 'proposal)))))))

  (edoc "Toggle this Finder query's explicit hidden-entry policy without discarding shared filesystem inventory."
        (receiver id (view finder)) (id model "Finder view"))
  (define (toggle-hidden! id)
    (let* ([v (get (collection:summary (query id)) 'value '())] [r (read-model (get v 'source #f))])
      (hashtable-delete! completing id)
      (filesystem:configure! head:ui-actor (get r 'id #f) (get r 'revision #f) (not (get (get r 'value '()) 'hidden #f)))))

  (edoc "Request completion against this filter revision and query basis. The base solves over its index; a newer edit, refresh or Tab supersedes the result."
        (receiver id (view finder)) (id model "Finder view"))
  (define (complete! id)
    (let* ([s (entry-source id)] [v (get (collection:summary (query id)) 'value '())])
      (hashtable-set! completing id (list #f (get v 'basis #f) (get s 'revision #f)))
      (service! id #f)))
  (define (service! id frame)
    (let* ([d (interaction:snapshot id)] [returning (get (view:state d) 'returning #f)]
           [selection (get (view:state (interaction:snapshot (table id))) 'selection #f)] [v (summary id)])
      (when (and returning (or (not (equal? (car returning) (text id)))
                             (and (eq? (get v 'status #f) 'ready) selection (= (cadr selection) (get v 'generation 0)))))
        (interaction:set-state! head:ui-actor id #f (list (cons 'history (history id))))))
    (let ([request (hashtable-ref completing id #f)])
      (when request
        (let* ([v (summary id)] [c (get (get v 'details '()) 'completion '())])
          (cond [(or (not (equal? (caddr request) (get (entry-source id) 'revision #f)))
                   (not (equal? (cadr request) (get v 'basis #f)))) (hashtable-delete! completing id)]
            [(not (car request))
             (when (and (eq? (get v 'status #f) 'ready) (get v 'complete #f))
               (if (> (get (get v 'details '()) 'unreadable 0) 0) (hashtable-delete! completing id)
                 (let ([intent (filesystem:complete! head:ui-actor (query id) (get v 'generation 0))])
                   (when intent (hashtable-set! completing id (cons intent (cdr request)))))))]
            [(and (pair? c) (= (cadr c) (car request)) (memq (car c) '(ready unavailable)))
             (hashtable-delete! completing id)
             (when (eq? (car c) 'ready)
               (guard (ex [(kernel:refusal? ex) (void)] [else (raise ex)])
                 (entry:set-text! (entry id) (caddr c) (caddr request))))])))))
  (define (event! id source d event)
    (case (car event)
      [(text) (focus-entry! id) (entry:insert! (entry id) (cadr event)) #t]
      [(cancel) (hashtable-delete! completing id) #f]
      [else #f]))

  (define (default!)
    (window:tool! "finder" (lambda (commands) (create! commands (head:default-directory)))))
  (define (show!)
    (let* ([b (window:show-widget! (head:current-window) (default!))]
           [host (head:buffer-fact b 'widget-id #f)] [app (child host 'app)])
      (head:show-buffer! b) (focus-entry! app) (widget:pump!) app))

  (edoc "Reopen the retained Finder with its filter intact; first use starts in the current document's directory."
        (returns model "Finder view"))
  (define (open!) (show!))

  (edoc "Open the retained Finder and navigate to a directory, clearing other filter keys."
        (directory directory "directory to list") (returns model))
  (define (open-directory! directory) (let ([app (show!)]) (navigate! app directory) app))

  (define (escaped text)
    (apply string-append (map (lambda (c)
                                (cond [(char=? c #\\) "\\\\"]
                                  [(eq? (char-general-category c) 'Cc) (format "\\x~x;" (char->integer c))]
                                  [else (string c)])) (string->list text))))
  (define (normalize-path text caret)
    (cond [(or (string=? text "") (string:prefix? "/" text) (string:prefix? "\"/" text)) (list text caret)]
      [(string:prefix? "~" text)
       (let ([home (file:expand "~")]) (list (string-append home (substring text 1 (string-length text))) (+ caret (- (string-length home) 1))))]
      [(string:prefix? "\"" text) (list (string:insert text 1 "/") (if (positive? caret) (+ caret 1) caret))]
      [else (list (string-append "/" text) (+ caret 1))]))
  (define (project-filter text context)
    (let ([missing (and (equal? text (get context 'input-filter #f)) (get (get context 'details '()) 'missing #f))])
      (let loop ([parts (glyph:clusters text)] [at 0] [quoted? #f] [escape? #f] [out '()])
        (if (null? parts) (reverse out)
          (let* ([end (+ at (caar parts))] [s (substring text at end)] [c (string-ref text at)]
                 [separator? (and (char=? c #\space) (not quoted?))])
            (loop (cdr parts) end
              (if (and (char=? c #\") (not escape?)) (not quoted?) quoted?)
              (and quoted? (char=? c #\\) (not escape?))
              (cons (list (if separator? " ∧ " (escaped s))
                      (if (and missing (< at (cdr missing)) (> end (car missing))) '(italic) '())) out)))))))
  (define (name-cell cell cells attrs)
    (if (not (eq? (car cell) 'ready)) '("")
      (let* ([raw (cadr cell)] [name (escaped raw)]
             [text (string-append name (if (ready cells 'link) "@" "") (if (eq? (ready cells 'kind) 'directory) "/" ""))])
        (cons text (filter values
                     (map (lambda (span)
                            (and (eq? (car span) 'name)
                              (list (string-length (escaped (substring raw 0 (cadr span))))
                                (string-length (escaped (substring raw 0 (caddr span)))) 'mark))) (get attrs 'matches '())))))))
  (define (present formatter)
    (lambda (cell cells attrs)
      (if (eq? (car cell) 'ready) (list (formatter (cadr cell) cells)) '(""))))
  (define (size-text bytes cells)
    (let scale ([n bytes] [units '("B" "KiB" "MiB" "GiB" "TiB")])
      (if (and (>= n 1024) (pair? (cdr units))) (scale (/ n 1024.0) (cdr units))
        (if (string=? (car units) "B") (format "~a B" n) (format "~,1f ~a" n (car units))))))
  (define (date-text nanoseconds cells)
    (let ([d (time-utc->date (make-time 'time-utc (mod nanoseconds 1000000000) (div nanoseconds 1000000000)))])
      (format "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" (date-year d) (date-month d) (date-day d) (date-hour d) (date-minute d))))
  (define (permissions mode cells)
    (let ([s (string-copy "----------")])
      (string-set! s 0 (cond [(ready cells 'link) #\l] [(eq? (ready cells 'kind) 'directory) #\d] [else #\-]))
      (do ([i 0 (+ i 1)]) ((= i 9))
        (unless (zero? (logand mode (expt 2 (- 8 i)))) (string-set! s (+ i 1) (string-ref "rwxrwxrwx" i))))
      (for-each (lambda (bit i yes no) (unless (zero? (logand mode bit)) (string-set! s i (if (char=? (string-ref s i) #\x) yes no))))
        '(#o4000 #o2000 #o1000) '(3 6 9) '(#\s #\s #\t) '(#\S #\S #\T)) s))
  (define (status-data id source inputs)
    (let* ([v (get source 'value '())] [details (get v 'details '())]
           [n (get details 'matches 0)] [unreadable (get details 'unreadable 0)] [c (get details 'completion '())])
      (string-append (format "[~a~a match~a]" n (if (and (get v 'complete #f) (zero? unreadable)) "" "+") (if (= n 1) "" "es"))
        (if (get details 'hidden #f) " [showing hidden]" "")
        (cond [(eq? (get v 'status #f) 'unavailable) " [Unavailable]"]
          [(not (get v 'complete #f)) " [Searching…]"]
          [(and (pair? c) (eq? (car c) 'unavailable)) " [Completion unavailable]"]
          [(and (pair? c) (eq? (car c) 'pending)) " [Completing…]"] [else ""])
        (if (> unreadable 0) (format " [~a unreadable]" unreadable) ""))))

  (edoc "Install Finder's composition, path presentation and domain actions. Shared entries and tables own editing, selection, sorting, pointer interaction and wheel scrolling." (public))
  (define (init!)
    (widget:register! 'finder 1
      (append (layout:container 'y)
        (list '(receivers (table table)) (cons 'capture-contexts '(finder)) (cons 'event event!) (cons 'service service!)
          (cons 'release (lambda (id) (hashtable-delete! completing id)))
          (cons 'actions (list (cons 'choose choose!) (cons 'navigate navigate!) (cons 'parent parent!) (cons 'enter enter!) (cons 'complete complete!) (cons 'toggle-hidden toggle-hidden!))))))
    (widget:register! 'finder-status 1
      (list (cons 'prepare status-data)
        (cons 'render (lambda (s d w h r) (if (zero? (car r)) (list (glyph:fit s w)) '())))
        (cons 'measure (lambda (s d axis cross child) (if (eq? axis 'y) '(1 1) (list 0 (glyph:cells s)))))
        (cons 'decorate (lambda (s d w h r) (if (zero? (car r)) (list (list (list 0 0 (min w (glyph:cells s)) 1) 'ghost)) '())))))
    (entry:register-presentation! 'finder 1 project-filter)
    (edit:register-policy! 'rooted-path 1
      (lambda (lines positions)
        (let* ([text (vector-ref lines 0)] [results (map (lambda (p) (normalize-path text (cdr p))) positions)])
          (values (vector (caar results)) (map (lambda (r) (cons 0 (cadr r))) results)))))
    (table:register-presentation! 'finder 1
      (list (list 'name 14 'text '(kind link) name-cell)
        (list 'size 6 'right '() (present size-text))
        (list 'modified 16 'text '() (present date-text)) (list 'created 16 'text '() (present date-text))
        (list 'permissions 11 'text '(kind link) (present permissions))
        (list 'count 7 'right '(exact) (present (lambda (n cells) (format "~a~a" n (if (ready cells 'exact) "" "+")))))))
    (for-each (lambda (p) (keymap:bind-default! 'finder (car p) (keymap:call table:move! target-table (cdr p))))
      '(("DOWN" . next) ("C-n" . next) ("UP" . previous) ("C-p" . previous) ("S-TAB" . previous)
        ("HOME" . first) ("C-a" . first) ("M-<" . first) ("END" . last) ("C-e" . last) ("M->" . last)
        ("PGDN" . page-next) ("C-v" . page-next) ("PGUP" . page-previous) ("M-v" . page-previous)))
    (keymap:bind-default! 'finder "RET" (keymap:call table:invoke! target-table 'activate))
    (keymap:bind-default! 'finder "RIGHT" (keymap:call enter! widget:target))
    (keymap:bind-default! 'finder "LEFT" (keymap:call parent! widget:target))
    (keymap:bind-default! 'finder "TAB" (keymap:call complete! widget:target))
    (keymap:bind-default! 'finder "C-u" (keymap:call entry:delete! target-entry 'all))
    (keymap:bind-default! 'finder "C-b" (keymap:call entry:move! target-entry 'left))
    (keymap:bind-default! 'finder "C-f" (keymap:call entry:move! target-entry 'right))
    (keymap:bind-default! 'finder "M-." (keymap:call toggle-hidden! widget:target))
    (keymap:bind-default! 'finder "C-r" (keymap:call filesystem:refresh! head:ui-actor))
    (for-each (lambda (n column) (keymap:bind-default! 'finder (format "F~a" n) (keymap:call table:toggle-sort! target-table column)))
      '(1 2 3 4 5 6) '(name size modified created permissions count))
    (for-each (lambda (key) (keymap:bind-default! 'finder key (keymap:call widget:invoke! widget:target 'return))) '("ESC" "C-g"))
    (keymap:bind-default! "C-x C-f" open!)
    (head:set-directory-opener! open-directory!)))
