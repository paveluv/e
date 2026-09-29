;; bindings.sls -- inspect mouse, keyboard and widget command bindings.
;; C-x TAB opens <bindings> in the pop-up and pages an existing listing.
;; Mouse bindings follow the pointer; keyboard and widget commands follow
;; the active window. Each binding shows its public API and documentation.

(import (only (foundation edoc) elibrary))
(elibrary (apps bindings)
  (export hide! init! open! page-up! return! show!)
  (import (rnrs)
          (only (chezscheme) format iota list-head make-weak-eq-hashtable quotient void)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head mouse) mouse:)
          (prefix (head paint) paint:)
          (prefix (head prompt) prompt:)
          (prefix (head widget) widget:)
          (prefix (sys glyph) glyph:))

  (define view #f) ; the <bindings> buffer while it is shown
  (define over '()) ; ((window . buffer) ...) what a window showed before the listing took it
  (define listed #f) ; (buffer contexts read-only?) the listing describes
  (define swept? #f) ; whether the listings an older checkpoint restored have been dropped
  (define keyboard-cache #f)
  (define commands-cache #f)
  (define listed-pointer '()) ; the last inspected target, retained while browsing this help
  (define symbolic-spans (make-weak-eq-hashtable)) ; rendered text -> unresolved character spans

  ;; Preserve semantic marks while fitting text into columns. Plain strings
  ;; remain the display/cache keys; buffer facts publish the final row marks.
  (define (spans text) (hashtable-ref symbolic-spans text '()))
  (define (marked text ranges)
    (unless (null? ranges) (hashtable-set! symbolic-spans text ranges))
    text)
  (define (join . parts)
    (let loop ([parts parts] [offset 0] [ranges '()] [out '()])
      (if (null? parts) (marked (apply string-append (reverse out)) (apply append (reverse ranges)))
        (loop (cdr parts) (+ offset (string-length (car parts)))
          (cons (map (lambda (r) (cons (+ offset (car r)) (+ offset (cdr r)))) (spans (car parts))) ranges)
          (cons (car parts) out)))))
  (define (slice text start end)
    (marked (substring text start end)
      (map (lambda (r) (cons (- (max start (car r)) start) (- (min end (cdr r)) start)))
        (filter (lambda (r) (and (< (car r) end) (> (cdr r) start))) (spans text)))))

  (define (stale-listing? b)
    ;; a <bindings> or <bindings 2> local buffer that is not the view: one an older
    ;; checkpoint brought back as text
    (let ([name (head:buffer-name b)])
      (and (not (eq? b view)) (not (head:buffer-store-id b))
           (string:prefix? "<bindings" name) (string:suffix? ">" name))))

  (define (sweep!)
    ;; the listings an older checkpoint restored go; the view is made fresh
    (for-each (lambda (b) (when (stale-listing? b) (head:forget-buffer! b))) (head:buffers))
    (set! swept? #t))

  ;;; The rows ------------------------------------------------------------------------

  (define (action-procedure action)
    ;; the procedure a key action runs, or #f
    (cond [(procedure? action) action]
          [(keymap:call-action? action) (keymap:call-action-procedure action)]
          [(keymap:prefill-action? action) (keymap:prefill-action-procedure action)]
          [else #f]))

  (define (edits? action)
    ;; whether the command declares (edits): refused where the text is read-only
    (let* ([proc (action-procedure action)] [sigs (and proc (edoc:edoc-of proc))])
      (and (pair? sigs)
           (exists (lambda (f) (and (pair? f) (eq? (car f) 'edits))) (edoc:signature-flags (car sigs))))))

  (define (summary-of action)
    ;; what the procedure a key action runs does, from its documentation
    (let* ([proc (action-procedure action)] [sigs (and proc (edoc:edoc-of proc))])
      (if (pair? sigs) (edoc:signature-summary (car sigs)) "")))

  (define (trace action . substitutions)
    (map (lambda (row)
           (list (join (if (= (car row) 0) "" (string-append (make-string (* 2 (min 8 (car row))) #\space) "→ "))
                   (marked (cadr row) (list-ref row 4)) (if (cadddr row) (string-append " [" (cadddr row) "]") ""))
             (if (caddr row) (summary-of (caddr row)) "")))
      (apply keymap:action-trace action substitutions)))

  (define (shadowed? sequence nearer)
    ;; whether a nearer context binds the sequence, or a prefix of it, so
    ;; the key never reaches this binding
    (exists (lambda (context)
              (exists (lambda (n) (keymap:resolved-binding context (list-head sequence n)))
                      (map (lambda (i) (+ i 1)) (iota (length sequence)))))
            nearer))

  (define (context-groups context nearer read-only? keep describe)
    ;; keep, when given, admits a binding: the commands allowed in a
    ;; prompt for the global section while one is open
    ;; (keys command description) for a context's bindings that work here:
    ;; not shadowed by a nearer context, and not editing where the text is
    ;; read-only; the keys running one command together, groups by their
    ;; first key; a lambda shows as the anonymous command it is, a name
    ;; being owed
    (define (add key command description groups)
      (let ([hit (find (lambda (g) (equal? (cadr g) command)) groups)])
        (if hit
            (map (lambda (g) (if (eq? g hit) (cons (cons key (car g)) (cdr g)) g)) groups)
            (cons (list (list key) command description) groups))))
    (let loop ([owned (keymap:context-bindings context)] [groups '()])
      (if (null? owned)
          (list-sort (lambda (a b) (string<? (car (car a)) (car (car b))))
                     (map (lambda (g) (cons (list-sort string<? (car g)) (cdr g))) groups))
          (let* ([b (cdr (car owned))] [action (keymap:binding-action b)]
                 [command (and action (if describe (describe b) (trace action)))])
            (loop (cdr owned)
                  (if (and command
                           (not (shadowed? (keymap:binding-sequence b) nearer))
                           (not (and read-only? (edits? action)))
                           (or (not keep) (keep b)))
                      (add (keymap:sequence-text (keymap:binding-sequence b)) command (summary-of action) groups)
                      groups))))))

  ;;; The text -------------------------------------------------------------------------

  (define (cells s) (glyph:cells s))

  (define (pad s width)
    (if (>= (cells s) width) s (join s (make-string (- width (cells s)) #\space))))

  (define (wrap text width)
    ;; the text as lines of at most width cells, broken at spaces, a word
    ;; wider than the column broken where the column ends
    (define (chop word)
      ;; a word as pieces the column holds
      (let loop ([word word] [out '()])
        (if (<= (cells word) width)
            (reverse (cons word out))
            (let cut ([n (string-length word)])
              (if (or (<= n 1) (<= (cells (substring word 0 n)) width))
                  (loop (slice word n (string-length word)) (cons (slice word 0 n) out))
                  (cut (- n 1)))))))
    (let loop ([words (apply append (map chop (filter (lambda (w) (> (string-length w) 0)) (split-words text))))]
               [line ""] [out '()])
      (cond
        [(null? words) (reverse (if (string=? line "") out (cons line out)))]
        [(string=? line "") (loop (cdr words) (car words) out)]
        [(<= (+ (cells line) 1 (cells (car words))) width)
         (loop (cdr words) (join line " " (car words)) out)]
        [else (loop words "" (cons line out))])))

  (define (split-words s)
    (let loop ([i 0] [start 0] [out '()])
      (cond
        [(= i (string-length s)) (reverse (cons (slice s start i) out))]
        [(char=? (string-ref s i) #\space) (loop (+ i 1) (+ i 1) (cons (slice s start i) out))]
        [else (loop (+ i 1) start out)])))

  (define (bracket keys i)
    ;; the margin of a group's row i: a line down the keys sharing the
    ;; command, from the middle of the first key's row to the middle of the
    ;; last's; nothing beside a lone key or a description running on
    (let ([last (- (length keys) 1)])
      (cond [(< last 1) "  "]
            [(= i 0) " ╷"]
            [(< i last) " │"]
            [(= i last) " ╵"]
            [else "  "])))

  (define (section title groups width)
    ;; a heading, then each group's keys down the first column beside its
    ;; command, a long call wrapped at its spaces, and its description
    ;; wrapped in the last column, the keys of a group joined by a line in
    ;; the margin
    (define (steps group)
      (if (string? (cadr group)) (list (cdr group)) (cadr group)))
    (define (step-lines step command-width text-width)
      (let* ([command (wrap (car step) command-width)] [text (wrap (cadr step) text-width)]
             [height (max (length command) (length text) 1)])
        (map (lambda (i) (cons (if (< i (length command)) (list-ref command i) "")
                           (if (< i (length text)) (list-ref text i) ""))) (iota height))))
    (if (null? groups)
        '()
        (let* ([key-width (apply max (map (lambda (g) (apply max (map cells (car g)))) groups))]
               ;; the key column takes what its widest key needs; of what is
               ;; left, the margin and separators apart, the command column
               ;; takes what its widest command needs while the description
               ;; keeps twenty-four cells, and shrinks to eight cells before
               ;; the description shrinks below that
               [room (- width key-width 6)]
               [command-width (min (apply max (apply append (map (lambda (g) (map (lambda (s) (cells (car s))) (steps g))) groups))) (max 8 (- room 24)))]
               [text-width (max 8 (- room command-width))])
          (cons title
                (apply append
                  (map (lambda (g)
                         (let* ([keys (car g)]
                                [lines (apply append (map (lambda (s) (step-lines s command-width text-width)) (steps g)))]
                                [height (max (length keys) (length lines) 1)])
                           (let loop ([i 0] [out '()])
                             (if (= i height) (reverse out)
                                 (loop (+ i 1)
                                       (cons (join
                                               (bracket keys i) (pad (if (< i (length keys)) (list-ref keys i) "") key-width) "  "
                                               (pad (if (< i (length lines)) (car (list-ref lines i)) "") command-width) "  "
                                               (if (< i (length lines)) (cdr (list-ref lines i)) ""))
                                             out))))))
                       groups))))))

  (define (capture-note context)
    ;; a capturing context's other keys go to the app: one row saying so,
    ;; with the toggle and the keys the editor keeps
    (let ([capture (keymap:context-capture context)])
      (if (not capture) '()
          (list (list (list "other keys") "to the app"
                      (format "Every other key goes to the app; ~a keep~a to the editor unless ~a toggles full capture"
                              (string:join (map (lambda (k) (keymap:sequence-text (list k))) (cddr capture)) " and ")
                              (if (= (length (cddr capture)) 1) "s" "")
                              (keymap:sequence-text (list (car capture)))))))))

  (define (read-only-text? b)
    ;; whether an editing command is refused in the buffer: an app's, or one
    ;; read-only outright; a guard deciding per edit does not count
    (or (head:app-buffer? b)
        (let ([guard (head:buffer-read-only b)]) (and guard (not (procedure? guard))))))

  (define (prompt-context)
    ;; the context of an open prompt's content view, or #f
    (let ([body (prompt:content)]) (and body (prompt:content-context body))))

  (define (contexts b key)
    (let* ([root (head:buffer-fact b 'widget-id #f)] [outer (if root '(global) (append (mode:key-contexts b) '(global)))])
      (if (not root) outer
        (let loop ([scopes (cadr (widget:key-scopes root key))] [out '()])
          (if (null? scopes) (append out outer)
            (let ([out (append out (filter (lambda (c) (not (memq c out))) (cadar scopes)))])
              (if (caddar scopes) out (loop (cdr scopes) out))))))))

  (define (reachable? b context binding)
    (let* ([sequence (keymap:binding-sequence binding)] [path (contexts b (car sequence))])
      (let loop ([path path] [nearer '()])
        (and (pair? path)
          (if (eq? (car path) context) (not (shadowed? sequence nearer))
            (loop (cdr path) (cons (car path) nearer)))))))

  (define (describe-binding b context binding)
    ;; Reify the known receiver; never run arbitrary argument producers to
    ;; describe a key. The resulting Scheme call works outside key dispatch.
    (let* ([root (head:buffer-fact b 'widget-id #f)]
           [scope (find (lambda (scope) (memq context (cadr scope)))
                    (cadr (widget:key-scopes root (car (keymap:binding-sequence binding)))))])
      (trace (keymap:binding-action binding)
        (if scope (list (cons widget:target (car scope))) '()))))

  (define (listing b width)
    ;; the keys that work now: with a prompt open, its content view's
    ;; context, the prompt's keys and the global commands allowed in a
    ;; prompt; else the buffer's mode contexts' bindings, an app's own keys
    ;; among them, then the global ones; each context's keys less those a
    ;; nearer context takes, and less the editing commands where the text
    ;; is read-only, in the width given
    (let ([width (max 40 width)] [read-only? (read-only-text? b)] [prompting? (prompt:active?)]
          [widget? (head:buffer-fact b 'widget-id #f)])
      (let loop ([contexts (if prompting?
                               (append (if (prompt-context) (list (prompt-context)) '()) '(prompt global))
                               (contexts b ""))]
                 [nearer '()] [out '()])
        (if (null? contexts)
            (apply append (reverse out))
            (let ([context (car contexts)])
              (loop (cdr contexts) (cons context nearer)
                    (cons (section (if (eq? context 'global) "Global keys" (format "~a keys" context))
                                   (append (if (and prompting? (eq? context 'global))
                                               (context-groups context nearer #f (lambda (b) (prompt:allowed? (keymap:binding-action b))) #f)
                                               (if (and widget? (not prompting?))
                                                 (context-groups context '() (and (eq? context 'global) read-only?)
                                                   (lambda (binding) (reachable? b context binding))
                                                   (lambda (binding) (describe-binding b context binding)))
                                                 (context-groups context nearer (and (not prompting?) read-only?) #f #f)))
                                           (capture-note context))
                                   width)
                          out)))))))

  (define (heading? line)
    ;; a section title: a line that is not a row, rows starting with two spaces
    (and (> (string-length line) 0) (not (char=? (string-ref line 0) #\space))))

  (define (styles line)
    ;; the section titles in bold, the grouping line in the margin faint
    (let ([v (make-vector (string-length line) (if (heading? line) 'bold 'plain))])
      (when (and (> (string-length line) 1) (memv (string-ref line 1) '(#\╷ #\│ #\╵)))
        (vector-set! v 1 'chrome))
      v))

  (define (row-styles b row line)
    (let* ([rows (head:buffer-fact b 'symbolic-spans '#())]
           [ranges (if (< row (vector-length rows)) (vector-ref rows row) '())])
      (and (pair? ranges)
        (let ([v (styles line)])
          (for-each (lambda (r)
                      (do ([i (car r) (+ i 1)]) ((>= i (min (cdr r) (vector-length v))))
                        (vector-set! v i 'italic))) ranges)
          v))))

  ;;; The buffer in the pop-up ------------------------------------------------------------

  (define (view-windows)
    ;; the windows showing the listing, the pop-up first when it does
    (let ([ws (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows))])
      (if (memq (head:popup) ws) (cons (head:popup) (remq (head:popup) ws)) ws)))

  (define (showing?) (and view (memq view (head:buffers)) (pair? (view-windows)) #t))

  (define (describable? w)
    ;; a window the listing can describe: one showing something other than
    ;; the listing, the pop-up only while it shows
    (and w (not (eq? (head:window-buffer w) view))
         (not (and (head:popup? w) (= (head:popup-rows) 0)))))

  (define (subject)
    ;; the buffer whose keys the listing describes: the current window's,
    ;; unless it shows the listing; the pop-up's then is the buffer the
    ;; listing took over there, an app's say, since the user is in that app;
    ;; else the buffer of the window selected before; none otherwise
    (let* ([w (head:current-window)] [p (head:previous-window)]
           [taken (and (head:popup? w) (assq w over))]
           [under (and taken (memq (cdr taken) (head:buffers)) (cdr taken))])
      (cond [(describable? w) (head:window-buffer w)]
            [under under]
            [(describable? p) (head:window-buffer p)]
            [else #f])))

  (define (listing-width)
    ;; the narrowest window showing the listing, a cell short of its edge so
    ;; no line wraps; the screen's width before any shows it, or while the
    ;; windows are not yet tiled and report no width to speak of
    (let* ([ws (view-windows)]
           [narrowest (if (null? ws) 0 (apply min (map head:window-content-width ws)))])
      ;; the screen's width less the listing's scrollbar column
      (- (if (> narrowest 40) narrowest (- (paint:screen-cols) 1)) 1)))

  (define (pointer-bindings)
    (let ([at (mouse:position)])
      (if (and at (head:window-at (- (car at) 1) (- (cdr at) 1)
                    (lambda (entry) (eq? (head:window-buffer (car entry)) view))))
          listed-pointer
          (mouse:bindings))))

  (define (situation b pointer)
    ;; what the listing depends on: the buffer, its contexts, its text being
    ;; read-only, an open prompt with its content's context, and the width
    ;; it is laid out for, which a resize of the terminal changes; the width
    ;; comes last, so the rest compares on its own
    (let ([root (head:buffer-fact b 'widget-id #f)])
      (list b (list (contexts b "") (keymap:generation) (and root (cadr (widget:key-scopes root ""))))
        (read-only-text? b) (prompt:active?) (prompt-context)
        (map (lambda (binding) (list (car binding) (action-basis (cadr binding)))) pointer)
        (if (and root (not (prompt:active?))) (widget:command-bindings root) '())
        (listing-width))))

  (define (action-basis action)
    (if (keymap:call-action? action)
      (cons (keymap:call-action-procedure action) (map action-basis (keymap:call-action-arguments action))) action))

  (define (command-template procedure arguments)
    ;; Fixed arguments are expressions; remaining formal names are supplied
    ;; by the invoking control, not invented values or a runnable nullary call.
    (define (formal name)
      (let ([text (string-copy (symbol->string name))]) (marked text (list (cons 0 (string-length text))))))
    (let* ([text (keymap:action-text (keymap:call (apply procedure arguments)))]
           [sigs (edoc:edoc-of procedure)] [sig (and sigs (find (lambda (s) (eq? (edoc:signature-kind s) 'procedure)) sigs))]
           [remaining (if sig
                        (let skip ([f (edoc:signature-formals sig)] [n (length arguments)])
                          (if (and (> n 0) (pair? f)) (skip (cdr f) (- n 1)) f)) 'arguments)]
           [tail (let spell ([f remaining])
                   (cond [(null? f) ""] [(pair? f) (join " " (formal (car f)) (spell (cdr f)))]
                     [else (join " . " (formal f))]))])
      (join (substring text 0 (- (string-length text) 1)) tail ")")))

  (define (command-sections bindings width)
    (let ([basis (list bindings width)])
      (unless (and commands-cache (equal? (car commands-cache) basis))
        (set! commands-cache
          (cons basis
            (if (null? bindings) '()
              (cons "Widget commands"
                (apply append
                  (map (lambda (row)
                         (section
                           (format "~a (~a): ~a"
                             (if (null? (cadr row)) "root" (string:join (map symbol->string (cadr row)) "/"))
                             (caddr row) (edoc:type-spelling 'model (car row)))
                           (map (lambda (binding)
                                  (let* ([proc (list-ref binding 4)]
                                         [public? (and proc (not (string=? (keymap:action-text proc) "anonymous command")))])
                                    (list (list (symbol->string (car binding)))
                                      (if public? (command-template proc (cons (cadr binding) (cadddr binding)))
                                        (command-template (keymap:call-action-procedure (keymap:call widget:act!))
                                          (cons* (cadr binding) (caddr binding) (cadddr binding))))
                                      (string-append (if (list-ref binding 5) "" "Unavailable target. ")
                                        (if proc (summary-of proc) "Target action is not registered."))))) (cadddr row)) width)) bindings)))))))
      (cdr commands-cache)))

  (define (fill! b pointer now)
    ;; the listing for a buffer into the view: from the top for a new
    ;; keyboard context; keep the reader's place through pointer or width changes
    (let* ([width (listing-width)]
           [same? (and listed (equal? (list-head listed 5) (list-head now 5)))]
           [keyboard-key (append (list-head now 5) (list (list-ref now 6) width))]
           [keyboard (if (and keyboard-cache (equal? (car keyboard-cache) keyboard-key)) (cdr keyboard-cache) (listing b width))]
           [lines (append (section "Mouse bindings"
                            (map (lambda (binding)
                                   (list (list (mouse:gesture-text (car binding))) (trace (cadr binding)) "")) pointer) width)
                    keyboard (command-sections (list-ref now 6) width))]
           [lines (if (null? lines) (list "no bindings") lines)])
      (set! keyboard-cache (cons keyboard-key keyboard))
      (set! listed now)
      (set! listed-pointer pointer)
      (head:view-replace! view lines (list (cons 'symbolic-spans (list->vector (map spans lines)))))
      (unless same?
        (for-each (lambda (w)
                    (head:window-top-set! w 0) (head:window-topseg-set! w 0)
                    (head:window-prow-set! w 0) (head:window-pcol-set! w 0))
                  (view-windows)))))

  (define (refresh! b)
    (let* ([pointer (pointer-bindings)] [now (situation b pointer)])
      (and (not (equal? listed now))
           (begin (fill! b pointer now) #t))))

  (define (ensure-view!)
    ;; the <bindings> buffer, made fresh when none is live
    (unless (and view (memq view (head:buffers)))
      (sweep!)
      (set! view (head:new-local-buffer! "bindings"))
      ;; transient: a checkpoint keeps no listing, so a restart brings none back
      (head:buffer-fact-set! view 'resume-kind 'bindings)
      ;; long, and read by position: a scrollbar on the configured side
      (head:buffer-fact-set! view 'scrollbar #t)
      (head:set-buffer-status! view status)
      (head:register-view! view void)
      (mode:choose! "bindings" view)))

  (define (drop-view!)
    (when (and view (memq view (head:buffers))) (head:forget-buffer! view))
    (set! view #f)
    (set! keyboard-cache #f)
    (set! commands-cache #f)
    (set! listed-pointer '())
    (set! listed #f))

  (define (follow!)
    ;; the listing keeps to the active window's buffer and its mode; the view
    ;; goes once no window shows it, the pop-up cleared by its ↓ say
    (unless swept? (sweep!))
    (when (and view (memq view (head:buffers)))
      (if (null? (view-windows))
          (drop-view!)
          (let ([b (subject)])
            (when (and b (not (eq? b view))) (refresh! b))))))

  ;;; Pages -----------------------------------------------------------------------

  (define page-cache (make-weak-eq-hashtable)) ; window -> (key . starts), the last computation

  (define (page-rows w)
    ;; the text rows a window shows: the pop-up's count before it is tiled
    (max 1 (if (head:popup? w) (head:popup-rows) (head:window-size w))))

  (define (page-starts w)
    ;; the lines starting each page of the listing in a window, counted in
    ;; the window's visual rows so a wrapped line takes what it takes; kept
    ;; per window until the text, the rows, the wrapping or the width change
    (let* ([lines (head:buffer-lines view)] [rows (page-rows w)]
           [wrapped? (and (> (head:window-width w) 1) (paint:window-wrapped? w))]
           [key (list rows wrapped? (head:window-content-width w))]
           [hit (hashtable-ref page-cache w #f)])
      (if (and hit (eq? (caar hit) lines) (equal? (cdar hit) key))
          (cdr hit)
          (let ([starts
                 (let loop ([i 0] [used 0] [starts '()])
                   (cond
                     [(= i (vector-length lines)) (reverse (if (null? starts) '(0) starts))]
                     [else
                      (let ([take (if wrapped? (paint:line-segments w (vector-ref lines i)) 1)])
                        (if (or (= used 0) (<= (+ used take) rows))
                            (loop (+ i 1) (+ used take) (if (= used 0) (cons i starts) starts))
                            (loop i 0 starts)))]))])
            (hashtable-set! page-cache w (cons (cons lines key) starts))
            starts))))

  (define (page-index w starts)
    ;; the page a window is on: the one holding the line at the middle of
    ;; the window's rows, so the painter's margins, which shift the top a
    ;; few rows at either end of the text, leave the page as it was paged to
    (let* ([lines (head:buffer-lines view)] [n (vector-length lines)]
           [rows (page-rows w)]
           [wrapped? (and (> (head:window-width w) 1) (paint:window-wrapped? w))]
           [mid (let walk ([i (max 0 (min (head:window-top w) (- n 1)))] [used 0])
                  (let ([take (if wrapped? (paint:line-segments w (vector-ref lines i)) 1)])
                    (if (or (>= (+ i 1) n) (> (+ used take) (quotient rows 2)))
                        i
                        (walk (+ i 1) (+ used take)))))])
      (let loop ([starts starts] [i 0] [found 0])
        (cond [(null? starts) found]
              [(<= (car starts) mid) (loop (cdr starts) (+ i 1) i)]
              [else found]))))

  (define (page! direction)
    ;; the listing a page further where it shows, the pop-up first: down,
    ;; from the top again past the end; up, from the last page again past
    ;; the top; the pages as the status bar counts them, and point in the
    ;; middle of the page, where the painter's scroll margin leaves the top
    ;; where it was put
    (let* ([w (car (view-windows))] [starts (page-starts w)]
           [i (mod (+ (page-index w starts) direction) (length starts))]
           [top (list-ref starts i)]
           [end (if (< (+ i 1) (length starts)) (- (list-ref starts (+ i 1)) 1) (- (vector-length (head:buffer-lines view)) 1))])
      (head:window-top-set! w top)
      (head:window-prow-set! w (min end (+ top (quotient (page-rows w) 2))))
      (head:window-pcol-set! w 0)))

  (define (page-down!) (page! 1))

  (define (status b w)
    ;; the bar of a window showing the listing, after the buffer's name the
    ;; painter puts first: the page the window is on of how many; the paging
    ;; keys are in the listing itself, never in a message or a bar
    (let ([starts (page-starts w)])
      (format "page ~a of ~a" (+ 1 (page-index w starts)) (length starts))))

  (edoc "Inspect mouse, keyboard and widget command bindings in the read-only pop-up <bindings>, with their public APIs and documentation. Mouse bindings follow the pointer; keyboard and widget commands follow the active window. If already shown, page down, wrapping to the top past the end.")
  (define (show!)
    (cond
      [(showing?)
       ;; the situation changed under the listing, a prompt opened say: it
       ;; refills; unchanged, or with nothing else to describe, it pages
       (let ([b (subject)])
         (unless (and b (not (eq? b view)) (refresh! b)) (page-down!)))]
      [else
       (let ([b (or (subject) (head:window-buffer (head:current-window)))])
         (ensure-view!)
         (remember-over! (head:popup))
         (head:set-window-buffer! (head:popup) view)
         (refresh! b)
         (head:show-popup! (head:popup-default-rows)))]))

  (define (remember-over! w)
    ;; what a window shows before the listing takes it, for bindings:return!
    (unless (eq? (head:window-buffer w) view)
      (set! over (cons (cons w (head:window-buffer w)) (remp (lambda (e) (eq? (car e) w)) over)))))

  (edoc "Page the bindings listing up where it shows, from the last page again past the top; not shown, show it as C-x TAB does.")
  (define (page-up!)
    (if (showing?) (page! -1) (show!)))

  (edoc "Show the bindings listing in the current window as the read-only buffer <bindings>, for the buffer the window shows now; the listing follows the active window from then on, and C-x TAB and C-x S-TAB page it there.")
  (define (open!)
    (let ([b (head:current-buffer)])
      (ensure-view!)
      (remember-over! (head:current-window))
      (head:show-buffer! view)
      (unless (eq? b view) (refresh! b))))

  (edoc "Put the listing away from the current window and show what the window showed before it, the pop-up hiding when it showed nothing else; ESC and C-g in <bindings>.")
  (define (return!)
    (let* ([w (head:current-window)] [back (cond [(assq w over) => cdr] [else #f])])
      (unless (and view (eq? (head:window-buffer w) view)) (error 'bindings:return! "the current window shows no bindings listing"))
      (set! over (remp (lambda (e) (eq? (car e) w)) over))
      (cond
        [(and (head:popup? w) (or (not back) (not (memq back (head:buffers))) (eq? back (head:window-buffer (head:popup)))))
         (hide!)]
        [(and back (memq back (head:buffers))) (head:set-window-buffer! w back)]
        [else (hide!)])
      (when (and view (null? (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows)))) (drop-view!))))

  (edoc "Put the bindings listing away: the pop-up shows its placeholder again and hides, and a window showing the listing shows another buffer.")
  (define (hide!)
    (when (and view (eq? (head:window-buffer (head:popup)) view)) (head:hide-popup!))
    (set! over '())
    (drop-view!))

  (edoc "Install the binding inspector: its mode, C-x TAB and C-x S-TAB showing or paging the listing, the listing following the active window before every frame, and its exclusion from checkpoints.")
  (define (init!)
    (mode:register! "bindings" '() '() styles #f row-styles)
    (head:register-resume! 'bindings (lambda (b positions) (values #f positions)) (lambda args #f))
    (keymap:bind-default! "C-x TAB" show!)
    (keymap:bind-default! "C-x S-TAB" page-up!)
    (keymap:bind-default! 'bindings "ESC" return!)
    (keymap:bind-default! 'bindings "C-g" return!)
    ;; both work everywhere, inside a prompt too, where the listing is the prompt's keys
    (prompt:allow! show!)
    (prompt:allow! page-up!)
    (head:add-pre-redraw-hook! follow!)))
