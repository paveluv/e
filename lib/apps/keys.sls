;; keys.sls -- the key bindings helper: C-x TAB shows in the pop-up the
;; keys that work in the active window's buffer, its mode contexts'
;; bindings first, an app's own keys among them, then the global ones; keys running one command share a row,
;; with the command and its description beside them, the description
;; wrapped in its column.  The listing is a local read-only buffer,
;; <keys>, browsed like any other; C-x TAB pages it down from anywhere,
;; and back to the top past the end, and it follows the active window.

(import (only (foundation edoc) elibrary))
(elibrary (apps keys)
  (export (rename (keys-hide! hide!)) init! (rename (keys-open! open!)) (rename (keys-page-up! page-up!)) (rename (keys-return! return!))
          (rename (keys-show! show!)))
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

  (define view #f) ; the <keys> buffer while it is shown
  (define over '()) ; ((window . buffer) ...) what a window showed before the listing took it
  (define listed #f) ; (buffer contexts read-only?) the listing describes
  (define swept? #f) ; whether the listings an older checkpoint restored have been dropped
  (define keyboard-cache #f)
  (define section-cache '()) ; context -> (derived calls . rendered lines)
  (define description-cache (make-hashtable equal-hash equal?)) ; (context sequence) -> (call command summary)
  (define derived-cache #f) ; routing basis -> (context binding receiver) declarations
  (define commands-cache #f)
  (define listed-pointer '()) ; the last inspected target, retained while browsing this help

  (define (stale-listing? b)
    ;; a <keys> or <keys 2> local buffer that is not the view: one an older
    ;; checkpoint brought back as text
    (let ([name (head:buffer-name b)])
      (and (not (eq? b view)) (not (head:buffer-store-id b))
           (string:prefix? "<keys" name) (string:suffix? ">" name))))

  (define (sweep!)
    ;; the listings an older checkpoint restored go; the view is made fresh
    (for-each (lambda (b) (when (stale-listing? b) (head:forget-buffer! b))) (head:buffers))
    (set! swept? #t))

  ;;; The rows ------------------------------------------------------------------------

  (define (edits? action)
    ;; whether the command declares (edits): refused where the text is read-only
    (let* ([proc (keymap:action-procedure action)] [sigs (and proc (edoc:edoc-of proc))])
      (and (pair? sigs)
           (exists (lambda (f) (and (pair? f) (eq? (car f) 'edits))) (edoc:signature-flags (car sigs))))))

  (define (summary-of action)
    ;; what the procedure a key action runs does, from its documentation
    (let* ([proc (keymap:action-procedure action)] [sigs (and proc (edoc:edoc-of proc))]
           [text (if (pair? sigs) (edoc:signature-summary (car sigs)) "")]
           [reason (keymap:action-reason action)])
      (if reason (string-append "Unavailable: " reason ". " text) text)))

  (define (shadowed? sequence nearer)
    ;; whether a nearer context binds the sequence, or a prefix of it, so
    ;; the key never reaches this binding
    (exists (lambda (context)
              (exists (lambda (n) (keymap:resolved-binding context (list-head sequence n)))
                      (map (lambda (i) (+ i 1)) (iota (length sequence)))))
            nearer))

  (define (context-groups context nearer read-only? keep receiver)
    ;; keep, when given, admits a binding: the commands allowed in a
    ;; prompt for the global section while one is open
    ;; (keys command description) for a context's bindings that work here:
    ;; not shadowed by a nearer context, and not editing where the text is
    ;; read-only; the keys running one command together, groups by their
    ;; first key; a lambda shows as the anonymous command it is, a name
    ;; being owed
    (define (add key command description groups)
      (let ([hit (find (lambda (g) (string=? (cadr g) command)) groups)])
        (if hit
            (map (lambda (g) (if (eq? g hit) (cons (cons key (car g)) (cdr g)) g)) groups)
            (cons (list (list key) command description) groups))))
    (define (description binding id action)
      (let* ([key (list context (keymap:binding-sequence binding))] [basis (list id (action-basis action))]
             [old (hashtable-ref description-cache key #f)])
        (if (and old (equal? (car old) basis)) (cdr old)
          (let ([text (list (keymap:action-text action (list (cons widget:target id))) (summary-of action))])
            (hashtable-set! description-cache key (cons basis text)) text))))
    (let loop ([owned (keymap:context-bindings context)] [groups '()])
      (if (null? owned)
          (list-sort (lambda (a b) (string<? (car (car a)) (car (car b))))
                     (map (lambda (g) (cons (list-sort string<? (car g)) (cdr g))) groups))
          (let* ([b (cdr (car owned))]
                 [admit? (and (not (shadowed? (keymap:binding-sequence b) nearer)) (or (not keep) (keep b)))]
                 [id (and admit? (receiver b))]
                 [action (and admit? (keymap:binding-action b id))]
                 [text (and action (description b id action))])
            (loop (cdr owned)
                  (if (and text
                           (not (and read-only? (edits? action))))
                      (add (keymap:sequence-text (keymap:binding-sequence b)) (car text) (cadr text) groups)
                      groups))))))

  ;;; The text -------------------------------------------------------------------------

  (define (cells s) (glyph:cells s))

  (define (pad s width)
    (if (>= (cells s) width) s (string-append s (make-string (- width (cells s)) #\space))))

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
                  (loop (substring word n (string-length word)) (cons (substring word 0 n) out))
                  (cut (- n 1)))))))
    (let loop ([words (apply append (map chop (filter (lambda (w) (> (string-length w) 0)) (split-words text))))]
               [line ""] [out '()])
      (cond
        [(null? words) (reverse (if (string=? line "") out (cons line out)))]
        [(string=? line "") (loop (cdr words) (car words) out)]
        [(<= (+ (cells line) 1 (cells (car words))) width)
         (loop (cdr words) (string-append line " " (car words)) out)]
        [else (loop words "" (cons line out))])))

  (define (split-words s)
    (let loop ([i 0] [start 0] [out '()])
      (cond
        [(= i (string-length s)) (reverse (cons (substring s start i) out))]
        [(char=? (string-ref s i) #\space) (loop (+ i 1) (+ i 1) (cons (substring s start i) out))]
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
    (if (null? groups)
        '()
        (let* ([key-width (apply max (map (lambda (g) (apply max (map cells (car g)))) groups))]
               ;; the key column takes what its widest key needs; of what is
               ;; left, the margin and separators apart, the command column
               ;; takes what its widest command needs while the description
               ;; keeps twenty-four cells, and shrinks to eight cells before
               ;; the description shrinks below that
               [room (- width key-width 6)]
               [command-width (min (apply max (map (lambda (g) (cells (cadr g))) groups)) (max 8 (- room 24)))]
               [text-width (max 8 (- room command-width))])
          (cons title
                (apply append
                  (map (lambda (g)
                         (let* ([keys (car g)] [command (wrap (cadr g) command-width)]
                                [text (wrap (caddr g) text-width)]
                                [height (max (length keys) (length command) (length text) 1)])
                           (let loop ([i 0] [out '()])
                             (if (= i height) (reverse out)
                                 (loop (+ i 1)
                                       (cons (string-append
                                               (bracket keys i) (pad (if (< i (length keys)) (list-ref keys i) "") key-width) "  "
                                               (pad (if (< i (length command)) (list-ref command i) "") command-width) "  "
                                               (if (< i (length text)) (list-ref text i) ""))
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

  (define (receiver b context binding)
    (let* ([root (head:buffer-fact b 'widget-id #f)]
           [scope (and root (find (lambda (scope) (memq context (cadr scope)))
                              (cadr (widget:key-scopes root (car (keymap:binding-sequence binding))))))])
      (if scope (car scope) 'editor)))

  (define (derived-bindings b routing)
    ;; Only resolved values enter the cache key, never fresh check closures.
    ;; This runs only while Keys is shown, and resolvers cannot request data.
    (unless (and derived-cache (equal? (car derived-cache) routing))
      (set! derived-cache
        (cons routing
          (apply append
            (map (lambda (context)
                   (filter values
                     (map (lambda (owned)
                            (let* ([binding (cdr owned)] [raw (keymap:binding-action binding)])
                              (and (reachable? b context binding)
                                (let* ([id (receiver b context binding)] [action (keymap:binding-action binding id)])
                                  (and (not (eq? raw action))
                                    (list context binding id))))))
                       (keymap:context-bindings context)))) (contexts b ""))))))
    (map (lambda (entry)
           (list (car entry) (keymap:binding-sequence (cadr entry))
             (action-basis (keymap:binding-action (cadr entry) (caddr entry))))) (cdr derived-cache)))

  (define (context-section context derived render)
    ;; A changed selection only invalidates its own context's help. In
    ;; particular, do not rediscover and format all global keys per row.
    (let* ([basis (filter (lambda (entry) (eq? (car entry) context)) derived)]
           [cached (assq context section-cache)])
      (if (and cached (equal? (cadr cached) basis)) (cddr cached)
        (let ([lines (render)])
          (set! section-cache (cons (cons* context basis lines) (remq cached section-cache))) lines))))

  (define (listing b width derived)
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
                    (cons (context-section context derived
                            (lambda () (section (if (eq? context 'global) "Global keys" (format "~a keys" context))
                                         (append (if (and prompting? (eq? context 'global))
                                                   (context-groups context nearer #f (lambda (b) (prompt:allowed? (keymap:binding-action b))) (lambda (b) 'editor))
                                                   (if (and widget? (not prompting?))
                                                     (context-groups context '() (and (eq? context 'global) read-only?)
                                                       (lambda (binding) (reachable? b context binding)) (lambda (binding) (receiver b context binding)))
                                                     (context-groups context nearer (and (not prompting?) read-only?) #f (lambda (b) 'editor))))
                                           (capture-note context))
                                         width)))
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
    (let* ([root (head:buffer-fact b 'widget-id #f)] [scopes (and root (widget:key-scopes root ""))]
           [routing (list (contexts b "") (keymap:generation) (and scopes (cadr scopes)))])
      (list b routing
        (read-only-text? b) (prompt:active?) (prompt-context)
        (if (prompt:active?) '() (derived-bindings b (list b routing (and scopes (car scopes)))))
        (map (lambda (binding) (list (car binding) (action-basis (cadr binding)))) pointer)
        (if (and root (not (prompt:active?))) (widget:command-bindings root) '())
        (listing-width))))

  (define (action-basis action)
    (if (keymap:call-action? action)
      (list (keymap:call-action-procedure action) (keymap:action-reason action)
        (map action-basis (keymap:call-action-arguments action))) action))

  (define (command-template procedure arguments)
    ;; Fixed arguments are expressions; remaining formal names are supplied
    ;; by the invoking control, not invented values or a runnable nullary call.
    (let* ([text (keymap:action-text (apply keymap:call procedure arguments))]
           [sigs (edoc:edoc-of procedure)] [sig (and sigs (find (lambda (s) (eq? (edoc:signature-kind s) 'procedure)) sigs))]
           [remaining (if sig
                        (let skip ([f (edoc:signature-formals sig)] [n (length arguments)])
                          (if (and (> n 0) (pair? f)) (skip (cdr f) (- n 1)) f)) 'arguments)]
           [tail (let spell ([f remaining])
                   (cond [(null? f) ""] [(pair? f) (string-append " " (symbol->string (car f)) (spell (cdr f)))]
                     [else (format " . ~a" f)]))])
      (string-append (substring text 0 (- (string-length text) 1)) tail ")")))

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
                                        (command-template widget:act! (cons* (cadr binding) (caddr binding) (cadddr binding))))
                                      (string-append (if (list-ref binding 5) "" "Unavailable target. ")
                                        (if proc (summary-of proc) "Target action is not registered."))))) (cadddr row)) width)) bindings)))))))
      (cdr commands-cache)))

  (define (fill! b pointer now)
    ;; the listing for a buffer into the view: from the top for a new
    ;; keyboard context; keep the reader's place through pointer or width changes
    (let* ([width (listing-width)]
           [same? (and listed (equal? (list-head listed 5) (list-head now 5)))]
           [keyboard-key (append (list-head now 6) (list width))]
           [keyboard (begin
                       (unless same? (hashtable-clear! description-cache))
                       (unless (and same? (= (list-ref listed 8) width)) (set! section-cache '()))
                       (if (and keyboard-cache (equal? (car keyboard-cache) keyboard-key)) (cdr keyboard-cache)
                         (listing b width (list-ref now 5))))]
           [lines (append (section "Mouse bindings"
                            (map (lambda (binding)
                                   (list (list (mouse:gesture-text (car binding))) (keymap:action-text (cadr binding)) (summary-of (cadr binding)))) pointer) width)
                    keyboard (command-sections (list-ref now 7) width))]
           [lines (if (null? lines) (list "no keys") lines)])
      (set! keyboard-cache (cons keyboard-key keyboard))
      (set! listed now)
      (set! listed-pointer pointer)
      (head:view-replace! view lines)
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
    ;; the <keys> buffer, made fresh when none is live
    (unless (and view (memq view (head:buffers)))
      (sweep!)
      (set! view (head:new-local-buffer! "keys"))
      ;; transient: a checkpoint keeps no listing, so a restart brings none back
      (head:buffer-fact-set! view 'resume-kind 'keys)
      ;; long, and read by position: a scrollbar on the configured side
      (head:buffer-fact-set! view 'scrollbar #t)
      (head:set-buffer-status! view status)
      (head:register-view! view void)
      (mode:choose! "keys" view)))

  (define (drop-view!)
    (when (and view (memq view (head:buffers))) (head:forget-buffer! view))
    (set! view #f)
    (set! keyboard-cache #f)
    (set! section-cache '())
    (hashtable-clear! description-cache)
    (set! derived-cache #f)
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

  (edoc "Show the keys that work in the active window's buffer in the pop-up, window 0, as the read-only buffer <keys>: its mode contexts' bindings, an app's own keys among them, then the global ones, keys running one command sharing a row with the command and what it does; shown already, in the pop-up or a window, page it down there, and from the top again past the end. The listing follows the active window.")
  (define (keys-show!)
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
    ;; what a window shows before the listing takes it, for keys:return!
    (unless (eq? (head:window-buffer w) view)
      (set! over (cons (cons w (head:window-buffer w)) (remp (lambda (e) (eq? (car e) w)) over)))))

  (edoc "Page the keys listing up where it shows, from the last page again past the top; not shown, show it as C-x TAB does.")
  (define (keys-page-up!)
    (if (showing?) (page! -1) (keys-show!)))

  (edoc "Show the keys listing in the current window as the read-only buffer <keys>, for the buffer the window shows now; the listing follows the active window from then on, and C-x TAB and C-x S-TAB page it there.")
  (define (keys-open!)
    (let ([b (head:current-buffer)])
      (ensure-view!)
      (remember-over! (head:current-window))
      (head:show-buffer! view)
      (unless (eq? b view) (refresh! b))))

  (edoc "Put the listing away from the current window and show what the window showed before it, the pop-up hiding when it showed nothing else; ESC and C-g in <keys>.")
  (define (keys-return!)
    (let* ([w (head:current-window)] [back (cond [(assq w over) => cdr] [else #f])])
      (unless (and view (eq? (head:window-buffer w) view)) (error 'keys:return! "the current window shows no keys listing"))
      (set! over (remp (lambda (e) (eq? (car e) w)) over))
      (cond
        [(and (head:popup? w) (or (not back) (not (memq back (head:buffers))) (eq? back (head:window-buffer (head:popup)))))
         (keys-hide!)]
        [(and back (memq back (head:buffers))) (head:set-window-buffer! w back)]
        [else (keys-hide!)])
      (when (and view (null? (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows)))) (drop-view!))))

  (edoc "Put the key listing away: the pop-up shows its placeholder again and hides, and a window showing the listing shows another buffer.")
  (define (keys-hide!)
    (when (and view (eq? (head:window-buffer (head:popup)) view)) (head:hide-popup!))
    (set! over '())
    (drop-view!))

  (edoc "Install the keys helper: its mode, C-x TAB and C-x S-TAB showing or paging the listing, the listing following the active window before every frame, and its exclusion from checkpoints.")
  (define (init!)
    (mode:register! "keys" '() '() styles #f #f)
    (head:register-resume! 'keys (lambda (b positions) (values #f positions)) (lambda args #f))
    (keymap:bind-default! "C-x TAB" keys-show!)
    (keymap:bind-default! "C-x S-TAB" keys-page-up!)
    (keymap:bind-default! 'keys "ESC" keys-return!)
    (keymap:bind-default! 'keys "C-g" keys-return!)
    ;; both work everywhere, inside a prompt too, where the listing is the prompt's keys
    (prompt:allow! keys-show!)
    (prompt:allow! keys-page-up!)
    (head:add-pre-redraw-hook! follow!)))
