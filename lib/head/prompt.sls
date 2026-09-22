;; prompt.sls -- the prompt: the library (prompt).
;;
;; The head's modal input in the echo area -- Emacs's minibuffer.
;; (prompt:read! label ...) runs a line editor with the cursor parked
;; in the echo area: history, TAB completion with a candidate list
;; that borrows the current window (the <completions> view), a ghost
;; suggestion, multiline and reindenting variants for M-x, and the
;; global window commands a prompt may run without losing its input
;; (registered with allow!).  (prompt:key! question allowed) asks a
;; focused single-key question; confirm? is yes/no over it.
;;
;; An interaction owns C-g and the cursor (interaction): C-g cancels
;; the prompt as a key rather than interrupting the editor, and the
;; cursor follows the prompt, not a parked evaluation.  The prompt
;; writes the echo area's model (echo) and asks the painter for
;; frames; it reads keys from the head's pump.  Under (prompt:in-window
;; #t) the same editor draws into the current window instead: a local
;; view with the line on its bottom row and the candidate list paged
;; above it.  Exported names drop the module stem: (prompt:read! "Find
;; file: " file:complete), (prompt:confirm? "Really?"), (prompt:active?).

(import (only (foundation edoc) elibrary))
(elibrary (head prompt)
  (export (rename (prompt-active? active?)) allow! completion-highlight completion-kind completion-label confirm?
          content (rename (draft-input draft)) (rename (prompt-edge-motion edge-motion))
          (rename (prompt-ghost ghost)) (rename (prompt-in-window in-window))
          (rename (prompt-inspector inspector)) interaction (rename (query-key! key!)) line
          make-candidate make-completer (rename (make-content-view make-content))
          (rename (prompt-multiline multiline)) (rename (prompt! read!))
          (rename (prompt-reindent reindent)) transient (rename (validate-input validate)))
  (import (rnrs)
          (rnrs r5rs)
          (only (chezscheme)
                make-parameter parameterize box unbox set-box! format void
                make-weak-eq-hashtable make-list list-head iota
                current-time add-duration make-time time<?)
          (prefix (core kernel) kernel:)
          (prefix (foundation string) string:)
          (prefix (head echo) echo:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head style) style:)
          (prefix (sys glyph) glyph:)
          (prefix (sys tty) tty:))

  ;;; The echo area, as the prompt writes it --------------------------------------

  ;; The prompt drives the echo area's model directly; these identifier
  ;; macros keep its many writes readable.
  (define-syntax message
    (identifier-syntax [id (echo:text)] [(set! id v) (echo:set-text! v)]))
  (define-syntax message-ghost
    (identifier-syntax [id (echo:ghost)] [(set! id v) (echo:set-ghost! v)]))
  (define-syntax message-styles
    (identifier-syntax [id (echo:styles)] [(set! id v) (echo:set-styles! v)]))
  (define-syntax echo-cursor
    (identifier-syntax [id (echo:cursor)] [(set! id v) (echo:set-cursor! v)]))
  (define-syntax echo-indent
    (identifier-syntax [id (echo:indent)] [(set! id v) (echo:set-indent! v)]))
  (define-syntax echo-input-end
    (identifier-syntax [id (echo:input-end)] [(set! id v) (echo:set-input-end! v)]))
  (define-syntax echo-scroll
    (identifier-syntax [id (echo:scroll)] [(set! id v) (echo:set-scroll! v)]))
  (define-syntax echo-spans
    (identifier-syntax [id (echo:spans)] [(set! id v) (echo:set-spans! v)]))

  ;;; Interaction -------------------------------------------------------------------

  (edoc "Run an interaction that owns C-g and the cursor: uninterrupted, the cursor following its rules rather than a parked evaluation's."
        (thunk thunk "the interaction")
        (returns any "what the thunk returns"))
  (define (interaction thunk)
    ;; An interaction owns C-g (head:call-uninterrupted) and the cursor:
    ;; while it runs, the cursor follows the interaction's rules, not a
    ;; parked evaluation's.
    (head:call-uninterrupted
      (lambda () (parameterize ([paint:cursor-in-echo #f]) (thunk)))))

  ;;; Commands a prompt may run ------------------------------------------------------

  ;; The global commands a prompt runs without losing its input -- pure
  ;; window management, registered by whoever defines them -- each with
  ;; an optional prompt-safe stand-in whose string result is the note to
  ;; show (a command that would nest a prompt is refused with one).
  (define allowed-commands (kernel:make-registry))

  (edoc "Allow a global command to run from inside a prompt without losing its input, or a prompt-safe stand-in for it."
        (command procedure "the command")
        (stand-in (list-of procedure) "a replacement to run instead, at most one"))
  (define (allow! command . stand-in)
    (kernel:registry-add! allowed-commands
      (cons command (and (pair? stand-in) (car stand-in)))))

  ;;; Questions, completions, and the prompt ------------------------------------------

  (edoc "Ask a single-key question in the echo area and read one of the allowed characters; a marked option, m)erge, shows its letter bold. #f when cancelled."
        (question string "the question")
        (allowed string "the acceptable characters")
        (rest (list-of thunk) "a repaint to run before waiting, at most one")
        (returns (or char #f))
        (prompts))
  (define (query-key! question allowed . rest)
    ;; A focused single-key question. Decode complete terminal events so an
    ;; arrow's leading ESC cannot cancel the question and leave its remaining
    ;; bytes to move point. Callers mark an option as m)erge internally; the
    ;; marker is removed for display and its first letter uses the bold choice
    ;; face. This keeps option structure separate from presentation.
    (define (render-question text)
      (let loop ([i 0] [chars '()] [marked '()])
        (if (= i (string-length text))
            (let* ([out (list->string (reverse chars))]
                   [styles (make-vector (string-length out) 'plain)])
              (let mark ([flags (reverse marked)] [j 0])
                (unless (null? flags)
                  (when (car flags) (vector-set! styles j 'choice))
                  (mark (cdr flags) (+ j 1))))
              (cons out styles))
            (let* ([ch (string-ref text i)]
                   [marker? (and (< (+ i 2) (string-length text))
                                 (char=? (string-ref text (+ i 1)) #\))
                                 (char-alphabetic?
                                   (string-ref text (+ i 2)))
                                 (string:search allowed
                                   (string (char-downcase ch)) 0
                                   (string-length allowed)))])
              (loop (+ i (if marker? 2 1))
                    (cons ch chars) (cons marker? marked))))))
    ;; no input yet, no answer: a question asked before the terminal's
    ;; reader runs cancels instead of waiting forever
    (if (not (head:input-live?)) #f
      (let* ([rendered (render-question question)]
             [shown (string-append (car rendered) " ")]
             [shown-styles (let* ([source (cdr rendered)]
                                  [v (make-vector (string-length shown) 'plain)])
                             (let copy ([i 0])
                               (when (< i (vector-length source))
                                 (vector-set! v i (vector-ref source i))
                                 (copy (+ i 1))))
                             v)]
             [repaint (and (pair? rest) (car rest))])
        (define (repaint-extra!)
          (when repaint
            (repaint)
            (paint:place-cursor!)))
        (interaction
          (lambda ()
            (dynamic-wind
              (lambda ()
                (set! message shown)
                (set! message-ghost "")
                (set! message-styles (cons shown (lambda (_) shown-styles)))
                (set! echo-indent 0)
                (set! echo-input-end (string-length shown))
                (set! echo-cursor (string-length shown))
                (paint:redraw!)
                (repaint-extra!))
              (lambda ()
                (let wait ()
                  (let ([event (head:read-key-event #f)])
                    (cond [(eof-object? event) #f]
                      [(string=? event "C-g") #\alarm]
                      [(string=? event "ESC") #\esc]
                      [(tty:key-event-character event)
                       => (lambda (choice)
                            (if (string:search allowed
                                  (string (char-downcase choice))
                                  0 (string-length allowed))
                              choice
                              (begin
                                (paint:visual-bell!)
                                (repaint-extra!)
                                (wait))))]
                      [else
                       (paint:visual-bell!)
                       (repaint-extra!)
                       (wait)]))))
              (lambda ()
                (set! echo-cursor #f)
                (set! echo-indent #f)
                (set! echo-input-end #f)
                (set! message-styles #f)
                (set! message "")
                (set! message-ghost ""))))))))

  (define active-refresh (make-parameter #f))
  (define window-owner (make-parameter #f))

  (edoc "Whether a prompt is reading input now."
        (returns boolean))
  (define (prompt-active?)
    (or (and (active-refresh) #t) (and echo-cursor #t)))

  (edoc "Whether a prompt shows its completions and content in the pop-up window rather than the echo area."
        (value boolean))
  (define prompt-in-window (make-parameter #f))

  (edoc "How a completion value is labelled in the list: (label value) gives the shown text."
        (value procedure))
  (define completion-label (make-parameter (lambda (value) value)))

  (edoc "What a list procedure's completions are, for the status line of the list, \"12 matches of file\"; a completer's own kind takes precedence, and the prompt's label stem stands in."
        (value (or string #f)))
  (define completion-kind (make-parameter #f))

  (edoc "Which completion labels take the editor face: (highlight? label)."
        (value procedure))
  (define completion-highlight (make-parameter (lambda (label) #f)))
  ;; A cursor-aware source returns (values start end expansions candidates).
  ;; Expansions may be a thunk: resolve only for a new Tab normalization,
  ;; not when refreshing the live list or cycling already prepared results.
  ;; Candidates replace [start,end); #f start means no completable token.
  ;; Unlike a prefix completer, it normalizes on the first Tab and keeps its
  ;; candidate list live after the second. Existing list procedures stay simple.
  ;; An optional settle procedure, (settle text position), receives the input
  ;; after a sole match has been inserted and returns the (text . position)
  ;; to continue with: M-x closes forms and steps to the next argument. An
  ;; optional kind, (kind text position) or a constant string, names what
  ;; the completions are for the list's status line.
  (define-record-type (completer %make-completer completer?) (fields lookup settle kind))

  (edoc "A cursor-aware completer: (lookup text position) gives (values start end expansions candidates), start #f meaning no completable token; an optional settle step, (settle text position), gives the (text . position) to continue with after a sole match is inserted; an optional kind, (kind text position) or a string, names what the completions are for the list's status line."
        (lookup procedure "the completion source")
        (settle (or procedure #f) "the settle step")
        (kind (or procedure string #f) "what the completions are"))
  (define make-completer
    (case-lambda
      [(lookup)
       (%make-completer lookup #f #f)]
      [(lookup settle)
       (%make-completer lookup settle #f)]
      [(lookup settle kind)
       (%make-completer lookup settle kind)]))

  ;; A display label and its character styles are independent of the string
  ;; inserted on selection. The lookup result owns both, including during cycling.
  (edoc "A completion candidate."
        (value string "the text inserted on selection")
        (label string "the text shown in the list")
        (styles (or vector #f) "the label's styles"))
  (define-record-type candidate
    (fields value label styles))

  ;; A live completion view supplies its minimum height, renderer and key
  ;; handler. (render input window available-height page) returns styled lines
  ;; and a page count. Choices replace input or run an action returning new
  ;; input (#f leaves it alone, e.g. when sorting a column).
  (edoc "A live completion view below a prompt's input."
        (minimum-height integer "the rows it needs at least")
        (render procedure "(render input window available-height page) giving styled lines and a page count")
        (handle (or procedure #f) "(handle event) giving new input, or #f to leave it"))
  (define-record-type content-view
    (fields minimum-height render handle))

  (edoc "The content view a prompt in a window shows below its input, or #f."
        (value (or (record content-view) #f)))
  (define content (make-parameter #f))
  ;; A validator returns #f to accept, a short explanation to keep editing,
  ;; or (transient text) for an inline-only notice that expires after two
  ;; seconds. Its deadline travels with the note, so keys that retain it
  ;; cannot restart its lifetime. Editing discards it like any other note.
  (define-record-type (notice make-notice notice?)
    (fields text deadline))

  (edoc "A validation notice shown inline for two seconds, bracketed."
        (text string "the notice")
        (returns (record notice)))
  (define (transient text)
    (make-notice (string-append " [" text "]")
      (add-duration (current-time 'time-monotonic) (make-time 'time-duration 0 2))))

  ;; A draft box carries (input . cursor) across invocations.
  (edoc "The prompt's validator: (validate input) gives #f to accept, a note to keep editing, or a transient notice."
        (value (or procedure #f)))
  (define validate-input (make-parameter #f))

  (edoc "A box carrying (input . cursor) across invocations of a prompt, or #f."
        (value (or any #f)))
  (define draft-input (make-parameter #f))

  (edoc "The prompt's suggestion: (ghost input) gives the grey text after the input, or #f."
        (value procedure))
  (define prompt-ghost (make-parameter (lambda (s) #f)))

  (edoc "What M-. does in a prompt: (inspect input position), or #f for nothing."
        (value (or procedure #f)))
  (define prompt-inspector (make-parameter #f))

  (edoc "How M-RET and a paste insert a line break: (insert input position text) gives the new (input . position), or #f to insert none."
        (value (or procedure #f)))
  (define prompt-multiline (make-parameter #f))

  (edoc "What C-a and C-e do: (move action input position repeated?) gives the new position, or #f for the input's ends."
        (value (or procedure #f)))
  (define prompt-edge-motion (make-parameter #f))

  (edoc "How the input is reindented after an edit: (reindent input position) gives the new (input . position), or #f for none."
        (value (or procedure #f)))
  (define prompt-reindent (make-parameter #f))

  ;; Rows retain their source coordinates. The same mapping places the
  ;; cursor and handles mouse input after wrapping, paging or clipping.
  ;; input is a source interval; choices are (start end value [hover-face]) intervals.
  (define-record-type row (fields text styles input choices))

  (edoc "A row of a content view: its text, styles and (start end value) choices, hovered with a face."
        (text string "the row text")
        (styles (or vector #f) "its styles")
        (choices list "(start end value) intervals")
        (hover-face (list-of symbol) "the face of a hovered choice, at most one")
        (returns (record row)))
  (define (line text styles choices . hover-face)
    (make-row text styles #f
      (map (lambda (choice) (append choice (if (null? hover-face) '(hover) hover-face))) choices)))
  ;; Cache only styles and numeric choice spans: a row also owns its text,
  ;; which would keep the weak key alive after a prompt is dismissed.
  (define line-presentation (make-weak-eq-hashtable))
  (define (choice-at choices column)
    (find (lambda (entry) (<= (car entry) column (- (cadr entry) 1)))
      choices))
  (define prompt-modes
    (begin
      (head:add-pre-redraw-hook! (lambda () (when (active-refresh) ((active-refresh)))))
      (for-each
        (lambda (name)
          (mode:register! name '() '()
            ;; Presentation can change while the text stays equal (for example,
            ;; a different completion query underlines different characters).
            ;; Read the current row instead of the mode's text-only style cache.
            (lambda (line) #f) #f
            (lambda (buffer row line)
              (let ([info (hashtable-ref line-presentation line #f)])
                (and info (car info))))))
        '("prompt" "completions"))
      (paint:add-highlighter!
        (lambda ()
          (paint:hover-ranges
            (lambda (w row column)
              (and (active-refresh) (or (not (window-owner)) (eq? w (window-owner)))
                   (let* ([line (vector-ref (head:buffer-lines (head:window-buffer w)) row)]
                          [info (hashtable-ref line-presentation line #f)]
                          [choice (and info (choice-at (cdr info) column))])
                     ;; Labels leave their padding plain; row candidates tint it too.
                     (and choice
                          (let trim ([end (cadr choice)])
                            (if (and (eq? (caddr choice) 'hover) (> end (car choice))
                                     (char-whitespace? (string-ref line (- end 1))))
                                (trim (- end 1)) (list (car choice) end (caddr choice))))))))
            caddr)))))

  (define (format-rows candidates width)
    ;; One candidate per row. A label made of the value, two spaces and a
    ;; hint wraps at word boundaries with its continuation rows indented to
    ;; the hint; any other label wraps from the margin. Every row of a
    ;; candidate chooses it.
    (define (slice styles from to)
      (let ([out (make-vector (- to from) 'plain)])
        (do ([i from (+ i 1)]) ((= i to) out)
          (when (< i (vector-length styles)) (vector-set! out (- i from) (vector-ref styles i))))))
    (list->vector
      (apply append
        (map (lambda (value)
               (let* ([rich? (candidate? value)]
                      [chosen (if rich? (candidate-value value) value)]
                      [label (if rich? (candidate-label value) value)]
                      [styles (if rich? (candidate-styles value) (make-vector (string-length label) 'plain))]
                      [head (+ (string-length chosen) 2)]
                      [indent (if (and (< (* 2 head) width) (> (string-length label) head)
                                       (string=? (substring label 0 head) (string-append chosen "  ")))
                                  head 0)]
                      [tail (substring label indent (string-length label))]
                      [breaks (paint:compute-breaks tail (max 1 (- width indent)))]
                      [count (vector-length breaks)])
                 (map (lambda (i)
                        (let* ([from (vector-ref breaks i)]
                               [to (if (= (+ i 1) count) (string-length tail) (vector-ref breaks (+ i 1)))]
                               [segment (substring tail from to)]
                               [text (if (= i 0)
                                         (string-append (substring label 0 indent) segment)
                                         (string-append (make-string indent #\space) segment))]
                               [faces (if (= i 0)
                                          (slice styles 0 (+ indent to))
                                          (list->vector
                                            (append (make-list indent 'plain)
                                                    (vector->list (slice styles (+ indent from) (+ indent to))))))])
                          (make-row text faces #f (list (list 0 (string-length text) chosen)))))
                      (iota count))))
             candidates))))

  (define (format-columns candidates width labeler highlight?)
    ;; Labelled candidates take a row each; plain strings fill columns.
    (if (exists candidate? candidates)
        (format-rows candidates width)
        (format-grid candidates width labeler highlight?)))

  (define (format-grid candidates width labeler highlight?)
    (let* ([labels (map (lambda (value) (if (candidate? value) (candidate-label value) (labeler value)))
                     candidates)]
           [column (min width (+ 2 (fold-left max 0 (map glyph:cells labels))))]
           [columns (max 1 (div width (max 1 column)))])
      (let rows ([values candidates] [labels labels] [out '()])
        (if (null? values) (list->vector (reverse out))
            (let fill ([values values] [labels labels] [count 0]
                       [text ""] [styles '()] [choices '()])
              (if (or (= count columns) (null? values))
                  (rows values labels
                    (cons (make-row text (list->vector (apply append (reverse styles)))
                            #f (reverse choices)) out))
                  (let* ([label (car labels)] [shown (glyph:fit label column)]
                         [value (car values)]
                         [base (if (candidate? value) 'plain (if (highlight? label) 'editor 'plain))]
                         [faces (make-vector (string-length shown) base)]
                         [start (string-length text)] [end (+ start (string-length shown))])
                    (when (candidate? value)
                      ;; fit preserves a prefix of whole glyph clusters. Stop
                      ;; copying at its ellipsis/padding so neither is underlined.
                      (let ([visible
                             (if (<= (glyph:cells label) column) (string-length label)
                                 (let trim ([i (- (string-length shown) 1)])
                                   (if (char=? (string-ref shown i) #\space) (trim (- i 1)) i)))])
                        (do ([i 0 (+ i 1)]) ((= i visible))
                          (vector-set! faces i (vector-ref (candidate-styles value) i)))))
                    (fill (cdr values) (cdr labels) (+ count 1)
                      (string-append text shown)
                      (cons (vector->list faces) styles)
                      (cons (list start end (if (candidate? value) (candidate-value value) value))
                        choices)))))))))

  (define (input-rows content styles width)
    ;; Prewrap through the normal cell/cluster geometry, then render a
    ;; bounded slice without a second viewport competing for the cursor.
    (let logical ([start 0] [out '()])
      (let* ([end (or (string:search content "\n" start (string-length content))
                      (string-length content))]
             [line (substring content start end)]
             [breaks (paint:compute-breaks line (max 1 (- width 1)))]
             [count (vector-length breaks)])
        (let visual ([i 0] [out out])
          (if (= i count)
              (if (= end (string-length content)) (reverse out)
                  (logical (+ end 1) out))
              (let* ([from (+ start (vector-ref breaks i))]
                     [to (if (= (+ i 1) count) end (+ start (vector-ref breaks (+ i 1))))]
                     [text (substring content from to)]
                     [shown (if (= (+ i 1) count) text
                                (string-append (glyph:fit text (max 1 (- width 1))) "\\"))]
                     [face (make-vector (string-length shown) 'chrome)])
                (do ([j 0 (+ j 1)]) ((= j (min (- to from) (vector-length face))))
                  (vector-set! face j (vector-ref styles (+ from j))))
                (visual (+ i 1) (cons (make-row shown face (cons from to) '()) out))))))))

  (define (label-stem label)
    (let* ([end (let loop ([i 0])
                  (cond [(= i (string-length label)) i]
                        [(memv (string-ref label i) '(#\: #\()) i]
                        [else (loop (+ i 1))]))]
           [end (let trim ([i end])
                  (if (and (> i 0) (char-whitespace? (string-ref label (- i 1))))
                      (trim (- i 1)) i))])
      (if (= end 0) "prompt"
          (list->string
            (map (lambda (c) (if (char-whitespace? c) #\- (char-downcase c)))
                 (string->list (substring label 0 end)))))))

  (define (prompt-window-command event)
    ;; Resolve entire chords so their tail cannot leak into the input.
    (define (action-thunk action)
      (let ([allowed (assq action (kernel:registry-items allowed-commands))])
        (and allowed
             (lambda ()
               (guard (ex [else (string-append "  " (kernel:condition-text ex))])
                 (if (cdr allowed) ((cdr allowed)) (begin (action) "")))))))
    (and (not (tty:key-event-character event))
         (let loop ([sequence (list event)])
           (cond
             [(keymap:binding-prefix? 'global sequence)
              (let ([next (head:read-key-event #f)])
                (if (eof-object? next) (lambda () "")
                    (loop (append sequence (list next)))))]
             [(keymap:resolved-binding 'global sequence)
              => (lambda (hit)
                   (or (action-thunk (keymap:binding-action (cdr hit)))
                       (and (> (length sequence) 1) (lambda () ""))))]
             [(> (length sequence) 1) (lambda () "")]
             [else #f]))))

  (edoc "Read a line of input in the echo area with editing, history and completion; #f when cancelled."
        (label string "the prompt text")
        (rest (list-of any) "in order, each optional: a completer or completion procedure, the initial input, a history box, an alternate completer and a normalizer")
        (returns (or string #f))
        (prompts))
  (define (prompt! label . rest)
    ;; Each call owns its input, candidates and temporary view. A nested
    ;; call can borrow the echo area without changing its parent's state.
    (define (optional n)
      (if (< n (length rest)) (list-ref rest n) #f))
    (define complete (optional 0))
    (define initial (or (optional 1) ""))
    (define history (optional 2))
    (define alt-complete (optional 3))
    (define normalize (optional 4))
    (define validator (validate-input))
    (define draft (draft-input))
    (define labeler (completion-label))
    (define kind (completion-kind))
    (define highlight? (completion-highlight))
    (define styler (paint:echo-highlight))
    (define ghost (prompt-ghost))
    (define in-window? (and (prompt-in-window) (not (window-owner))))
    (define body (and in-window? (content)))
    (define owner (head:current-window))
    (define input (if (and draft (unbox draft)) (car (unbox draft)) initial))
    (define position (if (and draft (unbox draft)) (cdr (unbox draft)) (string-length input)))
    (define note "")
    (define hist-pos -1)
    (define stash "")
    (define last-edge #f)
    (define completion-source #f)
    (define completion-range #f)
    (define prepared #f)
    (define completion-options '())
    (define completion-matches '())
    (define option-index 0)
    (define candidates #f)
    (define candidate-rows '#())
    (define candidate-width 0)
    (define page 0)
    (define pages 1)
    (define view #f)
    (define target #f)
    (define previous #f)
    (define borrowed '())
    (define shown-rows '#())
    (define clicked #f)
    (define validation-message #f)

    (define (view-windows)
      (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows)))
    (define (popup-height values)
      ;; Rows for the candidate columns at the pop-up's width, at most half
      ;; the screen; the layout keeps the windows above their minimum.
      (let ([width (max 1 (if (> (head:window-width (head:popup)) 1)
                              (head:window-content-width (head:popup))
                              (paint:screen-cols)))])
        (max 1 (min (quotient (paint:screen-rows) 2)
                    (vector-length
                      (format-columns values width (if completion-source (lambda (s) s) labeler) highlight?))))))
    (define (release-view!)
      (when view
        (let ([gone? (not (memq view (head:buffers)))]
              [fallback (if (memq previous (head:buffers)) previous
                            (or (find (lambda (b) (not (eq? b view))) (head:buffers))
                                (head:new-buffer! "*scratch*")))])
          (head:call-with-display-update
            (lambda ()
              (for-each
                (lambda (w)
                  (when (and (not (head:popup? w))
                             (or (eq? (head:window-buffer w) view)
                                 (and gone? (memq w borrowed))))
                    (head:set-window-buffer! w fallback)))
                (head:windows))
              ;; The pop-up hides again, unless an outer prompt's list is
              ;; waiting to come back into it.
              (when (head:popup? target)
                (if (memq previous (head:buffers))
                    (head:set-window-buffer! target previous)
                    (head:hide-popup!)))
              (head:forget-buffer! view))))
        (set! view #f) (set! target #f) (set! borrowed '())))
    (define (window-lost?)
      (and in-window?
           (or (not (memq owner (head:windows)))
               (not (eq? owner (head:current-window)))
               (not (eq? (head:window-buffer owner) view))
               (not (memq view (head:buffers))))))
    (define (kind-text)
      ;; what the completions are: the completer's own kind, else the
      ;; completion-kind parameter, else the prompt's label stem
      (let ([own (and (completer? completion-source) (completer-kind completion-source))])
        (cond [(procedure? own) (or (guard (ex [else #f]) (own input position)) (label-stem label))]
              [(string? own) own]
              [kind kind]
              [else (label-stem label)])))
    (define (status-text b)
      ;; the list's status line: the buffer's name, then how many matches, of
      ;; what, and the page when they take several -- never key hints; the
      ;; name yields to the count in a narrow window
      (let* ([name (head:buffer-name b)]
             [text (cond
                     [candidates
                      (let* ([count (length candidates)]
                             [head (format "~a match~a of ~a" count (if (= count 1) "" "es") (kind-text))])
                        (if (> pages 1) (format "~a; page ~a of ~a" head (+ page 1) pages) head))]
                     [(> pages 1) (format "page ~a of ~a" (+ page 1) pages)]
                     [else #f])]
             [room (max 1 (- (head:window-width target) 12))])
        (cond [(not text) name]
              [(<= (+ (glyph:cells name) (glyph:cells text) 2) room) (string-append name "  " text)]
              [else text])))
    (define (mouse! event)
      (cond
        [(and (or body candidates) (member event '("WHEEL-UP" "WHEEL-DOWN")))
         (set! page (mod (+ page (if (string=? event "WHEEL-UP") -1 1)) pages)) #t]
        [(and (string=? event "MOUSE-CLICK")
              (or (not in-window?) (eq? (head:current-window) owner)))
         (let ([at (head:app-event-buffer-position)])
           (when (and at (<= 0 (car at)) (< (car at) (vector-length shown-rows)))
             (let* ([row (vector-ref shown-rows (car at))] [source (row-input row)]
                    [choice (choice-at (row-choices row) (cdr at))])
               (cond [source
                      (set! clicked
                        (cons input (min (string-length input)
                                      (max 0 (- (min (cdr source) (+ (car source) (cdr at)))
                                                (string-length label))))))]
                     [choice
                      (let ([value (if (procedure? (caddr choice)) ((caddr choice)) (caddr choice))])
                        (if value
                            (set! clicked (if completion-source (replace-completion input value)
                                            (cons value (string-length value))))
                            (set! page 0)))]))))
         (if in-window? #t 'keep-focus)]
        [else #f]))
    (define (take-view!)
      ;; A window prompt shows its list in its own window; every other
      ;; completion list opens the pop-up window above the echo area.
      (unless view
        (set! target (if in-window? owner (head:popup)))
        (set! previous (head:window-buffer target))
        (head:call-with-display-update
          (lambda ()
            (set! view
              (head:register-app!
                (head:new-local-buffer! (if in-window? (label-stem label) "completions"))
                render! mouse!))
            (mode:choose! (if in-window? "prompt" "completions") view)
            (head:set-app-presentation! view 0 #f #f (if in-window? 'text 'default))
            (head:set-app-status-position! view status-text)
            (head:set-app-manages-viewport! view #t)
            (when body (head:set-app-selectable! view #f))
            (head:set-window-buffer! target view)
            (set! borrowed (list target))
            (unless in-window? (head:show-popup! (popup-height (or candidates '()))))))))
    (define (dismiss-completions!)
      (set! completion-source #f) (set! completion-range #f) (set! prepared #f)
      (set! completion-options '()) (set! completion-matches '()) (set! option-index 0)
      (set! candidates #f) (set! pages 1) (set! page 0)
      (unless in-window? (release-view!)))
    (define (replace-completion s value)
      (cons (string-append (substring s 0 (car completion-range)) value
                           (string:tail s (cdr completion-range)))
            (+ (car completion-range) (string-length value))))
    (define (prepared? completer s pos)
      (and prepared (eq? completer (car prepared))
           (string=? s (cadr prepared)) (= pos (caddr prepared))))
    (define (set-candidates! values)
      (unless (equal? values candidates)
        (set! candidates values) (set! candidate-width 0) (set! page 0)
        (when (and view (head:popup? target) (pair? values))
          (head:show-popup! (popup-height values)))))
    (define (invalidate-input! new-s new-pos)
      (unless (and (string=? new-s input) (= new-pos position))
        (unless (prepared? completion-source new-s new-pos) (set! prepared #f))
        (if completion-source
            (let-values ([(start end expansion values) ((completer-lookup completion-source) new-s new-pos)])
              (if (and start (= start (car completion-range)))
                  (begin (set! completion-range (cons start end))
                         ;; A normalized or cycled spelling keeps its options,
                         ;; but the matches underline the symbol as it now reads.
                         (set! completion-matches values)
                         (when candidates (set-candidates! values)))
                  (dismiss-completions!)))
            (unless (string=? new-s input) (dismiss-completions!)))))
    (define (continue-or-end! completer s pos)
      ;; after a sole completion: the session goes on when the completer
      ;; still offers more than the token now at pos, one within what the
      ;; completion wrote -- a directory's entries, inside the literal the
      ;; completion opened, say -- and the list shows it; else the session ends
      (let-values ([(start end options values) ((completer-lookup completer) s pos)])
        (if (and start completion-range (<= (car completion-range) start pos) (pair? values)
                 (not (and (null? (cdr values))
                           (string=? (if (candidate? (car values)) (candidate-value (car values)) (car values))
                                     (substring s start end)))))
            (begin
              (set! completion-range (cons start end))
              (set! completion-matches values)
              (set-candidates! values)
              (take-view!))
            (dismiss-completions!))))
    (define (show-completions! values)
      (if (equal? values candidates)
          (set! page (mod (+ page 1) (max 1 pages)))
          (set-candidates! values))
      (take-view!))
    (define (page-rows width available)
      (cond [body
             (let-values ([(lines count) ((content-view-render body) input target available page)])
               (set! pages (max 1 count)) (set! page (mod page pages)) lines)]
            [(not candidates) '()]
            [else
             (unless (= width candidate-width)
               (set! candidate-rows
                 (format-columns candidates width (if completion-source (lambda (s) s) labeler) highlight?))
               (set! candidate-width width))
             (let* ([all (vector-length candidate-rows)] [size (max 1 available)])
               (set! pages (max 1 (div (+ all size -1) size)))
               (set! page (min page (- pages 1)))
               (let ([from (* page size)])
                 (if (= available 0) '()
                     (map (lambda (i) (vector-ref candidate-rows i))
                          (map (lambda (i) (+ from i)) (iota (min size (- all from))))))))]))
    (define (note-text)
      (cond [(not (notice? note)) note]
            [(time<? (current-time 'time-monotonic) (notice-deadline note))
             (head:request-frame-at! (notice-deadline note)) (notice-text note)]
            [else (set! note "") ""]))
    (define (render-echo!)
      (let ([shown-note (note-text)])
        (set! message (string-append label input))
        (set! echo-input-end (+ (string-length label) (string-length input)))
        (set! message-ghost (if (string=? shown-note "") (or (ghost input) "") shown-note))
        (set! echo-indent (string-length label))
        (set! echo-cursor (+ (string-length label) position))))
    (define (render!)
      (when (and view target (memq target (head:windows))
                 (eq? (head:window-buffer target) view))
        (set! borrowed (view-windows))
        (let* ([width (max 1 (head:window-content-width target))]
               [height (max 1 (head:window-size target))]
               [note (note-text)]
               [text (string-append label input note)]
               [tail (if (string=? note "") (or (ghost input) "") "")]
               [content (string-append text tail)]
               [end (+ (string-length label) (string-length input))]
               [cursor (+ (string-length label) position)]
               [styles (make-vector (string-length content) 'chrome)]
               [inner
                (let ([saved echo-input-end])
                  (dynamic-wind
                    (lambda () (set! echo-input-end end))
                    (lambda () (and styler (guard (ex [else #f]) (styler text))))
                    (lambda () (set! echo-input-end saved))))])
          (style:fill-range! styles end (string-length content) 'ghost)
          (do ([i (string-length label) (+ i 1)]) ((= i end))
            (vector-set! styles i
              (if (and inner (< i (vector-length inner))) (vector-ref inner i) 'plain)))
          (let* ([all (if in-window? (input-rows content styles width) '())]
                 [cursor-row
                  (let loop ([rows all] [i 0])
                    (if (or (null? rows) (null? (cdr rows))
                            (< cursor (car (row-input (cadr rows))))) i
                        (loop (cdr rows) (+ i 1))))]
                 [count (min (length all) (max 1 (- height (cond [body (content-view-minimum-height body)]
                                                             [candidates 1] [else 0]))))]
                 [from (min cursor-row (max 0 (- (length all) count)))]
                 [input-part (list-head (list-tail all from) count)]
                 [choices (page-rows width (- height count))]
                 [pad (if in-window? (max 0 (- height count (length choices))) 0)]
                 [rows (if body (append choices (make-list pad (make-row "" '#() #f '())) input-part)
                           (append (make-list pad (make-row "" '#() #f '())) choices input-part))]
                 [point (if in-window?
                            (cons (+ pad (length choices) (- cursor-row from))
                              (- cursor (car (row-input (list-ref all cursor-row))))) '(0 . 0))])
            (set! shown-rows (list->vector rows))
            (head:view-replace! view (map row-text rows) '()
              (list (cons target point) (cons (cons 'top target) '(0 . 0))))
            (let ([lines (head:buffer-lines view)])
              (do ([i 0 (+ i 1)]) ((= i (vector-length shown-rows)))
                (let ([row (vector-ref shown-rows i)])
                  (hashtable-set! line-presentation (vector-ref lines i)
                    (cons (row-styles row)
                          (map (lambda (choice) (list (car choice) (cadr choice)
                                                  (if (pair? (cdddr choice)) (cadddr choice) 'hover)))
                            (row-choices row)))))))))))

    (define (record-history! s)
      (when (and history (> (string-length s) 0))
        (let ([h (unbox history)])
          (unless (and (pair? h) (string=? (car h) s))
            (set-box! history (cons s h))))))
    (define (clear-validation!)
      (when (and validation-message (eq? (echo:text-owner) validation-message))
        (set! message ""))
      (set! validation-message #f))
    (define (run-prompt)
      (let loop ([s input] [pos position] [next-note ""])
        (when (and (not in-window?) view
                   (or (not (memq target (head:windows)))
                       (not (eq? (head:window-buffer target) view))))
          (dismiss-completions!))
        (invalidate-input! s pos)
        (set! input s) (set! position pos) (set! note next-note)
        (when draft (set-box! draft (cons s pos)))
        (if (window-lost?) #f
            (let ()
              (define len (string-length s))
              (define (edited new-s new-pos . completed)
                (set! hist-pos -1)
                (clear-validation!)
                (let* ([reindent (prompt-reindent)]
                       [result (if reindent
                                   (guard (ex [else (cons new-s new-pos)]) (reindent new-s new-pos))
                                   (cons new-s new-pos))])
                  (when (pair? completed)
                    (set! prepared (list (car completed) (car result) (cdr result))))
                  (loop (car result) (cdr result) "")))
              (define (history-show entry) (clear-validation!) (loop entry (string-length entry) ""))
              (define (history-up)
                (let ([h (if history (unbox history) '())])
                  (if (< (+ hist-pos 1) (length h))
                      (begin (when (= hist-pos -1) (set! stash s))
                             (set! hist-pos (+ hist-pos 1)) (history-show (list-ref h hist-pos)))
                      (loop s pos note))))
              (define (history-down)
                (cond [(= hist-pos -1) (loop s pos note)]
                      [(= hist-pos 0) (set! hist-pos -1) (history-show stash)]
                      [else (set! hist-pos (- hist-pos 1)) (history-show (list-ref (unbox history) hist-pos))]))
              (define (vertical-move delta)
                ;; Straight up or down on the screen, whatever each row's
                ;; indent is.
                (let* ([p (paint:echo-position echo-cursor)]
                       [k (paint:echo-index-at (+ (car p) delta) (cdr p))])
                  (loop s (min (max 0 (- k (string-length label))) len) note)))
              (define (complete-input completer)
                (set! hist-pos -1)
                (if (completer? completer)
                    (let-values ([(start end options values) ((completer-lookup completer) s pos)])
                      (cond
                        [(not start)
                         ;; nothing open at point: the datum before it, a closed
                         ;; string or form, is final, and Tab settles the input
                         ;; around it, closing complete forms and stepping to a due
                         ;; argument, whose candidates then show; else the note
                         (let ([settled (if (completer-settle completer) ((completer-settle completer) s pos) (cons s pos))])
                           (if (and (string=? (car settled) s) (= (cdr settled) pos))
                               (begin (dismiss-completions!) (loop s pos " [No symbol]"))
                               (begin
                                 (set! completion-source completer) (set! completion-range (cons pos pos))
                                 (continue-or-end! completer (car settled) (cdr settled))
                                 (edited (car settled) (cdr settled)))))]
                        [else
                         (set! completion-source completer) (set! completion-range (cons start end))
                         (cond
                           [(null? values)
                            (when candidates (set-candidates! values))
                            (loop s pos " [No match]")]
                           [(and (null? (cdr values)) (completer-settle completer))
                            ;; One match: insert it, close the list, and let the
                            ;; completer settle what follows the symbol.
                            (let* ([options (if (procedure? options) (options) options)]
                                   [value (car values)]
                                   [text (if (pair? options) (car options)
                                             (if (candidate? value) (candidate-value value) value))]
                                   [next (replace-completion s text)]
                                   [settled ((completer-settle completer) (car next) (cdr next))])
                              (if (and (string=? (car settled) s) (= (cdr settled) pos))
                                  (begin (dismiss-completions!) (loop s pos ""))
                                  (begin
                                    (continue-or-end! completer (car settled) (cdr settled))
                                    (edited (car settled) (cdr settled)))))]
                           [(prepared? completer s pos)
                            (if (null? (cdr completion-options))
                                (begin (show-completions! completion-matches) (loop s pos ""))
                                (begin
                                  (set! option-index (mod (+ option-index 1) (length completion-options)))
                                  (set-candidates! completion-matches) (take-view!)
                                  (let ([next (replace-completion s (list-ref completion-options option-index))])
                                    (edited (car next) (cdr next) completer))))]
                           [else
                            (set! completion-options (if (procedure? options) (options) options)) (set! option-index 0)
                            (set! completion-matches values)
                            (when candidates (set-candidates! values))
                            (let ([next (replace-completion s (car completion-options))])
                              (if (string=? (car next) s)
                                  (begin
                                    (set! prepared (list completer s (cdr next)))
                                    (loop s (cdr next) (if candidates ""
                                                           (format " [~a matches; Tab to list]" (length values)))))
                                  (edited (car next) (cdr next) completer)))])]))
                    (begin
                      (when completion-source (dismiss-completions!))
                      (let ([values (completer s)])
                        (cond [(null? values) (dismiss-completions!) (loop s pos " [No match]")]
                          [(null? (cdr values))
                           (dismiss-completions!)
                           (if (string=? (car values) s) (loop s len " [Sole completion]")
                             (edited (car values) (string-length (car values))))]
                          [else
                           (let ([prefix (string:common-prefix values)])
                             (if (> (string-length prefix) len) (edited prefix (string-length prefix))
                               (begin (show-completions! values) (loop s pos ""))))])))))
              (if in-window? (render!) (render-echo!))
              (paint:redraw!)
              (let* ([event (head:read-key-event #t)]
                     [action (and (not (eof-object? event)) (keymap:event-binding 'prompt event))]
                     [previous-edge last-edge])
                (set! last-edge #f)
                (cond
                  [(or (eof-object? event) (window-lost?)) #f]
                  [clicked
                   (let ([change clicked])
                     (set! clicked #f)
                     (when completion-source (dismiss-completions!))
                     (if (string=? (car change) s) (loop s (cdr change) "")
                         (edited (car change) (cdr change))))]
                  [(or (and (or body candidates) (member event '("PAGEUP" "PAGEDOWN")))
                       (and body (string=? event "S-TAB")))
                   (set! page (mod (+ page (if (member event '("PAGEUP" "S-TAB")) -1 1)) pages))
                   (loop s pos "")]
                  [(and body (content-view-handle body) ((content-view-handle body) event))
                   (set! page 0) (loop s pos "")]
                  [(eq? action 'cancel) (set! message "Quit") #f]
                  [(eq? action 'accept)
                   (let* ([out (if normalize (normalize s) s)]
                          [problem (and validator (validator out))])
                     (if problem
                         (begin
                           (clear-validation!)
                           (when (and in-window? (not (notice? problem)))
                             (set! validation-message (list 'validation))
                             (echo:set-text! problem validation-message))
                           (loop out (if (string=? out s) pos (string-length out))
                             (if (notice? problem) problem (string-append " [" problem "]"))))
                         (begin (record-history! out) (set! message "") out)))]
                  [(memq action '(beginning end))
                   (set! last-edge action)
                   (let ([move (prompt-edge-motion)])
                     (loop s (if move (move action s pos (eq? previous-edge action))
                                 (if (eq? action 'beginning) 0 len)) ""))]
                  [(eq? action 'backward) (loop s (max 0 (- pos 1)) "")]
                  [(eq? action 'forward) (loop s (min len (+ pos 1)) "")]
                  [(eq? action 'up)
                   (if (or in-window? (= (car (paint:echo-position echo-cursor)) 0))
                       (history-up) (vertical-move -1))]
                  [(eq? action 'down)
                   (if (or in-window? (= (car (paint:echo-position echo-cursor)) (- (length echo-spans) 1)))
                       (history-down) (vertical-move 1))]
                  [(eq? action 'delete-forward)
                   (if (< pos len) (edited (string:delete s pos (+ pos 1)) pos) (loop s pos ""))]
                  [(eq? action 'delete-backward)
                   (if (= pos 0) (loop s pos "") (edited (string:delete s (- pos 1) pos) (- pos 1)))]
                  [(eq? action 'kill) (head:set-copy-text! (string:tail s pos)) (edited (substring s 0 pos) pos)]
                  [(eq? action 'yank)
                   (let ([text (head:copy-text)])
                     (edited (string:insert s pos text) (+ pos (string-length text))))]
                  [(eq? action 'complete) (if complete (complete-input complete) (loop s pos ""))]
                  [(eq? action 'alternate-complete) (if alt-complete (complete-input alt-complete) (loop s pos ""))]
                  [(eq? action 'inspect)
                   (let ([inspect (prompt-inspector)])
                     (when inspect (guard (ex [else (void)]) (inspect s pos))))
                   (loop s pos "")]
                  [(eq? action 'newline)
                   (let ([insert (prompt-multiline)])
                     (if insert
                         (let ([result (insert s pos "\n")]) (edited (car result) (cdr result)))
                         (loop s pos "")))]
                  [(eq? action 'paste)
                   (let* ([lines (tty:paste-lines (head:read-paste))] [insert (prompt-multiline)])
                     (if insert
                         (let ([result (insert s pos (string:join lines "\n"))])
                           (edited (car result) (cdr result)))
                         (let ([text (string:join lines " ")])
                           (edited (string:insert s pos text) (+ pos (string-length text))))))]
                  [(prompt-window-command event) => (lambda (run) (loop s pos (run)))]
                  [(tty:key-event-character event) => (lambda (c) (edited (string:insert s pos (string c)) (+ pos 1)))]
                  [else (loop s pos "")]))))))
    ;; no input yet, no reading: see query-key!
    (if (not (head:input-live?)) #f
      (interaction
        (lambda ()
          ;; These options belong to this invocation. Nested questions must
          ;; not validate a filename, overwrite its draft or borrow its table.
          (parameterize ([active-refresh (lambda ()
                                           (when (and (not in-window?) (notice? note)) (render-echo!)))]
                         [window-owner (if in-window? owner (window-owner))]
                         [validate-input #f] [draft-input #f] [content #f])
            (dynamic-wind
              (lambda () (when in-window? (take-view!)))
              run-prompt
              (lambda ()
                (release-view!)
                (clear-validation!)
                (set! echo-cursor #f) (set! echo-indent #f) (set! echo-input-end #f)
                (set! echo-scroll 0) (set! message-ghost ""))))))))

  (edoc "Ask a yes-or-no question with a single key."
        (label string "the question")
        (returns boolean)
        (effects internal)
        (prompts))
  (define (confirm? label)
    (let ([answer (query-key! (string-append label " y)es or n)o") "yn")])
      (and answer (memv (char->integer answer) '(121 89)))))
)
