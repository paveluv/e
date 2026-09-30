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
  (export (rename (prompt-accept! accept!)) (rename (prompt-active? active?)) allow! (rename (prompt-allowed? allowed?))
          (rename (prompt-alternate-complete! alternate-complete!)) (rename (prompt-backward! backward!))
          (rename (prompt-beginning! beginning!)) (rename (prompt-cancel! cancel!)) (rename (prompt-complete! complete!))
          completion-highlight completion-kind completion-label confirm? content (rename (content-view-context content-context))
          (rename (prompt-delete-backward! delete-backward!)) (rename (prompt-delete-forward! delete-forward!))
          (rename (prompt-down! down!)) (rename (draft-input draft)) (rename (prompt-edge-motion edge-motion))
          (rename (prompt-end! end!)) (rename (prompt-forward! forward!)) (rename (prompt-ghost ghost))
          (rename (prompt-in-window in-window)) (rename (prompt-inspect! inspect!)) (rename (prompt-inspector inspector))
          interaction (rename (query-key! key!)) (rename (prompt-kill! kill!)) line
          (rename (make-content-view make-content)) (rename (prompt-multiline multiline))
          (rename (prompt-newline! newline!)) (rename (prompt-paste! paste!)) (rename (prompt! read!))
          (rename (prompt-reindent reindent)) transient
          (rename (prompt-type! type!)) (rename (prompt-up! up!)) (rename (validate-input validate)) (rename (prompt-yank! yank!)))
  (import (rnrs)
          (rnrs r5rs)
          (only (chezscheme)
                make-parameter parameterize box unbox set-box! format void
                make-weak-eq-hashtable make-list list-head iota
                current-time add-duration make-time time<?)
          (prefix (core kernel) kernel:)
          (prefix (foundation string) string:)
          (prefix (head completion) completion:)
          (prefix (head completion-layout) completion-layout:)
          (prefix (head completion-state) completion-state:)
          (prefix (head dispatch) dispatch:)
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
        (returns any "what the thunk returns") (effects internal))
  (define (interaction thunk)
    ;; An interaction owns C-g (head:call-uninterrupted) and the cursor:
    ;; while it runs, the cursor follows the interaction's rules, not a
    ;; parked evaluation's.
    (head:call-uninterrupted
      (lambda ()
        (parameterize ([paint:cursor-in-echo #f])
          (dynamic-wind dispatch:cancel! thunk dispatch:cancel!)))))

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

  (edoc "Whether a global command may run from inside a prompt, having been allowed."
        (command any "the command, as a key binds it")
        (returns boolean))
  (define (prompt-allowed? command)
    (and (assq command (kernel:registry-items allowed-commands)) #t))

  ;;; The prompt's commands -----------------------------------------------------------
  ;;
  ;; The keys of a prompt are bound in the prompt context to these commands,
  ;; so the keys listing shows them and a key's meaning is documented here.
  ;; Each asks the open prompt for one action, which its loop carries out on
  ;; the input it holds; outside a prompt they are refused.

  (define requested #f)

  (define (request! action)
    (unless (prompt-active?) (error 'prompt "no prompt is open"))
    (set! requested action))

  (define (take-requested!)
    (let ([r requested]) (set! requested #f) r))

  (edoc "Cancel the prompt, its input dropped.")
  (define (prompt-cancel!) (request! 'cancel))

  (edoc "Accept the prompt's input, where it validates as the prompt requires.")
  (define (prompt-accept!) (request! 'accept))

  (edoc "Move the prompt's point to the beginning of its line, and again to the beginning of the whole input.")
  (define (prompt-beginning!) (request! 'beginning))

  (edoc "Move the prompt's point to the end of its line, and again to the end of the whole input.")
  (define (prompt-end!) (request! 'end))

  (edoc "Move the prompt's point one character back.")
  (define (prompt-backward!) (request! 'backward))

  (edoc "Move the prompt's point one character forward.")
  (define (prompt-forward!) (request! 'forward))

  (edoc "Move up a line of a multi-line input, or back through the prompt's history from its first line.")
  (define (prompt-up!) (request! 'up))

  (edoc "Move down a line of a multi-line input, or forward through the prompt's history from its last line.")
  (define (prompt-down!) (request! 'down))

  (edoc "Delete the character after the prompt's point.")
  (define (prompt-delete-forward!) (request! 'delete-forward))

  (edoc "Delete the character before the prompt's point.")
  (define (prompt-delete-backward!) (request! 'delete-backward))

  (edoc "Kill the input from the prompt's point to its end into the copy buffer.")
  (define (prompt-kill!) (request! 'kill))

  (edoc "Insert the copy buffer's text at the prompt's point.")
  (define (prompt-yank!) (request! 'yank))

  (edoc "Complete the input at point, a sole candidate whole, a common prefix as far as it goes, the candidates listed otherwise.")
  (define (prompt-complete!) (request! 'complete))

  (edoc "Complete with the prompt's other completer, the editor-defined names at M-x say.")
  (define (prompt-alternate-complete!) (request! 'alternate-complete))

  (edoc "Describe the name at the prompt's point, where the prompt has an inspector.")
  (define (prompt-inspect!) (request! 'inspect))

  (edoc "Insert a line break at the prompt's point, in a multi-line input.")
  (define (prompt-newline!) (request! 'newline))

  (edoc "Insert a bracketed paste at the prompt's point, its lines joined with spaces unless the input is multi-line.")
  (define (prompt-paste!) (request! 'paste))

  (edoc "Insert text at the prompt's point, as typing does; SELF-INSERT, any character, runs it with the character typed."
        (text string "the text to insert"))
  (define (prompt-type! text) (request! (cons 'type text)))

  (define (run-action! action)
    ;; a bound action run inside the prompt: a command, or a call with its
    ;; producers run and its other arguments as given
    (cond [(procedure? action) (action)]
          [(keymap:call-action? action) (keymap:run! action)]
          [else (void)]))

  (define (prompt-action event)
    ;; what a key asks of the prompt: a symbol as bound, or what the bound
    ;; command requests when run, a character through SELF-INSERT
    (head:set-current-keys! (list event))
    (let ([bound (or (keymap:event-binding 'prompt event)
                     (and (tty:key-event-character event) (keymap:event-binding 'prompt "SELF-INSERT")))])
      (cond [(symbol? bound) bound]
            [(or (procedure? bound) (keymap:call-action? bound))
             (set! requested #f)
             (run-action! bound)
             (take-requested!)]
            [else #f])))

  (define (content-action! body event)
    ;; a key the content view's own context binds, run: #t when one was
    (let ([bound (and (content-view-context body) (keymap:event-binding (content-view-context body) event))])
      (and (or (procedure? bound) (keymap:call-action? bound))
           (begin (head:set-current-keys! (list event)) (run-action! bound) #t))))

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

  ;; A live completion view supplies its minimum height, renderer and key
  ;; handler. (render input window available-height page) returns styled lines
  ;; and a page count. Choices replace input or run an action returning new
  ;; input (#f leaves it alone, e.g. when sorting a column).
  (edoc "A live completion view below a prompt's input."
        (minimum-height integer "the rows it needs at least")
        (render procedure "(render input window available-height page) giving styled lines and a page count")
        (handle (or procedure #f) "(handle event) giving new input, or #f to leave it")
        (context (or symbol #f) "a keymap context whose bindings act while the view shows, listed among the prompt's keys"))
  (define-record-type (content-view %make-content-view content-view?)
    (fields minimum-height render handle context))

  (edoc "A content view for a prompt in a window: the rows it needs, its renderer, its event handler or #f, and optionally the keymap context whose bindings act while it shows."
        (minimum-height integer "the rows it needs at least")
        (render procedure "(render input window available-height page) giving styled lines and a page count")
        (handle (or procedure #f) "(handle event) giving new input, or #f to leave it")
        (context (list-of symbol) "the keymap context, at most one")
        (returns (record content-view)))
  (define make-content-view
    (case-lambda
      [(minimum-height render handle) (%make-content-view minimum-height render handle #f)]
      [(minimum-height render handle context) (%make-content-view minimum-height render handle context)]))

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

  (edoc "A row of a content view: its text, styles and (start end value) choices, hovered with a face."
        (text string "the row text")
        (styles (or vector #f) "its styles")
        (choices list "(start end value) intervals")
        (hover-face (list-of symbol) "the face of a hovered choice, at most one")
        (returns (record row)))
  (define (line text styles choices . hover-face)
    (completion-layout:make-row text styles #f
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
                   (let* ([line (head:window-line w row)]
                          [info (hashtable-ref line-presentation line #f)]
                          [choice (and info (choice-at (cdr info) column))])
                     ;; Labels leave their padding plain; row candidates tint it too.
                     (and choice
                          (let trim ([end (cadr choice)])
                            (if (and (eq? (caddr choice) 'hover) (> end (car choice))
                                     (char-whitespace? (string-ref line (- end 1))))
                                (trim (- end 1)) (list (car choice) end (caddr choice))))))))
            caddr)))))

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
                (visual (+ i 1) (cons (completion-layout:make-row shown face (cons from to) '()) out))))))))

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
    (and (or (dispatch:pending?) (not (tty:key-event-character event)))
      (let* ([reply (dispatch:resolve! (list 'prompt (head:current-window)) '((prompt (global) #f)) event)]
             [status (car reply)] [action (caddr reply)]
             [allowed (and (eq? status 'command) (assq action (kernel:registry-items allowed-commands)))])
        (cond
          [allowed (lambda () (guard (ex [else (string-append "  " (kernel:condition-text ex))])
                                (if (cdr allowed) ((cdr allowed)) (begin (action) ""))))]
          [(eq? status 'prefix) (lambda () (string-append "  " (keymap:sequence-text (list-ref reply 3)) "-"))]
          [(or (memq status '(cancelled invalid)) (> (length (list-ref reply 3)) 1)) (lambda () "")]
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
    (define completion-state
      (completion-state:create complete
        (lambda (s pos)
          (let ([reindent (prompt-reindent)])
            (if reindent (guard (ex [else (cons s pos)]) (reindent s pos)) (cons s pos))))))
    (define page-sequence 0)
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
    (define shown-generation 0)
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
                      (completion-layout:format-columns values width (if completion-source (lambda (s) s) labeler) highlight?))))))
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
              ;; waiting to come back into it; any other buffer found there,
              ;; a document shown in the pop-up by mistake, must not keep it
              ;; open
              (when (head:popup? target)
                (if (and (memq previous (head:buffers)) (head:app-buffer? previous)
                         (equal? (head:buffer-fact previous 'mode #f) "completions"))
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
      (let ([own (and (completion:source? completion-source) (completion:source-kind completion-source))])
        (cond [(procedure? own) (or (guard (ex [else #f]) (own input position)) (label-stem label))]
              [(string? own) own]
              [kind kind]
              [else (label-stem label)])))
    (define (status-text b)
      ;; the list's status line after its name, which the painter puts first:
      ;; how many matches, of what, and the page when they take several --
      ;; never key hints
      (cond
        [candidates
         (let* ([count (length candidates)]
                [head (format "~a match~a of ~a" count (if (= count 1) "" "es") (kind-text))])
           (if (> pages 1) (format "~a; page ~a of ~a" head (+ page 1) pages) head))]
        [(> pages 1) (format "page ~a of ~a" (+ page 1) pages)]
        [else ""]))
    (define (mouse! event)
      (cond
        [(and (or body candidates) (member event '("WHEEL-UP" "WHEEL-DOWN" "S-WHEEL-UP" "S-WHEEL-DOWN")))
         (set! page (min (- pages 1) (max 0 (+ page (if (member event '("WHEEL-UP" "S-WHEEL-UP")) -1 1))))) #t]
        [(and (string=? event "MOUSE-CLICK")
              (or (not in-window?) (eq? (head:current-window) owner)))
         (let ([at (head:app-event-buffer-position)])
           (when (and at (<= 0 (car at)) (< (car at) (vector-length shown-rows)))
             (let* ([row (vector-ref shown-rows (car at))] [source (completion-layout:row-input row)]
                    [choice (choice-at (completion-layout:row-choices row) (cdr at))])
               (cond [source
                      (set! clicked
                        (cons input (min (string-length input)
                                      (max 0 (- (min (cdr source) (+ (car source) (cdr at)))
                                                (string-length label))))))]
                     [choice
                      (let ([value (if (procedure? (caddr choice)) ((caddr choice)) (caddr choice))])
                        (if value
                            (if (and body (not candidates)) (set! clicked (cons value (string-length value)))
                              (when (completion-state:choose! completion-state shown-generation value)
                                (let ([snapshot (completion-state:snapshot completion-state)])
                                  (set! clicked (cons (cadr snapshot) (caddr snapshot))))))
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
    (define (sync-completion!)
      (let* ([snapshot (completion-state:snapshot completion-state)]
             [values (list-ref snapshot 3)] [sequence (list-ref snapshot 5)])
        (set! completion-source (list-ref snapshot 6))
        (unless (equal? values candidates)
          (set! candidates values) (set! candidate-width 0) (set! page 0)
          (when (and view (head:popup? target) (pair? values))
            (head:show-popup! (popup-height values))))
        (when (> sequence page-sequence) (set! page (mod (+ page (- sequence page-sequence)) (max 1 pages))))
        (set! page-sequence sequence)
        (if candidates (take-view!)
          (begin (set! pages 1) (set! page 0) (unless in-window? (release-view!))))))
    (define (dismiss-completions!)
      (completion-state:dismiss! completion-state) (sync-completion!))
    (define (page-rows width available)
      (cond [body
             (let-values ([(lines count) ((content-view-render body) input target available page)])
               (set! pages (max 1 count)) (set! page (mod page pages)) lines)]
            [(not candidates) '()]
            [else
             (unless (= width candidate-width)
               (set! candidate-rows
                 (completion-layout:format-columns candidates width (if completion-source (lambda (s) s) labeler) highlight?))
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
                            (< cursor (car (completion-layout:row-input (cadr rows))))) i
                        (loop (cdr rows) (+ i 1))))]
                 [count (min (length all) (max 1 (- height (cond [body (content-view-minimum-height body)]
                                                             [candidates 1] [else 0]))))]
                 [from (min cursor-row (max 0 (- (length all) count)))]
                 [input-part (list-head (list-tail all from) count)]
                 [choices (page-rows width (- height count))]
                 [pad (if in-window? (max 0 (- height count (length choices))) 0)]
                 [rows (if body (append choices (make-list pad (completion-layout:make-row "" '#() #f '())) input-part)
                           (append (make-list pad (completion-layout:make-row "" '#() #f '())) choices input-part))]
                 [point (if in-window?
                            (cons (+ pad (length choices) (- cursor-row from))
                              (- cursor (car (completion-layout:row-input (list-ref all cursor-row))))) '(0 . 0))])
            (set! shown-rows (list->vector rows))
            (set! shown-generation (car (completion-state:snapshot completion-state)))
            (head:view-replace! view (map completion-layout:row-text rows) '()
              (list (cons target point) (cons (cons 'top target) '(0 . 0))))
            (let ([lines (head:buffer-lines view)])
              (do ([i 0 (+ i 1)]) ((= i (vector-length shown-rows)))
                (let ([row (vector-ref shown-rows i)])
                  (hashtable-set! line-presentation (vector-ref lines i)
                    (cons (completion-layout:row-styles row)
                          (map (lambda (choice) (list (car choice) (cadr choice)
                                                  (if (pair? (cdddr choice)) (cadddr choice) 'hover)))
                            (completion-layout:row-choices row)))))))))))

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
        (completion-state:refresh! completion-state s pos)
        (sync-completion!)
        (set! input s) (set! position pos)
        (set! note (if (string=? next-note "") (list-ref (completion-state:snapshot completion-state) 4) next-note))
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
              (define (complete-input completer backwards?)
                (set! hist-pos -1) (clear-validation!)
                (completion-state:normalize! completion-state completer backwards?)
                (sync-completion!)
                (let ([snapshot (completion-state:snapshot completion-state)])
                  (loop (cadr snapshot) (caddr snapshot) (list-ref snapshot 4))))
              (if in-window? (render!) (render-echo!))
              (paint:redraw!)
              (let* ([event (head:read-key-event #t)]
                     [action (and (not (eof-object? event)) (prompt-action event))]
                     [previous-edge last-edge])
                (set! last-edge #f)
                (cond
                  [(or (eof-object? event) (window-lost?)) #f]
                  [(and (dispatch:pending?) (prompt-window-command event)) => (lambda (run) (loop s pos (run)))]
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
                  [(and body (content-action! body event)) (set! page 0) (loop s pos "")]
                  [(and body (content-view-handle body) ((content-view-handle body) event))
                   (set! page 0) (loop s pos "")]
                  [(eq? action 'cancel) (completion-state:finish! completion-state #f) (set! message "Quit") #f]
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
                         (begin (completion-state:finish! completion-state #t) (record-history! out) (set! message "") out)))]
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
                  [(eq? action 'complete) (complete-input complete #f)]
                  [(eq? action 'alternate-complete) (complete-input alt-complete #t)]
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
                  [(and (pair? action) (eq? (car action) 'type))
                   (edited (string:insert s pos (cdr action)) (+ pos (string-length (cdr action))))]
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
                (completion-state:finish! completion-state #f)
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
