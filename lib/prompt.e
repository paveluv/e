;; prompt.e -- the prompt: the library (prompt).
;;
;; The head's modal input in the echo area -- Emacs's minibuffer.
;; (prompt:read! label ...) runs a line editor with the cursor parked
;; in the echo area: history, TAB completion with a candidate list
;; that borrows the current window (the *completions* view), a ghost
;; suggestion, multiline and reindenting variants for M-x, and the
;; global window commands a prompt may run without losing its input
;; (registered with allow!).  (prompt:key! question allowed) asks a
;; focused single-key question; confirm? is yes/no over it.
;;
;; An interaction owns C-g and the cursor (interaction): C-g cancels
;; the prompt as a key rather than interrupting the editor, and the
;; cursor follows the prompt, not a parked evaluation.  The prompt
;; writes the echo area's model (echo) and asks the painter for
;; frames; it reads keys from the head's pump.  Exported names drop
;; the module stem: (prompt:read! "Find file: " file:complete),
;; (prompt:confirm? "Really?"), (prompt:active?).

(library (prompt)
  (export (rename (prompt! read!) (query-key! key!) (prompt-active? active?)
                  (prompt-ghost ghost) (prompt-inspector inspector)
                  (prompt-multiline multiline) (prompt-edge-motion edge-motion)
                  (prompt-reindent reindent))
          confirm? completion-highlight allow! interaction)
  (import (rnrs) (rnrs r5rs)
          (only (chezscheme)
                make-parameter parameterize box unbox set-box! format void)
          (prefix (kernel) kernel:)
          (prefix (head) head:)
          (prefix (echo) echo:)
          (prefix (paint) paint:)
          (prefix (keymap) keymap:)
          (prefix (tty) tty:)
          (prefix (mode) mode:)
          (prefix (file) file:)
          (prefix (string) string:)
          (prefix (style) style:))

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

  (define (allow! command . stand-in)
    (kernel:registry-add! allowed-commands
      (cons command (and (pair? stand-in) (car stand-in)))))

  ;;; Questions, completions, and the prompt ------------------------------------------

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
              (set! message-ghost "")))))))

  (define (prompt-active?)
    ;; True while a prompt owns the echo area (cursor parked there).
    (and echo-cursor #t))

  (define (completion-label c)
    ;; A candidate as shown in the completions list: the part after the last
    ;; separator -- a path's last component (with the trailing slash kept on
    ;; directories), an expression's trailing symbol; plain names unchanged.
    ;; A label that comes out empty (a view name like "[log]" ends in a
    ;; separator) falls back to the whole candidate.
    (if (string:suffix? "/" c)
        (string-append (file:base-name (substring c 0 (- (string-length c) 1))) "/")
        (let loop ([i (- (string-length c) 1)])
          (cond [(< i 0) c]
                [(memv (string-ref c i) '(#\/ #\space #\( #\) #\[ #\]))
                 (let ([tail (string:tail c (+ i 1))])
                   (if (string=? tail "") c tail))]
                [else (loop (- i 1))]))))

  (define (format-columns labels width)
    ;; The labels laid out in columns across width, one string per line.
    (let* ([w (+ 2 (fold-left max 0 (map string-length labels)))]
           [ncols (max 1 (quotient width w))])
      (let loop ([xs labels] [acc '()])
        (if (null? xs)
            (reverse acc)
            (let row ([i 0] [xs xs] [line ""])
              (if (or (= i ncols) (null? xs))
                  (loop xs (cons line acc))
                  (row (+ i 1) (cdr xs) (string-append line (paint:fit (car xs) w)))))))))

  ;; The *completions* buffer: shown in the current window until the
  ;; prompt ends (show-completions!), a head's own chrome.

  (define completions-restore #f)

  ;; Prompts may parameterize this to make candidates stand out in the
  ;; list -- M-x highlights the symbols the editor itself defines.

  (define completion-highlight (make-parameter (lambda (label) #f)))

  ;; The completions mode, registered once: it highlights the labels
  ;; the completion-highlight predicate in force at styling time
  ;; selects.  Styling happens inside the prompt's redraws -- within
  ;; the prompt's parameterization -- and every layout builds fresh
  ;; label strings, so the line-style memo never serves a stale
  ;; predicate.

  (define completions-mode-registered
    (mode:register! "completions" '() '()
      (lambda (s)
        (let ([highlight? (completion-highlight)]
              [styles (make-vector (string-length s) 'plain)]
              [n (string-length s)])
          (let loop ([i 0])
            (cond [(>= i n) styles]
                  [(char=? (string-ref s i) #\space) (loop (+ i 1))]
                  [else
                   (let ([j (let end ([j i])
                              (if (or (>= j n)
                                      (char=? (string-ref s j) #\space))
                                  j
                                  (end (+ j 1))))])
                     (when (highlight? (substring s i j))
                       (style:fill-range! styles i j 'editor))
                     (loop j))]))))))

  ;; The *completions* buffer is a view like any other: registered once
  ;; as an app whose refresh pages the list to the window it borrowed,
  ;; ephemeral (a head's own chrome, not adopted by other heads), and
  ;; styled by the completions mode.

  (define completions-buffer
    (let ([b (head:register-app! "*completions*"
                                 (lambda () (update-completions-size!)))])
      (head:buffer-fact-set! b 'ephemeral #t)
      (mode:choose! b "completions")
      b))

  (define completions-labels #f)   ; the labels shown: repeat detection

  (define completions-rows '#())   ; the full column layout

  (define completions-cols 0)      ; the width the layout was built for

  (define completions-page 0)

  (define completions-pages 1)

  ;; a paged candidate list says which page it shows -- as a status
  ;; hint the completion code owns, not a case inside the painter

  (define completions-status-hinted
    (paint:add-buffer-status-hint!
      (lambda (b active?)
        (and (eq? b completions-buffer) (> completions-pages 1)
             (list (cons (format "  page ~a/~a"
                                 (+ completions-page 1) completions-pages)
                         #f))))))

  (define completions-filled #f)   ; (page size) the buffer holds

  (define (completions-window)
    (and completions-buffer
         (find (lambda (w) (eq? (head:window-buffer w) completions-buffer))
               (head:windows))))

  (define (completions-layout! labels width)
    (set! completions-rows (list->vector (format-columns labels width)))
    (set! completions-cols width)
    (set! completions-page 0)
    (set! completions-filled #f))

  (define (show-completions! labels)
    ;; The candidate list borrows the current window: it shows
    ;; *completions* until the prompt ends, then gets its buffer back
    ;; with point and viewport intact.  A list taller than the window
    ;; is paged, and repeated TAB on the same candidates cycles the
    ;; pages.  No pop-ups: the layout tree is the only source of
    ;; windows, so every seam sees one kind of window.
    (cond
      [(and completions-restore (equal? labels completions-labels))
       (set! completions-page (mod (+ completions-page 1)
                                   (max 1 completions-pages)))
       (set! completions-filled #f)
       #t]
      [completions-restore                       ; already up: refresh it
       (set! completions-labels labels)
       (completions-layout! labels (head:window-content-width (head:current)))
       #t]
      [else
       (set! completions-labels labels)
       (completions-layout! labels (head:window-content-width (head:current)))
       (let ([target (head:current)]
             [shown (head:window-buffer (head:current))])
         (head:set-window-buffer! target completions-buffer)
         (set! completions-restore
           (lambda ()
             (when (and (memq target (head:windows))
                        (eq? (head:window-buffer target) completions-buffer))
               (head:set-window-buffer! target (if (memq shown (head:buffers))
                                                 shown
                                                 (car (head:buffers))))))))
       #t]))

  (define (update-completions-size!)
    ;; Page the list to the window it borrowed: the whole list when it
    ;; fits, the largest possible page otherwise -- the buffer holds
    ;; the current page.
    (let ([w (completions-window)])
      (when w
        (unless (= completions-cols (head:window-content-width w)) ; resized
          (completions-layout! completions-labels (head:window-content-width w)))
        (let* ([all (max 1 (vector-length completions-rows))]
               [size (max 1 (min all (head:window-size w)))])
          (set! completions-pages (div (+ all size -1) size))
          (when (>= completions-page completions-pages)
            (set! completions-page 0))
          (unless (equal? completions-filled (list completions-page size))
            (set! completions-filled (list completions-page size))
            (let* ([from (* completions-page size)]
                   [to (min (vector-length completions-rows) (+ from size))]
                   [out (make-vector (max 1 (- to from)) "")])
              (do ([i from (+ i 1)]) ((>= i to))
                (vector-set! out (- i from)
                             (vector-ref completions-rows i)))
              (head:buffer-lines-set! completions-buffer out)
              (head:window-top-set! w 0)
              (head:window-prow-set! w 0) (head:window-pcol-set! w 0)))))))

  (define (dismiss-completions!)
    ;; the borrowed window gets its buffer back; the view stays, as
    ;; views do
    (when completions-restore
      (completions-restore)
      (set! completions-restore #f)
      (set! completions-labels #f)))

  ;; Prompts may parameterize this to suggest what could follow the input --
  ;; M-x uses it to show the pending parameters of the call being typed.
  ;; The suggestion is drawn in grey after the cursor; #f for none.

  (define prompt-ghost (make-parameter (lambda (s) #f)))

  ;; M-. at a prompt hands the input and cursor position here --
  ;; describe wires it to pop the reference page for the symbol at
  ;; (or just before) the cursor.  A procedure (text pos), or #f.

  (define prompt-inspector (make-parameter #f))

  ;; A structured multiline prompt supplies (text position inserted-text ->
  ;; (new-text . new-position)). M-x uses this for M-RET and bracketed paste;
  ;; ordinary prompts retain compact single-line paste behavior.

  (define prompt-multiline (make-parameter #f))

  ;; Optional prompt-specific C-a/C-e behavior: (action text position second?
  ;; -> new-position). second? records the immediately preceding edge command,
  ;; independently of where the cursor happened to be.

  (define prompt-edge-motion (make-parameter #f))

  ;; Optional whole-input normalization after every prompt edit:
  ;; (text position -> (new-text . new-position)). M-x uses this to reindent
  ;; all logical lines after each character, deletion, completion, or paste.

  (define prompt-reindent (make-parameter #f))

  (define (complete! s complete k)
    ;; TAB in a prompt, as in Emacs: extend s to the longest common prefix
    ;; of its completions; when it cannot be extended, show the candidate
    ;; list.  k continues the prompt loop as (k new-s note).
    (let ([cands (complete s)])
      (cond
        [(null? cands) (dismiss-completions!) (k s " [No match]")]
        [(null? (cdr cands))
         (dismiss-completions!)
         (if (string=? (car cands) s)
             (k s " [Sole completion]")
             (k (car cands) ""))]
        [else
         (let ([lcp (string:common-prefix cands)])
           (cond [(> (string-length lcp) (string-length s)) (k lcp "")]
                 [(show-completions! (map completion-label cands)) (k s "")]
                 [else (k s (format " {~a}"
                                    (string:join (map completion-label cands)
                                                 " ")))]))])))

  (define (prompt-window-command event)
    ;; Resolve the event, and any chord it opens, through the global
    ;; keymap while a prompt runs. A window-management command yields a
    ;; thunk that runs it and returns the note to show; any other
    ;; complete chord is consumed whole so its tail cannot leak into the
    ;; input; a plain key or self-inserting character that is not a
    ;; window command stays with the prompt.
    (define (action-thunk action)
      (let ([allowed (assq action (kernel:registry-items allowed-commands))])
        (and allowed
             (lambda ()
               (guard (ex [else (string-append "  " (kernel:condition-text ex))])
                 (if (cdr allowed)
                     ((cdr allowed))
                     (begin (action) "")))))))
    (and (not (tty:key-event-character event))
         (let loop ([sequence (list event)])
           (cond
             [(keymap:binding-prefix? 'global sequence)
              (let ([next (head:read-key-event #f)])
                (if (eof-object? next)
                    (lambda () "")
                    (loop (append sequence (list next)))))]
             [(keymap:resolved-binding 'global sequence)
              => (lambda (hit)
                   (or (action-thunk (keymap:binding-action (cdr hit)))
                       (and (> (length sequence) 1) (lambda () ""))))]
             [(> (length sequence) 1) (lambda () "")]
             [else #f]))))

  (define (prompt! label . rest)
    ;; Read input in the echo area, with the cursor parked there. Optional
    ;; arguments: a completer (string -> list of candidate strings) enabling
    ;; TAB completion, initial input (pre-filled, editable), a history box
    ;; (a list of previous inputs, newest first) navigated with the up and
    ;; down arrows -- accepting an input records it there -- an
    ;; alternative completer bound to Shift-TAB, and a normalizer
    ;; applied to the accepted input before recording and returning.
    ;; Whichever way the prompt ends, the completions list is dismissed
    ;; and the window gets its buffer back.
    (define (optional n)
      (let loop ([r rest] [n n])
        (cond [(null? r) #f]
              [(= n 0) (car r)]
              [else (loop (cdr r) (- n 1))])))
    (define complete (optional 0))
    (define initial (or (optional 1) ""))
    (define history (optional 2))
    (define alt-complete (optional 3))
    ;; applied to the accepted input before it is recorded and
    ;; returned: eval closes forgiven parentheses here, so the history
    ;; carries the completed expression
    (define normalize (optional 4))
    (define hist-pos -1)   ; -1: editing; 0..: showing that history entry
    (define stash "")      ; the in-progress input while browsing history
    (define last-edge #f)  ; beginning/end, only across consecutive presses
    (define (record-history! s)
      (when (and history (> (string-length s) 0))
        (let ([h (unbox history)])
          (unless (and (pair? h) (string=? (car h) s))
            (set-box! history (cons s h))))))
    (define (run-prompt)
      (let loop ([s initial] [pos (string-length initial)] [note ""])
        (define len (string-length s))
        (define (edited new-s new-pos) ; an edit restarts history browsing
          (set! hist-pos -1)
          (let ([reindent (prompt-reindent)])
            (if reindent
                (let ([result (guard (ex [else (cons new-s new-pos)])
                                (reindent new-s new-pos))])
                  (loop (car result) (cdr result) ""))
                (loop new-s new-pos ""))))
        (define (history-show entry)
          (loop entry (string-length entry) ""))
        (define (history-up)
          (let ([h (if history (unbox history) '())])
            (if (< (+ hist-pos 1) (length h))
                (begin
                  (when (= hist-pos -1) (set! stash s))
                  (set! hist-pos (+ hist-pos 1))
                  (history-show (list-ref h hist-pos)))
                (loop s pos note))))
        (define (history-down)
          (cond [(= hist-pos -1) (loop s pos note)]
                [(= hist-pos 0) (set! hist-pos -1) (history-show stash)]
                [else (set! hist-pos (- hist-pos 1))
                      (history-show (list-ref (unbox history) hist-pos))]))
        (define (vertical-move delta)
          ;; Move the cursor between the prompt's visual lines, keeping
          ;; the column, clamped into the editable input.
          (let* ([p (paint:echo-position echo-cursor)]
                 [target (list-ref echo-spans (+ (car p) delta))]
                 [indent (paint:echo-indent-now)]
                 [col (cdr p)]
                 [k (if (= (+ (car p) delta) 0)
                        (min col (cdr target))
                        (+ (car target) (max 0 (- col indent))))]
                 [k (min k (cdr target))]
                 [new-pos (min (max 0 (- k (string-length label)))
                               (string-length s))])
            (loop s new-pos note)))
        (define (cursor-on-top?)
          (= (car (paint:echo-position echo-cursor)) 0))
        (define (cursor-on-bottom?)
          (= (car (paint:echo-position echo-cursor)) (- (length echo-spans) 1)))
        (set! message (string-append label s note))
        (set! echo-input-end (+ (string-length label) len))
        (set! message-ghost
          (if (string=? note "") (or ((prompt-ghost) s) "") ""))
        (set! echo-indent (string-length label))
        (set! echo-cursor (+ (string-length label) pos))
        (paint:redraw!)
        ;; Mouse reports are live here: clicks focus windows and work the
        ;; window controls without canceling the prompt.
        (let* ([event (head:read-key-event #t)]
               [action (and (not (eof-object? event))
                            (keymap:event-binding 'prompt event))]
               [previous-edge last-edge])
          (set! last-edge #f)
          (cond
            [(eof-object? event) #f]
            [(eq? action 'cancel) (set! message "Quit") #f]
            [(eq? action 'accept)
             (let ([out (if normalize (normalize s) s)])
               (record-history! out) (set! message "") out)]
            [(eq? action 'beginning)
             (set! last-edge 'beginning)
             (let ([move (prompt-edge-motion)])
               (loop s (if move
                           (move 'beginning s pos
                                 (eq? previous-edge 'beginning))
                           0)
                     ""))]
            [(eq? action 'backward) (loop s (max 0 (- pos 1)) "")]
            [(eq? action 'end)
             (set! last-edge 'end)
             (let ([move (prompt-edge-motion)])
               (loop s (if move
                           (move 'end s pos (eq? previous-edge 'end))
                           len)
                     ""))]
            [(eq? action 'forward) (loop s (min len (+ pos 1)) "")]
            [(eq? action 'up)
             (if (cursor-on-top?) (history-up) (vertical-move -1))]
            [(eq? action 'down)
             (if (cursor-on-bottom?) (history-down) (vertical-move 1))]
            [(eq? action 'delete-forward)
             (if (< pos len)
                 (edited (string:delete s pos (+ pos 1)) pos)
                 (loop s pos ""))]
            [(eq? action 'delete-backward)
             (if (= pos 0)
                 (loop s pos "")
                 (edited (string:delete s (- pos 1) pos) (- pos 1)))]
            [(eq? action 'kill)
             (head:set-kill-ring! (string:tail s pos))
             (edited (substring s 0 pos) pos)]
            [(eq? action 'yank)
             (edited (string:insert s pos (head:kill-ring))
                     (+ pos (string-length (head:kill-ring))))]
            [(eq? action 'complete)
             (set! hist-pos -1)
             (if complete
                 (complete! s complete
                            (lambda (new-s note)
                              (if (string=? note "")
                                  (edited new-s (string-length new-s))
                                  (loop new-s (string-length new-s) note))))
                 (loop s pos ""))]
            [(eq? action 'alternate-complete)
             (set! hist-pos -1)
             (if alt-complete
                 (complete! s alt-complete
                            (lambda (new-s note)
                              (if (string=? note "")
                                  (edited new-s (string-length new-s))
                                  (loop new-s (string-length new-s) note))))
                 (loop s pos ""))]
            [(eq? action 'inspect)
             (let ([p (prompt-inspector)])
               (when p (guard (ex [else (void)]) (p s pos))))
             (loop s pos "")]
            [(eq? action 'newline)
             (let ([insert (prompt-multiline)])
               (if insert
                   (let ([result (insert s pos "\n")])
                     (edited (car result) (cdr result)))
                   (loop s pos "")))]
            [(eq? action 'paste)
             (let* ([lines (tty:paste-lines (head:read-paste))]
                    [insert (prompt-multiline)])
               (if insert
                   (let ([result (insert s pos (string:join lines "\n"))])
                     (edited (car result) (cdr result)))
                   (let ([text (string:join lines " ")])
                     (edited (string:insert s pos text)
                             (+ pos (string-length text))))))]
            [(prompt-window-command event)
             => (lambda (run) (loop s pos (run)))]
            [(tty:key-event-character event)
             => (lambda (c)
                  (edited (string:insert s pos (string c)) (+ pos 1)))]
            [else (loop s pos "")]))))
    ;; The prompt owns C-g while it runs, and its echo-area state is
    ;; restored however it exits -- an error unwinding through it
    ;; included.
    (interaction
      (lambda ()
        (dynamic-wind
          void
          run-prompt
          (lambda ()
            (set! echo-cursor #f)
            (set! echo-indent #f)
            (set! echo-input-end #f)
            (set! echo-scroll 0)
            (set! message-ghost "")
            (dismiss-completions!))))))

  (define (confirm? label)
    ;; Ordinary yes/no questions share the focused, highlighted, visual-bell
    ;; choice engine used by file conflict decisions.
    (let ([answer (query-key! (string-append label " y)es or n)o") "yn")])
      (and answer (memv (char->integer answer) '(121 89)))))


) ;; library (prompt)
