;; eval.e -- M-x: evaluate Scheme expressions, for the e editor.
;;
;; An e extension module: the library (eval), loaded at startup by the
;; core, which calls init!.  M-x prompts for an expression (the opening
;; parenthesis is pretyped and deletable, so a bare symbol works too;
;; missing closing parentheses are forgiven), evaluates it in the
;; editor's top level, logs the expression (component eval, which also
;; carries the history), and shows the result in the echo area,
;; transiently like any message -- the log keeps what flashed by.  The
;; expression styles as Scheme while typed.  TAB completes symbols
;; (Shift-TAB: only editor-defined ones,
;; which are also highlighted in the completions pop-up), the parameters
;; still to be supplied appear as a grey suggestion while typing, up and
;; down arrows browse the history, and C-g interrupts a runaway
;; evaluation. Parameter suggestions are read live from the describe
;; module's structured entries, with source and arity as fallbacks.
;; C-x C-e runs eval! over the whole current buffer, or an explicit
;; region/buffer target, in that same top level.

(library (eval)
  (export init! eval! eval!! eval-copy-result)
  (import (chezscheme) (core)
          (prefix (paint) paint:)
          (prefix (log) log:)
          (prefix (keymap) keymap:)
          (only (describe) doc-lookup doc-forms register-descriptions!)
          (only (edit) regions-of region-text)
          (only (scheme-format) scheme-indent-lines)
          (only (sys) call-with-streamed-output
                duplicate-standard-output-port terminal-output-port))

  ;;; Symbol completion -------------------------------------------------------

  (define (complete-symbol-where s keep? empty-ok?)
    ;; Complete the trailing symbol token of s against the bindings of
    ;; the editor's top level that satisfy keep?.  An empty token
    ;; completes to everything kept when empty-ok? -- the pop-up pages
    ;; a list as large as the whole environment.
    (let* ([start (let loop ([i (- (string-length s) 1)])
                    (cond [(< i 0) 0]
                          [(memv (string-ref s i)
                                 '(#\space #\newline #\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\,))
                           (+ i 1)]
                          [else (loop (- i 1))]))]
           [head (substring s 0 start)]
           [part (string-tail s start)])
      (if (and (string=? part "") (not empty-ok?))
          '()
          (let ([names (sort string<?
                             (filter (lambda (name)
                                       (and (string-prefix? part name)
                                            (keep? (string->symbol name))))
                                     (map symbol->string
                                          (environment-symbols
                                            (interaction-environment)))))])
            ;; A unique completion is final: append a space so the next
            ;; argument can start at once.
            (if (and (pair? names) (null? (cdr names)))
                (list (string-append head (car names) " "))
                (map (lambda (name) (string-append head name)) names))))))

  (define (complete-symbol s)
    (complete-symbol-where s (lambda (sym) #t) #t))

  (define (complete-editor-symbol s)
    ;; Only the symbols the editor (and its modules) define -- few enough
    ;; that an empty token usefully lists them all.
    (complete-symbol-where s editor-symbol? #t))

  ;;; Signatures ----------------------------------------------------------------

  (define (arity-params mask)
    ;; Generic parameter names from procedure-arity-mask: arg1 ... for the
    ;; smallest accepted count, [argN] for the optional ones beyond it, and
    ;; ... when any further count is accepted (a negative mask).
    (if (= mask 0)
        '()
        (let* ([rest? (< mask 0)]
               [lo (let loop ([n 0]) (if (logbit? n mask) n (loop (+ n 1))))]
               [hi (if rest?
                       lo
                       (let loop ([n 0] [hi 0])
                         (cond [(> (expt 2 n) mask) hi]
                               [(logbit? n mask) (loop (+ n 1) n)]
                               [else (loop (+ n 1) hi)])))])
          (append
            (let loop ([i 1])
              (if (> i lo) '() (cons (format "arg~a" i) (loop (+ i 1)))))
            (let loop ([i (+ lo 1)])
              (if (> i hi) '() (cons (format "[arg~a]" i) (loop (+ i 1)))))
            (if rest? '("...") '())))))

  (define (signature-arity sig)
    (let loop ([p (cdr sig)] [n 0])
      (if (pair? p) (loop (cdr p) (+ n 1)) n)))

  (define (signature-tokens sig)
    ;; A documented call shape becomes display tokens. A parenthesized
    ;; parameter is optional and a dotted tail is a rest parameter.
    (let loop ([p (cdr sig)])
      (cond [(null? p) '()]
            [(symbol? p) (list (format ". ~a" p))]
            [(pair? (car p))
             (cons (format "[~a]"
                           (string-join (map (lambda (x) (format "~a" x))
                                             (car p))
                                        " "))
                   (loop (cdr p)))]
            [else (cons (format "~a" (car p)) (loop (cdr p)))])))

  (define (described-params sym)
    ;; Pick the longest documented procedure form for this name.
    (guard (ex [else #f])
      (let ([best #f])
        (for-each
          (lambda (entry)
            (for-each
              (lambda (form)
                (when (equal? (car form) "procedure")
                  (let ([sig (guard (ex [else #f])
                               (with-input-from-string (cdr form) read))])
                    (when (and (pair? sig) (eq? (car sig) sym)
                               (or (not best)
                                   (> (signature-arity sig)
                                      (signature-arity best))))
                      (set! best sig)))))
              (doc-forms entry)))
          (doc-lookup sym))
        (and best (signature-tokens best)))))

  (define (symbol-params sym)
    ;; The parameters of the procedure sym names, as a list of display
    ;; tokens: from its live describe entry when available, else its source,
    ;; else its arity in brackets.  #f for anything else.
    (and (top-level-bound? sym)
         (let ([v (top-level-value sym)])
           (and (procedure? v)
                (let ([src (((inspect/object v) 'code) 'source)])
                  (cond
                    [(described-params sym)]
                    [(and src
                          (pair? (src 'value))
                          (eq? (car (src 'value)) 'lambda))
                     (let loop ([p (cadr (src 'value))])
                       (cond [(null? p) '()]
                             [(symbol? p) (list (format ". ~a" p))]
                             [else (cons (format "~a" (car p))
                                         (loop (cdr p)))]))]
                    [else (arity-params (procedure-arity-mask v))]))))))

  (define (drop-params tokens n)
    ;; The parameter tokens left after n arguments: one is consumed per
    ;; argument, but a rest marker (... or a dotted tail) absorbs any count.
    (cond [(or (null? tokens) (= n 0)) tokens]
          [(string=? (car tokens) "...") tokens]
          [(string-prefix? ". " (car tokens)) tokens]
          [(and (pair? (cdr tokens)) (string=? (cadr tokens) "...")) tokens]
          [else (drop-params (cdr tokens) (- n 1))]))

  (define (open-call-frames text)
    ;; The unclosed calls in text, innermost first, each as
    ;; (operator . arguments-so-far) -- operator is its token string, #f
    ;; when it is not a plain symbol, or 'pending when not yet typed.  A
    ;; trailing partial atom or string counts as an argument in progress.
    (define n (string-length text))
    (define (atom-end i)
      (if (or (>= i n)
              (memv (string-ref text i)
                    '(#\space #\tab #\newline #\( #\) #\[ #\] #\")))
          i
          (atom-end (+ i 1))))
    (define (string-end j)
      (cond [(>= j n) n]
            [(char=? (string-ref text j) #\\) (string-end (+ j 2))]
            [(char=? (string-ref text j) #\") (+ j 1)]
            [else (string-end (+ j 1))]))
    (define (datum stack tok)
      ;; A completed datum: the pending operator slot, or one more argument.
      (if (null? stack)
          stack
          (let ([frame (car stack)])
            (cons (if (eq? (car frame) 'pending)
                      (cons (or tok #f) 0)
                      (cons (car frame) (+ (cdr frame) 1)))
                  (cdr stack)))))
    (let loop ([i 0] [stack '()])
      (if (>= i n)
          stack
          (let ([c (string-ref text i)])
            (cond
              [(memv c '(#\space #\tab #\newline #\' #\` #\,))
               (loop (+ i 1) stack)]
              [(memv c '(#\( #\[)) (loop (+ i 1) (cons (cons 'pending 0) stack))]
              [(memv c '(#\) #\]))
               (loop (+ i 1) (if (pair? stack) (datum (cdr stack) #f) stack))]
              [(char=? c #\") (loop (string-end (+ i 1)) (datum stack #f))]
              [else (let ([j (atom-end (+ i 1))])
                      (loop j (datum stack (substring text i j))))])))))

  (define (signature-ghost s)
    ;; The grey suggestion for the M-x input: the parameters of the
    ;; innermost open call's operator that have not been supplied yet.
    (guard (ex [else #f])
      (let ([stack (open-call-frames s)])
        (and (pair? stack)
             (string? (caar stack))
             (let ([tokens (symbol-params (string->symbol (caar stack)))])
               (and tokens
                    (let ([left (drop-params tokens (cdar stack))])
                      (and (pair? left)
                           (string-append
                             (if (string-suffix? " " s) "" " ")
                             (string-join left " "))))))))))

  ;;; Evaluation ----------------------------------------------------------------

  (define eval-copy-result (make-parameter #t))

  (define (close-expression text)
    ;; text completed with the parentheses it is missing (up to a few), so
    ;; it reads as one datum; #f when that is not enough to make it read.
    (let loop ([extra 0])
      (and (<= extra 8)
           (let ([t (string-append text (make-string extra #\)))])
             (if (guard (ex [else #f])
                   (with-input-from-string t read)
                   #t)
                 t
                 (loop (+ extra 1)))))))

  (define (format-exchange d)
    ;; The eval entry: (query . result) formatted "query => result";
    ;; a bare string (an old-style record) as itself.
    (if (pair? d)
        (string-append (car d) " => " (cdr d))
        (format "~a" d)))

  (define (editorize! text styles)
    ;; Overlay for eval contexts, which run in the editor's top level:
    ;; symbols the editor itself defines take the editor style, on top
    ;; of cells the base styling left plain or italic.
    (let ([n (string-length text)])
      (define (boundary? c)
        (memv c '(#\space #\tab #\newline #\( #\) #\[ #\] #\" #\' #\` #\,
                  #\;)))
      (let loop ([i 0])
        (when (< i n)
          (if (boundary? (string-ref text i))
              (loop (+ i 1))
              (let end ([j (+ i 1)])
                (if (and (< j n) (not (boundary? (string-ref text j))))
                    (end (+ j 1))
                    (begin
                      (when (and (guard (ex [else #f])
                                   (editor-symbol?
                                     (string->symbol (substring text i j))))
                                 (memq (vector-ref styles i) '(plain italic)))
                        (let fill ([k i])
                          (when (< k j)
                            (vector-set! styles k 'editor)
                            (fill (+ k 1)))))
                      (loop j)))))))
      styles))

  (define (style-exchange text)
    ;; Scheme highlighting over the whole exchange, echo and *log*
    ;; alike -- the editor's own names in the editor style: eval runs
    ;; in the editor's environment, whatever a random file does.
    (let ([scheme (find-mode "scheme")])
      (and scheme (editorize! text ((mode-styles scheme) text)))))

  (define mx-echo-styles
    ;; Scheme highlighting for the M-x prompt: the label stays grey,
    ;; the expression styles as Scheme with the editor's own names in
    ;; the editor style.
    (paint:prompt-styler "M-x "
      (lambda (input)
        (guard (ex [else #f])
          (let ([scheme (find-mode "scheme")])
            (and scheme
                 (editorize! input ((mode-styles scheme) input))))))))

  (define (trim-right s)
    ;; s without trailing blanks, so auto-closed parentheses attach
    ;; directly to the input (completion appends a space, for one).
    (let loop ([n (string-length s)])
      (if (and (> n 0) (memv (string-ref s (- n 1)) '(#\space #\tab)))
          (loop (- n 1))
          (substring s 0 n))))

  (define (normalize-input s)
    ;; The prompt's normalizer: trailing blanks go and the forgiven
    ;; closing parentheses are appended, so the history and the kept
    ;; echo carry the completed expression.  A lone "(" -- the
    ;; untouched pretyped prompt -- passes through for the
    ;; cancellation check.
    (let ([t (trim-right s)])
      (if (or (string=? t "") (string=? t "("))
          t
          (or (close-expression t) t))))

  (define (leading-blanks text)
    (let loop ([i 0])
      (if (and (< i (string-length text))
               (memv (string-ref text i) '(#\space #\tab)))
          (loop (+ i 1))
          i)))

  (define (nearest-stop stops current)
    (if (pair? stops)
        (fold-left (lambda (best stop)
                     (if (< (abs (- stop current))
                            (abs (- best current)))
                         stop
                         best))
                   (car stops) stops)
        stops))

  (define (reindent-scheme-input text pos)
    ;; Reindent every logical line and keep the cursor attached to the same
    ;; text even when an earlier edit shifts this line left or right.
    (let* ([lines (split-lines text)]
           [v (list->vector lines)]
           [stops (scheme-indent-lines v 0 (- (vector-length v) 1))]
           [before (split-lines (substring text 0 pos))]
           [point-row (- (length before) 1)]
           [point-col (string-length (car (reverse before)))])
      (let loop ([rows lines] [cols stops] [row 0]
                 [built '()] [offset 0] [cursor #f])
        (if (null? rows)
            (cons (string-join (reverse built) "\n") cursor)
            (let* ([line (car rows)]
                   [old (leading-blanks line)]
                   [target (and (car cols) (nearest-stop (car cols) old))]
                   [laid (if target
                             (string-append (make-string target #\space)
                                            (string-tail line old))
                             line)]
                   [cursor (if (= row point-row)
                               (+ offset
                                  (if target
                                      (max target (+ point-col (- target old)))
                                      point-col))
                               cursor)])
              (loop (cdr rows) (cdr cols) (+ row 1)
                    (cons laid built) (+ offset (string-length laid) 1)
                    cursor))))))

  (define (indent-scheme-insertion text pos inserted)
    ;; Preserve multiline insertion as entered; the prompt's central edit path
    ;; immediately runs reindent-scheme-input over the complete result.
    (cons (string-append (substring text 0 pos) inserted
                         (string-tail text pos))
          (+ pos (string-length inserted))))

  (define (mx-edge-motion action text pos second?)
    ;; First C-a/C-e addresses the logical line; a consecutive second press
    ;; addresses the whole M-x input even when the first did not move point.
    (if second?
        (if (eq? action 'beginning) 0 (string-length text))
        (let* ([start (let back ([i pos])
                        (if (and (> i 0)
                                 (not (char=? (string-ref text (- i 1))
                                              #\newline)))
                            (back (- i 1))
                            i))]
               [end (let forward ([i pos])
                      (if (and (< i (string-length text))
                               (not (char=? (string-ref text i) #\newline)))
                          (forward (+ i 1))
                          i))])
          (if (eq? action 'end)
              end
              (let skip ([i start])
                (if (and (< i end)
                         (memv (string-ref text i) '(#\space #\tab)))
                    (skip (+ i 1))
                    i))))))

  (define (evaluate-text text)
    ;; Evaluate every datum in text in the M-x interaction environment and
    ;; return the values of the last one. An empty input returns no values.
    (let ([in (open-input-string text)])
      (let loop ([last '()])
        (let ([form (read in)])
          (if (eof-object? form)
              (apply values last)
              (loop (call-with-values
                      (lambda () (eval form (interaction-environment)))
                      list)))))))

  (define (evaluation-outcome label text)
    ;; Evaluation is one undo step in every buffer it edits, and C-g can
    ;; interrupt it whether it came from M-x or eval!.
    (define (run)
      (guard (ex [(interrupted? ex) "interrupted"]
                 [else (format "error: ~a" (error-text ex))])
        (call-with-interrupt
          (lambda ()
            (call-as-one-edit! label
              (lambda ()
                (let-values ([vals (evaluate-text text)]) vals)))))))
    (let ([lock (make-mutex)]
          [terminal (duplicate-standard-output-port)])
      (define (record! component line)
        (parameterize ([terminal-output-port terminal])
          (with-mutex lock (log:log! component line))))
      (dynamic-wind
        void
        (lambda ()
          (values
            (parameterize ([terminal-output-port terminal])
              (call-with-streamed-output
                (lambda (line) (record! 'stdout line))
                (lambda (line) (record! 'stderr line))
                run))
            '()))
        (lambda () (close-port terminal)))))

  (define (report-evaluation! query outcome output-records)
    (let* ([failed? (string? outcome)]
           [void? (and (not failed?)
                       (or (null? outcome)
                           (and (null? (cdr outcome))
                                (eq? (car outcome) (void)))))]
           [result (if failed?
                       outcome
                       (string-join (map (lambda (v) (format "~s" v)) outcome)
                                    ", "))])
      (let* ([copied? (and (eval-copy-result) (not failed?) (not void?))]
             [result-record
              (log:log! 'eval (cons query (if void? "#<void>" result)) #f)])
        (when copied? (copy-to-kill-buffer! result))
        (present-log-entries!
          (append output-records (list result-record))
          (if copied? " [stored in kill ring]" "")))))

  (define (eval! . rest)
    ;; Evaluate the text in where (the whole current buffer by default) in
    ;; the M-x interaction environment and show its result in the echo area.
    (let* ([where (if (pair? rest) (car rest) (current-buffer))]
           [query (if (pair? rest) (format "(eval! ~s)" where) "(eval!)")]
           [text (string-join (map region-text (regions-of where)) "\n")])
      (let-values ([(outcome output-records)
                    (evaluation-outcome query text)])
        (report-evaluation! query outcome output-records))
      (void)))

  (define (eval!!)
    ;; Read an expression -- the prompt pretypes "(", deletable, so a
    ;; bare symbol evaluates too -- and evaluate it in the editor's
    ;; own top level.  The expression is logged (component eval, which
    ;; also carries the history); the result shows in the echo area,
    ;; transiently like any message, and lands in the log with it.
    (let ([s (parameterize ([prompt-ghost signature-ghost]
                            [prompt-multiline indent-scheme-insertion]
                            [prompt-edge-motion mx-edge-motion]
                            [prompt-reindent reindent-scheme-input]
                            [completion-highlight
                             (lambda (label)
                               (editor-symbol? (string->symbol label)))]
                            [paint:echo-highlight mx-echo-styles])
               (prompt! "M-x " complete-symbol "("
                        (box (log:log-history 'eval car))
                        complete-editor-symbol normalize-input))])
      (when (and s (> (string-length s) 0) (not (string=? s "(")))
        ;; Keep the prompt on screen while its expression evaluates --
        ;; forgiven parentheses included -- with the cursor parked at
        ;; its end, drawn as the evaluation-in-progress underline.
        ;; An indicator, not a record: the expression is already
        ;; logged under eval.
        (paint:show-prompt-message! "M-x " s mx-echo-styles)
        (let-values ([(outcome output-records)
                      (parameterize ([paint:cursor-in-echo #t])
                        (redraw!)
                        (evaluation-outcome s s))])
          ;; One structured record per exchange: history reads the query,
          ;; while the view and echo show the formatted pair.
          (report-evaluation! s outcome output-records)))))

  (define (init!)
    (register-descriptions!
      '(((eval!) (("procedure" . "(eval! [where])")) "void"
         ("(eval)") eval "Evaluation commands" #f
         "Evaluate every Scheme datum in `where` in the same interaction environment as M-x and show the last datum's result in the echo area. Non-void results are stored in the kill ring when `eval-copy-result` is true. Standard output and error are logged per line under `stdout` and `stderr`, including child-process output. By default, evaluate the whole current buffer; `where` accepts the same buffer, name, region, predicate, and list forms as the editing commands.")
        ((eval!!) (("procedure" . "(eval!!)")) "void"
         ("(eval)") eval "Evaluation commands" #f
         "Prompt for a Scheme expression, evaluate it in the editor's interaction environment, and record the expression and result in the log. Non-void results are stored in the kill ring when `eval-copy-result` is true. Standard output and error are logged per line under `stdout` and `stderr`, including child-process output.")))
    (log:register-log-formatter! 'eval format-exchange style-exchange)
    (keymap:bind-default-key! "C-x C-e" eval!)
    (keymap:bind-default-key! "M-x" eval!!)))
