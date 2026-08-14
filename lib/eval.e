;; eval.e -- M-x: evaluate Scheme expressions, for the e editor.
;;
;; An e extension module: the library (eval), loaded at startup by the
;; core, which calls init!.  M-x prompts for an expression (the opening
;; parenthesis is supplied and cannot be deleted; missing closing
;; parentheses are forgiven), evaluates it in the editor's top level, and
;; logs the exchange to a read-only *eval* transcript buffer as numbered
;; entries.  TAB completes symbols (Shift-TAB: only editor-defined ones,
;; which are also highlighted in the completions pop-up), the parameters
;; still to be supplied appear as a grey suggestion while typing, up and
;; down arrows browse the history, and C-c interrupts a runaway
;; evaluation.  Exports register-signatures! for signature-table modules
;; like scheme-sigs.e.

(library (eval)
  (export init! eval-expression-command! register-signatures!)
  (import (chezscheme) (core))

  ;;; Symbol completion -------------------------------------------------------

  (define (complete-symbol-where s keep? empty-ok?)
    ;; Complete the trailing symbol token of s against the bindings of the
    ;; editor's top level that satisfy keep?.  An empty token completes to
    ;; everything kept when empty-ok? -- or to nothing, for predicates that
    ;; would offer the whole environment.
    (let* ([start (let loop ([i (- (string-length s) 1)])
                    (cond [(< i 0) 0]
                          [(memv (string-ref s i)
                                 '(#\space #\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\,))
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
    (complete-symbol-where s (lambda (sym) #t) #f))

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

  ;; Builtins are compiled without source, so their parameter names are not
  ;; recoverable at run time; other modules can supply them (transcribed
  ;; from the documentation) with register-signatures!.
  (define signature-table (make-eq-hashtable))

  (define (register-signatures! signatures)
    ;; Each signature is the documented call shape as a datum:
    ;; (name param ...), where a parenthesized param is optional, ... allows
    ;; any more, and a dotted tail is a rest parameter.  They are converted
    ;; to display tokens ("param", "[param]", ". param") once, here.
    (for-each
      (lambda (sig)
        (eq-hashtable-set! signature-table (car sig)
          (let loop ([p (cdr sig)])
            (cond [(null? p) '()]
                  [(symbol? p) (list (format ". ~a" p))]
                  [(pair? (car p))
                   (cons (format "[~a]"
                                 (string-join (map (lambda (x) (format "~a" x))
                                                   (car p))
                                              " "))
                         (loop (cdr p)))]
                  [else (cons (format "~a" (car p)) (loop (cdr p)))]))))
      signatures))

  (define (symbol-params sym)
    ;; The parameters of the procedure sym names, as a list of display
    ;; tokens: from its source when available, else a registered signature,
    ;; else its arity in brackets.  #f for anything else.
    (and (top-level-bound? sym)
         (let ([v (top-level-value sym)])
           (and (procedure? v)
                (let ([src (((inspect/object v) 'code) 'source)])
                  (cond
                    [(and src
                          (pair? (src 'value))
                          (eq? (car (src 'value)) 'lambda))
                     (let loop ([p (cadr (src 'value))])
                       (cond [(null? p) '()]
                             [(symbol? p) (list (format ". ~a" p))]
                             [else (cons (format "~a" (car p))
                                         (loop (cdr p)))]))]
                    [(eq-hashtable-ref signature-table sym #f)]
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
                    '(#\space #\tab #\( #\) #\[ #\] #\")))
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
              [(memv c '(#\space #\tab #\' #\` #\,)) (loop (+ i 1) stack)]
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
      (let ([stack (open-call-frames (string-append "(" s))])
        (and (pair? stack)
             (string? (caar stack))
             (let ([tokens (symbol-params (string->symbol (caar stack)))])
               (and tokens
                    (let ([left (drop-params tokens (cdar stack))])
                      (and (pair? left)
                           (string-append
                             (if (string-suffix? " " s) "" " ")
                             (string-join left " "))))))))))

  ;;; The *eval* transcript ------------------------------------------------------

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

  (define (eval-prompt-end s)
    ;; The index just past a leading "[n]> ", or #f.
    (and (string-prefix? "[" s)
         (let loop ([i 1])
           (and (< i (string-length s))
                (cond [(char-numeric? (string-ref s i)) (loop (+ i 1))]
                      [(and (> i 1) (string-prefix? "]> " (string-tail s i)))
                       (+ i 3)]
                      [else #f])))))

  (define eval-styles
    ;; The *eval* buffer's mode (see init!): "[n]> expr" lines get a grey
    ;; prompt marker and Scheme highlighting for the expression (when a
    ;; scheme mode is loaded); everything else is output, in one distinct
    ;; color.
    (lambda (s)
      (let ([p (eval-prompt-end s)])
        (if p
            (let ([styles (make-vector (string-length s) 'comment)]
                  [scheme (find-mode "scheme")])
              (let ([inner (and scheme ((mode-styles scheme) (string-tail s p)))])
                (when inner
                  (let loop ([i p])
                    (when (< i (string-length s))
                      (vector-set! styles i (vector-ref inner (- i p)))
                      (loop (+ i 1))))))
              styles)
            (make-vector (string-length s) 'string)))))

  (define mx-history (box '()))
  (define mx-counter 1)

  (define (show-eval-result! expr result)
    ;; Append the exchange to the read-only *eval* buffer and make sure it
    ;; is shown, scrolled to the latest entry; on a screen too small for a
    ;; second window, fall back to the echo area.
    (let ([b (or (buffer-named "*eval*")
                 (let ([b (new-buffer "*eval*")])
                   (set-buffer-mode! b "eval")
                   (set-buffer-read-only! b #t)
                   (set! mx-counter 1)
                   b))])
      (buffer-append! b (format "[~a]> ~a" mx-counter expr) result)
      (set! mx-counter (+ mx-counter 1))
      (unless (display-buffer! b)
        (set-message! (string-append expr " => " result)))))

  (define (eval-expression-command!)
    ;; The prompt supplies the opening parenthesis, so it cannot be deleted;
    ;; the expression is evaluated in the editor's own top level.
    (let ([s (parameterize ([prompt-ghost signature-ghost]
                            [completion-highlight
                             (lambda (label)
                               (editor-symbol? (string->symbol label)))])
               (prompt! "M-x (" complete-symbol "" mx-history
                        complete-editor-symbol))])
      (when (and s (> (string-length s) 0))
        (let ([expr (or (close-expression (string-append "(" s))
                        (string-append "(" s))])
          ;; Keep the prompt on screen while its expression evaluates.
          (set-message! (string-append "M-x (" s))
          (redraw!)
          (let ([result
                 (guard (ex [(interrupted? ex) "interrupted"]
                            [else (format "error: ~a" (error-text ex))])
                   (call-with-interrupt
                     " [evaluating, press C-c to interrupt]"
                     (lambda ()
                       ;; The whole expression is one undo step in every
                       ;; buffer it edits, labeled with itself.
                       (call-as-one-edit! expr
                         (lambda ()
                           (let-values ([vals (eval (with-input-from-string
                                                      expr read)
                                                    (interaction-environment))])
                             (string-join (map (lambda (v) (format "~s" v))
                                               vals)
                                          ", ")))))))])
            (echo! "")
            (show-eval-result! expr result))))))

  (define (init!)
    (register-mode! "eval" '() '() eval-styles)
    (bind-key! "M-x" eval-expression-command!)))
