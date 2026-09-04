;; style.e -- faces and the style DSL: the library (style), v2 core
;; dissolution (dev/DESIGN2.md).  Pure infrastructure with no init!.
;;
;; A face is a name; a style is a declarative expression --
;; ((foreground 244) italic) -- compiled once into the raw SGR
;; parameter string terminals consume.  The built-in faces and any
;; overrides (set-style!, from config.e or modules) live here;
;; painting is the head's business, and the head learns about face
;; redefinitions through the changed hook (its painted rows are
;; cached by content, not by face definitions, so a redefinition must
;; repaint everything).

(library (style)
  (export (rename (compile-style compile)) (rename (style-escape escape)) (rename (set-style! set!)) (rename (style-code code))
          (rename (set-styles-changed-hook! set-changed-hook!)) fill-range!)
  (import (rnrs)
          (only (chezscheme) format void)
          (prefix (kernel) kernel:)
          (prefix (string) string:))

  ;;; The DSL -----------------------------------------------------------------

  (define style-attributes
    ;; Values are SGR parameters; a string carries colon subparameters
    ;; through verbatim. Cancellations exist so an overlay face layered
    ;; on a syntax style can remove attributes, not only add them.
    '((reset . 0) (bold . 1) (dim . 2) (italic . 3) (underline . 4)
      (blink . 5) (reverse . 7) (hidden . 8) (strike . 9)
      (double-underline . 21)
      (curly-underline . "4:3") (dotted-underline . "4:4")
      (dashed-underline . "4:5")
      ;; boxes around the cells; few terminals draw either
      (framed . 51) (encircled . 52)
      (overline . 53)
      (superscript . 73) (subscript . 74)
      (normal-intensity . 22) (no-italic . 23) (no-underline . 24)
      (no-blink . 25) (no-reverse . 27) (no-hidden . 28)
      (no-strike . 29) (no-frame . 54) (no-overline . 55)))

  (define style-colors
    '((black . 0) (red . 1) (green . 2) (yellow . 3)
      (blue . 4) (magenta . 5) (cyan . 6) (white . 7)))

  (define (style-byte who value)
    (unless (and (integer? value) (exact? value) (<= 0 value 255))
      (error who "color component must be an integer from 0 through 255"
             value))
    value)

  (define (named-color value)
    (and (symbol? value)
         (let* ([text (symbol->string value)]
                [bright? (string:prefix? "bright-" text)]
                [name (if bright? (string->symbol (string:tail text 7)) value)]
                [hit (assq name style-colors)])
           (and hit (cons (cdr hit) bright?)))))

  (define (compile-color clause foreground?)
    (unless (= (length clause) 2)
      (error 'compile-style "color clause must contain exactly one color"
             clause))
    (let ([value (cadr clause)] [base (if foreground? 30 40)])
      (cond
        [(eq? value 'default) (list (+ base 9))]
        [(named-color value)
         => (lambda (named)
              (list (+ base (car named) (if (cdr named) 60 0))))]
        [(number? value)
         (list (+ base 8) 5 (style-byte 'compile-style value))]
        [(and (list? value) (= (length value) 4) (eq? (car value) 'rgb))
         (cons (+ base 8)
               (cons 2 (map (lambda (v) (style-byte 'compile-style v))
                            (cdr value))))]
        [else
         (error 'compile-style
                "color must be named, 0..255, or (rgb red green blue)"
                value)])))

  (define (compile-underline-color clause)
    ;; SGR 58/59: the underline's own color, kept by terminals that
    ;; support styled underlines; default restores the text color.
    (unless (= (length clause) 2)
      (error 'compile-style "color clause must contain exactly one color"
             clause))
    (let ([value (cadr clause)])
      (cond
        [(eq? value 'default) (list 59)]
        [(named-color value)
         => (lambda (named)
              (list 58 5 (+ (car named) (if (cdr named) 8 0))))]
        [(number? value) (list 58 5 (style-byte 'compile-style value))]
        [(and (list? value) (= (length value) 4) (eq? (car value) 'rgb))
         (cons 58 (cons 2 (map (lambda (v) (style-byte 'compile-style v))
                               (cdr value))))]
        [else
         (error 'compile-style
                "color must be named, 0..255, (rgb red green blue), or default"
                value)])))

  (define (compile-style expression)
    ;; Compile a declarative style into the raw SGR parameter string used by
    ;; terminals: ((foreground 244) italic), for example.
    (unless (list? expression)
      (error 'compile-style "expected a list of style clauses" expression))
    (let ([codes
           (apply append
             (map (lambda (clause)
                    (cond
                      [(assq clause style-attributes)
                       => (lambda (x) (list (cdr x)))]
                      [(and (list? clause) (pair? clause)
                            (memq (car clause) '(foreground fg)))
                       (compile-color clause #t)]
                      [(and (list? clause) (pair? clause)
                            (memq (car clause) '(background bg)))
                       (compile-color clause #f)]
                      [(and (list? clause) (pair? clause)
                            (eq? (car clause) 'underline-color))
                       (compile-underline-color clause)]
                      [else (error 'compile-style "unknown style clause"
                                   clause)]))
                  expression))])
      (string:join (map (lambda (code)
                          (if (string? code) code (number->string code)))
                        (if (null? codes) '(0) codes))
                   ";")))

  (define (style-escape expression)
    (format "\x1b;[~am" (compile-style expression)))

  ;;; Faces -------------------------------------------------------------------

  (define style-overrides (kernel:make-registry))

  ;; The head's repaint trigger: painted rows are cached by content
  ;; and marks, not by face definitions, so a redefined face must
  ;; repaint everything.  Installed once by the core; a failure here
  ;; never loses the override.
  (define styles-changed-hook #f)

  (define (set-styles-changed-hook! proc)
    (set! styles-changed-hook proc))

  (define (set-style! style spec)
    (kernel:registry-add!
      style-overrides
      (cons style
            (cond [(number? spec)
                   (style-escape `((foreground ,spec)))]
                  [(string? spec) (format "\x1b;[~am" spec)]
                  [else (style-escape spec)])))
    (when styles-changed-hook
      (guard (ex [else (void)]) (styles-changed-hook))))

  (define (style-override style)
    (let ([hit (kernel:registry-find style-overrides
                                     (lambda (e) (eq? (car e) style)))])
      (and hit (cdr hit))))

  (define default-styles
    ;; Built-in faces use the public DSL too, keeping one compilation path for
    ;; defaults and config.e overrides.
    (map (lambda (entry) (cons (car entry) (style-escape (cadr entry))))
      '((plain (reset))
        (chrome ((foreground bright-black)))
        (comment ((foreground bright-black)))
        (string ((foreground green)))
        (keyword (bold (foreground cyan)))
        (number ((foreground magenta)))
        (literal (bold (foreground magenta)))
        (delimiter ((foreground 245)))
        (editor ((foreground 135)))
        (rainbow1 ((foreground 196)))
        (rainbow2 ((foreground 208)))
        (rainbow3 ((foreground 220)))
        (rainbow4 ((foreground 40)))
        (rainbow5 ((foreground 33)))
        (rainbow6 ((foreground 57)))
        (rainbow7 ((foreground 129)))
        (quote ((foreground cyan)))
        (bold (bold))
        (italic (italic))
        (mark (underline))
        (selection ((background blue)))
        (active ((background 24)))
        (active-shadow ((background 31)))
        (choice (bold (foreground 135)))
        (match ((background cyan) (foreground black)))
        (match-point ((background yellow) (foreground black))))))

  (define (style-code style)
    (or (style-override style)
        (let ([hit (assq style default-styles)])
          (if hit (cdr hit) (cdar default-styles)))))
  ;;; Styles vectors --------------------------------------------------------------

  (define (fill-range! v from to face)
    ;; face into the per-column styles vector v over [from, to)
    (let loop ([i from])
      (when (< i to) (vector-set! v i face) (loop (+ i 1)))))
)
