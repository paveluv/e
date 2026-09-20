;; style.sls -- faces and the style DSL: the library (style).  Pure
;; infrastructure with no init!.
;;
;; A face is a name; a style is a declarative expression --
;; ((foreground 244) italic) -- compiled once into the raw SGR
;; parameter string terminals consume.  The built-in faces and any
;; overrides (style:set!, from config.e or modules) live here;
;; painting is the painter's business, and it learns about face
;; redefinitions through the changed hook (its painted rows are
;; cached by content, not by face definitions, so a redefinition must
;; repaint everything).

(import (only (foundation edoc) elibrary))
(elibrary (head style)
  (export (rename (style-code code)) color-scheme! (rename (compile-style compile))
          (rename (style-escape escape)) fill-range! (rename (set-style! set!))
          (rename (set-styles-changed-hook! set-changed-hook!)))
  (import (rnrs)
          (only (chezscheme) format void)
          (prefix (core kernel) kernel:)
          (prefix (foundation string) string:))

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

  (edoc "Compile a declarative style, ((foreground 244) italic) say, into the SGR parameter string terminals take."
        (expression list "the style clauses")
        (returns string))
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

  (edoc "The escape sequence selecting a declarative style."
        (expression list "the style clauses")
        (returns string))
  (define (style-escape expression)
    (format "\x1b;[~am" (compile-style expression)))

  ;;; Faces -------------------------------------------------------------------

  (define style-overrides (kernel:make-registry))

  ;; A face as an edoc type: completion lists the built-in faces and the
  ;; overridden ones.
  (edoc-type style "a face, by name"
    (predicate symbol?)
    (complete (lambda (partial)
                (map (lambda (name) (cons name #f))
                     (append (map car default-styles) (map car (kernel:registry-items style-overrides))))))
    (write (lambda (v) (string-append "'" (symbol->string v)))))


  ;; The painter's repaint trigger: painted rows are cached by content
  ;; and marks, not by face definitions, so a redefined face must
  ;; repaint everything.  Installed by the command layer's init!; a
  ;; failure here never loses the override.
  (define styles-changed-hook #f)

  (edoc "Install the hook run when a face or the color scheme changes."
        (proc procedure "the hook"))
  (define (set-styles-changed-hook! proc)
    (set! styles-changed-hook proc))

  (define (styles-changed!)
    (when styles-changed-hook
      (guard (ex [else (void)]) (styles-changed-hook))))

  (define current-color-scheme 'dark)

  (edoc "Adopt the terminal's color scheme, dark or light, restyling every face; #f means dark."
        (scheme (or (one-of dark light) #f) "the scheme"))
  (define (color-scheme! scheme)
    (unless (memq scheme '(dark light #f))
      (error 'color-scheme! "expected dark, light or #f" scheme))
    (let ([next (or scheme 'dark)])
      (unless (eq? next current-color-scheme)
        (set! current-color-scheme next)
        (styles-changed!))))

  (edoc "Override a face: a color number, a raw SGR parameter string, or declarative style clauses."
        (style style "the face")
        (spec (or integer string list) "its look"))
  (define (set-style! style spec)
    (kernel:registry-add!
      style-overrides
      (cons style
            (cond [(number? spec)
                   (style-escape `((foreground ,spec)))]
                  [(string? spec) (format "\x1b;[~am" spec)]
                  [else (style-escape spec)])))
    (styles-changed!))

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
        (ghost ((foreground bright-black) italic))
        (header ((foreground 252) (background 240)))
        (hover (bold dotted-underline (underline-color 242)))
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
        (active ((background 31)))
        (candidate (bold (background (rgb 28 40 60))))
        (candidate-hover (bold (background (rgb 28 40 60)) dotted-underline (underline-color 242)))
        (choice (bold (foreground 135)))
        (match ((background cyan) (foreground black)))
        (match-point ((background yellow) (foreground black))))))

  (define light-styles
    ;; Most faces use terminal colors or attributes and need no variant.
    (list (cons 'header (style-escape '((foreground 236) (background 253))))
          (cons 'hover (style-escape '(bold dotted-underline (underline-color 248))))
          (cons 'candidate (style-escape '(bold (background (rgb 226 235 250)))))
          (cons 'candidate-hover (style-escape '(bold (background (rgb 226 235 250)) dotted-underline (underline-color 248))))))

  (edoc "The SGR parameter string of a face, or of layered faces such as (editor mark); a surface's raw parameters pass through."
        (style (or symbol string list) "the face")
        (returns string))
  (define (style-code style)
    ;; Layer semantic faces without copying their colors into another face:
    ;; (editor mark), for example, keeps the current editor color and underlines.
    (if (list? style) (apply string-append (map style-code style))
      (or (style-override style)
        ;; Surfaces carry SGR parameters as values, without allocating a
        ;; face in this head's registry. Only parameter bytes can enter CSI.
        (and (string? style)
             (for-all (lambda (c) (or (char<=? #\0 c #\9) (memv c '(#\; #\:))))
                      (string->list style))
             (format "\x1b;[~am" style))
        (let ([hit (or (and (eq? current-color-scheme 'light) (assq style light-styles))
                       (assq style default-styles))])
          (if hit (cdr hit) (cdar default-styles))))))
  ;;; Styles vectors --------------------------------------------------------------

  (edoc "Set a face into a per-column styles vector over [from, to)."
        (v vector "the styles")
        (from integer "the first column")
        (to integer "the column after the last")
        (face style "the face"))
  (define (fill-range! v from to face)
    ;; face into the per-column styles vector v over [from, to)
    (let loop ([i from])
      (when (< i to) (vector-set! v i face) (loop (+ i 1)))))
)
