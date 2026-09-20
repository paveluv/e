;; mode.sls -- the mode registry: the library (mode).
;;
;; A buffer's mode NAME is a store property every head reads; a mode
;; record is this head's registry object for that name: the file-name
;; endings and #! interpreters it claims, its line styler, and
;; optionally a display transform and a buffer-aware row styler.
;; Detection turns a path and a first line into a mode; the memoized
;; stylers answer per-line and per-row questions without re-analysis
;; -- line styles keyed by line-string identity (edits replace
;; strings, never mutate them), whole-buffer analyses by revision.
;;
;; The painter imports this module directly, and the head's adopt
;; hook is installed here: a foreign buffer adopted without a mode
;; fact gets detection.  Exported names drop the module stem:
;; (mode:register! "scheme" '(".ss") '("scheme") styler),
;; (mode:of b), ((mode:styles m) line).

(import (only (edoc) elibrary))
(elibrary (mode)
  (export (rename (mode-name name)
                  (mode-extensions extensions)
                  (mode-interpreters interpreters)
                  (mode-styles styles) (mode-render render)
                  (mode-row-styles row-styles)
                  (register-mode! register!)
                  (add-mode-extension! add-extension!)
                  (find-mode find) (detect-mode detect) (assign-mode! assign!)
                  (set-buffer-mode! choose!) (mode-of of)
                  (buffer-mode-name name-of)
                  (buffer-line-styles line-styles)
                  (memoize-buffer-analysis memoize-analysis)
                  (refresh-buffer-modes! refresh!))
          mode? key-context
          register-indenter! register-formatter! indent-on-tab! indenter indent-on-tab? formatter)
  (import (rnrs)
          (only (chezscheme)
                make-weak-eq-hashtable eq-hashtable-ref eq-hashtable-set!
                vector-copy void)
          (prefix (kernel) kernel:)
          (prefix (head) head:)
          (prefix (keymap) keymap:)
          (prefix (string) string:))

  ;;; The registry ------------------------------------------------------------

  ;; A mode provides syntax highlighting for the buffers it matches.
  ;; Extension modules call register! with the mode's name, the
  ;; file-name endings it claims, the interpreter names recognized in
  ;; a #! first line (for files without a matching extension), and a
  ;; styles function mapping a line to a vector of per-column style
  ;; symbols understood by style:code, or #f for an unstyled
  ;; line.  Brackets styled 'delimiter take part in bracket matching;
  ;; in a buffer without a mode every bracket counts.

  (edoc "A mode: how the buffers it matches are styled and rendered."
        (name string "the mode's name")
        (extensions (list-of string) "the file-name endings it claims")
        (interpreters (list-of string) "the #! interpreter names it claims")
        (styles (or procedure #f) "line to per-column style symbols, or #f for unstyled")
        (render (or procedure #f) "(render buffer row line) giving a same-length display transform, or #f")
        (row-styles (or procedure #f) "(row-styles buffer row line) giving a styles vector, or #f for the plain styles"))
  (define-record-type mode
    (fields name extensions interpreters styles
            ;; optional display transform: (render buffer row line) ->
            ;; a string of the SAME character length, or a same-length vector
            ;; of strings whose concatenation meets that contract. Character
            ;; to cell geometry must match the source (including clusters).
            ;; Invalid substitutions paint source; character styles project
            ;; to every cell of the leading character's glyph.
            render
            ;; optional buffer-aware styling: (row-styles buffer row
            ;; line) -> a styles vector, or #f for the plain styles
            ;; function.  Uncached here -- the mode memoizes.
            row-styles)
    (protocol (lambda (new)
                (case-lambda
                  [(n e i s) (new n e i s #f #f)]
                  [(n e i s r) (new n e i s r #f)]
                  [(n e i s r rs) (new n e i s r rs)]))))

  (define modes (kernel:make-registry))

  ;; A mode as an edoc type: its registered name; completion lists the modes
  ;; with the endings they claim.
  (edoc-type mode "a mode, by name"
    (predicate (lambda (v) (and (string? v) (find-mode v) #t)))
    (complete (lambda (partial)
                (map (lambda (m) (cons (mode-name m) (string:join (mode-extensions m) " ")))
                     (kernel:registry-items modes))))
    (write (lambda (v) (call-with-string-output-port (lambda (p) (write v p))))))


  (define mode-extension-additions (kernel:make-registry))

  (edoc "Register a mode: its name, the file-name endings it claims, the interpreters of a #! line, a line styles function, then optionally a render transform and a buffer-aware row-styles procedure."
        (name string "the mode's name")
        (extensions (list-of string) "the file-name endings")
        (interpreters (list-of string) "the #! interpreter names")
        (styles (or procedure #f) "line to styles vector, or #f")
        (extra (list-of (or procedure #f)) "a render transform, then a row-styles procedure"))
  (define (register-mode! name extensions interpreters styles . extra)
    ;; extra: an optional render transform, then an optional
    ;; buffer-aware row-styles procedure (see the mode record).
    (kernel:registry-add! modes
      (make-mode name extensions interpreters styles
                 (and (pair? extra) (car extra))
                 (and (pair? extra) (pair? (cdr extra)) (cadr extra)))))

  (edoc "Give an existing mode another file-name ending, as a registry entry that config reload retracts."
        (name mode "the mode")
        (extension string "the ending, with its dot"))
  (define (add-mode-extension! name extension)
    ;; Add a suffix to an existing mode without replacing its implementation.
    ;; This is a registry so config-owned additions disappear on config reload.
    (unless (and (string? extension) (> (string-length extension) 1)
                 (char=? (string-ref extension 0) #\.))
      (error 'add-mode-extension! "expected an extension beginning with ."
             extension))
    (unless (find-mode name)
      (error 'add-mode-extension! "no such mode" name))
    (kernel:registry-add! mode-extension-additions (cons extension name))
    (for-each (lambda (b) (when (head:buffer-mode-auto b) (assign-mode! b)))
              (head:buffers))
    (void))

  (edoc "The mode for a file, by its extension then by the #! interpreter line, or #f."
        (path (or file #f) "the file")
        (first-line string "its first line")
        (returns (or (record mode) #f)))
  (define (detect-mode path first-line)
    ;; The mode for a file: by extension, then by the #! interpreter line.
    (or (and path
             (let ([addition
                    (find (lambda (entry)
                            (string:suffix? (car entry) path))
                          (kernel:registry-items mode-extension-additions))])
               (and addition (find-mode (cdr addition)))))
        (and path
             (kernel:registry-find modes
               (lambda (m)
                 (exists (lambda (ext) (string:suffix? ext path))
                         (mode-extensions m)))))
        (and (string:prefix? "#!" first-line)
             (kernel:registry-find modes
               (lambda (m)
                 (exists (lambda (name)
                           (string:search first-line name 0
                                          (string-length first-line)))
                         (mode-interpreters m)))))))

  (edoc "Give a buffer the mode its file and first line detect, following detection from then on."
        (b buffer "the buffer"))
  (define (assign-mode! b)
    (set-mode-of! b
      (detect-mode (head:buffer-file b) (vector-ref (head:buffer-lines b) 0)) #t))

  (edoc "The registered mode called name, or #f."
        (name mode "the mode's name")
        (returns (or (record mode) #f)))
  (define (find-mode name)
    (kernel:registry-find modes (lambda (m) (string=? (mode-name m) name))))

  (edoc "Give a buffer the registered mode called name, or none with #f, regardless of its file name."
        (b buffer "the buffer")
        (name (or mode #f) "the mode's name"))
  (define (set-buffer-mode! b name)
    ;; Give b the registered mode called name (#f for none), regardless of
    ;; its file name -- how transcript buffers get their highlighting.
    (set-mode-of! b (and name (find-mode name)) #f))

  (edoc "The keymap context of a buffer's mode, named after it, or #f; a capture context needs a live app."
        (b buffer "the buffer")
        (returns (or symbol #f)))
  (define (key-context b)
    ;; A mode may carry its own key bindings under a context named
    ;; after it; they take precedence over the global map while a
    ;; buffer of that mode is current. Capture contexts require a live app;
    ;; an exited transcript keeps its mode's presentation, not its controls.
    (let ([name (buffer-mode-name b)])
      (and name
           (let ([context (string->symbol name)])
             (and (or (not (keymap:context-capture context)) (head:app-buffer? b))
                  context)))))

  (edoc "The name of a buffer's mode, or #f without one."
        (b buffer "the buffer")
        (returns (or string #f)))
  (define (buffer-mode-name b)
    ;; The name of b's mode, or #f without one.
    (let ([m (mode-of b)]) (and m (mode-name m))))

  (edoc "A buffer's mode record, or #f."
        (b buffer "the buffer")
        (returns (or (record mode) #f)))
  (define (mode-of b)
    (let ([n (head:buffer-fact b 'mode #f)]) (and n (find-mode n))))

  (define (set-mode-of! b m . auto?)
    (head:buffer-facts-set! b
      (cons (cons 'mode (and m (mode-name m)))
            (if (pair? auto?) (list (cons 'mode-auto (car auto?))) '()))))

  ;;; Indenters and formatters ------------------------------------------------------

  ;; Both are provided per mode by modules and consumed by edit's
  ;; indentation and formatting commands.  An indenter maps rows to where
  ;; their text should start: (proc buffer from to) -> one entry per row
  ;; of from..to -- #f leaving a row alone, a column, or an ascending list
  ;; of columns when several indentations are valid.  A formatter rewrites
  ;; rows wholesale: (proc buffer from to) -> the replacement lines, or #f
  ;; when the rows cannot be formatted.
  (define indenters (kernel:make-registry))   ; entries (mode proc tab?)
  (define formatters (kernel:make-registry))  ; entries (mode proc)

  (define (indenter-entry name)
    (kernel:registry-find indenters (lambda (x) (string=? (car x) name))))

  (edoc "Register a mode's indenter: (proc buffer from to) gives each row's column, its list of stops, or #f to leave it; tab says whether TAB runs it, on when omitted."
        (name mode "the mode")
        (proc procedure "the indenter")
        (tab boolean "whether TAB indents"))
  (define register-indenter!
    (case-lambda
      [(name proc) (register-indenter! name proc #t)]
      [(name proc tab) (kernel:registry-add! indenters (list name proc tab))]))

  (edoc "Register a mode's formatter: (proc buffer from to) gives the replacement lines, or #f when the rows cannot be formatted."
        (name mode "the mode")
        (proc procedure "the formatter"))
  (define (register-formatter! name proc)
    (kernel:registry-add! formatters (list name proc)))

  (edoc "Set whether TAB indents in a mode, overriding the flag its indenter registered with."
        (name mode "the mode")
        (flag boolean "whether TAB indents"))
  (define (indent-on-tab! name flag)
    (let ([entry (indenter-entry name)])
      (unless entry (error 'indent-on-tab! "no indenter for mode" name))
      (kernel:registry-add! indenters (list name (cadr entry) flag))))

  (edoc "A mode's indenter, (proc buffer from to), or #f."
        (name string "the mode's name")
        (returns (or procedure #f)))
  (define (indenter name)
    (let ([entry (indenter-entry name)]) (and entry (cadr entry))))

  (edoc "Whether TAB runs a mode's indenter."
        (name string "the mode's name")
        (returns boolean))
  (define (indent-on-tab? name)
    (let ([entry (indenter-entry name)]) (and entry (caddr entry) #t)))

  (edoc "A mode's formatter, (proc buffer from to), or #f."
        (name string "the mode's name")
        (returns (or procedure #f)))
  (define (formatter name)
    (let ([entry (kernel:registry-find formatters (lambda (x) (string=? (car x) name)))])
      (and entry (cadr entry))))

  (define (no-styles s) #f)

  ;; Computed styles, memoized per line string.  Edits replace line
  ;; strings (never mutate them), so string identity keys the cache and
  ;; can never go stale; weak keys keep it bounded by the live lines.
  ;; Each entry remembers its mode, in case an identical string is shared
  ;; between buffers of different modes.

  (define style-cache (make-weak-eq-hashtable))

  (edoc "The memoized line styles function of a buffer's mode; every line plain without one, and a raising mode styles plain."
        (b buffer "the buffer")
        (returns procedure)
        (effects internal))
  (define (buffer-line-styles b)
    ;; The line-styles function of b's mode; unstyled without one.
    (let ([m (mode-of b)])
      (if m
          (lambda (s)
            (let ([hit (eq-hashtable-ref style-cache s #f)])
              (if (and hit (eq? (car hit) m))
                  (cdr hit)
                  ;; a raising mode styles the line plain rather than
                  ;; taking the redraw (and the editor) down
                  (let ([styles (guard (ex [else #f])
                                  ((mode-styles m) s))])
                    (eq-hashtable-set! style-cache s (cons m styles))
                    styles))))
          no-styles)))

  (edoc "Turn a whole-buffer analyzer into a row provider that reruns it at most once per buffer revision."
        (analyze procedure "(analyze buffer) giving the analysis")
        (returns procedure))
  (define (memoize-buffer-analysis analyze)
    ;; Turn a whole-buffer analyzer into a row provider.  Buffer content has
    ;; one revision stamp, so validation is O(1) and analysis runs at most
    ;; once between edits, however many visible rows ask for its result.
    (let ([cache (make-weak-eq-hashtable)])
      (lambda (b row)
        (let* ([revision (head:buffer-revision b)]
               [hit (eq-hashtable-ref cache b #f)])
          (unless (and hit (= (car hit) revision))
            (set! hit
              (cons revision (analyze (vector-copy (head:buffer-lines b)))))
            (eq-hashtable-set! cache b hit))
          (let ([product (cdr hit)])
            (and (< row (vector-length product))
                 (vector-ref product row)))))))

  (edoc "Re-resolve every buffer's mode by name, so buffers pick up a reloaded mode or lose one that is gone.")
  (define (refresh-buffer-modes!)
    ;; Re-resolve every buffer's mode by name, so buffers pick up a
    ;; reloaded mode's new styles (or lose a mode that is gone).
    (for-each (lambda (b)
                (if (head:buffer-mode-auto b)
                    (assign-mode! b)
                    (let ([m (mode-of b)])
                      (when m
                        (set-mode-of! b (find-mode (mode-name m)))))))
              (head:buffers)))



  ;;; The head's adopt hook -------------------------------------------------------

  ;; a foreign buffer adopted with no mode fact yet gets detection,
  ;; recorded as the shared fact
  (define adopt-hooked (head:set-adopt-hook! assign-mode!))

) ;; library (mode)
