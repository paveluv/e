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

(import (only (foundation edoc) elibrary))
(elibrary (head mode)
  (export add-context! (rename (add-mode-extension! add-extension!)) (rename (assign-current-mode! assign!))
          (rename (set-buffer-mode! choose!)) derive! (rename (detect-mode detect))
          (rename (mode-extensions extensions)) (rename (find-mode find)) formatter
          indent-on-tab! indent-on-tab? indenter (rename (mode-interpreters interpreters))
          key-context key-contexts (rename (buffer-line-styles line-styles))
          (rename (memoize-buffer-analysis memoize-analysis)) mode? (rename (mode-name name))
          (rename (buffer-mode-name name-of)) (rename (mode-of of))
          (rename (refresh-buffer-modes! refresh!)) (rename (register-mode! register!))
          register-formatter! register-indenter! (rename (mode-render render))
          (rename (mode-row-styles row-styles)) (rename (mode-styles styles)))
  (import (rnrs)
          (only (chezscheme) record-writer)
          (only (chezscheme)
                make-weak-eq-hashtable eq-hashtable-ref eq-hashtable-set!
                vector-copy void)
          (prefix (core kernel) kernel:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:))

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
        (row-styles (or procedure #f) "(row-styles buffer row line) giving a styles vector, or #f for the plain styles")
        (parent (or mode #f) "the parent of a derived mode"))
  (define-record-type mode
    (fields name extensions interpreters (immutable styles own-styles)
            ;; optional display transform: (render buffer row line) ->
            ;; a string of the SAME character length, or a same-length vector
            ;; of strings whose concatenation meets that contract. Character
            ;; to cell geometry must match the source (including clusters).
            ;; Invalid substitutions paint source; character styles project
            ;; to every cell of the leading character's glyph.
            (immutable render own-render)
            ;; optional buffer-aware styling: (row-styles buffer row
            ;; line) -> a styles vector, or #f for the plain styles
            ;; function.  Uncached here -- the mode memoizes.
            (immutable row-styles own-row-styles) parent)
    (protocol (lambda (new)
                (case-lambda
                  [(n e i s) (new n e i s #f #f #f)]
                  [(n e i s r) (new n e i s r #f #f)]
                  [(n e i s r rs) (new n e i s r rs #f)]
                  [(n e i s r rs p) (new n e i s r rs p)]))))

  (define modes (kernel:make-registry))

  ;; A mode as an edoc type: its registered name; completion lists the modes
  ;; with the endings they claim. A mode is a registry entry looked up by
  ;; name, so its name is its spelling; the record behind it is for programs
  ;; and prints as #<mode name>.
  (define (mode-details m)
    (string:join (mode-extensions m) " "))

  (edoc-type mode "a mode, by name, registered or not"
    (predicate (lambda (v) (and (string? v) (> (string-length v) 0))))
    (complete (lambda (partial) (map (lambda (m) (cons (mode-name m) (mode-details m))) (kernel:registry-items modes))))
    (write (lambda (v) (call-with-string-output-port (lambda (p) (write v p)))))
    (within string))

  (define mode-printing
    (record-writer (record-type-descriptor mode)
      (lambda (r p wr) (display "#<mode " p) (display (mode-name r) p) (display ">" p))))

  (define mode-extension-additions (kernel:make-registry))

  (edoc "Register a mode, and re-resolve every open buffer's mode at once: its name, the file-name endings it claims, the interpreters of a #! line, a line styles function, then optionally a render transform and a buffer-aware row-styles procedure."
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
                 (and (pair? extra) (pair? (cdr extra)) (cadr extra))))
    (refresh-buffer-modes!))

  (edoc "Register a submode: a distinct mode with a parent, following the parent's current styles, rendering, row styles, indentation, formatting, Tab policy and key bindings except where its own optional styles, render transform and row styles override them; endings belong only to the new mode, the parent may register later, and a cycle is refused."
        (name string "the new mode name")
        (parent string "the parent mode's name")
        (extensions (list-of string) "the new mode's file endings")
        (extra (list-of (or procedure #f)) "own line styles, then a render transform, then a row-styles procedure"))
  (define (derive! name parent extensions . extra)
    (let walk ([next parent] [seen (list name)])
      (when (member next seen) (error 'derive! "cyclic mode derivation" name parent))
      (let ([m (find-mode next)])
        (when (and m (mode-parent m)) (walk (mode-parent m) (cons next seen)))))
    (kernel:registry-add! modes
      (make-mode name extensions '()
                 (and (pair? extra) (car extra))
                 (and (pair? extra) (pair? (cdr extra)) (cadr extra))
                 (and (pair? extra) (pair? (cdr extra)) (pair? (cddr extra)) (caddr extra))
                 parent))
    (refresh-buffer-modes!))

  (define (presentation m get)
    ;; a mode's own part, else its parent's, resolved by name at each use
    (and m (or (get m)
               (and (mode-parent m) (presentation (find-mode (mode-parent m)) get)))))

  (edoc "A mode's effective line styler, following its current parent, or #f."
        (m (record mode) "the mode") (returns (or procedure #f)))
  (define (mode-styles m) (presentation m own-styles))

  (edoc "A mode's effective render transform, following its current parent, or #f."
        (m (record mode) "the mode") (returns (or procedure #f)))
  (define (mode-render m) (presentation m own-render))

  (edoc "A mode's effective row styler, following its current parent, or #f."
        (m (record mode) "the mode") (returns (or procedure #f)))
  (define (mode-row-styles m) (presentation m own-row-styles))

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
    (refresh-buffer-modes!)
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

  (define (scratch-mode b)
    ;; *scratch*, the editor's notepad, speaks Scheme without a file name
    ;; to say so, as Emacs's *scratch* speaks Lisp
    (and (not (head:buffer-file b))
         (string:prefix? "*scratch*" (head:buffer-name b))
         (find-mode "scheme")))

  (edoc "Give a buffer the mode its file and first line detect, Scheme for a *scratch* buffer, following detection from then on."
        (b buffer "the buffer"))
  (define (assign-mode! b)
    (set-mode-of! b
      (or (detect-mode (head:buffer-file b) (vector-ref (head:buffer-lines b) 0))
          (scratch-mode b))
      #t))

  (edoc "The registered mode called name, or #f."
        (name mode "the mode's name")
        (returns (or (record mode) #f)))
  (define (find-mode name)
    (and (string? name)
         (kernel:registry-find modes (lambda (m) (string=? (mode-name m) name)))))

  (define (the-buffer b)
    ;; the optional buffer argument, by name or as its literal, else the current buffer
    (if (pair? b) (edoc:type-value 'buffer (car b)) (head:current-buffer)))

  (edoc "Give a buffer, the current one without a second argument, the registered mode called name, or none with #f, regardless of its file name; it then follows only that name."
        (name (or mode #f) "the mode's name, or #f for none")
        (b (list-of buffer) "the buffer, at most one"))
  (define (set-buffer-mode! name . b)
    ;; how transcript buffers get their highlighting, and how a user picks
    ;; a mode by hand
    (let ([name (and name (edoc:type-value 'mode name))])
      (set-mode-of! (the-buffer b) (and name (find-mode name)) #f)))

  (edoc "Give a buffer, the current one without an argument, the mode its file and first line detect, Scheme for a *scratch* buffer, following detection from then on."
        (b (list-of buffer) "the buffer, at most one"))
  (define (assign-current-mode! . b)
    (assign-mode! (the-buffer b)))

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

  ;; A context a buffer has by its state rather than its mode: merge while
  ;; its text holds conflict markers, say.  The app binding keys in it
  ;; registers the context with the predicate; the registration retracts
  ;; with the module.  Such a context comes before the mode's.
  (define state-contexts (kernel:make-registry))

  (edoc "Register a keymap context a buffer has while a predicate holds of it, before its mode's contexts: (mode:add-context! 'merge merging?) say, by the app that binds keys in the context."
        (name symbol "the context")
        (holds? procedure "(holds? buffer) giving whether the buffer has the context now"))
  (define (add-context! name holds?)
    (unless (and (symbol? name) (procedure? holds?))
      (error 'add-context! "expected a context name and a predicate" name holds?))
    (kernel:registry-add! state-contexts (cons name holds?)))

  (define (state-contexts-of b)
    ;; the registered contexts whose predicates hold of b; a raising
    ;; predicate withholds its context rather than taking a key down
    (fold-right (lambda (entry acc) (if (guard (ex [else #f]) ((cdr entry) b)) (cons (car entry) acc) acc))
                '() (kernel:registry-items state-contexts)))

  (edoc "The keymap contexts of a buffer, nearest first: those it has by its state, registered with add-context!, then its mode's and its parents', each named after its mode; a capture context needs a live app."
        (b buffer "the buffer")
        (returns (list-of symbol)))
  (define (key-contexts b)
    (append
      (state-contexts-of b)
      (let loop ([m (mode-of b)] [acc '()])
        (if (not m)
            (reverse acc)
            (loop (and (mode-parent m) (find-mode (mode-parent m)))
                  (let ([context (string->symbol (mode-name m))])
                    (if (or (not (keymap:context-capture context)) (head:app-buffer? b))
                        (cons context acc)
                        acc)))))))

  (edoc "The name of a buffer's mode, the current buffer's without an argument, or #f without one."
        (b (list-of buffer) "the buffer, at most one")
        (returns (or string #f)))
  (define (buffer-mode-name . b)
    ;; The name of b's mode, or #f without one.
    (let ([m (apply mode-of b)]) (and m (mode-name m))))

  (edoc "A buffer's mode record, the current buffer's without an argument, or #f."
        (b (list-of buffer) "the buffer, at most one")
        (returns (or (record mode) #f)))
  (define (mode-of . b)
    (let ([n (head:buffer-fact (the-buffer b) 'mode #f)]) (and n (find-mode n))))

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
  (define tab-overrides (kernel:make-registry)) ; entries (mode flag)

  (define (own-entry registry name)
    (kernel:registry-find registry (lambda (x) (string=? (car x) name))))

  (define (inherited-entry registry name)
    (or (own-entry registry name)
        (let ([m (find-mode name)])
          (and m (mode-parent m) (inherited-entry registry (mode-parent m))))))

  (define (indenter-entry name)
    (inherited-entry indenters name))

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
      (kernel:registry-add! tab-overrides (list name flag))))

  (edoc "A mode's indenter, (proc buffer from to), or #f."
        (name string "the mode's name")
        (returns (or procedure #f)))
  (define (indenter name)
    (let ([entry (indenter-entry name)]) (and entry (cadr entry))))

  (edoc "Whether TAB runs a mode's indenter."
        (name string "the mode's name")
        (returns boolean))
  (define (indent-on-tab? name)
    (let ([override (own-entry tab-overrides name)] [entry (own-entry indenters name)])
      (cond [(not (indenter-entry name)) #f]
            [override (and (cadr override) #t)]
            [entry (and (caddr entry) #t)]
            [else (let ([m (find-mode name)])
                    (and m (mode-parent m) (indent-on-tab? (mode-parent m))))])))

  (edoc "A mode's formatter, (proc buffer from to), or #f."
        (name string "the mode's name")
        (returns (or procedure #f)))
  (define (formatter name)
    (let ([entry (inherited-entry formatters name)])
      (and entry (cadr entry))))

  (define (no-styles s) #f)

  ;; Computed styles, memoized per line string.  Edits replace line
  ;; strings (never mutate them), so string identity keys the cache and
  ;; can never go stale; weak keys keep it bounded by the live lines.
  ;; Each entry remembers the effective styler, so a parent's replacement
  ;; invalidates derived-mode results even when the child record is unchanged.

  (define style-cache (make-weak-eq-hashtable))

  (edoc "The memoized line styles function of a buffer's mode; every line plain without one, and a raising mode styles plain."
        (b buffer "the buffer")
        (returns procedure)
        (effects internal))
  (define (buffer-line-styles b)
    ;; The line-styles function of b's mode; unstyled without one.
    (let* ([m (mode-of b)] [styler (and m (mode-styles m))])
      (if styler
          (lambda (s)
            (let ([hit (eq-hashtable-ref style-cache s #f)])
              (if (and hit (eq? (car hit) styler))
                  (cdr hit)
                  ;; a raising mode styles the line plain rather than
                  ;; taking the redraw (and the editor) down
                  (let ([styles (guard (ex [else #f])
                                  (styler s))])
                    (eq-hashtable-set! style-cache s (cons styler styles))
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

  (edoc "Re-resolve every buffer's mode: a buffer with a mode keeps it by name, picking up a reloaded record; a buffer without one that follows detection takes the mode detection now finds.")
  (define (refresh-buffer-modes!)
    ;; A detected or chosen mode stays: registration is additive, never a
    ;; theft. A mode gone from the registry leaves its name on the buffer,
    ;; plain text until it returns.
    (for-each (lambda (b)
                (let ([name (head:buffer-fact b 'mode #f)])
                  (cond [(not name) (when (head:buffer-mode-auto b) (assign-mode! b))]
                        [(find-mode name) => (lambda (m) (set-mode-of! b m))]
                        [else (void)])))
              (head:buffers)))

  ;;; The head's adopt hook -------------------------------------------------------

  ;; a foreign buffer adopted with no mode fact yet gets detection,
  ;; recorded as the shared fact
  (define adopt-hooked (head:set-adopt-hook! assign-mode!))

) ;; library (mode)
