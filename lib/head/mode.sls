;; mode.sls -- the mode registry: the library (mode).
;;
;; A buffer's mode NAME is a store property every head reads; a mode
;; record is this head's registry object for that name: the file-name
;; endings and #! interpreters it claims, its line styler, and
;; optionally a display transform and a whole-source row styler.
;; Detection turns a path and a first line into a mode; the memoized
;; stylers answer per-line and per-row questions without re-analysis
;; -- line styles keyed by line-string identity (edits replace
;; strings, never mutate them), whole-text analyses by immutable text identity.
;;
;; The painter imports this module directly, and the head's adopt
;; hook is installed here: a foreign buffer adopted without a mode
;; fact gets detection.  Exported names drop the module stem:
;; (mode:register! "scheme" '(".ss") '("scheme") styler),
;; (mode:of b), ((mode:line-styles m) line). Presentation callbacks
;; consume mode:source snapshots rather than head buffer records.

(import (only (foundation edoc) elibrary))
(elibrary (head mode)
  (export add-context! (rename (add-mode-extension! add-extension!)) add-highlighter! (rename (assign-current-mode! assign!))
          (rename (set-buffer-mode! choose!)) derive! (rename (detect-mode detect))
          (rename (mode-extensions extensions)) (rename (find-mode find)) formatter
          highlights indent indent-on-tab! indent-on-tab? indenter (rename (mode-interpreters interpreters))
          key-context key-contexts line-styles
          memoize-analysis mode? (rename (mode-name name))
          (rename (buffer-mode-name name-of)) (rename (mode-of of))
          (rename (refresh-buffer-modes! refresh!)) (rename (register-mode! register!))
          register-formatter! register-indenter! (rename (mode-render render)) required-facts
          (rename (mode-row-styles row-styles)) source source-fact source-lines (rename (mode-styles styles)))
  (import (rnrs)
          (only (chezscheme) record-writer)
          (only (chezscheme)
                make-weak-eq-hashtable eq-hashtable-ref eq-hashtable-set!
                list-head vector-copy void)
          (prefix (core kernel) kernel:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head head) head:)
          (prefix (head render) render:))

  ;; Mode callbacks receive explicit text and only the facts they declare.
  ;; Neither the snapshot nor its analysis cache owns a buffer or window.
  (edoc "Read-only source input shared by text presentations."
        (lines any "immutable vector or deferred text") (facts list "declared presentation facts"))
  (define-record-type (presentation-source %make-source presentation-source?)
    (fields (immutable lines source-lines) (immutable facts source-facts)))

  (edoc "Capture immutable presentation text and the mode's declared facts, without a buffer or window. Callbacks borrow these values read-only; geometry and document mutation belong to their host."
        (lines any "immutable vector or deferred text") (facts list "declared (name . value) inputs")
        (returns (record presentation-source)))
  (define (source lines facts)
    (unless (and (or (vector? lines) (render:deferred? lines))
              (list? facts) (for-all (lambda (p) (and (pair? p) (symbol? (car p)))) facts))
      (error 'source "expected text and named facts"))
    (%make-source lines facts))

  (edoc "Read an explicitly supplied presentation fact; undeclared facts use the fallback. No store or host lookup is performed."
        (source (record presentation-source) "presentation snapshot") (key symbol "fact") (fallback any "value when absent") (returns any))
  (define (source-fact source key fallback)
    (cond [(assq key (source-facts source)) => cdr] [else fallback]))

  (define highlighters (kernel:make-registry))

  (edoc "Register a pure context highlighter, owned by its module. It receives (source mode caret) and returns ((span-datum face) ...) in logical characters. Use bounded computation and no I/O; asynchronous tool results belong in annotation inputs."
        (proc procedure "explicit presentation callback"))
  (define (add-highlighter! proc)
    (unless (procedure? proc) (error 'add-highlighter! "expected a procedure"))
    (kernel:registry-add! highlighters proc))

  (edoc "Compute logical context highlights for a focused text view. Invalid or raising providers contribute nothing, and deferred app displays do not invoke text highlighters."
        (source (record presentation-source) "borrowed text and declared facts") (mode any "resolved mode or false")
        (caret position "logical caret") (returns list "((span-datum face) ...)"))
  (define (highlights source mode caret)
    (let ([lines (source-lines source)])
      (if (not (vector? lines)) '()
        (apply append
          (map (lambda (proc)
                 (guard (ex [else '()])
                   (let ([ranges (proc source mode caret)])
                     (if (and (list? ranges)
                           (for-all (lambda (r)
                                      (and (list? r) (= (length r) 2) (symbol? (cadr r))
                                        (let ([s (text:datum->span (car r))])
                                          (for-all (lambda (p) (and (< (car p) (vector-length lines))
                                                                 (<= (cdr p) (string-length (vector-ref lines (car p))))))
                                            (list (text:span-start s) (text:span-end s)))))) ranges)) ranges '()))))
            (kernel:registry-items highlighters))))))

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
        (render (or procedure #f) "(render source row line) giving a same-length display transform, or #f")
        (row-styles (or procedure #f) "(row-styles source row line) giving a styles vector, or #f for the plain styles")
        (facts list "named inputs required by these presentation callbacks")
        (parent (or mode #f) "the parent of a derived mode"))
  (define-record-type mode
    (fields name extensions interpreters (immutable styles own-styles)
            ;; optional display transform: (render source row line) ->
            ;; a string of the SAME character length, or a same-length vector
            ;; of strings whose concatenation meets that contract. Character
            ;; to cell geometry must match the source (including clusters).
            ;; Invalid substitutions paint source; character styles project
            ;; to every cell of the leading character's glyph.
            (immutable render own-render)
            ;; optional whole-source styling: (row-styles source row
            ;; line) -> a styles vector, or #f for the plain styles
            ;; function.  Uncached here -- the mode memoizes.
            (immutable row-styles own-row-styles) facts parent))

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

  (define (argument xs n fallback) (if (< n (length xs)) (list-ref xs n) fallback))
  (define (check-presentation! extra slots)
    (unless (and (<= (length extra) (+ slots 1))
              (for-all (lambda (p) (or (not p) (procedure? p))) (list-head extra (min slots (length extra))))
              (let ([facts (argument extra slots '())])
                (and (list? facts) (for-all symbol? facts))))
      (error 'mode "expected presentation procedures followed by optional fact names" extra)))

  (edoc "Register a mode and re-resolve open buffers. Render and row-style callbacks receive an explicit mode:source snapshot, row and line; their optional fact names declare the metadata the host supplies."
        (name string "the mode's name")
        (extensions (list-of string) "the file-name endings")
        (interpreters (list-of string) "the #! interpreter names")
        (styles (or procedure #f) "line to styles vector, or #f")
        (extra (list-of (or procedure #f (list-of symbol))) "optional render transform, row-styles procedure and fact names"))
  (define (register-mode! name extensions interpreters styles . extra)
    (check-presentation! extra 2)
    (kernel:registry-add! modes
      (make-mode name extensions interpreters styles
                 (argument extra 0 #f) (argument extra 1 #f) (argument extra 2 '()) #f))
    (refresh-buffer-modes!))

  (edoc "Register a submode: a distinct mode with a parent, following the parent's current styles, rendering, row styles, indentation, formatting, Tab policy and key bindings except where its own optional styles, render transform and row styles override them; endings belong only to the new mode, the parent may register later, and a cycle is refused."
        (name string "the new mode name")
        (parent string "the parent mode's name")
        (extensions (list-of string) "the new mode's file endings")
        (extra (list-of (or procedure #f (list-of symbol))) "optional own line styles, render transform, row-styles procedure and fact names"))
  (define (derive! name parent extensions . extra)
    (check-presentation! extra 3)
    (let walk ([next parent] [seen (list name)])
      (when (member next seen) (error 'derive! "cyclic mode derivation" name parent))
      (let ([m (find-mode next)])
        (when (and m (mode-parent m)) (walk (mode-parent m) (cons next seen)))))
    (kernel:registry-add! modes
      (make-mode name extensions '()
                 (argument extra 0 #f) (argument extra 1 #f) (argument extra 2 #f)
                 (argument extra 3 '()) parent))
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

  (edoc "The distinct fact names needed by a mode's effective render and row-style callbacks. Inherited callbacks retain their requirements; overridden callbacks drop theirs."
        (m (or (record mode) #f) "resolved mode") (returns list))
  (define (required-facts m)
    (define (needs m get)
      (cond [(not m) '()] [(get m) (mode-facts m)]
        [(mode-parent m) (needs (find-mode (mode-parent m)) get)] [else '()]))
    (fold-left (lambda (out name) (if (memq name out) out (append out (list name)))) '()
      (append (needs m own-render) (needs m own-row-styles))))

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

  (define (detected-mode b)
    (or (detect-mode (or (head:buffer-file b)
                       (head:buffer-fact b 'source-file #f))
          (head:buffer-line b 0))
        (scratch-mode b)))

  (edoc "Give a buffer the mode its file and first line detect, Scheme for a *scratch* buffer, following detection from then on."
        (b buffer "the buffer"))
  (define (assign-mode! b)
    (set-mode-of! b (detected-mode b) #t))

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

  (edoc "The keymap context of a buffer's mode, named after it, or false."
        (b buffer "the buffer") (returns (or symbol #f)))
  (define (key-context b)
    (let ([name (buffer-mode-name b)]) (and name (string->symbol name))))

  ;; A context a buffer has by its state rather than its mode: merge while
  ;; its text holds conflict markers, say.  The app binding keys in it
  ;; registers the context with the predicate; the registration retracts
  ;; with the module.  Such a context comes before the mode's.
  (define state-contexts (kernel:make-registry))

  (edoc "Register a keymap context a buffer has while a predicate holds of it, before its mode's contexts: (mode:add-context! 'conflicted conflicted?) say, by the app that binds keys in the context."
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

  (edoc "Read a mode's inherited key contexts, nearest first. The legacy buffer adapter also prepends its state contexts."
        (source any "resolved mode, false, or legacy buffer") (returns (list-of symbol)))
  (define (key-contexts source)
    (if (head:buffer? source) (append (state-contexts-of source) (key-contexts (mode-of source)))
      (let loop ([m source] [out '()])
        (if (not m) (reverse out)
          (loop (and (mode-parent m) (find-mode (mode-parent m))) (cons (string->symbol (mode-name m)) out))))))

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
  ;; their text should start: (proc source from to) over a mode:source snapshot -> one entry per row
  ;; of from..to -- #f leaving a row alone, a column, or an ascending list
  ;; of columns when several indentations are valid.  A formatter rewrites
  ;; rows wholesale: (proc source from to) over a mode:source snapshot -> the replacement lines, or #f
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

  (edoc "Register a mode's indenter: (proc source from to) over a mode:source snapshot gives each row's column, its list of stops, or #f to leave it; tab says whether TAB runs it, on when omitted."
        (name mode "the mode")
        (proc procedure "the indenter")
        (tab boolean "whether TAB indents"))
  (define register-indenter!
    (case-lambda
      [(name proc) (register-indenter! name proc #t)]
      [(name proc tab) (kernel:registry-add! indenters (list name proc tab))]))

  (edoc "Register a mode's formatter: (proc source from to) over a mode:source snapshot gives the replacement lines, or #f when the rows cannot be formatted."
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

  (edoc "A mode's indenter, (proc source from to) over a mode:source snapshot, or #f."
        (name string "the mode's name")
        (returns (or procedure #f)))
  (define (indenter name)
    (let ([entry (indenter-entry name)]) (and entry (cadr entry))))

  (edoc "Compute indentation and logical positions against an immutable source, without editing. Cycle chooses the next stop and pads blank lines; otherwise use the nearest stop and leave blank lines unchanged. Returns proposed lines and positions, or false lines without an indenter."
        (name (or mode #f) "mode") (source (record presentation-source) "source snapshot") (from integer "first row") (to integer "last row")
        (cycle? boolean "cycle stops") (positions list "logical positions to preserve") (effects internal))
  (define (indent name source from to cycle? positions)
    (define (leading line)
      (let loop ([i 0]) (if (and (< i (string-length line)) (memv (string-ref line i) '(#\space #\tab))) (loop (+ i 1)) i)))
    (define (column? n) (and (integer? n) (exact? n) (>= n 0)))
    (let* ([proc (and name (indenter name))] [lines (source-lines source)] [last (min to (- (vector-length lines) 1))])
      (if (not proc) (values #f positions)
        (let ([columns (proc source from last)] [out lines])
          (unless (and (list? columns) (<= (length columns) (+ 1 (- last from)))
                    (for-all (lambda (c) (or (not c) (column? c) (and (list? c) (pair? c) (for-all column? c)))) columns))
            (error 'indent "invalid indenter result" columns))
          (let loop ([row from] [columns columns])
            (when (pair? columns)
              (let* ([line (vector-ref lines row)] [lead (leading line)] [stops (car columns)]
                     [column (if (pair? stops)
                               (if cycle? (or (find (lambda (n) (> n lead)) stops) (car stops))
                                 (fold-left (lambda (best n) (if (< (abs (- n lead)) (abs (- best lead))) n best)) (car stops) (cdr stops))) stops)]
                     [next (if (and column (or cycle? (< lead (string-length line))))
                             (string-append (make-string column #\space) (substring line lead (string-length line))) line)])
                (unless (string=? next line)
                  (when (eq? out lines) (set! out (vector-copy lines)))
                  (vector-set! out row next))
                (when (and column (or cycle? (< lead (string-length line))))
                  (set! positions (map (lambda (p) (if (= (car p) row)
                                                     (cons row (if (<= (cdr p) lead) column (+ (cdr p) (- column lead)))) p)) positions)))
                (loop (+ row 1) (cdr columns)))))
          (values out positions)))))

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

  (edoc "A mode's formatter, (proc source from to) over a mode:source snapshot, or #f."
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

  (edoc "The memoized line styler of an explicitly resolved mode, or plain without one. A raising mode styles plain."
        (m (or (record mode) #f) "resolved mode")
        (returns procedure)
        (effects internal))
  (define (line-styles m)
    (let ([styler (and m (mode-styles m))])
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

  (edoc "Memoize a text-only analyzer across presentations of the same immutable text. The returned (source row) provider needs no buffer or window. The analyzer receives an owned vector of borrowed immutable lines and returns a row vector."
        (analyze procedure "(analyze lines) giving the analysis")
        (returns procedure))
  (define (memoize-analysis analyze)
    (let ([cache (make-weak-eq-hashtable)])
      (lambda (source row)
        (let* ([lines (source-lines source)] [hit (eq-hashtable-ref cache lines #f)]
               [product (or hit (analyze (vector-copy (render:lines-vector lines))))])
          (unless hit (eq-hashtable-set! cache lines product))
          (and (<= 0 row) (< row (vector-length product)) (vector-ref product row))))))

  (edoc "Re-resolve every buffer's mode: a buffer with a mode keeps it by name, picking up a reloaded record; a buffer without one that follows detection takes the mode detection now finds.")
  (define (refresh-buffer-modes!)
    ;; A detected or chosen mode stays: registration is additive, never a
    ;; theft. A mode gone from the registry leaves its name on the buffer,
    ;; plain text until it returns. Readers resolve that name on every use;
    ;; only newly successful detection needs to change shared facts.
    (for-each (lambda (b)
                (when (and (not (head:buffer-fact b 'mode #f)) (head:buffer-mode-auto b))
                  (let ([m (detected-mode b)])
                    (when m (set-mode-of! b m #t)))))
              (head:buffers)))

  ;;; The head's adopt hook -------------------------------------------------------

  ;; a foreign buffer adopted with no mode fact yet gets detection,
  ;; recorded as the shared fact
  (define adopt-hooked (head:set-adopt-hook! assign-mode!))

) ;; library (mode)
