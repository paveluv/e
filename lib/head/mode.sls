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
;; Editors observe explicit documents for detection at acquisition. Exported
;; names drop the module stem:
;; (mode:register! "scheme" '(".ss") '("scheme") styler),
;; (mode:of b), ((mode:line-styles m) line). Presentation callbacks
;; consume mode:source snapshots rather than head buffer records.

(import (only (foundation edoc) elibrary))
(elibrary (head mode)
  (export (rename (add-mode-extension! add-extension!)) add-highlighter! (rename (assign-document! assign!))
          (rename (set-buffer-mode! choose!)) derive! (rename (detect-mode detect))
          (rename (mode-extensions extensions)) (rename (find-mode find)) formatter
          highlights indent indent-on-tab! indent-on-tab? indenter (rename (mode-interpreters interpreters))
          key-contexts line-styles
          memoize-analysis mode? (rename (mode-name name))
          (rename (buffer-mode-name name-of)) (rename (mode-of of))
          (rename (refresh-buffer-modes! refresh!)) (rename (register-mode! register!))
          register-formatter! register-indenter! (rename (mode-render render)) required-facts
          (rename (mode-row-styles row-styles)) source source-fact source-lines (rename (mode-styles styles)))
  (import (rnrs)
          (only (chezscheme) record-writer)
          (only (chezscheme) make-weak-eq-hashtable eq-hashtable-ref
            eq-hashtable-set! list-head vector-copy void equal-hash)
          (prefix (core kernel) kernel:)
          (prefix (core property) property:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head head) head:)
          (prefix (head render) render:)
          (prefix (state store) store:))

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
    (complete (lambda (partial) (map (lambda (m) (list (mode-name m) #f (mode-details m))) (kernel:registry-items modes))))
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

  (define documents (make-hashtable equal-hash equal?))
  (define (document-mode id first facts)
    (define (fact key) (cond [(assq key facts) => cdr] [else #f]))
    (or (detect-mode (or (fact 'file) (fact 'source-file)) first)
      (and (not (fact 'file)) (string:prefix? "*scratch*" (store:buffer-name id)) (find-mode "scheme"))))

  (edoc "Detect a document's mode from its file and first line, choosing Scheme for scratch documents. Resume automatic detection."
    (id buffer "document") (public))
  (define (assign-document! id)
    (let-values ([(text revision facts) (store:snapshot-state id)])
      (let ([m (document-mode id (vector-ref text 0) facts)])
        (hashtable-set! documents id #t)
        (store:set-properties! head:ui-actor id
          (list (cons 'mode (and m (mode-name m))) '(mode-auto . #t))
          (property:select facts '(mode mode-auto file source-file))))))

  (define (refresh-document! id)
    (hashtable-set! documents id #t)
    (when (and (not (store:property id 'mode #f)) (store:property id 'mode-auto #t))
      (let-values ([(text revision facts) (store:snapshot-state id)])
        ;; Recheck the coherent capture: an explicit choice can arrive while
        ;; acquiring it. Automatic detection never takes that choice away.
        (when (and (not (cond [(assq 'mode facts) => cdr] [else #f]))
                (cond [(assq 'mode-auto facts) => cdr] [else #t]))
          (let ([m (document-mode id (vector-ref text 0) facts)])
            (when (or m (not (assq 'mode facts)))
              (store:set-properties! head:ui-actor id
                (list (cons 'mode (and m (mode-name m))) '(mode-auto . #t))
                (property:select facts '(mode mode-auto file source-file)))))))))

  (edoc "The registered mode called name, or #f."
        (name mode "the mode's name")
        (returns (or (record mode) #f)))
  (define (find-mode name)
    (and (string? name)
         (kernel:registry-find modes (lambda (m) (string=? (mode-name m) name)))))

  (edoc "Assign a registered mode to an explicit document, or none with false. This choice overrides automatic detection until assign! is called."
    (name (or mode #f) "mode name, or false") (id buffer "document"))
  (define (set-buffer-mode! name id)
    (when (and name (not (find-mode name))) (error 'mode "mode is not registered" name))
    (edoc:type-value 'buffer id)
    (hashtable-set! documents id #t)
    (store:set-properties! head:ui-actor id (list (cons 'mode name) '(mode-auto . #f))))

  (edoc "Read a mode's inherited key contexts, nearest first."
    (source (or (record mode) #f) "resolved mode") (returns (list-of symbol)))
  (define (key-contexts source)
    (let loop ([m source] [out '()])
      (if (not m) (reverse out)
        (loop (and (mode-parent m) (find-mode (mode-parent m))) (cons (string->symbol (mode-name m)) out)))))

  (edoc "Read the registered mode name of an explicit document, or false without one."
    (id buffer "document") (returns (or string #f)))
  (define (buffer-mode-name id)
    (let ([m (mode-of id)]) (and m (mode-name m))))

  (edoc "Resolve an explicit document's mode in this head's registry, or false."
    (id buffer "document") (returns (or (record mode) #f)))
  (define (mode-of id) (find-mode (store:property id 'mode #f)))

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
        (flag boolean "whether TAB indents") (public))
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

  (edoc "Observe explicit documents once, preserving chosen modes and successful detection. With no arguments, revisit documents already observed by this head after registry changes. A failed detection remains eligible for modes registered later; repeated observation does no acquisition or publication."
    (ids (list-of buffer) "documents to observe"))
  (define (refresh-buffer-modes! . ids)
    ;; A detected or chosen mode stays: registration is additive, never a
    ;; theft. A mode gone from the registry leaves its name on the buffer,
    ;; plain text until it returns. Readers resolve that name on every use;
    ;; only newly successful detection needs to change shared facts.
    (for-each refresh-document!
      (if (pair? ids) (filter (lambda (id) (not (hashtable-contains? documents id))) (map (lambda (id) (edoc:type-value 'buffer id)) ids))
        (filter (lambda (id) (if (store:exists? id) #t (begin (hashtable-delete! documents id) #f)))
          (vector->list (hashtable-keys documents)))))
  )

) ;; library (mode)
