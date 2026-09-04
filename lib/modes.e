;; modes.e -- the mode registry: the library (modes), v2 core
;; dissolution (docs/DESIGN2.md).
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
;; (modes:register! "scheme" '(".ss") '("scheme") styler),
;; (modes:of b), ((modes:styles m) line).

(library (modes)
  (export (rename (mode-name name)
                  (mode-extensions extensions)
                  (mode-interpreters interpreters)
                  (mode-styles styles) (mode-render render)
                  (mode-row-styles row-styles)
                  (register-mode! register!)
                  (add-mode-extension! add-extension!)
                  (find-mode find) (assign-mode! assign!)
                  (set-buffer-mode! choose!) (mode-of of)
                  (buffer-mode-name name-of)
                  (buffer-line-styles line-styles)
                  (memoize-buffer-analysis memoize-analysis)
                  (refresh-buffer-modes! refresh!))
          mode?)
  (import (rnrs)
          (only (chezscheme)
                make-weak-eq-hashtable eq-hashtable-ref eq-hashtable-set!
                vector-copy void)
          (prefix (kernel) kernel:)
          (prefix (head) head:)
          (prefix (strings) strings:))

  ;;; The registry ------------------------------------------------------------

  ;; A mode provides syntax highlighting for the buffers it matches.
  ;; Extension modules call register! with the mode's name, the
  ;; file-name endings it claims, the interpreter names recognized in
  ;; a #! first line (for files without a matching extension), and a
  ;; styles function mapping a line to a vector of per-column style
  ;; symbols understood by styles:style-code, or #f for an unstyled
  ;; line.  Brackets styled 'delimiter take part in bracket matching;
  ;; in a buffer without a mode every bracket counts.

  (define-record-type mode
    (fields name extensions interpreters styles
            ;; optional display transform: (render buffer row line) ->
            ;; a string of the SAME length, or a same-length vector containing
            ;; one display string per logical cell. The latter permits a cell
            ;; to contain a grapheme and represents a wide glyph's continuation
            ;; with "". The buffer text is untouched and columns stay 1:1.
            render
            ;; optional buffer-aware styling: (row-styles buffer row
            ;; line) -> a styles vector, or #f for the plain styles
            ;; function.  Uncached by the core -- the mode memoizes.
            row-styles)
    (protocol (lambda (new)
                (case-lambda
                  [(n e i s) (new n e i s #f #f)]
                  [(n e i s r) (new n e i s r #f)]
                  [(n e i s r rs) (new n e i s r rs)]))))

  (define modes (kernel:make-registry))

  (define mode-extension-additions (kernel:make-registry))

  (define (register-mode! name extensions interpreters styles . extra)
    ;; extra: an optional render transform, then an optional
    ;; buffer-aware row-styles procedure (see the mode record).
    (kernel:registry-add! modes
      (make-mode name extensions interpreters styles
                 (and (pair? extra) (car extra))
                 (and (pair? extra) (pair? (cdr extra)) (cadr extra)))))

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

  (define (detect-mode path first-line)
    ;; The mode for a file: by extension, then by the #! interpreter line.
    (or (and path
             (let ([addition
                    (find (lambda (entry)
                            (strings:suffix? (car entry) path))
                          (kernel:registry-items mode-extension-additions))])
               (and addition (find-mode (cdr addition)))))
        (and path
             (kernel:registry-find modes
               (lambda (m)
                 (exists (lambda (ext) (strings:suffix? ext path))
                         (mode-extensions m)))))
        (and (strings:prefix? "#!" first-line)
             (kernel:registry-find modes
               (lambda (m)
                 (exists (lambda (name)
                           (strings:search first-line name 0
                                           (string-length first-line)))
                         (mode-interpreters m)))))))

  (define (assign-mode! b)
    (set-mode-of! b
      (detect-mode (head:buffer-file b) (vector-ref (head:buffer-lines b) 0)))
    (head:buffer-mode-auto-set! b #t))

  (define (find-mode name)
    (kernel:registry-find modes (lambda (m) (string=? (mode-name m) name))))

  (define (set-buffer-mode! b name)
    ;; Give b the registered mode called name (#f for none), regardless of
    ;; its file name -- how transcript (head:buffers) get their highlighting.
    (set-mode-of! b (and name (find-mode name)))
    (head:buffer-mode-auto-set! b #f))

  (define (buffer-mode-name b)
    ;; The name of b's mode, or #f without one.
    (let ([m (mode-of b)]) (and m (mode-name m))))

  (define (mode-of b)
    (let ([n (head:buffer-fact b 'mode #f)]) (and n (find-mode n))))

  (define (set-mode-of! b m)
    (head:buffer-fact-set! b 'mode (and m (mode-name m))))
  ;; The head's window tree lives in the (head) seam module now: the
  ;; records, the split geometry, and the seat state (windows, the
  ;; layout root, the selected window, the layout's divider output).
  ;; Facade aliases and identifier-syntax facades below; app-aware
  ;; layout surgery (set-layout-root!), wrap policy, and the main loop
  ;; stay here until the rest of the head moves.

  (define (no-styles s) #f)

  ;; Computed styles, memoized per line string.  Edits replace line
  ;; strings (never mutate them), so string identity keys the cache and
  ;; can never go stale; weak keys keep it bounded by the live lines.
  ;; Each entry remembers its mode, in case an identical string is shared
  ;; between (head:buffers) of different modes.

  (define style-cache (make-weak-eq-hashtable))

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

  ;; Faces may be recolored from config.e. Overrides are owned registrations,
  ;; so dropping the line from config.e and reloading restores the default.
  ;; Faces and the style DSL live in the (styles) seam module now;
  ;; the core keeps these facade aliases until its call sites and the
  ;; extension modules migrate to styles: prefixes, and installs the
  ;; repaint trigger for face redefinitions (painted rows are cached
  ;; by content, not by face definitions).

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

  (define (refresh-buffer-modes!)
    ;; Re-resolve every buffer's mode by name, so (head:buffers) pick up a
    ;; reloaded mode's new styles (or lose a mode that is gone).
    (for-each (lambda (b)
                (if (head:buffer-mode-auto b)
                    (assign-mode! b)
                    (let ([m (mode-of b)])
                      (when m
                        (set-mode-of! b (find-mode (mode-name m)))))))
              (head:buffers)))

  ;; Saving a module's source reloads it on the spot (a fresh .e file
  ;; in the lib directory is loaded for the first time), and saving
  ;; config.e applies it, so editing the editor from inside itself
  ;; takes effect on save.  Both on by default; (modules-reload-on-save
  ;; #f) or (config-reload-on-save #f) -- in config.e for an
  ;; installation, at M-x for a session -- turns either off.


  ;;; The head's adopt hook -------------------------------------------------------

  ;; a foreign buffer adopted with no mode fact yet gets detection,
  ;; recorded as the shared fact
  (define adopt-hooked (head:set-adopt-hook! assign-mode!))

) ;; library (modes)
