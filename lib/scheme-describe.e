;; scheme-describe.e -- a Scheme manual inside the editor: the library
;; (scheme-describe).
;;
;; An e extension module: the library (scheme-describe), loaded at
;; startup by the core.  Serves the reference documentation for the
;; whole default corpus -- R6RS from TSPL4, the Chez extensions from the
;; Chez Scheme User's Guide -- extracted into reference/describe.sdata
;; by reference/download.sh (run it once; the corpus is not in git).
;;
;;   M-x (describe eq-hashtable-ref)
;;
;; pops up a *Describe* buffer with the entry: its forms, what it
;; returns, its libraries, source, and the full prose.  The database is
;; structured and queryable: doc-lookup returns the entries for a name,
;; doc-entries all of them (optionally filtered), and the doc-*
;; accessors take them apart, so M-x expressions can slice the corpus:
;;
;;   (length (doc-entries))
;;   (doc-entries (lambda (e) (eq? (doc-source e) 'csug)))
;;   (filter (lambda (e) (member "(rnrs io ports)" (doc-libraries e)))
;;           (doc-entries))

(library (scheme-describe)
  (export describe describe!
          doc-lookup doc-entries
          doc-names doc-forms doc-returns doc-libraries
          doc-source doc-chapter doc-url doc-description)
  (import (chezscheme) (core))

  (define-record-type (doc-entry make-doc-entry doc-entry?)
    (fields (immutable names doc-names)           ; symbols defined
            (immutable forms doc-forms)           ; ((kind . template) ...)
            (immutable returns doc-returns)       ; string or #f
            (immutable libraries doc-libraries)   ; ("(rnrs base)" ...)
            (immutable source doc-source)         ; tspl or csug
            (immutable chapter doc-chapter)       ; chapter title
            (immutable url doc-url)               ; page anchor
            (immutable description doc-description)))

  ;;; Loading -------------------------------------------------------------------

  (define (data-path)
    (string-append (caar (library-directories))
                   "/../reference/describe.sdata"))

  (define all-entries #f)   ; list of doc-entry, or #f before loading
  (define by-name #f)       ; symbol -> (doc-entry ...), tspl first

  (define (load-data!)
    (unless all-entries
      (unless (file-exists? (data-path))
        (error 'describe
               "no reference data -- run reference/download.sh once"))
      (set! all-entries
        (map (lambda (e) (apply make-doc-entry e))
             (with-input-from-file (data-path) read)))
      (set! by-name (make-eq-hashtable))
      (for-each (lambda (entry)
                  (for-each (lambda (name)
                              (eq-hashtable-update! by-name name
                                (lambda (old) (cons entry old)) '()))
                            (doc-names entry)))
                (reverse all-entries))))

  ;;; Queries -------------------------------------------------------------------

  (define (doc-lookup name)
    ;; The entries documenting name (a symbol or its string), TSPL
    ;; before CSUG; '() when it is not in the corpus.
    (load-data!)
    (eq-hashtable-ref by-name
                      (if (string? name) (string->symbol name) name)
                      '()))

  (define (doc-entries . maybe-pred)
    ;; Every entry, or those a predicate accepts.
    (load-data!)
    (if (pair? maybe-pred)
        (filter (car maybe-pred) all-entries)
        all-entries))

  ;;; Display -------------------------------------------------------------------

  (define (wrap-line s width)
    ;; s broken at spaces into lines of at most width columns; a line
    ;; without spaces (or short enough) stays whole.
    (let loop ([start 0] [acc '()])
      (let ([n (string-length s)])
        (if (<= (- n start) width)
            (reverse (cons (substring s start n) acc))
            (let find ([i (min (+ start width) (- n 1))])
              (cond [(<= i start)
                     ;; no space to break at: take the whole rest
                     (reverse (cons (substring s start n) acc))]
                    [(char=? (string-ref s i) #\space)
                     (loop (+ i 1) (cons (substring s start i) acc))]
                    [else (find (- i 1))]))))))

  (define (wrapped-lines text width)
    (apply append
           (map (lambda (line) (wrap-line line width))
                (split-on-newlines text))))

  (define (split-on-newlines s)
    (let loop ([i 0] [start 0] [acc '()])
      (cond [(= i (string-length s))
             (reverse (cons (substring s start i) acc))]
            [(char=? (string-ref s i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
            [else (loop (+ i 1) start acc)])))

  (define (entry-lines entry)
    (append
      (map (lambda (form)
             (format "~a: ~a" (car form) (cdr form)))
           (doc-forms entry))
      (if (doc-returns entry)
          (wrapped-lines (format "returns: ~a" (doc-returns entry)) 72)
          '())
      (if (pair? (doc-libraries entry))
          (list (format "libraries: ~a"
                        (string-join (doc-libraries entry) ", ")))
          '())
      (list (format "source: ~a, ~a  [~a]"
                    (case (doc-source entry)
                      [(tspl) "TSPL4"]
                      [(csug) "Chez Scheme User's Guide"]
                      [else (doc-source entry)])
                    (doc-chapter entry)
                    (doc-url entry))
            "")
      (wrapped-lines (doc-description entry) 72)))

  (define (describe! name)
    ;; Pop up a *Describe* buffer with the documentation for name (a
    ;; symbol or its string); every matching entry is shown.
    (let ([entries (doc-lookup name)])
      (if (null? entries)
          (set-message! (format "No documentation for ~a" name))
          (begin
            (let ([old (buffer-named "*Describe*")])
              (when old (kill-buffer! old)))
            (let ([b (new-buffer "*Describe*")])
              (apply buffer-append! b
                     (let loop ([es entries] [acc '()])
                       (if (null? es)
                           (reverse acc)
                           (loop (cdr es)
                                 (append
                                   (reverse (entry-lines (car es)))
                                   (if (null? acc)
                                       acc
                                       (cons "" (cons (make-string 72 #\-)
                                                      (cons "" acc)))))))))
              (set-buffer-read-only! b #t)
              (call-with-buffer b (lambda () (goto-point! '(0 . 0))))
              (if (display-buffer! b)
                  (set-message! "")
                  (set-message!
                    (format "~a: see the *Describe* buffer" name))))))
      (void)))

  (define-syntax describe
    (syntax-rules ()
      [(_ name) (describe! 'name)])))
