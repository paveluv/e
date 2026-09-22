;; literal.sls -- the values that print as the expressions reading them back.
;;
;; A buffer prints as (buffer "name"), a window as (window n), a region as
;; (region (buffer "name") '(row . col) '(row . col)), an actor's identity
;; as (head "desk") or (agent "claude"): what *eval* shows can be pasted
;; into the next expression, and M-x completes an argument of one of these
;; types to the same spelling. This library owns the constructors,
;; the printers and the edoc types behind those spellings. The kernel
;; imports it bare, so the names read as literals at the top level while
;; every other module arrives under its prefix.

(import (only (foundation edoc) elibrary))
(elibrary (head literal)
  (export agent base buffer head region region-buffer region-end region-start region? window)
  (import (chezscheme)
          (prefix (core identity) identity:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head head) head:)
          (prefix (head mode) mode:)
          (prefix (service file) file:)
          (prefix (state actor) actor:))

  ;;; Buffers and windows -------------------------------------------------------

  ;; The lookup is by name at evaluation time, so a killed buffer's form
  ;; reports itself. Window numbers are reused: (window 1) names whatever
  ;; window holds the number when it is evaluated.
  (edoc "The buffer with a given name, as buffers print: (buffer name); an error when there is none."
        (name buffer-name "the buffer's name")
        (returns buffer))
  (define (buffer name)
    (or (head:buffer-named name) (error 'buffer "no buffer named" name)))

  (edoc "The window numbered n at the left of its status line, as windows print: (window n); an error when there is none."
        (n integer "the window's number")
        (returns window))
  (define (window n)
    (or (head:window-numbered n) (error 'window "no window numbered" n)))

  (define buffer-printing
    (record-writer (record-type-descriptor head:buffer)
      (lambda (r p wr)
        (display "(buffer " p)
        (wr (head:buffer-name r) p)
        (display ")" p))))

  (define window-printing
    (record-writer (record-type-descriptor head:window)
      (lambda (r p wr)
        (display "(window " p)
        (wr (head:window-index r) p)
        (display ")" p))))

  ;;; Regions ------------------------------------------------------------------

  (edoc "A slice of one buffer between two (row . col) points."
        (buffer buffer "the buffer the slice is in")
        (start position "where it starts")
        (end position "where it ends"))
  (define-record-type (region-record make-region region?)
    (fields (immutable buffer region-buffer)
            (immutable start region-start)
            (immutable end region-end)))

  (define (point<? a b)
    (or (< (car a) (car b))
        (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

  (edoc "The slice of buffer b between two (row . col) points, given in either order."
        (b buffer "the buffer the slice is in")
        (start position "one end")
        (end position "the other end")
        (returns region))
  (define (region b start end)
    (if (point<? end start)
        (make-region b end start)
        (make-region b start end)))

  (define region-printing
    (record-writer (record-type-descriptor region-record)
      (lambda (r p wr)
        (display "(region " p)
        (wr (region-buffer r) p)
        (display " '" p) (wr (region-start r) p)
        (display " '" p) (wr (region-end r) p)
        (display ")" p))))

  ;;; Actors -------------------------------------------------------------------

  ;; An identity is plain data, (kind name more ...), and prints as the call
  ;; that makes it: (head "desk"), (agent "claude") and (base 'e) read back
  ;; without a quote, like a buffer or a window. The constructors check the
  ;; name; the store, the log and the directory take the lists they return.

  (define (identity kind name more)
    (let ([who (cons kind (cons name more))])
      (unless (identity:valid? who)
        (error kind "expected a symbol or a nonempty string as the name" name))
      who))

  (edoc "A head's identity, as heads print: (head name)."
        (name (or head-name symbol) "the head's name")
        (more (list-of datum) "further fields, rarely")
        (returns head))
  (define (head name . more) (identity 'head name more))

  (edoc "An agent's identity, as agents print: (agent name)."
        (name (or agent-name symbol) "the agent's name")
        (more (list-of datum) "further fields, rarely")
        (returns actor))
  (define (agent name . more) (identity 'agent name more))

  (edoc "The base's identity, as it prints: (base name)."
        (name (or string symbol) "the base's name, e by default")
        (more (list-of datum) "further fields, rarely")
        (returns actor))
  (define (base name . more) (identity 'base name more))

  (define (spell-datum x)
    ;; a field of an identity as the expression denoting it
    (if (or (string? x) (number? x) (boolean? x) (char? x)) (format "~s" x) (format "'~s" x)))

  (define (spell-identity who)
    (string-append "(" (symbol->string (car who))
                   (apply string-append (map (lambda (x) (string-append " " (spell-datum x))) (cdr who)))
                   ")"))

  (define (head? v) (and (identity:valid? v) (eq? (car v) 'head)))

  (define (directory-entries kind)
    ;; (identity . hint) for the registered actors, of one kind or of all;
    ;; an entry is (identity kind name attached-at capabilities)
    (fold-right
      (lambda (entry out)
        (if (or (not kind) (eq? (cadr entry) kind))
            (cons (cons (car entry) (let ([c (list-ref entry 4)]) (and (string? c) c))) out)
            out))
      '() (actor:attached)))

  (define (directory-names kind)
    ;; (name . hint) for the registered actors of a kind, the display names
    (map (lambda (entry) (cons (let ([name (cadr (car entry))]) (if (symbol? name) (symbol->string name) name)) (cdr entry)))
         (directory-entries kind)))

  (define (nonempty-string? v) (and (string? v) (> (string-length v) 0)))

  ;;; Types ---------------------------------------------------------------------

  ;; The literal notions as edoc types: what M-x offers at an argument of
  ;; that type, how a value is spelled as an expression, and what a value
  ;; must be. A buffer or a window is live, on this seat, now; an actor is
  ;; any identity, offered from the directory.


  (define (buffer-details b)
    ;; what a completion row shows beside a buffer
    (string:join
      (filter values
        (list (let ([file (head:buffer-file b)]) (and file (file:abbreviate file)))
              (mode:name-of b)
              (and (head:buffer-modified b) "modified")))
      "  "))

  (edoc-type buffer "a buffer, spelled (buffer \"name\"); completion offers the live ones"
    (predicate head:buffer?)
    (complete (lambda (partial) (map (lambda (b) (cons b (buffer-details b))) (head:buffers))))
    (read buffer)
    (write (lambda (b) (format "(buffer ~s)" (head:buffer-name b)))))

  (edoc-type buffer-name "the name of a live buffer"
    (predicate (lambda (v) (and (string? v) (head:buffer-named v) #t)))
    (complete (lambda (partial) (map (lambda (b) (cons (head:buffer-name b) (buffer-details b))) (head:buffers))))
    (write (lambda (v) (format "~s" v))))

  (edoc-type window "a window, spelled (window n); completion offers those on screen"
    (predicate head:window?)
    (complete (lambda (partial)
                (map (lambda (w) (cons w (head:buffer-name (head:window-buffer w))))
                     (remq (head:popup) (head:windows)))))
    (read window)
    (write (lambda (w) (format "(window ~a)" (head:window-index w)))))

  (edoc-type region "a slice of one buffer between two (row . col) points"
    (predicate region?))

  (edoc-type position "a (row . col) position in a buffer"
    (predicate text:position?))

  (edoc-type actor "an actor's identity, spelled (head \"desk\") or (agent \"claude\")"
    (predicate identity:valid?)
    (complete (lambda (partial) (directory-entries #f)))
    (write spell-identity))

  (edoc-type head "a head's identity, spelled (head \"name\")"
    (predicate head?)
    (complete (lambda (partial) (directory-entries 'head)))
    (write spell-identity)
    (within actor))

  (edoc-type head-name "the name of a head, registered or not"
    (predicate nonempty-string?)
    (complete (lambda (partial) (directory-names 'head)))
    (write (lambda (v) (format "~s" v)))
    (within string))

  (edoc-type agent-name "the name of an agent, registered or not"
    (predicate nonempty-string?)
    (complete (lambda (partial) (directory-names 'agent)))
    (write (lambda (v) (format "~s" v)))
    (within string))
)
