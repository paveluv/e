;; literal.sls -- the values that print as the expressions reading them back.
;;
;; A buffer prints as (buffer "name"), a window as (window n), a region as
;; (region (buffer "name") '(row . col) '(row . col)): what *eval* shows can
;; be pasted into the next expression, and M-x completes an argument of one
;; of these types to the same spelling. This library owns the constructors,
;; the printers and the edoc types behind those spellings. The kernel
;; imports it bare, so the names read as literals at the top level while
;; every other module arrives under its prefix.

(import (only (edoc) elibrary))
(elibrary (literal)
  (export buffer window region region? region-buffer region-start region-end)
  (import (chezscheme)
          (prefix (head) head:)
          (prefix (file) file:)
          (prefix (mode) mode:)
          (prefix (string) string:)
          (prefix (text) text:))

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

  ;;; Types ---------------------------------------------------------------------

  ;; The literal notions as edoc types: what M-x offers at an argument of
  ;; that type, how a value is spelled as an expression, and what a value
  ;; must be. A buffer or a window is live, on this seat, now.

  (define (live-buffer? v) (and (head:buffer? v) (memq v (head:buffers)) #t))

  (define (buffer-details b)
    ;; what a completion row shows beside a buffer
    (string:join
      (filter values
        (list (let ([file (head:buffer-file b)]) (and file (file:abbreviate file)))
              (mode:name-of b)
              (and (head:buffer-modified b) "modified")))
      "  "))

  (edoc-type buffer "a live buffer, spelled (buffer \"name\")"
    (predicate live-buffer?)
    (complete (lambda (partial) (map (lambda (b) (cons b (buffer-details b))) (head:buffers))))
    (read buffer)
    (write (lambda (b) (format "(buffer ~s)" (head:buffer-name b)))))

  (edoc-type buffer-name "the name of a live buffer"
    (predicate (lambda (v) (and (string? v) (head:buffer-named v) #t)))
    (complete (lambda (partial) (map (lambda (b) (cons (head:buffer-name b) (buffer-details b))) (head:buffers))))
    (write (lambda (v) (format "~s" v))))

  (edoc-type window "a window on screen, spelled (window n)"
    (predicate (lambda (v) (and (head:window? v) (memq v (head:windows)) #t)))
    (complete (lambda (partial)
                (map (lambda (w) (cons w (head:buffer-name (head:window-buffer w))))
                     (remq (head:popup) (head:windows)))))
    (read window)
    (write (lambda (w) (format "(window ~a)" (head:window-index w)))))

  (edoc-type region "a slice of one buffer between two (row . col) points"
    (predicate region?))

  (edoc-type position "a (row . col) position in a buffer"
    (predicate text:position?))
)
