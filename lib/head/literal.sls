;; Legacy window constructor and completion provider.
;; Shared buffers, models and actors use ordinary quoted data.

(import (only (foundation edoc) elibrary))
(elibrary (head literal)
  (export window)
  (import (chezscheme)
          (prefix (head seat) seat:))

  ;;; Windows -------------------------------------------------------------------

  (edoc "The window numbered n at the left of its status line, as windows print: (window n); an error when there is none."
        (n integer "the window's number")
        (returns window))
  (define (window n)
    (or (seat:window-numbered n) (error 'window "no window numbered" n)))

  (define window-printing
    (record-writer (record-type-descriptor seat:window)
      (lambda (r p wr)
        (display "(window " p)
        (wr (seat:window-index r) p)
        (display ")" p))))

  ;;; Types ---------------------------------------------------------------------

  ;; The literal notions as edoc types: what M-x offers at an argument of
  ;; that type, how a value is spelled as an expression, and what a value
  ;; must be. A window is live, on this seat, now.


  (edoc-type window "a window, spelled (window n); completion offers those on screen"
    (predicate seat:window?)
    (complete (lambda (partial)
                (map (lambda (w) (list w #f (seat:buffer-name (seat:window-buffer w))))
                     (remq (seat:popup) (seat:windows)))))
    (read window)
    (write (lambda (w) (format "(window ~a)" (seat:window-index w)))))

)
