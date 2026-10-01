;; Legacy window constructor and head completion providers.
;; Shared buffers, models and actors use ordinary quoted data.

(import (only (foundation edoc) elibrary))
(elibrary (head literal)
  (export window)
  (import (chezscheme)
          (prefix (core identity) identity:)
          (prefix (head head) head:)
          (prefix (state actor) actor:))

  ;;; Windows -------------------------------------------------------------------

  (edoc "The window numbered n at the left of its status line, as windows print: (window n); an error when there is none."
        (n integer "the window's number")
        (returns window))
  (define (window n)
    (or (head:window-numbered n) (error 'window "no window numbered" n)))

  (define window-printing
    (record-writer (record-type-descriptor head:window)
      (lambda (r p wr)
        (display "(window " p)
        (wr (head:window-index r) p)
        (display ")" p))))

  ;;; Actors -------------------------------------------------------------------

  (define (head? v) (and (identity:valid? v) (eq? (car v) 'head)))

  (define (directory-entries kind)
    ;; (identity . hint) for the registered actors, of one kind or of all;
    ;; an entry is (identity kind name attached-at capabilities)
    (fold-right
      (lambda (entry out)
        (if (or (not kind) (eq? (cadr entry) kind))
            (cons (list (car entry) #f (let ([c (list-ref entry 4)]) (and (string? c) c))) out)
            out))
      '() (actor:attached)))

  ;;; Types ---------------------------------------------------------------------

  ;; The literal notions as edoc types: what M-x offers at an argument of
  ;; that type, how a value is spelled as an expression, and what a value
  ;; must be. A window is live, on this seat, now; an actor is
  ;; any identity, offered from the directory.


  (edoc-type window "a window, spelled (window n); completion offers those on screen"
    (predicate head:window?)
    (complete (lambda (partial)
                (map (lambda (w) (list w #f (head:buffer-name (head:window-buffer w))))
                     (remq (head:popup) (head:windows)))))
    (read window)
    (write (lambda (w) (format "(window ~a)" (head:window-index w)))))

  (edoc-type actor "an actor identity datum: (kind name more ...)"
    (predicate identity:valid?)
    (portable #t) (within list)
    (complete (lambda (partial) (directory-entries #f))))

  (edoc-type head "a head identity datum: (head name more ...)"
    (predicate head?)
    (complete (lambda (partial) (directory-entries 'head)))
    (portable #t)
    (within actor))


)
