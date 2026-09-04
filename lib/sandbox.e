;; sandbox.e -- the read-only capability environment for expression
;; evaluation: the library (sandbox), for any constrained actor; pure
;; infrastructure with no init!.
;;
;; Safety here is enforcement, not detection: an expression evaluated
;; in (environment '(sandbox)) -- or in the narrower (environment
;; '(only (sandbox) name ...)) a policy grant builds -- can only
;; reach the bindings listed below: pure computation, string and list
;; work, and bounded read-only views of the editor.  No mutators, no
;; file system, no processes, no eval, and no way to conjure them: an
;; expression that wants more simply fails to resolve, and the actor
;; must then ask its owner (actor:ask!) for something stronger.
;;
;; display, write, newline, and format are included because a
;; constrained actor's evaluator captures the current output port
;; into the result -- printing is a read-only act there.
;;
;; Curation rules, kept iron:
;;
;;   1. Every editor binding here is keyed by buffer NAME and returns
;;      plain data -- strings, numbers, pairs, lists.  Never export
;;      anything that hands out a mutable structure the editor holds.
;;
;;   2. Evaluations are fueled by engines (policy.e), and an engine's
;;      expiry suspends without unwinding: a reader caught mid-lock
;;      would strand the store's mutex forever.  Every reader that
;;      can take a lock therefore runs with interrupts off -- fuel
;;      still bounds the computation around it.

(library (sandbox)
  (export
    ;; syntax
    quote quasiquote unquote unquote-splicing lambda define if cond
    case when unless and or begin do let let* letrec letrec*
    let-values let*-values set! else => _ ...
    ;; equality and types
    eq? eqv? equal? not boolean? symbol? procedure?
    ;; numbers
    number? integer? rational? real? exact? inexact? exact inexact
    zero? positive? negative? odd? even? nan? finite?
    + - * / = < > <= >= abs min max div mod expt exact->inexact
    floor ceiling round truncate sqrt gcd lcm
    number->string string->number
    ;; pairs and lists
    pair? null? list? cons car cdr caar cadr cdar cddr caddr cdddr
    cadddr list length append reverse list-tail list-ref map for-each
    assq assv assoc memq memv member filter partition remove
    fold-left fold-right exists for-all cons* last-pair list-copy
    list-sort iota
    ;; symbols, chars, strings
    symbol->string string->symbol
    char? char->integer integer->char char=? char<? char>?
    char-alphabetic? char-numeric? char-whitespace?
    char-upcase char-downcase
    string? make-string string string-length string-ref substring
    string-append string=? string<? string>? string-ci=?
    string->list list->string string-copy string-upcase
    string-downcase string-titlecase
    ;; vectors and hashtables (locally created ones)
    vector? make-vector vector vector-length vector-ref vector-set!
    vector->list list->vector vector-map vector-for-each vector-fill!
    vector-sort
    make-eq-hashtable make-eqv-hashtable make-hashtable
    hashtable? hashtable-ref hashtable-set! hashtable-delete!
    hashtable-contains? hashtable-size hashtable-keys
    hashtable-entries equal-hash string-hash symbol-hash
    ;; control
    apply values call-with-values dynamic-wind call/cc
    ;; captured output
    display write newline format
    ;; the editor, read-only, by name
    buffer-names buffer-lines-count buffer-text-line buffer-revision
    current-buffer-name point
    read-buffer list-buffers log-tail describe-text)
  (import (rnrs) (rnrs mutable-strings)
          (only (chezscheme)
                format iota list-copy string-upcase string-downcase
                string-titlecase last-pair cons* vector-sort nan?
                exact->inexact open-output-string get-output-string
                put-string disable-interrupts enable-interrupts)
          (prefix (store) store:)
          (prefix (only (head) buffer-name window-buffer current
                        window-prow window-pcol)
                  head:)
          (prefix (only (log) entries format-entry) log:)
          (prefix (only (describe) lookup) describe:)
          (prefix (only (doc) forms returns libraries description) doc:))

  ;; Rule 2 above: nothing between disable and enable may raise
  ;; without the wind exit running, and nothing here evaluates actor
  ;; code -- these are leaf reads.
  (define (uninterruptible thunk)
    (dynamic-wind disable-interrupts thunk enable-interrupts))

  (define (clipped text cap)
    (if (> (string-length text) cap)
        (string-append (substring text 0 cap) " ...")
        text))

  ;;; Data readers over the store -------------------------------------------

  (define (named who name)
    (or (uninterruptible
          (lambda ()
            (guard (ex [else #f]) (store:find-named name))))
        (error who "no buffer with that name ((buffer-names) lists them)"
               name)))

  (define (buffer-names)
    ;; every buffer's name, as the store knows them
    (uninterruptible
      (lambda ()
        (map store:buffer-name (store:buffer-list)))))

  (define (buffer-lines-count name)
    (let ([id (named 'buffer-lines-count name)])
      (uninterruptible (lambda () (store:line-count id)))))

  (define (buffer-text-line name n)
    (let ([id (named 'buffer-text-line name)])
      (uninterruptible (lambda () (store:line id n)))))

  (define (buffer-revision name)
    (let ([id (named 'buffer-revision name)])
      (uninterruptible (lambda () (store:revision id)))))

  (define (current-buffer-name)
    ;; the buffer the head's user is looking at
    (head:buffer-name (head:window-buffer (head:current))))

  (define (point)
    ;; the head's cursor: (row . col)
    (cons (head:window-prow (head:current)) (head:window-pcol (head:current))))

  ;;; Bounded, formatted views -----------------------------------------------

  ;; Designed for an actor reading through eval -- the counterpart of
  ;; what dedicated tools would otherwise be.  Everything returns a
  ;; string; nothing here mutates.

  (define (read-buffer name . range)
    ;; Numbered lines of the named buffer: (read-buffer name), or with
    ;; a start line, or with a start and a count.  At most 400 lines.
    (guard (ex [else (format "error: no buffer named ~s ((buffer-names) lists them)"
                             name)])
      (let ([id (named 'read-buffer name)])
        (uninterruptible
          (lambda ()
            (let*-values ([(text revision) (store:snapshot id)])
              (let* ([total (vector-length text)]
                     [start (if (pair? range) (car range) 0)]
                     [count (if (and (pair? range) (pair? (cdr range)))
                                (cadr range)
                                200)]
                     [from (min (max 0 start) total)]
                     [n (max 0 (min count 400 (- total from)))]
                     [out (open-output-string)])
                (put-string out
                            (format "~s: lines ~a-~a of ~a (revision ~a)\n"
                                    name from (+ from (max 0 (- n 1)))
                                    total revision))
                (do ([i 0 (+ i 1)]) ((= i n))
                  (put-string out
                              (format "~a: ~a\n" (+ from i)
                                      (clipped (vector-ref text (+ from i))
                                               500))))
                (get-output-string out))))))))

  (define (list-buffers)
    ;; Every buffer: name, line count, revision.
    (uninterruptible
      (lambda ()
        (apply string-append
               "name / lines / revision\n"
               (map (lambda (id)
                      (format "~s / ~a / ~a\n"
                              (store:buffer-name id)
                              (store:line-count id)
                              (store:revision id)))
                    (store:buffer-list))))))

  (define (log-tail . count)
    ;; The newest entries of *log* -- errors and messages land there.
    ;; Default 20, at most 200.
    (let* ([n (min (if (pair? count) (car count) 20) 200)]
           [entries (let take ([entries (log:entries)] [n n])
                      (if (or (= n 0) (null? entries))
                          '()
                          (cons (car entries)
                                (take (cdr entries) (- n 1)))))])
      (if (null? entries)
          "the log is empty"
          (apply string-append
                 (map (lambda (entry)
                        (string-append
                          (clipped (log:format-entry entry) 500) "\n"))
                      (reverse entries))))))

  (define (describe-text name)
    ;; The documentation corpus, flattened: R6RS, Chez Scheme, and
    ;; every e command and parameter.  The authoritative reference.
    (let ([entries (guard (ex [else '()])
                     (describe:lookup (if (symbol? name)
                                          name
                                          (string->symbol name))))])
      (if (null? entries)
          (format "no documentation entry for ~a" name)
          (clipped
            (apply string-append
                   (map (lambda (entry)
                          (format "~a\nreturns: ~a\nlibraries: ~a\n~a\n\n"
                                  (map cdr (doc:forms entry))
                                  (or (doc:returns entry) "-")
                                  (doc:libraries entry)
                                  (doc:description entry)))
                        entries))
            8000)))))
