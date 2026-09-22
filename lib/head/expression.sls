;; expression.sls -- the Scheme expressions of a buffer, by position.
;;
;; Chez's annotated reader gives the span of every complete datum in a
;; buffer's text, at every depth; an unreadable stretch, an unfinished
;; form being typed say, is skipped a character at a time so the complete
;; datums inside it still count. On those spans rest motion by expression
;; (C-M-f, C-M-b) and the evaluation of the expression before point
;; (C-x C-e) or of the top-level form around it (C-M-x), as in Emacs.
;; Positions are (row . col); offsets count characters in the buffer's
;; text with its rows joined by newlines.

(import (only (foundation edoc) elibrary))
(elibrary (head expression)
  (export backward container down form-end form-start forward next-list previous-list spans text
          top-level)
  (import (chezscheme)
          (prefix (head head) head:))

  ;; A span is #(start end compound?), in character offsets; compound
  ;; spans are lists and vectors, the rest atoms.
  (define (span-start s) (vector-ref s 0))
  (define (span-end s) (vector-ref s 1))
  (define (span-compound? s) (vector-ref s 2))

  (define (annotated value acc)
    ;; the spans of an annotated datum, the outer one before its parts
    (cond [(annotation? value)
           (let ([where (annotation-source value)] [body (annotation-expression value)])
             (annotated body
               (cons (vector (source-object-bfp where) (source-object-efp where) (or (pair? body) (vector? body)))
                     acc)))]
          [(pair? value) (annotated (cdr value) (annotated (car value) acc))]
          [(vector? value)
           (let loop ([i 0] [acc acc])
             (if (= i (vector-length value)) acc (loop (+ i 1) (annotated (vector-ref value i) acc))))]
          [else acc]))

  (define (token-start source at sfd)
    ;; where the datum a read from at would begin, past blanks and comments;
    ;; at itself when the text there is not even a token
    (guard (ex [else at])
      (let-values ([(kind value start end)
                    (read-token (open-string-input-port (substring source at (string-length source))) sfd at)])
        (if (eq? kind 'eof) (string-length source) start))))

  (edoc "The spans of every complete datum in a Scheme text, at every depth, as #(start end compound?) character offsets, an outer span before its parts; an unreadable stretch is skipped a character at a time, so the complete datums inside an unfinished form still count."
        (source string "the text")
        (returns list))
  (define (spans source)
    (let ([n (string-length source)] [sfd (source-file-descriptor "buffer" 0)])
      (let read-all ([at 0] [port (open-string-input-port source)] [acc '()])
        (if (>= at n)
            (reverse acc)
            (let ([outcome (guard (ex [else #f])
                             (let-values ([(datum efp) (get-datum/annotations port sfd at)])
                               (if (eof-object? datum) 'eof (cons efp (annotated datum acc)))))])
              (cond [(eq? outcome 'eof) (reverse acc)]
                    [outcome (read-all (car outcome) port (cdr outcome))]
                    [else
                     ;; the failed read began at a token nobody can finish:
                     ;; resume just inside it, on a fresh port
                     (let ([resume (+ (token-start source at sfd) 1)])
                       (read-all resume (open-string-input-port (substring source (min resume n) n)) acc))]))))))

  ;; Per buffer, the analysis of its text at a revision:
  ;; #(revision source line-starts spans)
  (define analyses (make-weak-eq-hashtable))

  (define (analysis b)
    (let ([revision (head:buffer-revision b)] [known (hashtable-ref analyses b #f)])
      (if (and known (eqv? (vector-ref known 0) revision))
          known
          (let* ([lines (head:buffer-lines b)] [count (vector-length lines)] [starts (make-vector count 0)]
                 [source (let loop ([row 0] [offset 0] [parts '()])
                           (if (= row count)
                               (apply string-append (reverse parts))
                               (let ([line (vector-ref lines row)])
                                 (vector-set! starts row offset)
                                 (loop (+ row 1) (+ offset (string-length line) 1)
                                       (if (= row (- count 1)) (cons line parts) (cons "\n" (cons line parts)))))))]
                 [fresh (vector revision source starts (spans source))])
            (hashtable-set! analyses b fresh)
            fresh))))

  (define (offset-of a position)
    ;; the character offset of a (row . col), clamped into its row
    (let* ([starts (vector-ref a 2)] [count (vector-length starts)] [source (vector-ref a 1)]
           [row (max 0 (min (car position) (- count 1)))]
           [line-end (if (= row (- count 1)) (string-length source) (- (vector-ref starts (+ row 1)) 1))])
      (min (+ (vector-ref starts row) (max 0 (cdr position))) line-end)))

  (define (position-of a offset)
    ;; the (row . col) of a character offset
    (let* ([starts (vector-ref a 2)] [count (vector-length starts)])
      (let loop ([row 0])
        (if (or (= row (- count 1)) (< offset (vector-ref starts (+ row 1))))
            (cons row (- offset (vector-ref starts row)))
            (loop (+ row 1))))))

  (define (surrounds? s offset) (and (< (span-start s) offset) (< offset (span-end s))))

  (define (atom-around all offset)
    ;; the atom whose characters surround offset, or #f
    (find (lambda (s) (and (not (span-compound? s)) (surrounds? s offset))) all))

  (define (container-around all offset)
    ;; the innermost list or vector whose characters surround offset, or #f
    (fold-left (lambda (best s)
                 (if (and (span-compound? s) (surrounds? s offset)
                          (or (not best) (> (span-start s) (span-start best))))
                     s best))
               #f all))

  (define (within? s container)
    (or (not container)
        (and (> (span-start s) (span-start container)) (<= (span-end s) (span-end container)))))

  (define (crossed-backward all offset)
    ;; the span a backward move from offset crosses: the atom around
    ;; offset, else the last span ending by offset inside the enclosing
    ;; one, the outermost of those ending together
    (or (atom-around all offset)
        (let ([container (container-around all offset)])
          (fold-left (lambda (best s)
                       (if (and (within? s container) (<= (span-end s) offset)
                                (or (not best) (> (span-end s) (span-end best))
                                    (and (= (span-end s) (span-end best)) (< (span-start s) (span-start best)))))
                           s best))
                     #f all))))

  (define (crossed-forward all offset)
    ;; the span a forward move from offset crosses: the atom around
    ;; offset, else the first span starting at or after offset inside the
    ;; enclosing one, the outermost of those starting together
    (or (atom-around all offset)
        (let ([container (container-around all offset)])
          (fold-left (lambda (best s)
                       (if (and (within? s container) (>= (span-start s) offset)
                                (or (not best) (< (span-start s) (span-start best))
                                    (and (= (span-start s) (span-start best)) (> (span-end s) (span-end best)))))
                           s best))
                     #f all))))

  (define (compound-forward all offset)
    ;; the next list or vector starting at or after offset inside the
    ;; enclosing one, atoms skipped, the outermost of those starting together
    (let ([container (container-around all offset)])
      (fold-left (lambda (best s)
                   (if (and (span-compound? s) (within? s container) (>= (span-start s) offset)
                            (or (not best) (< (span-start s) (span-start best))
                                (and (= (span-start s) (span-start best)) (> (span-end s) (span-end best)))))
                       s best))
                 #f all)))

  (define (compound-backward all offset)
    ;; the last list or vector ending by offset inside the enclosing one,
    ;; atoms skipped, the outermost of those ending together
    (let ([container (container-around all offset)])
      (fold-left (lambda (best s)
                   (if (and (span-compound? s) (within? s container) (<= (span-end s) offset)
                            (or (not best) (> (span-end s) (span-end best))
                                (and (= (span-end s) (span-end best)) (< (span-start s) (span-start best)))))
                       s best))
                 #f all)))

  (define (top-level-spans all)
    ;; the spans inside no other, in text order: an outer span precedes its parts
    (let loop ([rest all] [limit -1] [acc '()])
      (cond [(null? rest) (reverse acc)]
            [(>= (span-start (car rest)) limit) (loop (cdr rest) (span-end (car rest)) (cons (car rest) acc))]
            [else (loop (cdr rest) limit acc)])))

  (define (top-level-around all offset)
    ;; the top-level span holding offset, its edges included; else the
    ;; next one after offset; else the last one before it
    (let ([tops (top-level-spans all)])
      (or (find (lambda (s) (and (<= (span-start s) offset) (<= offset (span-end s)))) tops)
          (find (lambda (s) (> (span-start s) offset)) tops)
          (and (pair? tops) (car (last-pair tops))))))

  (define (edges b position pick)
    (let* ([a (analysis b)] [s (pick (vector-ref a 3) (offset-of a position))])
      (if s
          (values (position-of a (span-start s)) (position-of a (span-end s)))
          (values #f #f))))

  (edoc "The expression a backward move from a position in a buffer crosses, as (values start end) positions, or (values #f #f) without one: the atom around the position, else the last expression ending by it inside the enclosing one."
        (b buffer "the buffer")
        (position pair "(row . col)")
        (effects internal))
  (define (backward b position)
    (edges b position crossed-backward))

  (edoc "The expression a forward move from a position in a buffer crosses, as (values start end) positions, or (values #f #f) without one: the atom around the position, else the first expression starting at or after it inside the enclosing one."
        (b buffer "the buffer")
        (position pair "(row . col)")
        (effects internal))
  (define (forward b position)
    (edges b position crossed-forward))

  (edoc "The top-level form around a position in a buffer, as (values start end) positions, or (values #f #f) in a buffer without one: the form holding the position, its edges included, else the next one after it, else the last one before it."
        (b buffer "the buffer")
        (position pair "(row . col)")
        (effects internal))
  (define (top-level b position)
    (edges b position top-level-around))

  (edoc "The list or vector around a position in a buffer, as (values start end) positions, or (values #f #f) at top level."
        (b buffer "the buffer")
        (position pair "(row . col)")
        (effects internal))
  (define (container b position)
    (edges b position container-around))

  (edoc "The position just inside the next list or vector at a position's level in a buffer, atoms skipped, or #f without one."
        (b buffer "the buffer")
        (position pair "(row . col)")
        (returns (or pair #f))
        (effects internal))
  (define (down b position)
    (let* ([a (analysis b)] [s (compound-forward (vector-ref a 3) (offset-of a position))])
      (and s (position-of a (+ (span-start s) 1)))))

  (edoc "The next list or vector at a position's level in a buffer, atoms skipped, as (values start end) positions, or (values #f #f) without one."
        (b buffer "the buffer")
        (position pair "(row . col)")
        (effects internal))
  (define (next-list b position)
    (edges b position compound-forward))

  (edoc "The previous list or vector at a position's level in a buffer, atoms skipped, as (values start end) positions, or (values #f #f) without one."
        (b buffer "the buffer")
        (position pair "(row . col)")
        (effects internal))
  (define (previous-list b position)
    (edges b position compound-backward))

  (edoc "The start of the last top-level form beginning before a position in a buffer, or #f."
        (b buffer "the buffer")
        (position pair "(row . col)")
        (returns (or pair #f))
        (effects internal))
  (define (form-start b position)
    (let* ([a (analysis b)] [offset (offset-of a position)])
      (let loop ([tops (top-level-spans (vector-ref a 3))] [best #f])
        (cond [(and (pair? tops) (< (span-start (car tops)) offset)) (loop (cdr tops) (car tops))]
              [best (position-of a (span-start best))]
              [else #f]))))

  (edoc "The end of the first top-level form ending after a position in a buffer, or #f."
        (b buffer "the buffer")
        (position pair "(row . col)")
        (returns (or pair #f))
        (effects internal))
  (define (form-end b position)
    (let* ([a (analysis b)] [offset (offset-of a position)]
           [s (find (lambda (t) (> (span-end t) offset)) (top-level-spans (vector-ref a 3)))])
      (and s (position-of a (span-end s)))))

  (edoc "The text of a buffer between two positions, rows joined by newlines."
        (b buffer "the buffer")
        (start pair "(row . col)")
        (end pair "(row . col)")
        (returns string)
        (effects internal))
  (define (text b start end)
    (let ([a (analysis b)])
      (substring (vector-ref a 1) (offset-of a start) (offset-of a end))))
)
