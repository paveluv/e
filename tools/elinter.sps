#!/usr/bin/env scheme-script
;; elinter.sps -- the tree's source conventions, checked statically:
;;
;;   tools/elinter.sps
;;
;; Every .sls under lib/ is read with source positions, and each library
;; or elibrary form in it is checked: the exports are sorted by exported
;; name (a rename by the names it exports), and the import specs by
;; library name, the language's libraries first -- (rnrs ...), then
;; (chezscheme). In an elibrary body a blank line precedes an edoc form
;; (comments may sit between; consecutive edocs are one block), and the
;; definition an annotating edoc documents begins on the very next line.
;; Then the bang check runs over the tree, tools/edoc-coverage.sps
;; --effects. A finding prints as path:line: message; the exit status is
;; the number of findings, capped at 100, so the suite and the pre-commit
;; hook can run this.
(import (chezscheme))

(define (sls-files directory)
  ;; every .sls below directory, sorted by path
  (let loop ([names (directory-list directory)] [acc '()])
    (cond [(null? names) (list-sort string<? acc)]
          [(file-directory? (string-append directory "/" (car names)))
           (loop (cdr names) (append (sls-files (string-append directory "/" (car names))) acc))]
          [(let ([n (string-length (car names))])
             (and (> n 4) (string=? (substring (car names) (- n 4) n) ".sls")))
           (loop (cdr names) (cons (string-append directory "/" (car names)) acc))]
          [else (loop (cdr names) acc)])))

(define (read-annotated path)
  ;; the file's text and its top-level forms as annotations, positioned
  ;; in the text
  (let* ([text (call-with-input-file path get-string-all)]
         [sfd (source-file-descriptor path 0)]
         [port (open-string-input-port text)])
    (let loop ([bfp 0] [acc '()])
      (let-values ([(form efp) (get-datum/annotations port sfd bfp)])
        (if (eof-object? form) (values text (reverse acc)) (loop efp (cons form acc)))))))

(define (stripped x) (if (annotation? x) (annotation-stripped x) x))
(define (parts x) (if (annotation? x) (annotation-expression x) x))
(define (start x) (source-object-bfp (annotation-source x)))
(define (end x) (source-object-efp (annotation-source x)))

(define (line-of text pos)
  ;; the 1-based line holding a character position
  (let loop ([i 0] [line 1])
    (if (>= i pos) line (loop (+ i 1) (if (char=? (string-ref text i) #\newline) (+ line 1) line)))))

(define (library-form? form)
  (let ([d (stripped form)])
    (and (list? d) (>= (length d) 4) (memq (car d) '(library elibrary))
         (pair? (caddr d)) (eq? (car (caddr d)) 'export)
         (pair? (cadddr d)) (eq? (car (cadddr d)) 'import))))

;;; Sorted exports and imports ---------------------------------------------

(define (exported-names spec)
  ;; the names an export spec publishes, in its order
  (cond [(symbol? spec) (list spec)]
        [(and (pair? spec) (eq? (car spec) 'rename)) (map cadr (cdr spec))]
        [else '()]))

(define (check-exports! path text form report!)
  (let loop ([names (apply append (map exported-names (cdr (stripped form))))])
    (when (and (pair? names) (pair? (cdr names)))
      (if (string>? (symbol->string (car names)) (symbol->string (cadr names)))
          (report! path (line-of text (start form))
                   (format "exports not sorted: ~a should precede ~a" (cadr names) (car names)))
          (loop (cdr names))))))

(define (import-library spec)
  ;; the library an import spec names, under any wrappers
  (cond [(not (pair? spec)) #f]
        [(memq (car spec) '(prefix only except rename for)) (import-library (cadr spec))]
        [(eq? (car spec) 'library) (cadr spec)]
        [else spec]))

(define (import-key spec)
  ;; (group . text): (rnrs ...) first, (chezscheme) next, then the
  ;; tree's libraries, alphabetical by printed name within a group
  (let* ([name (filter symbol? (or (import-library spec) '()))]
         [text (apply string-append (map (lambda (s) (string-append (symbol->string s) " ")) name))])
    (cons (cond [(null? name) 2]
                [(eq? (car name) 'rnrs) 0]
                [(memq (car name) '(chezscheme scheme)) 1]
                [else 2])
          text)))

(define (key<? a b)
  (or (< (car a) (car b)) (and (= (car a) (car b)) (string<? (cdr a) (cdr b)))))

(define (check-imports! path text form report!)
  (let loop ([specs (cdr (stripped form))])
    (when (and (pair? specs) (pair? (cdr specs)))
      (if (key<? (import-key (cadr specs)) (import-key (car specs)))
          (report! path (line-of text (start form))
                   (format "imports not sorted: ~s should precede ~s"
                           (import-library (cadr specs)) (import-library (car specs))))
          (loop (cdr specs))))))

;;; The layout around edocs --------------------------------------------------

(define (edoc-form? d) (and (pair? d) (eq? (car d) 'edoc)))
(define (annotating-edoc? d) (and (edoc-form? d) (pair? (cdr d)) (string? (cadr d))))

(define (lines-of s)
  (let loop ([i 0] [from 0] [acc '()])
    (cond [(= i (string-length s)) (reverse (cons (substring s from i) acc))]
          [(char=? (string-ref s i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring s from i) acc))]
          [else (loop (+ i 1) from acc)])))

(define (blank? s) (for-all (lambda (c) (memv c '(#\space #\tab #\return))) (string->list s)))

(define (gap-separates? gap)
  ;; whether a blank line lies in the gap between two forms: past the
  ;; rest of the first form's line, before the indentation of the next
  (let* ([lines (lines-of gap)] [n (length lines)])
    (and (> n 2) (exists blank? (cdr (list-head lines (- n 1)))))))

(define (gap-adjoins? gap)
  ;; whether the gap is one line break, the next form starting on the
  ;; very next line
  (let ([lines (lines-of gap)])
    (and (= (length lines) 2) (blank? (cadr lines)))))

(define (check-edocs! path text forms report!)
  ;; forms: the import form, then the body
  (let loop ([previous (car forms)] [rest (cdr forms)])
    (when (pair? rest)
      (let* ([form (car rest)] [d (stripped form)])
        (when (edoc-form? d)
          (unless (or (edoc-form? (stripped previous))
                      (gap-separates? (substring text (end previous) (start form))))
            (report! path (line-of text (start form)) "no blank line before the edoc"))
          (when (annotating-edoc? d)
            (cond [(null? (cdr rest))
                   (report! path (line-of text (start form)) "the edoc annotates nothing")]
                  [(not (gap-adjoins? (substring text (end form) (start (cadr rest)))))
                   (report! path (line-of text (start form))
                            "the definition does not begin on the line after its edoc")])))
        (loop form (cdr rest))))))

;;; The run -----------------------------------------------------------------

(define findings 0)
(define libraries 0)

(define (report! path line message)
  (set! findings (+ findings 1))
  (printf "~a:~a: ~a\n" path line message))

(for-each
  (lambda (path)
    (let-values ([(text forms) (read-annotated path)])
      (for-each
        (lambda (form)
          (when (library-form? form)
            (set! libraries (+ libraries 1))
            (let ([subforms (parts form)])
              (check-exports! path text (caddr subforms) report!)
              (check-imports! path text (cadddr subforms) report!)
              (when (eq? (stripped (car subforms)) 'elibrary)
                (check-edocs! path text (cdddr subforms) report!)))))
        forms)))
  (sls-files "lib"))

(printf "elinter: ~a libraries checked, ~a finding~a\n" libraries findings (if (= findings 1) "" "s"))
(define disagreements (system "scheme --script tools/edoc-coverage.sps --effects"))
(exit (min 100 (+ findings disagreements)))
