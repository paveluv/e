#!/usr/bin/env scheme-script
;; scheme-format -- Scheme source formatter: the e editor's normal form,
;; from the shell.
;;
;; usage: scheme-format [-i] [--intrusive] [--width columns] [file ...]
;;
;; Formats Scheme source -- indentation (each line settling on the
;; stop nearest its current column), tabs widened, the ( ) vs [ ]
;; conventions, trailing whitespace trimmed, one final newline;
;; spacing within lines is the author's and stays unless --intrusive enables
;; comment-aware whitespace folding and width-based layout. See the e
;; README's "Indentation and formatting".  Without arguments,
;; stdin formats to stdout.  With -i the files are rewritten in
;; place (only when changed, permissions kept); without it their
;; formatted text prints to stdout.  A result that does not read
;; back as the very same data -- it cannot, short of a bug -- is
;; refused and the file left untouched.
;; A leading Unix shebang is preserved; Scheme reader directives stay
;; in the body checked for datum equality. File extensions are unrestricted.
;;
;; The engine is (scheme-format) in lib/foundation; its common dependencies
;; live in lib/sys and lib/core. The tool shares the base object cache.
;; See the e loader script for the scheme-script portability notes.

(import (chezscheme))

(define (directory-part path)
  (let loop ([i (- (string-length path) 1)])
    (cond [(< i 0) "."]
          [(char=? (string-ref path i) #\/) (substring path 0 (max 1 i))]
          [else (loop (- i 1))])))

(define e-home
  ;; the installation: this script lives in its tools directory
  (let ([dir (string-append
               (directory-part
                 (let ([cl (command-line)])
                   (if (and (pair? cl) (string? (car cl))) (car cl) "")))
               "/..")])
    (unless (file-directory? (string-append dir "/lib"))
      (display (format "scheme-format: no lib directory in ~a\n" dir)
               (current-error-port))
      (exit 1))
    (if (path-absolute? dir) dir (string-append (current-directory) "/" dir))))

(library-directories
  ;; the common kinds, as the loader selects them: every leaf directory
  ;; under lib beside the two implementation trees, which the engine and
  ;; its dependencies (string, kernel, path) never reach
  (let ([lib (string-append e-home "/lib")])
    (map (lambda (name) (cons (string-append lib "/" name) (string-append e-home "/eo/base")))
      (list-sort string<?
        (filter (lambda (name)
                  (and (not (member name '("base" "client")))
                       (file-directory? (string-append lib "/" name))))
                (directory-list lib))))))
(compile-imported-libraries #t)
(compile-file-message #f)

(define (read-text port)
  (let ([text (get-string-all port)]) (if (eof-object? text) "" text)))

(define (split-lines s)
  (let ([p (open-string-input-port s)])
    (let loop ([acc '()])
      (let ([l (get-line p)])
        (if (eof-object? l) (reverse acc) (loop (cons l acc)))))))

(define (read-data s)
  (let ([p (open-string-input-port s)])
    (let loop ([acc '()])
      (let ([x (read p)])
        (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))

(define (shebang-end text)
  ;; An interpreter path after #!, allowing its optional whitespace.
  ;; #!r6rs, #!fold-case and other reader directives are Scheme data syntax.
  (let ([n (string-length text)])
    (and (>= n 3) (string=? (substring text 0 2) "#!")
         (let scan ([i 2])
           (and (< i n)
                (if (memv (string-ref text i) '(#\space #\tab))
                    (scan (+ i 1))
                    (char=? (string-ref text i) #\/))))
         (let scan ([i 2])
           (if (or (= i n) (char=? (string-ref text i) #\newline)) i
               (scan (+ i 1)))))))

(define (format-text text format-lines)
  (let* ([header-end (shebang-end text)]
         [header (if header-end (string-append (substring text 0 header-end) "\n") "")]
         [body (if header-end
                   (substring text (min (+ header-end 1) (string-length text)) (string-length text))
                   text)]
         [v (list->vector (split-lines body))]
         [n (vector-length v)]
         [out (if (zero? n) '() (format-lines v 0 (- n 1)))]
         [new (apply string-append
                     (map (lambda (l) (string-append l "\n")) out))])
    (unless (equal? (read-data body) (read-data new))
      (error 'scheme-format "the result does not read back as the same data"))
    (string-append header new)))

(define (usage)
  (display "usage: scheme-format [-i] [--intrusive] [--width columns] [file ...]\n"
           (current-error-port)))

(define (options args)
  (let loop ([args args] [in-place? #f] [intrusive? #f] [width 100]
             [files '()])
    (cond [(null? args) (values in-place? intrusive? width (reverse files))]
          [(string=? (car args) "-i")
           (loop (cdr args) #t intrusive? width files)]
          [(string=? (car args) "--intrusive")
           (loop (cdr args) in-place? #t width files)]
          [(member (car args) '("-w" "--width"))
           (if (null? (cdr args))
               (begin (usage) (exit 1))
               (let ([n (string->number (cadr args))])
                 (unless (and n (integer? n) (exact? n) (>= n 20))
                   (display "scheme-format: width must be an integer >= 20\n"
                            (current-error-port))
                   (exit 1))
                 (loop (cddr args) in-place? #t n files)))]
          [(and (> (string-length (car args)) 0)
                (char=? (string-ref (car args) 0) #\-))
           (usage) (exit 1)]
          [else (loop (cdr args) in-place? intrusive? width
                      (cons (car args) files))])))

(define (run format-lines intrusive width-parameter condition-text)
  (define (checked source use)
    (guard (ex [else
                (display (format "scheme-format: ~a: ~a\n" source (condition-text ex))
                         (current-error-port))
                (exit 1)])
      (use)))
  (let-values ([(in-place? intrusive? width files)
                (options (command-line-arguments))])
    (intrusive intrusive?)
    (width-parameter width)
    (cond
      [(null? files)
       (when in-place?
         (usage)
         (exit 1))
       (checked "stdin"
         (lambda () (display (format-text (read-text (current-input-port)) format-lines))))]
      [else
       (for-each
         (lambda (path)
           (checked path
             (lambda ()
               (let* ([text (call-with-input-file path read-text)]
                      [new (format-text text format-lines)])
                 (cond
                   [(not in-place?) (display new)]
                   [(string=? new text) (void)]
                   [else
                    (let ([mode (guard (ex [else #f]) (get-mode path))])
                      (call-with-output-file path
                        (lambda (p) (put-string p new))
                        'replace)
                      (when mode (guard (ex [else (void)]) (chmod path mode)))
                      (display path) (newline))])))))
         files)])))

(eval `(begin
         (import (prefix (scheme-format) scheme-format:) (prefix (kernel) kernel:))
         (kernel:installation-directory ,e-home))
  (interaction-environment))
(run (eval 'scheme-format:lines (interaction-environment))
     (eval 'scheme-format:intrusive (interaction-environment))
     (eval 'scheme-format:width (interaction-environment))
     (eval 'kernel:condition-text (interaction-environment)))
