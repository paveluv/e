#!/usr/bin/env scheme-script

;; The row painter: data in, ANSI out -- testable against a string
;; port for the first time.  v2 core dissolution (docs/DESIGN2.md).
;; Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (paint) paint:)
             (prefix (styles) styles:)
             (only (sys) terminal-output-port)
             (only (chezscheme)
                   format open-output-string get-output-string
                   parameterize))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'paint-test label actual expected)))

     (define (contains? text needle)
       (let ([n (string-length text)] [m (string-length needle)])
         (let scan ([i 0])
           (cond [(> (+ i m) n) #f]
                 [(string=? (substring text i (+ i m)) needle) #t]
                 [else (scan (+ i 1))]))))

     (define (painted thunk)
       (let ([sink (open-output-string)])
         (parameterize ([terminal-output-port sink])
           (thunk))
         (get-output-string sink)))

     (define (stripped text)
       ;; the visible characters: ANSI escapes removed
       (let loop ([chars (string->list text)] [out '()] [in-escape #f])
         (cond [(null? chars) (list->string (reverse out))]
               [in-escape
                (loop (cdr chars) out
                      (not (or (char-alphabetic? (car chars))
                               (char=? (car chars) #\\))))]
               [(char=? (car chars) #\esc)
                (loop (cdr chars) out #t)]
               [else (loop (cdr chars) (cons (car chars) out) #f)])))

     ;; -- fit ------------------------------------------------------------------

     (check 'fit-pads (paint:fit "ab" 4) "ab  ")
     (check 'fit-truncates (paint:fit "abcdef" 4) "abcd")

     ;; -- goto -----------------------------------------------------------------

     (check 'goto (painted (lambda () (paint:goto 3 7))) "\x1b;[3;7H")

     ;; -- the row painter ---------------------------------------------------------

     (define (paint-line . args) (painted (lambda () (apply paint:display-editor-line args))))

     ;; plain text pads to the width
     (check 'plain-row
            (stripped (paint-line "hello" "hello" #f '() '() 0 #f #f 10
                                  1000))
            "hello     ")

     ;; the selection span emits the selection style over its columns
     (check 'selection-styled
            (contains? (paint-line "hello" "hello" '(1 . 3) '() '() 0 #f
                                   #f 10 1000)
                       (styles:code 'selection))
            #t)

     ;; a wrap edge paints a backslash in the last column
     (check 'wrap-edge
            (stripped (paint-line "abcdefgh" "abcdefgh" #f '() '() 0 #f
                                  'wrap 5 1000))
            "abcd\\")
     (check 'truncation-edge
            (stripped (paint-line "abcdefgh" "abcdefgh" #f '() '() 0 #f
                                  'trunc 5 1000))
            "abcd$")

     ;; control characters paint as single-cell spaces
     (check 'tab-is-one-cell
            (stripped (paint-line "a\tb" "a\tb" #f '() '() 0 #f #f 5
                                  1000))
            "a b  ")

     ;; a link opens and closes an OSC 8 around its run
     (let ([out (paint-line "see http://x.example now"
                            "see http://x.example now"
                            #f '() '((4 20 "http://x.example")) 0 #f #f
                            24 1000)])
       (check 'link-opens (contains? out "\x1b;]8;;http://x.example\x1b;\\")
              #t)
       (check 'link-closes (contains? out "\x1b;]8;;\x1b;\\") #t))

     ;; a highlight mark emits its named face on top of the base style
     (check 'mark-face
            (contains? (paint-line "abc" "abc" #f '((0 3 mark)) '() 0 #f
                                   #f 3 1000)
                       (styles:code 'mark))
            #t)

     ;; -- emit-runs ----------------------------------------------------------------

     (let ([out (painted
                  (lambda ()
                    (paint:emit-runs "abcd"
                                     (vector 'keyword 'keyword 'plain
                                             'plain)
                                     0 4)))])
       (check 'runs-coalesce (stripped out) "abcd")
       (check 'runs-styled (contains? out (styles:code 'keyword))
              #t))

     ;; -- soft-wrap breaks ----------------------------------------------------------

     (check 'no-break-when-it-fits
            (paint:compute-breaks "short" 10) '#(0))
     (check 'breaks-at-spaces
            (paint:compute-breaks "aaa bbb ccc" 5) '#(0 4 8))
     (check 'hard-break-without-spaces
            (paint:compute-breaks "aaaaaaaaaa" 4) '#(0 4 8))

     ;; -- hyperlink detection ---------------------------------------------------------

     (check 'detects-http
            (paint:detect-hyperlinks "see http://a.example/x here")
            '((4 22 "http://a.example/x")))
     (check 'trims-trailing-punctuation
            (paint:detect-hyperlinks "at https://e.dev/p, then")
            '((3 18 "https://e.dev/p")))
     (check 'angle-brackets-end-a-url
            (paint:detect-hyperlinks "<http://a.example>")
            '((1 17 "http://a.example")))
     (check 'bare-scheme-skipped
            (paint:detect-hyperlinks "http:// is not a link") '())
     (check 'several-links
            (map caddr (paint:detect-hyperlinks
                         "http://one.example and https://two.example"))
            '("http://one.example" "https://two.example"))

     (check 'valid-hyperlink
            (paint:valid-hyperlink? '(0 5 "http://x") 10) #t)
     (check 'invalid-hyperlink-range
            (paint:valid-hyperlink? '(5 3 "http://x") 10) #f)

     (format #t "~a paint checks passed\n" checks)))
