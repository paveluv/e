#!/usr/bin/env scheme-script

;; The pure string helpers below the seams.  Run from the repository
;; root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (string) string:)
             (only (chezscheme) format))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'string-test label actual expected)))

     ;; -- tail, prefix, suffix ------------------------------------------

     (check 'tail (string:tail "hello" 2) "llo")
     (check 'tail-at-end (string:tail "hello" 5) "")
     (check 'prefix (string:prefix? "he" "hello") #t)
     (check 'prefix-longer-than-string (string:prefix? "hello!" "hello") #f)
     (check 'prefix-empty (string:prefix? "" "x") #t)
     (check 'suffix (string:suffix? ".e" "edit.e") #t)
     (check 'suffix-not (string:suffix? ".ss" "edit.e") #f)

     ;; -- join -------------------------------------------------------------

     (check 'join (string:join '("a" "b" "c") ", ") "a, b, c")
     (check 'join-one (string:join '("a") ", ") "a")
     (check 'join-none (string:join '() ", ") "")

     ;; -- search (KMP) -------------------------------------------------------

     (check 'search (string:search "abcabd" "abd" 0 6) 3)
     (check 'search-miss (string:search "abcabd" "abe" 0 6) #f)
     (check 'search-limit-cuts-match (string:search "abcabd" "abd" 0 5) #f)
     (check 'search-from-start (string:search "abab" "ab" 1 4) 2)
     (check 'search-empty-needle (string:search "abc" "" 1 3) 1)
     (check 'search-folded (string:search "ABC" "bc" 0 3 #t) 1)
     (check 'search-exact-is-case-sensitive (string:search "ABC" "bc" 0 3) #f)
     (check 'search-overlapping-prefix (string:search "aaab" "aab" 0 4) 1)

     ;; -- lines --------------------------------------------------------------

     (check 'lines (string:lines "a\nb\nc") '("a" "b" "c"))
     (check 'lines-trailing-newline (string:lines "a\n") '("a" ""))
     (check 'lines-empty (string:lines "") '(""))
     (check 'lines-blank-inside (string:lines "a\n\nb") '("a" "" "b"))

     ;; -- common-prefix -------------------------------------------------------

     (check 'common-prefix
            (string:common-prefix '("interleave" "internal" "interface"))
            "inter")
     (check 'common-prefix-one (string:common-prefix '("solo")) "solo")
     (check 'common-prefix-none (string:common-prefix '("a" "b")) "")

     (format #t "~a string checks passed\n" checks)))
