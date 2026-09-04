#!/usr/bin/env scheme-script

;; The pure string helpers below the seams -- v2 core dissolution
;; (docs/DESIGN2.md).  Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (strings) strings:)
             (only (chezscheme) format))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'strings-test label actual expected)))

     ;; -- tail, prefix, suffix ------------------------------------------

     (check 'tail (strings:tail "hello" 2) "llo")
     (check 'tail-at-end (strings:tail "hello" 5) "")
     (check 'prefix (strings:prefix? "he" "hello") #t)
     (check 'prefix-longer-than-string (strings:prefix? "hello!" "hello") #f)
     (check 'prefix-empty (strings:prefix? "" "x") #t)
     (check 'suffix (strings:suffix? ".e" "core.e") #t)
     (check 'suffix-not (strings:suffix? ".ss" "core.e") #f)

     ;; -- join -------------------------------------------------------------

     (check 'join (strings:join '("a" "b" "c") ", ") "a, b, c")
     (check 'join-one (strings:join '("a") ", ") "a")
     (check 'join-none (strings:join '() ", ") "")

     ;; -- search (KMP) -------------------------------------------------------

     (check 'search (strings:search "abcabd" "abd" 0 6) 3)
     (check 'search-miss (strings:search "abcabd" "abe" 0 6) #f)
     (check 'search-limit-cuts-match (strings:search "abcabd" "abd" 0 5) #f)
     (check 'search-from-start (strings:search "abab" "ab" 1 4) 2)
     (check 'search-empty-needle (strings:search "abc" "" 1 3) 1)
     (check 'search-folded (strings:search "ABC" "bc" 0 3 #t) 1)
     (check 'search-exact-is-case-sensitive (strings:search "ABC" "bc" 0 3) #f)
     (check 'search-overlapping-prefix (strings:search "aaab" "aab" 0 4) 1)

     ;; -- lines --------------------------------------------------------------

     (check 'lines (strings:lines "a\nb\nc") '("a" "b" "c"))
     (check 'lines-trailing-newline (strings:lines "a\n") '("a" ""))
     (check 'lines-empty (strings:lines "") '(""))
     (check 'lines-blank-inside (strings:lines "a\n\nb") '("a" "" "b"))

     ;; -- common-prefix -------------------------------------------------------

     (check 'common-prefix
            (strings:common-prefix '("interleave" "internal" "interface"))
            "inter")
     (check 'common-prefix-one (strings:common-prefix '("solo")) "solo")
     (check 'common-prefix-none (strings:common-prefix '("a" "b")) "")

     (format #t "~a strings checks passed\n" checks)))
