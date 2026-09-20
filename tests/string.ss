#!/usr/bin/env scheme-script

;; The pure string helpers below the seams.  Run from the repository
;; root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (prefix (foundation string) string:)
             (prefix (foundation fuzzy) fuzzy:)
             (only (chezscheme) format))


     (define check test:check)

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

     ;; Segments start at the symbol's beginning or after any character that
     ;; is not a letter or a digit. Explicit separators stay inside literal
     ;; segments, which may still reorder.
     (for-each
       (lambda (case)
         (check (list 'fuzzy (car case)) (fuzzy:matches (car case) (cadr case)) (caddr case)))
       '(("bc" ("cba" "abc" "bca") ("bca"))
         ("abc" ("abc:tail" "ab:c" "abcde" "c:ab" "abc" "x:abc")
          ("abc" "abcde" "abc:tail" "x:abc" "ab:c" "c:ab"))
         ("abc" ("ab:a:bc" "abxbc" "xabc" "abdc") ("ab:a:bc"))
         ("fb" ("foobar" "foo_bar" "foo-bar" "foo:bar") ("foo-bar" "foo:bar" "foo_bar"))
         ("scr" ("(buffer \"*scratch*\")" "describe" "scratch") ("scratch" "(buffer \"*scratch*\")"))
         ("xx" ("x" "x-x" "xx" "xxx" "XX") ("xx" "xxx" "x-x"))
         ("aa" ("aba" "ab-a") ("ab-a"))
         ("λλ" ("λ" "λ-λ" "λλ") ("λλ" "λ-λ"))
         ("!" ("plain" "first!" "!first") ("!first"))
         ("s-w" ("split-some-window" "window-split" "split-window" "s-window") ("s-window"))
         ("w-s" ("split-window" "split-some-window" "ws-" "w-s" "window-split" "w-something")
          ("w-s" "w-something"))
         ("ker:" ("kernel:load" "keymap:resolved-binding" "ker:load" "ker-load") ("ker:load"))
         ("--" ("--x" "x---y" "x--y" "x-y") ("--x" "x---y"))
         ("" ("z" "aaa" "bb") ("aaa" "bb" "z"))
         ("missing" ("abc" "def") ())))

     ;; Presentation uses the very same alignment as ranking, including the
     ;; chosen occurrence of a repeated character and reordered fragments.
     (for-each
       (lambda (case)
         (let ([match (car (fuzzy:rank (car case) (list (cadr case))))])
           (check (list 'fuzzy-alignment (car case))
             (list (fuzzy:name match) (fuzzy:score match) (fuzzy:fragments match))
             (cdr case))))
       '(("windowsplit" "split-window!" (1 1 0 1 13) ((0 6 6) (6 0 5)))
         ("xx" "x-x-x" (1 0 0 1 5) ((0 0 1) (1 2 1)))
         ("split-w" "split-window!" (0 0 0 0 13) ((0 0 7)))
         ("window-split" "split-window-right!" (1 1 0 1 19) ((0 6 7) (7 0 5)))
         ("a-b" "ax-b:a-b" (0 0 5 0 8) ((0 5 3)))
         ("" "abc" (0 0 0 0 3) ())))

     ;; A caller's predicate confines the extensions to texts it can insert.
     (check 'fuzzy-expand-within-limits
       (list (fuzzy:expansions "ab" '("ab-x" "ab-y"))
             (fuzzy:expansions "ab" '("ab-x" "ab-y") (lambda (text) (not (memv #\- (string->list text))))))
       '(("ab-") ("ab")))

     (define symbols '("file-view:sort-by!" "split-window!" "split-window-right!"))
     (for-each
       (lambda (case)
         (check (list 'fuzzy-intent (car case)) (car (fuzzy:matches (car case) symbols)) (cadr case)))
       '(("splitright" "split-window-right!") ("rightsplit" "split-window-right!")
         ("sort-by" "file-view:sort-by!") ("sortby" "file-view:sort-by!")
         ("bysortfile" "file-view:sort-by!") ("fsort" "file-view:sort-by!")
         ("sorfile" "file-view:sort-by!")))

     ;; Every alternative has the same maximal length and preserves the set.
     ;; Include outsiders with the shared counts but incompatible boundaries:
     ;; normalization must refine the query, not just preserve character counts.
     (for-each
       (lambda (case)
         (let* ([query (car case)] [names (cadr case)]
                [before (fuzzy:matches query names)] [options (fuzzy:expansions query before)])
           (check (list 'fuzzy-expand query)
             (list (car options)
               (for-all
                 (lambda (option)
                   (and (= (string-length option) (string-length (caddr case)))
                        (or (null? before) (pair? (fuzzy:matches query (list option))))
                        (equal? (list-sort string<? (fuzzy:matches option names))
                                (list-sort string<? before))))
                 options))
             (list (caddr case) #t))))
       '(("splitwindow" ("split-window!" "split-window-right!" "split-windo!") "split-window")
         ("windowsplit" ("split-window!" "split-window-right!") "split-window")
         ("split-w" ("split-window!" "split-window-right!" "split:window") "split-window")
         ("sorfile" ("file-view:sort-by!" "sort-file" "sortfile") "sort-file")
         ("spwir" ("split-window-right!" "split-window!") "split-window-right!")
         ("split-windowr" ("split-window!" "split-window-right!" "paint:scroll-window!") "split-window-right!")
         ("split-window!r" ("split-window!" "split-window-right!" "paint:scroll-window!") "split-window!r")
         ("ab" ("ab-a" "azb" "a") "ab-a")
         ("k:c-w-ru" ("k:c-w-reg-u" "k:c-w-run-regs") "k:c-w-ru")
         ("ker" ("kernel:load" "keymap:resolved-binding") "ker")
         ("kern" ("kernel:load" "kernel:store" "keymap:resolved-binding") "kernel:")
         ("bc" ("abc" "bca" "cba" "b") "bca")
         ("xx" ("x-xa" "ax-x" "x") "x-xa")
         ("" ("a" "b") "")
         ("zz" ("abc" "def") "zz")))

     (check 'fuzzy-alternative-spellings
       (list (fuzzy:expansions "a" (fuzzy:matches "a" '("ac-b" "ab-c")))
             (fuzzy:expansions "splitwindow"
               (fuzzy:matches "splitwindow"
                 '("split-window!" "split-window-right!" "paint:scroll-window!")))
             ;; No candidate-order projection can express this maximum. Keep
             ;; one longest refinement, without pinning an arbitrary spelling.
             (let* ([names '("b-cd-a" "d-ab-c" "d-bc-a")]
                    [options (fuzzy:expansions "abcd" names)])
               (list (map string-length options)
                 (for-all
                   (lambda (option)
                     (and (pair? (fuzzy:matches "abcd" (list option)))
                          (= (length (fuzzy:matches option names)) (length names)))) options))))
       '(("ab" "ac") ("split-window") ((5) #t)))

     (test:finish! 'string)))
