#!/usr/bin/env scheme-script

;; The style DSL compiler: every attribute, color form, and cancellation
;; compiles to the documented SGR parameters. Run from the repository
;; root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (edit) (prefix (only (style) compile) style:))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'style-test label actual expected)))

     (check 'reset (style:compile '(reset)) "0")
     (check 'empty-is-reset (style:compile '()) "0")
     (check 'attributes
            (style:compile '(bold dim italic underline blink
                             reverse hidden strike))
            "1;2;3;4;5;7;8;9")
     (check 'extended-attributes
            (style:compile '(double-underline overline framed encircled
                             superscript subscript))
            "21;53;51;52;73;74")
     (check 'underline-variants
            (style:compile '(curly-underline dotted-underline
                             dashed-underline))
            "4:3;4:4;4:5")
     (check 'cancellations
            (style:compile '(normal-intensity no-italic no-underline
                             no-blink no-reverse no-hidden no-strike
                             no-frame no-overline))
            "22;23;24;25;27;28;29;54;55")
     (check 'named-colors
            (style:compile '((foreground red) (background bright-blue)))
            "31;104")
     (check 'palette-and-rgb
            (style:compile '((fg 245) (bg (rgb 1 2 3))))
            "38;5;245;48;2;1;2;3")
     (check 'default-colors
            (style:compile '((foreground default) (background default)))
            "39;49")
     (check 'underline-colors
            (style:compile '((underline-color 208)))
            "58;5;208")
     (check 'underline-color-named
            (style:compile '((underline-color bright-red)))
            "58;5;9")
     (check 'underline-color-rgb
            (style:compile '((underline-color (rgb 4 5 6))))
            "58;2;4;5;6")
     (check 'underline-color-default
            (style:compile '((underline-color default)))
            "59")
     (check 'order-preserved
            (style:compile '(bold (foreground cyan) curly-underline
                             (underline-color 135)))
            "1;36;4:3;58;5;135")
     (check 'unknown-attribute-rejected
            (guard (ex [else 'rejected]) (style:compile '(sparkle)))
            'rejected)
     (check 'unknown-color-rejected
            (guard (ex [else 'rejected])
              (style:compile '((foreground maroon))))
            'rejected)

     (format #t "~a style checks passed\n" checks)))
