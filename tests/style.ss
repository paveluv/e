#!/usr/bin/env scheme-script

;; The style DSL compiler: every attribute, color form, and cancellation
;; compiles to the documented SGR parameters. Run from the repository
;; root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (edit) (prefix (style) style:) (prefix (test) test:)
             (prefix (kernel) kernel:))

     (define check test:check)

     ;; Theme changes invalidate cached ink, preserve explicit overrides,
     ;; and leave faces with no light variant alone. Repeated reports are inert.
     (let ([dark (style:code 'header)] [chrome (style:code 'chrome)] [changes 0])
       (style:set-changed-hook! (lambda () (set! changes (+ changes 1))))
       (style:color-scheme! #f)
       (style:color-scheme! 'light)
       (let ([light (style:code 'header)])
         (style:color-scheme! 'light)
         (parameterize ([kernel:registering-module 'style-test])
           (style:set! 'header '(reverse)))
         (style:color-scheme! 'dark)
         (let ([override (style:code 'header)])
           (style:color-scheme! 'light)
           (let ([kept? (equal? override (style:code 'header))])
             (kernel:retract-module! 'style-test)
             (check 'theme-defaults-overrides-and-repaint
               (list (not (equal? dark light)) (equal? chrome (style:code 'chrome))
                     (equal? override (style:escape '(reverse))) kept?
                     (equal? light (style:code 'header)) changes
                     (guard (ex [else 'rejected]) (style:color-scheme! 'invalid)))
               '(#t #t #t #t #t 4 rejected)))))
       (style:set-changed-hook! #f)
       (style:color-scheme! #f))

     (check 'surface-sgr-values-are-parameters-only
       (map style:code '("" "31" "4:3;38:2::1:2:3" "31mBAD" "\x1b;[31" "31\n"))
       (list "\x1b;[m" "\x1b;[31m" "\x1b;[4:3;38:2::1:2:3m"
             (style:code 'plain) (style:code 'plain) (style:code 'plain)))

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

     (test:finish! 'style)))
