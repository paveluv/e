#!/usr/bin/env scheme-script

;; The formatter's CLI contract: data-only stdout, scripts and directives,
;; stdin/files, in-place permissions and idempotence, and useful failures.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (sys sys) sys:) (prefix (foundation string) string:) (prefix (test) test:))

     (define here (current-directory))
     (define root (format "/tmp/e-format-~a-~a λ" (get-process-id) (random 1000000)))
     (define install (string-append root "/install"))
     (define tool (string-append install "/tools/scheme-format.sps"))
     (define (read-text port)
       (let ([text (get-string-all port)]) (if (eof-object? text) "" text)))
     (define (write-text path text)
       (call-with-output-file path (lambda (port) (display text port)) 'replace))
     (define (copy-text from to) (write-text to (call-with-input-file from read-text)))
     (define (remove-tree! path)
       (if (file-directory? path)
           (begin
             (for-each (lambda (name) (remove-tree! (string-append path "/" name))) (directory-list path))
             (delete-directory path))
           (delete-file path)))
     (define (run options input)
       (let ([process (sys:open-process (append (list "scheme-script" tool) options))])
         (dynamic-wind void
           (lambda ()
             (sys:write-process! process (and input (string->utf8 input)))
             (let ([bytes (get-bytevector-all (sys:process-input process))])
               (let-values ([(status errors) (sys:process-result process)])
                 (list status (if (eof-object? bytes) "" (utf8->string bytes)) errors))))
           (lambda () (sys:close-process! process)))))

     (define body "(let ((x 1))\n(+    x  2))  \n\n")
     (define formatted "(let ([x 1])\n  (+    x  2))\n")
     (define shebang "#!/usr/bin/env scheme-script\n")
     (define cases
       (list (list "config.e" '() body formatted)
             (list "module.sls" '() (string-append "#!r6rs\n" body)
                   (string-append "#!r6rs\n" formatted))
             (list "suite.ss" '() (string-append shebang body) (string-append shebang formatted))
             (list "tool.sps" '("--width" "20") (string-append "#! /usr/bin/env scheme-script\n" body)
                   "#! /usr/bin/env scheme-script\n(let ([x 1])\n  (+ x 2))\n")
             (list "e" '() "#!/usr/bin/env scheme-script" shebang)
             (list "empty.e" '() "" "")))

     (for-each mkdir (list root install (string-append install "/lib")
                           (string-append install "/lib/foundation")
                           (string-append install "/lib/sys") (string-append install "/lib/core")
                           (string-append install "/eo") (string-append install "/tools")))
     (dynamic-wind void
       (lambda ()
         ;; A private installation starts with a cold cache; invoking it from
         ;; elsewhere must find its own libraries and keep compilation quiet.
         (copy-text "tools/scheme-format.sps" tool)
         (for-each
           (lambda (path) (copy-text (string-append "lib/" path) (string-append install "/lib/" path)))
           '("foundation/scheme-format.sls" "core/kernel.sls" "sys/path.sls" "foundation/string.sls"
             "foundation/edoc.sls"))
         (current-directory root)
         (for-each
           (lambda (entry)
             (let ([path (string-append root "/" (car entry))] [flags (cadr entry)]
                   [source (caddr entry)] [expected (cadddr entry)])
               (write-text path source)
               (chmod path #o751)
               (let* ([file-output (run (append flags (list path)) #f)]
                      [stdin-output (run flags source)]
                      [in-place (run (append flags (list "-i" path)) #f)]
                      [again (run (append flags (list "-i" path)) #f)])
                 (test:check (list 'cli (car entry))
                   (list file-output stdin-output in-place again
                         (call-with-input-file path read-text) (logand (get-mode path) #o777))
                   (list (list 0 expected "") (list 0 expected "")
                         (list 0 (if (string=? source expected) "" (string-append path "\n")) "")
                         '(0 "" "") expected #o751)))))
           cases)
         (let ([path (string-append root "/broken.ss")]
               [broken (string-append shebang "(let ((x 1))\n")])
           (write-text path broken)
           (for-each
             (lambda (file?)
               (let* ([result (run (if file? (list "-i" path) '()) (and (not file?) broken))]
                      [errors (caddr result)])
                 (test:check (list 'read-error (if file? 'file 'stdin))
                   (list (car result) (cadr result)
                         (and (string:search errors (if file? path "stdin") 0 (string-length errors)) #t)
                         (and (string:search errors "read" 0 (string-length errors)) #t)
                         (string:search errors "~?" 0 (string-length errors))
                         (call-with-input-file path read-text))
                   (list 1 "" #t #t #f broken))))
             '(#t #f))))
       (lambda () (current-directory here) (remove-tree! root)))
     (test:finish! 'format)))
