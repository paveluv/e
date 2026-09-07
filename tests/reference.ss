#!/usr/bin/env scheme-script

;; The base's corpus/query/fetch boundary. One isolated installation and
;; memory transport cover the pipeline without a head or network access.
(import (chezscheme))

(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (reference) reference:) (prefix (doc) doc:)
             (prefix (kernel) kernel:) (prefix (https) https:)
             (prefix (log) log:) (prefix (test) test:))

     (define root (format "/tmp/e-reference-~a" (get-process-id)))
     (define source (string-append (current-directory) "/lib/reference.e"))
     (define database (string-append root "/data/describe/describe.sdata"))
     (define (remove-tree! path)
       (if (file-directory? path)
           (begin
             (for-each (lambda (name) (remove-tree! (string-append path "/" name)))
                       (directory-list path))
             (delete-directory path))
           (delete-file path)))
     (define (fields entry)
       (map (lambda (get) (get entry))
            (list doc:names doc:forms doc:returns doc:libraries
                  doc:source doc:chapter doc:url doc:description)))
     (define (registered! description)
       (kernel:call-with-registration-update
         (lambda ()
           (kernel:retract-module! 'reference-test)
           (when description
             (parameterize ([kernel:registering-module 'reference-test])
               (doc:register!
                 `(((s9-reference) (("procedure" . "(s9-reference)"))
                    #f ("(fixture)") fixture "Module entry" #f ,description))))))))

     (define html
       (string-append
         "<title>Reference Fixture</title><a name=\"./s9-reference\"></a>"
         "<span class=formdef><b>procedure</b>: <tt>(s9-reference value)</tt></span><br>"
         "<span class=formdef><b>procedure</b>: <tt>(s9-alias value)</tt></span><br>"
         "<b>returns: </b>a result<br><b>libraries: </b>(rnrs base), (rnrs)<br>"
         "<p>Use <tt>value</tt> with <b>care</b> &amp; <i>thought</i>."
         "<h2>Next section</h2>Discard this section."))
     (define requests '())
     (define closed 0)
     (define fail-at #f)
     (define arrived (test:gate))
     (define release (test:gate))
     (define paused #f)
     (define presented (test:recorder))
     (define (connect host port)
       (when paused
         (set! paused #f)
         (arrived #t)
         (test:await 'resume-fetch release))
       (let* ([failed? (and fail-at (= (length requests) fail-at))]
              [body (if failed? "x" html)]
              [input (open-bytevector-input-port
                       (string->utf8
                         (format "HTTP/1.1 200 OK\r\nContent-Length: ~a\r\n\r\n~a"
                                 (string-length html) body)))])
         (https:make-channel
           (lambda (bv start count)
             (let ([n (get-bytevector-n! input bv start count)]) (if (eof-object? n) 0 n)))
           (lambda (bv) (set! requests (cons (list host port (utf8->string bv)) requests)))
           (lambda () (set! closed (+ closed 1)) (close-port input)))))
     (define (fetch!)
       (parameterize ([https:backend 'native] [https:connector connect]) (reference:fetch!)))

     (mkdir root)
     (mkdir (string-append root "/lib"))
     (mkdir (string-append root "/eo"))
     (dynamic-wind
       void
       (lambda ()
         (parameterize ([library-directories
                         (cons (cons (string-append root "/lib") (string-append root "/eo"))
                               (library-directories))])
           (test:check 'reference-has-no-head-dependencies
             (map (lambda (name) (kernel:module-requires? "reference" name))
                  '("edit" "head" "paint" "prompt" "markdown" "describe"))
             '(#f #f #f #f #f #f))
           (test:check 'missing-corpus (list (reference:entries) (reference:lookup "missing")) '(() ()))
           (registered! "local documentation")
           (test:check 'registered-without-corpus
             (map doc:description (reference:lookup 's9-reference)) '("local documentation"))
           (log:set-presenter!
             (lambda (entry show?)
               (presented (list show? (length (reference:lookup 's9-alias))))))

           ;; While a worker owns the fetch, readers retain complete state
           ;; and a second fetch is refused before it can open another file.
           (set! paused #t)
           (let ([finish (test:worker (lambda () (fetch!) 'done))])
             (test:await 'fetch-entered arrived)
             (dynamic-wind
               void
               (lambda ()
                 (test:check 'read-and-competing-fetch
                   (list (map doc:source (reference:lookup 's9-reference))
                         (test:raises? fetch!) (length requests))
                   '((fixture) #t 0)))
               (lambda () (release #t)))
             (test:check 'fetch-completes (finish) 'done))

           (test:check 'book-routing-and-transport-completion
             (list (map (lambda (host) (length (filter (lambda (r) (equal? (car r) host)) requests)))
                        '("www.scheme.com" "cisco.github.io"))
                   (for-all (lambda (r) (= (cadr r) 443)) requests) closed)
             '((8 14) #t 22))
           (test:check 'queries-share-corpus-order-and-live-registry
             (let ([entries (reference:lookup "s9-reference")])
               (list (map doc:source entries)
                     (equal? entries (reference:entries))
                     (map doc:source (reference:entries (lambda (e) (eq? (doc:source e) 'fixture))))
                     (length (reference:lookup 's9-alias))))
             (list (append (make-list 8 'tspl) (make-list 14 'csug) '(fixture)) #t '(fixture) 22))
           (test:check 'extraction-and-browser-links
             (let* ([entries (reference:lookup 's9-reference)] [first (car entries)])
               (list (fields first)
                     (map reference:browser-url (list first (list-ref entries 8) (car (reverse entries))))))
             '(((s9-reference s9-alias)
                (("procedure" . "(s9-reference value)") ("procedure" . "(s9-alias value)"))
                "a result" ("(rnrs base)" "(rnrs)") tspl "Reference Fixture"
                "binding.html#./s9-reference" "Use `value` with **care** & *thought*.")
               ("https://www.scheme.com/tspl4/binding.html#./s9-reference"
                "https://cisco.github.io/ChezScheme/csug10.0/binding.html#./s9-reference" #f)))
           (test:check 'fetch-progress-is-in-the-log
             (let ([messages (reverse (map caddr (log:entries 'describe)))])
               (list (length messages) (car messages) (list-ref messages 22) (list-ref messages 23)
                     (presented)))
             (list 24 "Fetching tspl4/binding.html (1/22)" "Extracting the reference corpus..."
                   "Describe database ready: 22 entries covering 2 names"
                   (append (make-list 23 '(#f 0)) '((#t 22)))))

           ;; A failed refresh closes its transport and releases ownership;
           ;; it must not replace the database or the already published index.
           (let ([before (map fields (reference:entries))]
                 [disk (call-with-input-file database get-string-all)])
             (set! fail-at (length requests))
             (test:check 'failed-refresh-keeps-complete-data
               (list (test:raises? fetch!)
                     (equal? before (map fields (reference:entries)))
                     (equal? disk (call-with-input-file database get-string-all))
                     (= closed (length requests)))
               '(#t #t #t #t))
             (set! fail-at #f)
             (fetch!)
             (test:check 'refresh-can-retry (map fields (reference:entries)) before))
           (for-each
             (lambda (description)
               (registered! description)
               (test:check (list 'module-registry-remains-live description)
                 (map doc:description (reference:entries (lambda (e) (eq? (doc:source e) 'fixture))))
                 (if description (list description) '())))
             '("updated documentation" #f))
           (let ([before (map fields (reference:entries))])
             (load source)
             (test:check 'fresh-base-instance-reads-the-saved-corpus
               (map fields (eval '(begin (import (prefix (reference) reference:)) (reference:entries))))
               before))))
       (lambda () (log:set-presenter! #f) (registered! #f) (remove-tree! root)))
     (test:finish! 'reference)))
