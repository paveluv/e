#!/usr/bin/env scheme-script

;; The base's corpus/query/fetch boundary. One isolated installation and
;; memory transport cover the pipeline without a head or network access.
(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (reference) reference:) (prefix (doc) doc:)
             (prefix (datum) datum:)
             (prefix (kernel) kernel:) (prefix (https) https:)
             (prefix (log) log:) (prefix (store) store:) (prefix (test) test:))

     (define root (format "/tmp/e-reference-~a" (get-process-id)))
     (define source (string-append (current-directory) "/lib/reference.e"))
     (define database (string-append root "/data/describe/describe.sdata"))
     (define page-heads '((head "west λ") (head "east")))
     (define page-ids '())
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
     (define (damage! data)
       (cond [(pair? data) (damage! (car data)) (damage! (cdr data)) (set-car! data 'damaged)]
             [(and (string? data) (> (string-length data) 0)) (string-set! data 0 #\X)]))
     (define (owned-reads? entry expected)
       (damage! (fields entry))
       (damage! (doc:to-datum entry))
       (equal? (fields entry) expected))
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
     (define log-token #f)
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
     (dynamic-wind
       void
       (lambda ()
         (parameterize ([kernel:installation-directory root])
           (test:check 'reference-has-no-head-dependencies
             (map (lambda (name) (kernel:module-requires? "reference" name))
                  '("edit" "head" "paint" "prompt" "markdown" "describe"))
             '(#f #f #f #f #f #f))
           (test:check 'missing-corpus (list (reference:entries) (reference:lookup "missing")) '(() ()))
           (registered! "local documentation")
           (test:check 'registered-without-corpus
             (map doc:description (reference:lookup 's9-reference)) '("local documentation"))

           ;; One nested-alias table covers every admission and every field,
           ;; with real registry/query readers where the record is published.
           (let* ([expected '((owned-name owned-alias) (("procedure" . "(owned-name value)"))
                              "a result" ("(fixture)") "fixture source" "Chapter" "chapter#anchor" "Original prose")]
                  [replacement (doc:from-datum '((replacement) () #f () fixture "" #f ""))])
             (for-each
               (lambda (kind)
                 (let ([input (datum:copy expected)])
                   (define (inspect entry)
                     (damage! input)
                     (let* ([input-owned? (equal? (fields entry) expected)]
                            [reads-owned? (owned-reads? entry expected)])
                       (append (list input-owned? reads-owned?)
                         (if (memq kind '(registered scoped))
                             (let ([before (map fields (reference:entries))])
                               ;; Mutation of any returned list, including its
                               ;; final scoped tail, cannot rewrite the next query.
                               (for-each
                                 (lambda (entries)
                                   (let walk ([entries entries])
                                     (unless (null? entries)
                                       (set-car! entries replacement)
                                       (walk (cdr entries)))))
                                 (list (doc:entries) (reference:entries) (reference:lookup 'owned-name)))
                               (list (and (equal? before (map fields (reference:entries)))
                                          (equal? (map fields (reference:lookup 'owned-alias)) (list expected)))))
                             '()))))
                   (dynamic-wind void
                     (lambda ()
                       (test:check (list kind 'owns-document-data)
                         (case kind
                           [(make) (inspect (apply doc:make input))]
                           [(datum) (inspect (doc:from-datum input))]
                           [(registered)
                            (parameterize ([kernel:registering-module 'reference-ownership])
                              (doc:register! (list input)))
                            (inspect (car (reference:lookup 'owned-name)))]
                           [(scoped)
                            (doc:call-with-entries (list input)
                              (lambda () (inspect (car (reference:lookup 'owned-name)))))])
                         (if (memq kind '(registered scoped)) '(#t #t #t) '(#t #t))))
                     (lambda () (kernel:retract-module! 'reference-ownership)))))
               '(make datum registered scoped))
             (let ([before (map fields (reference:entries))] [cycle (list 'cycle)])
               (set-cdr! cycle cycle)
               (test:check 'invalid-document-batches-do-not-publish-their-valid-prefix
                 (map (lambda (bad)
                        (list (test:raises? (lambda () (doc:register! (list expected bad))))
                              (equal? before (map fields (reference:entries)))))
                      (list '(too short) (append (list-head expected 7) (list cycle))
                            (append (list-head expected 7) (list void))))
                 (make-list 3 '(#t #t)))
               (test:check 'scoped-documents-restore-on-exit-and-do-not-escape
                 (list (test:raises?
                         (lambda ()
                           (doc:call-with-entries (list expected)
                             (lambda ()
                               (doc:call-with-entries '() void)
                               (unless (= (length (reference:lookup 'owned-name)) 1)
                                 (error 'reference-test "outer query was lost"))
                               (raise 'query-done))))
                         (lambda (ex) (eq? ex 'query-done)))
                       (equal? before (map fields (reference:entries)))
                       (reference:lookup 'owned-name))
                 '(#t #t ()))))

           ;; These are logical requester identities, with no head loaded.
           ;; Privacy and source facts must already hold at the create event.
           (let* ([created (test:recorder)]
                  [token
                   (store:subscribe! #f
                     (lambda (event)
                       (when (eq? (car event) 'create)
                         (let ([id (cadr event)])
                           (created
                             (list (store:property id 'audience)
                                   (map (lambda (key) (store:property id key))
                                        '(mode read-only disposable modified))
                                   (map (lambda (head) (store:visible? head id)) page-heads)))))))])
             (set! page-ids
               (fold-left (lambda (ids head)
                            (append ids (list (reference:page! head "s9-reference" '("C-x")))))
                          '() page-heads))
             (store:unsubscribe! token)
             (test:check 'private-markdown-facts-publish-with-content
               (list (map store:buffer-name page-ids) (created))
               '(("*describe*" "*describe*<2>")
                 ((((head "west λ")) ("markdown" #t #t #f) (#t #f))
                  (((head "east")) ("markdown" #t #t #f) (#f #t))))))
           (test:check 'base-produces-markdown-source
             (let-values ([(lines revision) (store:snapshot (car page-ids))])
               (vector->list lines))
             '("**keys**: C-x  " "" "**procedure**: `(s9-reference)`  "
               "libraries: (fixture)  " "source: fixture, Module entry  " "" "local documentation"))
           (let* ([head (car page-heads)] [id (car page-ids)]
                  [events (test:recorder)] [token (store:subscribe! id events)])
             (test:check 'same-or-missing-selection-leaves-the-page-alone
               (list (reference:page! head 's9-reference '("C-x"))
                     (reference:page! head 'missing '()) (reference:page head) (events))
               (list id #f (list id 0 's9-reference) '()))
             (store:unsubscribe! token)
             (registered! "updated page")
             (test:check 'refresh-publishes-live-documents-for-one-requester
               (list (reference:page! head 's9-reference '("C-x") (cons id 0))
                     (store:line id 6) (reference:page head)
                     (store:line (cadr page-ids) 6) (store:revision (cadr page-ids)))
               (list id "updated page" (list id 1 's9-reference) "local documentation" 0))
             (for-each
               (lambda (action)
                 (let* ([id (reference:page! head 's9-reference '("C-x"))]
                        [basis (cons id (store:revision id))])
                   (if (eq? action 'hide) (store:set-property! head id 'audience '())
                       (store:delete! head id))
                   (test:check (list 'page-refresh-respects action)
                     (list (reference:page! head 's9-reference '("C-x") basis)
                           (reference:page head) (store:visible? head id))
                     '(#f #f #f))))
               '(hide delete)))
           (let ([head (cadr page-heads)] [id (cadr page-ids)])
             (store:drop-property! head id 'audience)
             (test:check 'page-read-honors-default-audience-but-refresh-does-not-restore-it
               (list (caddr (reference:page head))
                     (reference:page! head 's9-reference '("C-x") (cons id (store:revision id)))
                     (store:property id 'audience))
               '(s9-reference #f #f))
             (reference:page! head 's9-reference '("C-x"))
             (registered! #f)
             (reference:page! head 's9-reference '("C-x") (cons id (store:revision id)))
             (test:check 'retracted-document-keeps-a-refreshable-selection
               (list (store:line id 0) (caddr (reference:page head)))
               '("No documentation for s9-reference" s9-reference))
             (registered! "local documentation"))

           (set! log-token
             (log:subscribe!
               (lambda (entry presentation)
                 (presented (list presentation (length (reference:lookup 's9-alias)))))))

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
               (list (let ([expected (fields first)])
                       ;; Corpus entries use the same immutable record and
                       ;; accessor contract as module and request documents.
                       (and (owned-reads? first expected) (fields first)))
                     (map reference:browser-url (list first (list-ref entries 8) (car (reverse entries))))))
             '(((s9-reference s9-alias)
                (("procedure" . "(s9-reference value)") ("procedure" . "(s9-alias value)"))
                "a result" ("(rnrs base)" "(rnrs)") tspl "Reference Fixture"
                "binding.html#./s9-reference" "Use `value` with **care** & *thought*.")
               ("https://www.scheme.com/tspl4/binding.html#./s9-reference"
                "https://cisco.github.io/ChezScheme/csug10.0/binding.html#./s9-reference" #f)))
           (test:check 'fetch-progress-is-in-the-log
             (let ([messages (reverse (map log:datum (log:entries 'describe)))])
               (list (length messages) (car messages) (list-ref messages 22) (list-ref messages 23)
                     (presented)))
             (list 24 "Fetching tspl4/binding.html (1/22)" "Extracting the reference corpus..."
                   "Describe database ready: 22 entries covering 2 names"
                   (append (make-list 23 '(#f 0)) '((append 22)))))

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
           (let ([before (list (map fields (reference:entries))
                               (map reference:page page-heads))])
             (load source)
             (test:check 'fresh-base-instance-recovers-corpus-and-selected-page
               (let ([next (eval `(begin (import (prefix (reference) reference:))
                                         (list (reference:entries) (map reference:page ',page-heads))))])
                 (list (map fields (car next)) (cadr next)))
               before))))
       (lambda ()
         (when log-token (log:unsubscribe! log-token))
         (registered! #f)
         (for-each (lambda (id) (when (store:exists? id) (store:delete! '(app describe) id))) page-ids)
         (remove-tree! root)))
     (test:finish! 'reference)))
