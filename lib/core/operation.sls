;; Named base operations share their ordinary edoc on both runtimes.
(import (only (foundation edoc) elibrary))
(elibrary (core operation)
  (export attachment dispatch! implementation register!)
  (import (chezscheme)
          (prefix (core endpoint) endpoint:)
          (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:)
          (prefix (foundation edoc) edoc:))

  (define operations (kernel:make-registry car))
  (define current-attachment (make-thread-parameter #f))

  (edoc "The opaque identity of the connection invoking this base operation, or false outside an invocation. Compare by identity for attachment-local ownership; this is not authority and cannot be serialized or persisted. The authenticated actor is actor:current."
        (returns any))
  (define (attachment) (current-attachment))

  (define (contract procedure)
    (let ([signatures (edoc:edoc-of procedure)])
      (unless (and signatures (= (length signatures) 1)
                   (eq? (edoc:signature-kind (car signatures)) 'procedure)
                   (list? (edoc:signature-formals (car signatures))))
        (error 'operation:register! "expected one documented fixed-arity procedure" procedure))
      (let* ([signature (car signatures)]
             [arguments (map (lambda (name)
                               (edoc:argument-type
                                 (find (lambda (a) (eq? name (edoc:argument-name a)))
                                   (edoc:signature-arguments signature))))
                          (edoc:signature-formals signature))]
             [result (edoc:signature-returns signature)]
             [results (and result
                        (let ([type (edoc:argument-type result)])
                          (if (and (pair? type) (eq? (car type) 'values)) (cdr type) (list type))))])
        (unless (for-all edoc:type-portable? (append arguments (or results '())))
          (error 'operation:register! "operation contracts must use portable types" arguments results))
        (list arguments results))))

  (edoc "Register a named base operation during its module's init!. The procedure's edoc supplies the fixed positional contract: returns describes one result, (returns (values ...)) multiple or zero, and no returns clause acknowledges void completion. Read operations admit any live session; control requires an all-buffer head."
        (name symbol "qualified public name owned by this module")
        (procedure procedure "documented implementation or generated client proxy")
        (admission (one-of read control) "existing connection admission class"))
  (define (register! name procedure admission)
    (let ([owner (kernel:registering-module)])
      (unless (and (symbol? owner) (symbol? name)
                   (let ([prefix (string-append (symbol->string owner) ":")]
                         [text (symbol->string name)])
                     (and (> (string-length text) (string-length prefix))
                          (string=? prefix (substring text 0 (string-length prefix)))))
                   (procedure? procedure) (memq admission '(read control)))
        (error 'operation:register! "expected a module-owned qualified operation and read/control admission" name admission))
      (kernel:registry-add! operations (list name procedure admission (contract procedure)))))

  (define (checked types values)
    (let ([owned (datum:copy values)])
      (unless (and (list? owned) (= (length types) (length owned))
                   (for-all edoc:type-accepts? types owned))
        (error 'operation "values do not satisfy the declared contract" types owned))
      owned))

  (edoc "Invoke one registered definition pinned before authorization. The connection supplies admission and authenticated actor context; arguments and results are owned portable data. Errors use the enclosing transport's normal error reply."
        (name symbol "qualified registered operation") (expected list "client edoc contract")
        (arguments list "positional values") (admit! procedure "check the pinned admission class")
        (connection any "authenticated attachment identity supplied by the transport, never by the caller")
        (returns list "completion acknowledgement or declared result values"))
  (define (dispatch! name expected arguments admit! connection)
    (let ([entry (kernel:registry-find operations (lambda (entry) (eq? name (car entry))))])
      (unless entry (error 'operation "operation is not registered" name))
      (admit! (caddr entry))
      (let ([spec (cadddr entry)])
        (unless (equal? expected spec) (error 'operation "operation contract changed; reload its client module" name))
        (let ([arguments (checked (car spec) arguments)])
          (call-with-values (lambda () (parameterize ([current-attachment connection]) (apply (cadr entry) arguments)))
            (lambda results
              (if (cadr spec)
                  (cons 'values (checked (cadr spec) results))
                  (begin
                    (unless (and (= (length results) 1) (eq? (car results) (void)))
                      (error 'operation "operation without returns must complete with void" name))
                    '(completed)))))))))

  (define (remote-call procedure arguments request)
    (let ([entry (kernel:registry-find operations (lambda (entry) (eq? procedure (cadr entry))))])
      (unless entry (error 'operation "client operation is not registered; load its module"))
      (let* ([spec (cadddr entry)]
             [answer (request 'invoke (car entry) spec (checked (car spec) arguments))])
        (if (cadr spec)
            (begin
              (unless (and (pair? answer) (eq? (car answer) 'values))
                (error 'operation "expected result values" answer))
              (apply values (checked (cadr spec) (cdr answer))))
            (begin
              (unless (equal? answer '(completed)) (error 'operation "expected completion acknowledgement" answer))
              (void))))))

  (edoc "Expansion support for elibrary's define-operation: keep the base implementation, or generate a client proxy from the same documented procedure.")
  (define-syntax implementation
    (syntax-rules ()
      [(_ name formals body)
       (endpoint:implementation name formals body remote-call)]))
)
