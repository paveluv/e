;; Included by edoc.ss: actual compiled libraries and calls, without another
;; process. Inspection must retain lexical arguments without running bodies.
(let* ([root (format "/tmp/e-forwarding-~a-~a" (get-process-id) (random 1000000))]
       [source (string-append root ".ss")] [object (string-append root ".so")])
  (define (compile-form form)
    (when (file-exists? source) (delete-file source))
    (when (file-exists? object) (delete-file object))
    (call-with-output-file source
      (lambda (p) (write '(import (chezscheme) (foundation edoc)) p) (newline p) (write form p)))
    (guard (ex [(syntax-violation? ex) 'refused])
      (parameterize ([compile-file-message #f]) (compile-file source object))
      'compiled))
  (define (step-data proc args)
    (map (lambda (s) (cons (forwarding-name (car s)) (cdr s)))
      (forwarding-steps proc args)))
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (check 'forwarding-library-compiles
        (compile-form
          '(elibrary (forward-fixture)
             (export send! select! choice! lexical! loop! finish! calls)
             (import (chezscheme))
             (edoc "Execution count." (value integer))
             (define calls (make-parameter 0))
             (edoc "Finish." (args (list-of any)))
             (define (finish! . args) (calls (+ 1 (calls))) args)
             (define (dispatch! . args) (apply finish! args))
             (define (inspect-send args) (list (list finish! args #f #f)))
             (edoc "Send." (id integer) (action symbol) (args (list-of any)))
             (define-forwarding (send! id action . args) dispatch! inspect-send)
             (edoc "Local owner." (id integer) (returns integer) (inspect))
             (define (owner id) (+ id 1))
             (edoc "Select." (id integer) (command (list-of symbol)))
             (define (select! id . command)
               (let* ([target (owner id)] [selection (finish! 'row)])
                 (send! target (if (null? command) 'activate (car command)) selection)))
             (edoc "Choose a clause." (args (list-of any)))
             (define choice!
               (case-lambda [() #f] [args (send! (apply 2 'spread args))]))
             (edoc "Lexical scopes." (id integer))
             (define (lexical! id)
               (let ([id 9]) (send! id 'shadow))
               (let ([changed id]) (set! changed 3) (send! changed 'mutated))
               (let () (begin (define send! list) (send! 'ordinary))))
             ;; A private dispatcher is registered even without an edoc.
             (define-forwarding (again! id) again-impl! inspect-loop)
             (define (again-impl! id) (loop! id))
             (define (inspect-loop args) (list (list loop! args #f #f)))
             (edoc "A cyclic route." (id integer))
             (define (loop! id) (again! id))))
        'compiled)
      (load object)
      (eval '(import (prefix (forward-fixture) fixture:) (prefix (head keymap) keymap:)))
      (let* ([select (eval 'fixture:select!)] [choice (eval 'fixture:choice!)]
             [calls (eval 'fixture:calls)] [lexical (eval 'fixture:lexical!)]
             [send (eval '(forward-callee fixture:send!))])
        (check 'compiled-templates-resolve-local-queries-and-optional-arguments-without-running-actions
          (list (step-data select '((value 4)))
            (step-data select '((value 4) (value trash)))
            (inspection-value (eval 'fixture:finish!) '((value never))) (calls))
          '(((forward-fixture:send! ((value 5) (value activate) (unknown selection)) #f #f))
            ((forward-fixture:send! ((value 5) (value trash) (unknown selection)) #f #f))
            (unknown computed) 0))
        (check 'clause-order-spread-shadowing-and-mutation-are-conservative
          (list (step-data choice '())
            (step-data choice '((value a) (value b)))
            (step-data lexical '((value 1)))
            (select 4 'trash))
          '(() ((forward-fixture:send! ((value 2) (value spread) (value a) (value b)) #f #f))
            ((forward-fixture:send! ((value 9) (value shadow)) #f #f)
             (forward-fixture:send! ((unknown changed) (value mutated)) #f #f))
            (5 trash (row))))
        (check 'dispatch-syntax-keeps-typed-documentation
          (list (signature-formals (car (edoc-of send)))
            (map argument-type (signature-arguments (car (edoc-of send)))))
          '((id action . args) (integer symbol (list-of any))))
        (check 'aliases-and-explicit-registration-compile
          (compile-form
            '(begin
               (import (rename (forward-fixture) (send! relay!)))
               (expression (expression (define (compiled-forward id) (relay! id 'renamed))))))
          'compiled)
        (load object)
        (check 'import-renames-preserve-the-dispatch-identity
          (list (step-data (eval 'compiled-forward) '((value 7)))
            (eval '(compiled-forward 7)))
          '(((forward-fixture:send! ((value 7) (value renamed)) #f #f)) (7 renamed)))
        (check 'editor-evaluation-preserves-top-level-declaration-scopes
          (begin
            (kernel:evaluate!
              '(begin
                 (define-record-type forwarding-record (fields item))
                 (define-values (forwarding-a forwarding-b) (values 1 2))
                 (define-syntax forwarding-value
                   (syntax-rules () [(_) (forwarding-record-item (make-forwarding-record forwarding-b))])))
              (interaction-environment))
            (kernel:evaluate! '(list forwarding-a (forwarding-value) (fixture:send! 3 'interactive))
              (interaction-environment)))
          '(1 2 (3 interactive)))
        (check 'unregistered-dispatch-is-a-compiler-error
          (map (lambda (form)
                 (compile-form `(begin (import (prefix (forward-fixture) fixture:)) ,form)))
            '((fixture:send! 1 'raw)
              fixture:send!
              (apply fixture:send! '(1 raw))
              (expression
                (let-syntax ([hidden (syntax-rules () [(_) (fixture:send! 1 'hidden)])]) (hidden)))
              (expression
                (let-syntax ([hidden (syntax-rules () [(_) (fixture:send! 1 'hidden)])])
                  (fixture:send! 1 'outer (hidden))))))
          '(refused refused refused refused refused))
        (let* ([trace (eval '(keymap:action-trace (keymap:call fixture:loop! 1)))]
               [last (car (reverse trace))])
          (check 'private-dispatch-and-cycles-remain-inspectable
            (list (length trace) (forwarding-name (caddr (cadr trace))) (cadddr last))
            '(3 forward-fixture:again! "cycle")))
        (let* ([calls-before (calls)]
               [producer (lambda () (calls (+ 1 (calls))) 4)]
               [make-action (eval '(lambda (p) (keymap:call fixture:select! p)))]
               [trace ((eval 'keymap:action-trace) (make-action producer))]
               [same-name (eval '(keymap:action-trace (keymap:call fixture:select! 4 'selection)))])
          (check 'inspection-keeps-unresolved-spans-distinct-from-constants-without-running-actions
            (list (= calls-before (calls)) (length trace)
              (map (lambda (row)
                     (map (lambda (span) (substring (cadr row) (car span) (cdr span))) (list-ref row 4))) same-name))
            '(#t 3 (() ("selection") ("selection")))))))
    (lambda ()
      (for-each (lambda (p) (when (file-exists? p) (delete-file p))) (list source object)))))
