;; A native namespace and retained results live only in this worker process.
(import (only (foundation edoc) elibrary))
(elibrary (run evaluator)
  (export run!)
  (import (chezscheme) (prefix (core evaluation) evaluation:)
          (prefix (service resource) resource:) (prefix (sys sys) sys:))

  (define (get r k) (cdr (assq k r)))
  (define (preview value)
    (call/cc
      (lambda (stop)
        (let ([out (make-string 1024)] [used 0])
          (define (result truncated?) (string-append (substring out 0 used) (if truncated? "…" "")))
          (let ([p (make-custom-textual-output-port "result preview"
                     (lambda (text start count)
                       (let ([n (min count (- 1024 used))])
                         (string-copy! text start out used n) (set! used (+ used n))
                         (when (< n count) (stop (result #t))) count)) #f #f void)])
            (parameterize ([print-length 16] [print-level 6] [print-graph #f])
              (if (condition? value) (display-condition value p) (write value p)))
            (flush-output-port p) (result #f))))))
  (define (small-copy value)
    (call/cc
      (lambda (fail)
        (let ([fuel 4096] [seen (make-eq-hashtable)])
          (define (charge n) (set! fuel (- fuel n)) (when (< fuel 0) (fail #f)))
          (define (number-size x)
            (cond [(not (exact? x)) 32]
              [(integer? x) (bitwise-length x)]
              [(real? x) (+ (number-size (numerator x)) (number-size (denominator x)))]
              [else (+ (number-size (real-part x)) (number-size (imag-part x)))]))
          (define (walk x)
            (charge 1)
            (cond
              [(or (pair? x) (vector? x))
               (when (hashtable-contains? seen x) (fail #f))
               (hashtable-set! seen x #t)
               (let ([out (if (pair? x) (cons (walk (car x)) (walk (cdr x)))
                            (begin (charge (vector-length x)) (vector-map walk x)))])
                 (hashtable-delete! seen x) out)]
              [(string? x) (charge (string-length x)) (string-copy x)]
              [(bytevector? x) (charge (bytevector-length x)) (bytevector-copy x)]
              [(symbol? x) (charge (string-length (symbol->string x))) x]
              [(number? x) (charge (number-size x)) x]
              [(or (null? x) (boolean? x) (char? x)) x]
              [else (fail #f)]))
          (list 'value (walk value))))))

  (edoc "Serve one private evaluator protocol on standard input/output. User code owns only its worker namespace; base resources use the installed broker."
        (returns any))
  (define (run!)
    (let ([in (sys:duplicate-standard-input-port)] [out (sys:duplicate-standard-output-port)]
          [output-lock (make-mutex)] [namespace #f] [handles (make-eqv-hashtable)]
          [next-handle 0] [symbols '()] [catalogue 0])
      (define (send datum)
        (with-mutex output-lock (write datum out) (newline out) (flush-output-port out)))
      (define (retain value)
        (let ([copy (small-copy value)] [label (preview value)])
          (if copy (append copy (list label))
            (begin (set! next-handle (+ next-handle 1)) (hashtable-set! handles next-handle value)
                   (list 'handle next-handle label)))))
      (define (capture thunk)
        (parameterize ([sys:capture-chunk-size 4096] [current-input-port (open-input-string "")])
          (evaluation:call! (lambda () (call-with-values thunk (lambda results (retain results))))
            (lambda (channel text) (send (list 'output channel text)))
            (lambda (run) (run)) (lambda (ex) #f))))
      (define (publish-catalogue!)
        (let ([names (if namespace (list-sort string<? (map symbol->string (environment-symbols namespace))) '())])
          (unless (equal? names symbols)
            (set! symbols names) (set! catalogue (+ catalogue 1))
            (let loop ([rest names] [left (length names)] [offset 0])
              (unless (null? rest)
                (let ([n (min 256 left)])
                  (send (list 'catalogue catalogue offset (list-head rest n)))
                  (loop (list-tail rest n) (- left n) (+ offset n))))))))
      (define (finish result)
        (publish-catalogue!)
        (send (list 'done (evaluation:status result) (and (pair? (evaluation:values result)) (car (evaluation:values result)))
                (and (evaluation:condition result)
                  (let ([ex (evaluation:condition result)])
                    (list (cons 'kind 'scheme) (cons 'who (and (who-condition? ex) (condition-who ex)))
                      (cons 'message (preview ex))))) catalogue (length symbols))))
      (resource:install!
        (lambda (operation . args)
          (send (list 'request operation args))
          (let ([reply (read in)])
            (unless (and (list? reply) (= (length reply) 2)) (error 'resource "invalid broker response"))
            (if (eq? (car reply) 'ok) (cadr reply) (error 'resource "base refused operation" (cadr reply))))))
      (let loop ([command (read in)])
        (unless (eof-object? command)
          (case (car command)
            [(initialize)
             (finish
               (capture
                 (lambda ()
                   (when namespace (error 'initialize "worker is already initialized"))
                   (let ([recipe (cadr command)])
                     (current-directory (get recipe 'directory))
                     (library-directories (get recipe 'roots))
                     (set! namespace (copy-environment (apply environment (get recipe 'imports)) #t))
                     (for-each (lambda (p) (define-top-level-value (car p) (cdr p) namespace)) (get recipe 'values))
                     (values)))))]
            [(evaluate)
             (finish (capture (lambda ()
                                (unless namespace (error 'evaluate "worker is not initialized"))
                                (let ([p (open-input-string (cadr command))])
                                  (let loop ([form (read p)] [last '()])
                                    (if (eof-object? form)
                                      (if (and (pair? (cddr command)) (caddr command))
                                        ((eval (caddr command) namespace) last)
                                        (apply values last))
                                      (call-with-values (lambda () (eval form namespace))
                                        (lambda result (loop (read p) result)))))))))]
            [(release)
             (for-each (lambda (id) (hashtable-delete! handles id)) (cadr command))
             (send '(released))]
            [else (error 'worker "unknown command" command)])
          (loop (read in)))))))
