;; One request owner reads/reaps a worker. Cancellation only signals it.
(import (only (foundation edoc) elibrary))
(elibrary (core worker)
  (export close! open! request!)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (sys sys) sys:))
  (define-record-type worker (fields process input gate lock (mutable closed?)))
  (define (send w datum)
    (sys:write-process! (worker-process w) (string->utf8 (format "~s\n" datum)) #f))

  (edoc "Start a private evaluator process. Initialization is an explicit request; the caller supervises lifetime and admits all effects."
        (returns any))
  (define (open!)
    (let* ([root (kernel:installation-directory)]
           [p (sys:open-process (list "scheme" "--script" (string-append root "/tools/environment-worker.sps") root))])
      (make-worker p (transcoded-port (sys:process-input p) (make-transcoder (utf-8-codec)))
        (make-mutex) (make-mutex) #f)))

  (edoc "Execute one private protocol command. Output/catalogue messages go to emit; resource requests go to the base broker. Calls serialize within this worker; no model/store writer may enclose this wait."
        (worker any "worker") (command datum "private command")
        (emit procedure "message sink; blocking applies pipe backpressure")
        (broker procedure "operation and arguments -> portable response") (returns datum))
  (define (request! worker command emit broker)
    (with-mutex (worker-gate worker)
      (when (with-mutex (worker-lock worker) (worker-closed? worker)) (error 'request! "worker is closed"))
      (guard (ex [else (with-mutex (worker-lock worker) (worker-closed?-set! worker #t))
                       (sys:close-process! (worker-process worker)) (raise ex)])
        (send worker command)
        (let loop ([message (read (worker-input worker))])
          (when (eof-object? message) (error 'request! "worker exited before completing its request"))
          (case (car message)
            [(done released) message]
            [(output catalogue) (emit message) (loop (read (worker-input worker)))]
            [(request)
             (send worker
               (guard (ex [else (list 'error (kernel:condition-text ex))])
                 (list 'ok (apply broker (cadr message) (caddr message)))))
             (loop (read (worker-input worker)))]
            [else (error 'request! "invalid worker message" message)])))))

  (edoc "Stop an evaluator, unblock its request owner and reap it. Running cancellation discards this namespace; a caller must fence its generation before calling."
        (worker any "worker"))
  (define (close! worker)
    (with-mutex (worker-lock worker)
      (unless (worker-closed? worker)
        (worker-closed?-set! worker #t)
        (sys:signal-process! (worker-process worker) 9)))
    (with-mutex (worker-gate worker)
      (sys:close-process! (worker-process worker))
      (unless (port-closed? (worker-input worker)) (close-port (worker-input worker))))))
