;; wire.sls -- length-prefixed plain data. No store, actor or display state.
(library (wire)
  (export version encode send! receive)
  (import (rnrs)
          (only (chezscheme) parameterize print-length print-level print-graph)
          (prefix (datum) datum:))

  ;; S3 requires the pre-screen startup-notice exchange. Older heads cannot
  ;; silently attach to restored work; maintenance keeps its own version 1.
  (define version 4)
  (define frame-limit #x1000000) ; 16 MiB, checked before reading a payload

  (define (frame-size! size)
    (unless (<= 1 size frame-limit)
      (error 'wire "frame must contain 1 through 16777216 bytes" size)))

  (define (encode value)
    (let* ([owned (datum:copy value)]
           [payload (string->utf8
                      (call-with-string-output-port
                        (lambda (out)
                          (parameterize ([print-length #f] [print-level #f] [print-graph #f])
                            (write owned out)))))]
           [size (bytevector-length payload)])
      (frame-size! size)
      (let ([frame (make-bytevector (+ size 4))])
        (bytevector-u32-set! frame 0 size (endianness big))
        (bytevector-copy! payload 0 frame 4 size)
        frame)))

  (define (send! port value)
    ;; The connection owns serialization of complete frames. An outbox can
    ;; retain encode's owned bytes and bound them without serializing twice.
    (put-bytevector port (encode value))
    (flush-output-port port))

  (define (receive port)
    (let ([header (get-bytevector-n port 4)])
      (if (eof-object? header) header
          (begin
            (unless (= (bytevector-length header) 4) (error 'wire "truncated frame header"))
            (let ([size (bytevector-u32-ref header 0 (endianness big))])
              (frame-size! size)
              (let ([payload (get-bytevector-n port size)])
                (unless (and (bytevector? payload) (= (bytevector-length payload) size))
                  (error 'wire "truncated frame payload"))
                (let* ([in (open-string-input-port (utf8->string payload))]
                       [value (read in)])
                  (when (eof-object? value) (error 'wire "empty datum"))
                  (unless (eof-object? (read in)) (error 'wire "more than one datum in a frame"))
                  (datum:copy value))))))))
)
