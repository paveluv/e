;; wire.sls -- length-prefixed plain data. No store, actor or display state.
(import (only (foundation edoc) elibrary))
(elibrary (foundation wire)
  (export encode receive send! version)
  (import (rnrs)
          (only (chezscheme) parameterize print-length print-level print-graph)
          (prefix (foundation datum) datum:))

  ;; S4 requires the source fingerprint in every normal hello. Maintenance
  ;; retains its version 1 contract so mismatched builds can still restart.
  (edoc "The wire protocol version a hello must carry."
        (value integer))
  (define version 5)
  (define frame-limit #x1000000) ; 16 MiB, checked before reading a payload

  (define (frame-size! size)
    (unless (<= 1 size frame-limit)
      (error 'wire "frame must contain 1 through 16777216 bytes" size)))

  (edoc "A value as one wire frame: a 4-byte big-endian length and the written datum in UTF-8."
        (value datum "the value")
        (returns bytevector))
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

  (edoc "Write a value to a port as one frame and flush."
        (port port "the output port")
        (value datum "the value"))
  (define (send! port value)
    ;; The connection owns serialization of complete frames. An outbox can
    ;; retain encode's owned bytes and bound them without serializing twice.
    (put-bytevector port (encode value))
    (flush-output-port port))

  (edoc "Read one frame from a port and read its datum; eof when the port ends."
        (port port "the input port")
        (returns any))
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
