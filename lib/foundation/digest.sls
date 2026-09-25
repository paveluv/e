;; digest.sls -- the cryptographic hashes e computes itself: the library
;; (digest).  One home for every message digest to come, SHA-256 alone for
;; now, as FIPS 180-4 specifies it, over bytes or a string's UTF-8, the
;; digest as bytes or as lowercase hex: (digest:sha256 "abc"),
;; (digest:hex (digest:sha256 bytes)).  The words stay 32-bit fixnums
;; throughout: every rotation masks before it shifts, every sum masks
;; after it adds, so no bignum is made on the way.

(import (only (foundation edoc) elibrary))
(elibrary (foundation digest)
  (export hex sha256)
  (import (rnrs) (only (chezscheme) vector-copy))

  (define mask32 #xffffffff)

  ;; the round constants: the first 32 bits of the fractional parts of the
  ;; cube roots of the first 64 primes
  (define rounds
    '#(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
       #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
       #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
       #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
       #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
       #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
       #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
       #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

  ;; the initial state: the first 32 bits of the fractional parts of the
  ;; square roots of the first 8 primes
  (define initial-state
    '#(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

  (define (add a b) (fxand (fx+ a b) mask32))

  (define (rotate-right x n)
    ;; a 32-bit word rotated right by n: its low n bits move to the top
    (fxior (fxarithmetic-shift-right x n)
           (fxarithmetic-shift-left (fxand x (fx- (fxarithmetic-shift-left 1 n) 1)) (fx- 32 n))))

  (define (big-sigma0 x) (fxxor (rotate-right x 2) (rotate-right x 13) (rotate-right x 22)))
  (define (big-sigma1 x) (fxxor (rotate-right x 6) (rotate-right x 11) (rotate-right x 25)))
  (define (small-sigma0 x) (fxxor (rotate-right x 7) (rotate-right x 18) (fxarithmetic-shift-right x 3)))
  (define (small-sigma1 x) (fxxor (rotate-right x 17) (rotate-right x 19) (fxarithmetic-shift-right x 10)))
  (define (choose e f g) (fxxor (fxand e f) (fxand (fxxor e mask32) g)))
  (define (majority a b c) (fxxor (fxand a b) (fxand a c) (fxand b c)))

  (edoc "The SHA-256 digest of data, bytes or a string's UTF-8, as 32 bytes."
        (data (or bytevector string) "the data")
        (returns bytevector))
  (define (sha256 data)
    ;; FIPS 180-4: the message padded to whole 64-byte blocks with a 1 bit,
    ;; zeros and its length in bits; each block expands into a schedule of
    ;; 64 words and turns the eight state words through 64 rounds
    (let* ([bytes (if (string? data) (string->utf8 data) data)]
           [n (bytevector-length bytes)]
           [padded (fx* 64 (fxdiv (fx+ n 72) 64))]
           [m (make-bytevector padded 0)]
           [w (make-vector 64 0)]
           [h (vector-copy initial-state)])
      (bytevector-copy! bytes 0 m 0 n)
      (bytevector-u8-set! m n #x80)
      (bytevector-u64-set! m (fx- padded 8) (* n 8) (endianness big))
      (do ([offset 0 (fx+ offset 64)]) ((fx=? offset padded))
        (do ([t 0 (fx+ t 1)]) ((fx=? t 16))
          (vector-set! w t (bytevector-u32-ref m (fx+ offset (fx* t 4)) (endianness big))))
        (do ([t 16 (fx+ t 1)]) ((fx=? t 64))
          (vector-set! w t
            (add (add (small-sigma1 (vector-ref w (fx- t 2))) (vector-ref w (fx- t 7)))
                 (add (small-sigma0 (vector-ref w (fx- t 15))) (vector-ref w (fx- t 16))))))
        (let round ([t 0] [a (vector-ref h 0)] [b (vector-ref h 1)] [c (vector-ref h 2)] [d (vector-ref h 3)]
                    [e (vector-ref h 4)] [f (vector-ref h 5)] [g (vector-ref h 6)] [hh (vector-ref h 7)])
          (if (fx=? t 64)
              (for-each (lambda (i v) (vector-set! h i (add (vector-ref h i) v)))
                        '(0 1 2 3 4 5 6 7) (list a b c d e f g hh))
              (let ([t1 (add (add hh (big-sigma1 e)) (add (add (choose e f g) (vector-ref rounds t)) (vector-ref w t)))]
                    [t2 (add (big-sigma0 a) (majority a b c))])
                (round (fx+ t 1) (add t1 t2) a b c (add d t1) e f g)))))
      (let ([digest (make-bytevector 32)])
        (do ([i 0 (fx+ i 1)]) ((fx=? i 8) digest)
          (bytevector-u32-set! digest (fx* i 4) (vector-ref h i) (endianness big))))))

  (edoc "Bytes as lowercase hex, two digits each."
        (bytes bytevector "the bytes")
        (returns string))
  (define (hex bytes)
    (let ([digits "0123456789abcdef"])
      (let loop ([i (fx- (bytevector-length bytes) 1)] [chars '()])
        (if (fx<? i 0)
            (list->string chars)
            (let ([byte (bytevector-u8-ref bytes i)])
              (loop (fx- i 1)
                    (cons (string-ref digits (fxarithmetic-shift-right byte 4))
                          (cons (string-ref digits (fxand byte 15)) chars)))))))))
