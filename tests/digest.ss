#!/usr/bin/env scheme-script

;; SHA-256 in-house, against FIPS 180-4's vectors and the padding edges: a
;; message of 55 bytes pads within its block, 56 and 63 need a second, 64
;; fills one and pads into another, and a million bytes carry the length
;; past a block's arithmetic.  Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (foundation digest) digest:) (prefix (test) test:))

     (define check test:check)
     (define (sha256-hex data) (digest:hex (digest:sha256 data)))
     (define (pattern n)
       ;; n bytes of a fixed sequence, the one the vectors were made from
       (let ([bytes (make-bytevector n)])
         (do ([i 0 (+ i 1)]) ((= i n) bytes)
           (bytevector-u8-set! bytes i (mod (+ (* i 7) 3) 256)))))

     (check 'the-empty-message
       (sha256-hex "") "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
     (check 'the-one-block-fips-vector
       (sha256-hex "abc") "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
     (check 'the-two-block-fips-vector
       (sha256-hex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
       "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
     (check 'the-padding-edges
       (map (lambda (n) (sha256-hex (pattern n))) '(55 56 63 64 65 119 120 121))
       '("e7313d333c272e639f790978283f9eb392e843d0f29b7016828bb1daa4aac70b"
         "4324d65f3c103567f5589c710bc08f8523f929a9272e3af36fc968e52abc6c27"
         "81c80242132f230c3bd41b3e63bbcff16107339549214a99614ff26664625055"
         "39e3d7b6b5d075d37d053ad89b24b41bef4f3c29760c84447cab3f3be1882241"
         "aacca6ff74fdbb296d165a45cecfa04e5127bc008770fbbdd48006f2d2fae95e"
         "9ce7368e4daf32341631b492e80359dc9f594b48453cd0dd5bf0b19279cc177e"
         "7836b787757e95e58b3ca5aec90b1b004e8deba1e50e9675af9cabf1a13a04b5"
         "1189a98a00c71bc1848ea8bdc9700b442bee0be7c3f45172303f1ab0b6f1617e"))
     (check 'a-million-bytes
       (sha256-hex (make-bytevector 1000000 97)) "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")
     (check 'a-string-hashes-as-its-utf8
       (list (sha256-hex "héllo wörld\n") (equal? (digest:sha256 "abc") (digest:sha256 (string->utf8 "abc"))))
       '("3828eeee974aa7486e7acc258e5c73a0115e168444d6688deb8d5d1306d1f57d" #t))
     (check 'hex-is-lowercase-two-digits-a-byte (digest:hex (bytevector 0 15 16 255)) "000f10ff")
     (test:finish! 'digest)))
