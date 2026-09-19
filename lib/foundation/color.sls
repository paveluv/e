;; color.sls -- OSC color specifications shared by terminal input and the
;; embedded terminal. A parsed color is three 8-bit RGB components, or #f.

(import (only (edoc) elibrary))
(elibrary (color)
  (export parse)
  (import (rnrs) (prefix (string) string:))

  (define (hex-component text)
    (and (<= 1 (string-length text) 4)
         (for-all (lambda (c) (or (char<=? #\0 c #\9)
                                (char<=? #\a c #\f) (char<=? #\A c #\F)))
                  (string->list text))
         (round (/ (* 255 (string->number text 16))
                   (- (expt 16 (string-length text)) 1)))))

  (edoc "A terminal color reply, rgb:rrrr/gggg/bbbb or #rrggbb, as (red green blue) in 0 to 255, or #f."
        (text string "the reply")
        (returns (or list #f)))
  (define (parse text)
    (let* ([size (string-length text)]
           [parts
            (cond
              [(string:prefix? "rgb:" text)
               (let* ([red-end (string:search text "/" 4 size)]
                      [green-end (and red-end (string:search text "/" (+ red-end 1) size))])
                 (and green-end
                      (list (substring text 4 red-end)
                            (substring text (+ red-end 1) green-end)
                            (string:tail text (+ green-end 1)))))]
              [(and (= size 7) (char=? (string-ref text 0) #\#))
               (map (lambda (start) (substring text start (+ start 2))) '(1 3 5))]
              [else #f])])
      (and parts
           (let ([values (map hex-component parts)])
             (and (for-all integer? values) values)))))
)
