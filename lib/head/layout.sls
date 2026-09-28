;; Backend geometry. No device, window, store or interaction dependencies.
(import (only (foundation edoc) elibrary))
(elibrary (head layout)
  (export contains? intersect linear translate)
  (import (chezscheme))

  (edoc "Intersect half-open rectangles; disjoint rectangles have zero extent."
        (a list "(x y width height)") (b list "rectangle") (returns list))
  (define (intersect a b)
    (let ([x (max (car a) (car b))] [y (max (cadr a) (cadr b))])
      (list x y (max 0 (- (min (+ (car a) (caddr a)) (+ (car b) (caddr b))) x))
        (max 0 (- (min (+ (cadr a) (cadddr a)) (+ (cadr b) (cadddr b))) y)))))

  (edoc "Translate a rectangle without changing its size."
        (rect list "rectangle") (x number "horizontal offset") (y number "vertical offset") (returns list))
  (define (translate rect x y) (cons (+ x (car rect)) (cons (+ y (cadr rect)) (cddr rect))))

  (edoc "Whether a point lies in a half-open rectangle; zero extents never contain a point."
        (rect list "rectangle") (x number "horizontal position") (y number "vertical position") (returns boolean))
  (define (contains? rect x y)
    (and (<= (car rect) x) (< x (+ (car rect) (caddr rect)))
      (<= (cadr rect) y) (< y (+ (cadr rect) (cadddr rect)))))

  (define (ordered-map proc xs)
    (reverse (fold-left (lambda (out x) (cons (proc x) out)) '() xs)))
  (define (share total weights)
    (let ([sum (apply + weights)])
      (if (zero? sum) (make-list (length weights) 0)
        (let* ([sizes (map (lambda (weight) (exact (floor (/ (* total weight) sum)))) weights)]
               [left (- total (apply + sizes))])
          (ordered-map (lambda (p)
                         (if (and (> left 0) (> (cdr p) 0)) (begin (set! left (- left 1)) (+ (car p) 1)) (car p)))
            (map cons sizes weights))))))

  (edoc "Allocate integer extents in child order. Collapse gaps before proportional minima; grow weights receive the remaining space."
        (extent integer "nonnegative available extent") (gap integer "nonnegative requested gap")
        (children list "(minimum preferred fit-or-grow) entries") (returns list "(start extent) per child"))
  (define (linear extent gap children)
    (unless (and (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0))) (list extent gap))
              (list? children)
              (for-all (lambda (c) (and (list? c) (= (length c) 3)
                                     (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0))) (list-head c 2))
                                     (<= (car c) (cadr c))
                                     (or (eq? (caddr c) 'fit)
                                       (and (list? (caddr c)) (= (length (caddr c)) 2) (eq? (caaddr c) 'grow)
                                         (rational? (cadr (caddr c))) (> (cadr (caddr c)) 0))))) children))
      (error 'linear "invalid allocation" extent gap children))
    (let* ([minima (map car children)] [minimum (apply + minima)]
           [gaps (max 0 (- (length children) 1))]
           [spacing (if (zero? gaps) 0 (min gap (div (max 0 (- extent minimum)) gaps)))]
           [available (- extent (* gaps spacing))]
           [sizes
            (if (> minimum available) (share available minima)
              (let* ([left (- available minimum)]
                     [fits (ordered-map (lambda (c)
                                          (let ([extra (if (eq? (caddr c) 'fit) (min left (- (cadr c) (car c))) 0)])
                                            (set! left (- left extra)) (+ (car c) extra))) children)]
                     [growth (share left (map (lambda (c) (if (eq? (caddr c) 'fit) 0 (cadr (caddr c)))) children))])
                (map + fits growth)))]
           [position 0])
      (ordered-map (lambda (size) (let ([at position]) (set! position (+ position size spacing)) (list at size))) sizes)))
)
