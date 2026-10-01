;; Load this file, then (tetris:open!). Arrows move/rotate, Space drops,
;; p pauses, r restarts. (tetris:create!) returns a widget for any host.
;; Each view keeps its own game in the ordinary saved interaction state.
(import (prefix (only (foundation edoc) expression) edoc:)
        (prefix (head head) head:) (prefix (head seat) seat:) (prefix (head interaction) interaction:)
        (prefix (head keymap) keymap:) (prefix (head style) style:)
        (prefix (head widget) widget:) (prefix (head window) window:)
        (prefix (state view) view:) (prefix (sys glyph) glyph:)
        (prefix (sys sys) sys:))

;; A game is #(board piece next score lines status). Board cells are 0..7;
;; a piece is (color pivot-x*2 pivot-y*2 cells), in logical board coordinates.
(define tetris:shapes
  '#(((0 . 1) (1 . 1) (2 . 1) (3 . 1)) ((0 . 0) (1 . 0) (0 . 1) (1 . 1))
     ((1 . 0) (0 . 1) (1 . 1) (2 . 1)) ((1 . 0) (2 . 0) (0 . 1) (1 . 1))
     ((0 . 0) (1 . 0) (1 . 1) (2 . 1)) ((0 . 0) (0 . 1) (1 . 1) (2 . 1))
     ((2 . 0) (0 . 1) (1 . 1) (2 . 1))))
(define (tetris:piece kind)
  (let ([pivot (case kind [(1) 3] [(2) 1] [else 2])])
    (list kind (+ 6 pivot) pivot
      (map (lambda (p) (cons (+ 3 (car p)) (cdr p))) (vector-ref tetris:shapes (- kind 1))))))
(define (tetris:move piece dx dy)
  (list (car piece) (+ (cadr piece) (* 2 dx)) (+ (caddr piece) (* 2 dy))
    (map (lambda (p) (cons (+ (car p) dx) (+ (cdr p) dy))) (cadddr piece))))
(define (tetris:rotate piece)
  (if (= (car piece) 2) piece
    (list (car piece) (cadr piece) (caddr piece)
      (map (lambda (p)
             (cons (div (- (+ (cadr piece) (caddr piece)) (* 2 (cdr p))) 2)
               (div (+ (- (caddr piece) (cadr piece)) (* 2 (car p))) 2))) (cadddr piece)))))
(define (tetris:fits? board piece)
  (for-all (lambda (p) (and (<= 0 (car p) 9) (<= 0 (cdr p) 19)
                         (zero? (bytevector-u8-ref board (+ (car p) (* 10 (cdr p))))))) (cadddr piece)))
(define (tetris:spawn! game)
  (vector-set! game 1 (tetris:piece (vector-ref game 2)))
  (vector-set! game 2 (+ 1 (random 7)))
  (unless (tetris:fits? (vector-ref game 0) (vector-ref game 1)) (vector-set! game 5 'over))
  game)
(define (tetris:new-game)
  (tetris:spawn! (vector (make-bytevector 200 0) #f (+ 1 (random 7)) 0 0 'playing)))
(define (tetris:lock! game)
  (let ([board (bytevector-copy (vector-ref game 0))] [piece (vector-ref game 1)])
    (for-each (lambda (p) (bytevector-u8-set! board (+ (car p) (* 10 (cdr p))) (car piece))) (cadddr piece))
    (let* ([kept (filter (lambda (y) (exists (lambda (x) (zero? (bytevector-u8-ref board (+ x (* 10 y))))) (iota 10))) (iota 20))]
           [cleared (- 20 (length kept))] [next (make-bytevector 200 0)])
      (for-each (lambda (from to) (bytevector-copy! board (* from 10) next (* (+ to cleared) 10) 10)) kept (iota (length kept)))
      (vector-set! game 0 next)
      (vector-set! game 3 (+ (vector-ref game 3) (vector-ref '#(0 100 300 500 800) cleared)))
      (vector-set! game 4 (+ (vector-ref game 4) cleared))
      (tetris:spawn! game))))
(define (tetris:step state command)
  (let ([game (vector-copy state)])
    (case command
      [(restart) (set! game (tetris:new-game))]
      [(pause) (unless (eq? (vector-ref game 5) 'over)
                 (vector-set! game 5 (if (eq? (vector-ref game 5) 'playing) 'paused 'playing)))]
      [(left right rotate down drop tick)
       (when (eq? (vector-ref game 5) 'playing)
         (let* ([board (vector-ref game 0)] [piece (vector-ref game 1)]
                [candidate
                 (case command
                   [(left right) (tetris:move piece (if (eq? command 'left) -1 1) 0)]
                   [(rotate)
                    (let ([turned (tetris:rotate piece)])
                      (or (exists (lambda (dx) (let ([p (tetris:move turned dx 0)]) (and (tetris:fits? board p) p))) '(0 -1 1 -2 2)) piece))]
                   [else (tetris:move piece 0 1)])])
           (cond
             [(eq? command 'drop)
              (let fall ([p piece])
                (let ([next (tetris:move p 0 1)])
                  (if (tetris:fits? board next) (fall next)
                    (begin (vector-set! game 1 p) (tetris:lock! game)))))]
             [(tetris:fits? board candidate) (vector-set! game 1 candidate)]
             [(memq command '(down tick)) (tetris:lock! game)])))]
      [else (error 'tetris:step "unknown command" command)])
    game))

;; The framework publishes changed interaction state and schedules frames.
;; Only deadlines are local; no threads, polling loop or custom wire messages.
(define tetris:deadlines (make-hashtable equal-hash equal?))
(define (tetris:release! id) (hashtable-delete! tetris:deadlines id))
(edoc:expression
  (define (tetris:play! id command)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (interaction:set-state! head:ui-actor id #f (tetris:step (view:state d) command))
      (when (memq command '(pause restart drop down)) (tetris:release! id)))))
(define (tetris:service! id frame)
  (let ([game (view:state (interaction:snapshot id))])
    (if (and (eq? (vector-ref game 5) 'playing)
          (or (not frame) (let ([r (widget:frame-clip frame)]) (and (>= (caddr r) 22) (>= (cadddr r) 20)))))
      (let ([due (or (hashtable-ref tetris:deadlines id #f) (sys:after 0.5))])
        (when (time>=? (current-time 'time-monotonic) due)
          (tetris:play! id 'tick)
          (set! due (sys:after 0.5)))
        (hashtable-set! tetris:deadlines id due)
        (head:request-frame-at! due))
      (tetris:release! id))))

(define (tetris:picture game)
  (let ([board (bytevector-copy (vector-ref game 0))] [piece (vector-ref game 1)])
    (unless (eq? (vector-ref game 5) 'over)
      (for-each (lambda (p) (bytevector-u8-set! board (+ (car p) (* 10 (cdr p))) (car piece))) (cadddr piece)))
    board))
(define (tetris:rows range)
  (map (lambda (n) (+ (car range) n)) (iota (max 0 (min (cdr range) (- 20 (car range)))))))
(define (tetris:render data d width height range)
  (if (or (< width 22) (< height 20))
    (if (zero? (car range)) (list (glyph:fit "Tetris needs 22×20" width)) '())
    (let* ([game (view:state d)] [board (tetris:picture game)]
           [info (list "TETRIS" (case (vector-ref game 5) [(paused) "Paused"] [(over) "Game over"] [else ""])
                   "" (format "Score: ~a" (vector-ref game 3)) (format "Lines: ~a" (vector-ref game 4))
                   "" (format "Next: ~a" (vector-ref '#(I O T S Z J L) (- (vector-ref game 2) 1)))
                   "" "← →  Move" "↑    Rotate" "↓    Soft drop" "SPC  Drop" "p    Pause" "r    Restart")])
      (map (lambda (y)
             (glyph:fit
               (string-append "│" (apply string-append (map (lambda (x) (if (zero? (bytevector-u8-ref board (+ x (* 10 y)))) "· " "██")) (iota 10)))
                 "│  " (if (< y (length info)) (list-ref info y) "")) width))
        (tetris:rows range)))))
(define tetris:faces '#(ghost tetris-i tetris-o tetris-t tetris-s tetris-z tetris-j tetris-l))
(define (tetris:decorate data d width height range)
  (if (or (< width 22) (< height 20)) '()
    (let ([board (tetris:picture (view:state d))])
      (apply append
        (map (lambda (y)
               (map (lambda (x) (list (list (+ 1 (* 2 x)) y 2 1)
                                  (vector-ref tetris:faces (bytevector-u8-ref board (+ x (* 10 y)))))) (iota 10)))
          (tetris:rows range))))))

(widget:register! 'tetris 1
  (list '(focus . #t) '(contexts tetris)
    (cons 'render tetris:render) (cons 'decorate tetris:decorate)
    (cons 'service tetris:service!) (cons 'release tetris:release!)
    (cons 'measure (lambda (data d axis cross child) (if (eq? axis 'x) '(22 43) '(20 20))))
    (cons 'actions (list (cons 'play tetris:play!)))))
(for-each (lambda (p) (keymap:bind-default! 'tetris (car p) (keymap:call tetris:play! widget:target (cadr p))))
  '(("LEFT" left) ("RIGHT" right) ("UP" rotate) ("DOWN" down) ("SPC" drop) ("p" pause) ("r" restart)))
(for-each (lambda (face color) (style:set! face (list 'bold (list 'fg color))))
  (cdr (vector->list tetris:faces)) '(cyan yellow magenta green red blue 208))

(define (tetris:create!)
  (view:create! head:ui-actor #f 'tetris 1 '((name . "<tetris>")) (tetris:new-game)))
(define (tetris:open!)
  (let ([id (tetris:create!)]) (window:show-widget! (seat:current-window) id) id))
