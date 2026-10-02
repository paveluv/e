;; A bounded notification projection. The base journal owns the history.
(import (only (foundation edoc) elibrary))
(elibrary (head message)
  (export create! init! present! show!)
  (import (chezscheme) (prefix (head head) head:) (prefix (head keymap) keymap:)
          (prefix (head widget) widget:) (prefix (service log) log:)
          (prefix (state view) view:) (prefix (sys glyph) glyph:))

  (define-record-type projection (fields segments (mutable width) (mutable rows)))
  (define-record-type mount (fields (mutable subscription) lock (mutable pending) (mutable turn) (mutable entries) (mutable projection)))
  (define mounts (make-hashtable equal-hash equal?))
  (define (turn) (car (keymap:command-state)))
  (define (bounded xs limit) (if (> (length xs) limit) (list-head xs limit) xs))
  (define (clipped text) (if (> (string-length text) 8192) (string-append (substring text 0 8192) "…") text))
  (define (mounted id)
    (or (hashtable-ref mounts id #f)
      (let ([m (make-mount #f (make-mutex) '() (turn) '() (make-projection '() #f #f))])
        (hashtable-set! mounts id m)
        (mount-subscription-set! m
          (log:subscribe!
            (lambda (entry presentation)
              (when (and presentation (equal? (log:actor entry) head:ui-actor))
                (with-mutex (mount-lock m)
                  (mount-pending-set! m (bounded (cons (cons entry presentation) (mount-pending m)) 16)))
                (head:wake-main!))))) m)))
  (define (release! id)
    (let ([m (hashtable-ref mounts id #f)])
      (when m (hashtable-delete! mounts id) (log:unsubscribe! (mount-subscription m)))))
  (define (segments text styles)
    (let loop ([parts (glyph:clusters text)] [at 0] [out '()])
      (if (null? parts) (reverse out)
        (let* ([p (car parts)] [s (substring text at (+ at (car p)))]
               [n (char->integer (string-ref s 0))]
               [face (if (symbol? styles) styles
                       (and (vector? styles) (< at (vector-length styles)) (vector-ref styles at)))])
          (loop (cdr parts) (+ at (car p))
            (cons (cond [(string=? s "\n") (list s 0 face)]
                    [(or (< n 32) (<= 127 n 159)) (list " " 1 face)]
                    [else (list s (cdr p) face)]) out))))))
  (define (publish! id m)
    (mount-projection-set! m (make-projection
                               (apply append
                                 (map (lambda (e)
                                        (append (segments (car e) 'chrome) (segments (cadr e) (caddr e))
                                          (segments (cadddr e) 'ghost) (list (list "\n" 0 #f))))
                                   (reverse (mount-entries m)))) #f #f))
    (widget:repaint! id #t))

  (edoc "Create a notification view under an explicit lifetime. It projects this head's presented journal entries and transient feedback; the base journal remains the history."
        (owner (or model #f) "lifetime owner") (returns model))
  (define (create! owner) (view:create! head:ui-actor #f 'message 1 '() '() owner))

  (edoc "Show transient feedback in a mounted message view, until the next input command. The ghost tail uses the shared italic ghost face. This changes only the head projection, without logging or wire publication."
        (receiver id (view message)) (id model "message view") (text string "feedback") (ghost string "ghost tail"))
  (define (show! id text ghost)
    (unless (and (string? text) (string? ghost)) (error 'show! "expected text and ghost strings"))
    (let-values ([(source d inputs) (widget:context id 'current)])
      (unless (eq? (view:kind d) 'message) (error 'show! "expected a message view"))
      (service! id #f)
      (let ([m (mounted id)])
        (mount-turn-set! m (turn))
        (mount-entries-set! m (list (list "" (clipped text) #f (clipped ghost))))
        (publish! id m))))

  (define (append-entry! m entry presentation ghost)
    (let* ([prefix (format "~a " (log:component entry))]
           [text (clipped (log:format-entry entry))] [style (log:styler (log:component entry))]
           [styles (and style (guard (ex [else #f]) (style text)))] [old (mount-entries m)])
      (mount-entries-set! m
        (bounded (cons (list prefix text styles ghost)
                   (if (and (eq? presentation 'progress) (pair? old) (equal? prefix (caar old))) (cdr old) old)) 16))))

  (edoc "Present existing journal records without logging them again. Preserve their component styles and append an italic ghost to the final record. Pending streamed records are drained first, so the result follows its output."
        (receiver id (view message)) (id model "message view") (entries list "journal records, oldest first") (ghost string "final ghost tail"))
  (define (present! id entries ghost)
    (unless (string? ghost) (error 'present! "expected a ghost string"))
    (let-values ([(source d inputs) (widget:context id 'current)])
      (unless (eq? (view:kind d) 'message) (error 'present! "expected a message view"))
      (service! id #f)
      (let ([m (mounted id)])
        (let loop ([rest entries])
          (when (pair? rest)
            (append-entry! m (car rest) 'append (if (null? (cdr rest)) (clipped ghost) ""))
            (loop (cdr rest))))
        (when (pair? entries) (publish! id m)))))
  (define (service! id frame)
    (let* ([m (mounted id)]
           [pending (with-mutex (mount-lock m)
                      (let ([pending (reverse (mount-pending m))]) (mount-pending-set! m '()) pending))]
           [changed? (not (= (turn) (mount-turn m)))])
      (when changed? (mount-turn-set! m (turn)) (mount-entries-set! m '()))
      (for-each
        (lambda (p)
          (append-entry! m (car p) (cdr p) "")) pending)
      (when (or changed? (pair? pending)) (publish! id m))))
  (define (rows projection width)
    (unless (equal? width (projection-width projection))
      (projection-width-set! projection width)
      (projection-rows-set! projection
        (if (zero? width) '()
          (let loop ([rest (projection-segments projection)] [x 0] [row '()] [out '()])
            (define (finish) (bounded (cons (reverse row) out) 8))
            (cond [(null? rest) (reverse (if (pair? row) (finish) out))]
              [(string=? (caar rest) "\n") (loop (cdr rest) 0 '() (finish))]
              [(and (> x 0) (> (+ x (cadar rest)) width)) (loop rest 0 '() (finish))]
              [else
               (let* ([p (car rest)] [size (min width (cadr p))]
                      [part (if (> (cadr p) width) " " (car p))])
                 (loop (cdr rest) (+ x size) (cons (list part x size (caddr p)) row) out))])))))
    (projection-rows projection))
  (define (viewport projection d width height range)
    (let ([all (rows projection width)])
      (list-head (list-tail all (min (car range) (length all))) (min (cdr range) (max 0 (- (length all) (car range)))))))

  (edoc "Register notification rendering and journal subscriptions. Loading allocates no view and subscribes to nothing until a view is mounted." (public))
  (define (init!)
    (widget:register! 'message 1
      (list (cons 'service service!) (cons 'release release!)
        (cons 'prepare (lambda (id source inputs) (mount-projection (mounted id))))
        (cons 'measure (lambda (projection d axis cross child)
                         (if (eq? axis 'y) (let ([n (max 1 (length (rows projection cross)))]) (list 1 n)) '(0 0))))
        (cons 'viewport viewport)
        (cons 'render (lambda (rows d width height range)
                        (map (lambda (row) (apply string-append (map car row))) rows)))
        (cons 'decorate (lambda (rows d width height range)
                          (apply append (map (lambda (row y)
                                               (filter values (map (lambda (p) (and (cadddr p) (list (list (cadr p) y (caddr p) 1) (cadddr p)))) row)))
                                          rows (map (lambda (n) (+ n (car range))) (iota (length rows)))))))
        (cons 'actions (list (cons 'show show!) (cons 'present present!))))))
)
