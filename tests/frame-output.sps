;; Scroll widgets retain logical anchors across resizing and scrollbar policy.
(model:register-kind! 'frame-test 1 string?)
(let* ([source (model:create! head:ui-actor 'frame-test 1 'session 'persistent '() "a\nb\nc\nd\ne")]
       [text (view:create! head:ui-actor source 'text 2 '() 0)]
       [scroll (view:create! head:ui-actor #f 'scroll 1 '() #f)])
  (view:arrange! head:ui-actor (list (list scroll 0 (list (list 'text text 'fit)) '())) '())
  (widget:mount! scroll 'scroll-output)
  (let ([first (widget:prepare! scroll 6 2)])
    (check 'scroll-renders-only-its-visible-range (widget:frame-lines first) '("> a   " "  b   "))
    (edoc:expression (widget:act! scroll 'scroll 2))
    (check 'scroll-anchor-is-logical-and-selection-independent
           (list (view:state (interaction:snapshot scroll)) (view:state (interaction:snapshot text))
             (widget:frame-lines (widget:prepare! scroll 6 2)))
           '((0 2) 0 ("  c   " "  d   ")))
    (check 'scrollbars-fit-the-content-without-changing-logical-anchors
           (reverse (fold-left (lambda (results choice)
                                 (interaction:flush!)
                                 (let ([d (interaction:snapshot scroll)])
                                   (widget:arrange! (list (list scroll (model:revision scroll) (view:children d) (list (cons 'scrollbar (car choice)))))))
                                 (let* ([f (widget:prepare! scroll (cadr choice) (caddr choice))]
                                        [child (car (widget:frame-children f))])
                                   (cons (list (caddr (widget:frame-rect child)) (- (cadr (widget:frame-rect child))) (view:state (interaction:snapshot scroll))) results)))
                      '() '((#f 6 2) (auto 6 2) (left 6 2) (#t 6 8) (auto 1 2) (auto 6 8) (auto 6 2))))
           '((6 2 (0 2)) (5 2 (0 2)) (5 2 (0 2)) (5 0 (0 2)) (1 2 (0 2)) (6 0 (0 2)) (5 2 (0 2))))
    (widget:reveal! text '(0 4))
    (check 'scrollbar-thumb-and-reveal-reach-the-same-last-page
           (list (widget:frame-lines (widget:prepare! scroll 6 2)) (view:state (interaction:snapshot text)))
           '(("  d  │" "  e  ┃") 0))
    (interaction:flush!)
    (let ([d (interaction:snapshot scroll)])
      (widget:arrange! (list (list scroll (model:revision scroll) (view:children d) '()))))
    (check 'zero-allocation-produces-no-output (widget:frame-lines (widget:prepare! scroll 0 0)) '())
    (widget:unmount! scroll)
    (view:retire! head:ui-actor scroll (model:revision scroll))))

;; Exercise the transaction itself rather than an incidental window policy.
;; Nested callbacks and failure injection cover output atomicity and recovery.
(let ([line "old"] [columns 80] [draw-hook void])
  (define (frame!)
    (tui:render! void
      (lambda ()
        (tui:begin-frame! (list 'output columns) 1)
        (tui:paint! 0 0 line (lambda () (tui:ansi! line)))
        (tui:title! "e: output fixture")
        (tui:cursor-style! "\x1b;[1 q")
        (draw-hook)) #f))
  (parameterize ([tui:input-delay 0])
    (painted frame!)
    (let ([sink (open-output-string)] [before #f] [nested #f])
      (set! draw-hook
        (lambda ()
          (unless before
            (set! before (get-output-string sink))
            (set! line "new") (set! columns 60)
            (frame!) (set! nested (get-output-string sink)))
          (tui:invalidate-screen-cache!)))
      (parameterize ([sys:terminal-output-port sink]) (frame!))
      (let ([text (string-append nested (get-output-string sink))])
        (check 'nested-frame-publishes-and-discards-obsolete-output
          (list before (sync-events nested) (sync-events text)
            (contains? text "old") (contains? text "new"))
          '("" (begin end) (begin end begin end) #f #t))))
    (for-each
      (lambda (failure)
        (let ([sink (open-output-string)])
          (set! line (symbol->string failure))
          (check (list 'preparation-failure-writes-nothing failure)
            (list
              (call/cc
                (lambda (escape)
                  (set! draw-hook
                    (lambda () (if (eq? failure 'unwind) (escape 'escaped) (error 'fixture "failed"))))
                  (parameterize ([sys:terminal-output-port sink])
                    (guard (ex [else 'raised]) (frame!) 'returned))))
              (get-output-string sink))
            (list (if (eq? failure 'unwind) 'escaped 'raised) ""))
          (set! draw-hook void)
          (check (list 'unpublished-frame-does-not-become-baseline failure)
            (contains? (painted frame!) line) #t))) '(error unwind))
    (let ([failed? #f] [writes '()])
      (let ([sink (make-custom-textual-output-port "failed frame"
                    (lambda (text start count)
                      (set! writes (cons (substring text start (+ start count)) writes))
                      (unless failed? (set! failed? #t) (error 'fixture "terminal write failed")) count)
                    #f #f void)])
        (check 'terminal-write-failure-propagates
          (parameterize ([sys:terminal-output-port sink]) (test:raises? frame!)) #t)
        (close-output-port sink))
      (let ([text (painted frame!)])
        (check 'failed-write-releases-sync-and-forgets-terminal-baseline
          (list (car (reverse (sync-events (apply string-append (reverse writes)))))
            (contains? text line) (contains? text "\x1b;]2;e: output fixture")
            (contains? text "\x1b;[1 q")) '(end #t #t #t))))))

;; A real widget publication owns pointer and caret geometry. Merely preparing
;; another size cannot change the displayed frame; failed writes revoke it.
(let* ([source (store:create! head:ui-actor "entry output" '("abcdef"))]
       [id (view:create! head:ui-actor source 'entry 1 '() '((0 . 2) (0 . 0)))])
  (define (frame!) (tui:render! head:before-frame!
                     (lambda () (tui:draw-root! (widget:prepare! id 10 1))) #f))
  (kernel:load-module! "entry")
  (tui:set-screen-rows! 3) (tui:set-screen-cols! 10)
  (widget:mount! id 'entry-output)
  (parameterize ([tui:input-delay 0])
    (tui:invalidate-screen-cache!)
    (painted (lambda () (tui:cursor-style! "\x1b;[3 q")))
    (let ([output (painted frame!)])
      (check 'entry-selection-and-block-cursor-reach-tui
        (list (contains? output (style:code 'selection)) (contains? output "\x1b;[1 q")
          (widget:caret (caar (widget:shown)))) '(#t #t (2 . 0))))
    (entry:select! id 4 4)
    (widget:prepare! id 1 1)
    (check 'preparation-retains-the-shown-caret (widget:caret (caar (widget:shown))) '(2 . 0))
    (painted frame!)
    (check 'unchanged-text-publishes-new-caret (widget:caret (caar (widget:shown))) '(4 . 0))
    (let ([sink (make-custom-textual-output-port "failed widget output"
                  (lambda args (error 'fixture "write failed")) #f #f void)])
      (parameterize ([sys:terminal-output-port sink]) (test:raises? frame!)))
    (check 'uncertain-output-disables-widget-hits (widget:shown) '())
    (painted frame!)
    (check 'successful-output-restores-the-widget-frame (widget:frame-id (caar (widget:shown))) id))
  (widget:unmount! id) (view:retire! head:ui-actor id (model:revision id)))

;; Bell feedback uses the ordinary owning pump even without an echo area.
(let* ([id (view:create! head:ui-actor #f 'bell-output 1 '() '())]
       [frames '()] [prepared 0] [owner (get-thread-id)] [threads '()]
       [timed-out? (test:gate)])
  (define (frame!) (tui:render! head:before-frame!
                     (lambda () (tui:draw-root! (widget:prepare! id 80 1))) #f))
  (define (flashing? text) (contains? text "\x1b;[7m"))
  (widget:register! 'bell-output 1
    (list (cons 'render (lambda args '("Question survives the bell")))))
  (widget:mount! id 'bell-output)
  (tui:set-screen-rows! 3) (tui:set-screen-cols! 80)
  (call/cc (lambda (done)
             (head:run-on-main! (lambda () (done #t)))
             (parameterize ([head:in-main-pump #t]) (head:read-key-event))))
  (parameterize ([kernel:registering-module 'output-bell-fixture])
    (head:add-pre-redraw-hook!
      (lambda ()
        (set! threads (cons (get-thread-id) threads))
        (set! prepared (+ prepared 1))
        (when (= prepared 2) (do ([i 0 (+ i 1)]) ((= i 100)) (tui:visual-bell!))))))
  (let ([stop (test:worker (lambda () (sleep (make-time 'time-duration 400000000 0))
                             (timed-out? #t) (head:wake-main!)))])
    (dynamic-wind
      (lambda () (tui:set-screen-live! #t))
      (lambda ()
        (parameterize ([tui:input-delay 0])
          (let ([result
                 (call/cc
                   (lambda (done)
                     (head:set-frame-hook!
                       (lambda (coalesce?)
                         (when (timed-out?) (done 'timed-out))
                         (let ([text (painted frame!)])
                           (set! frames (cons text frames))
                           (unless (flashing? text) (done 'expired)))))
                     (tui:visual-bell!) (head:read-key-event #f)))])
            (check 'bell-retrigger-expiry-and-repaint-share-the-owning-pump
              (list result (flashing? (car (reverse frames))) (flashing? (car frames))
                (>= prepared 3) (for-all (lambda (thread) (= thread owner)) threads)
                (for-all (lambda (text) (equal? (sync-events text) '(begin end))) frames)
                (contains? (car frames) "Question survives the bell"))
              '(expired #t #f #t #t #t #t)))))
      (lambda () (tui:set-screen-live! #f) (head:set-frame-hook! void)
        (kernel:retract-module! 'output-bell-fixture) (stop))))
  (widget:unmount! id) (view:retire! head:ui-actor id (model:revision id)))
