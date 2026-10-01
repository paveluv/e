;; Local installation of a base-owned composition. The ordinary head pump and
;; TUI transaction remain the only input and presentation paths.
(import (only (foundation edoc) elibrary))
(elibrary (head root)
  (export acquire! current init! install!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:)
          (prefix (head head) head:) (prefix (head interaction) interaction:)
          (prefix (head routing) routing:) (prefix (head tui) tui:)
          (prefix (head widget) widget:)
          (prefix (service composition) composition:) (prefix (state view) view:)
          (prefix (sys tty) tty:))

  (define binding #f)
  (define mounted #f)
  (define pending? #f)
  (define acquisition #f)
  (define main-thread (get-thread-id))
  (define (field r key) (cdr (assq key r)))
  (define (root-of r) (field (field r 'value) 'root))
  (define (require-main!)
    (unless (= main-thread (get-thread-id)) (error 'root "installation requires the head command thread")))

  (edoc "Acquire this head's persistent composition profile for startup. This selects no default editor and presents nothing. A different profile requires another startup."
        (profile string "nonempty stable profile") (returns list "binding snapshot"))
  (define (acquire! profile)
    (require-main!)
    (when (or pending? (and binding (not (equal? profile (field (field binding 'value) 'profile)))))
      (error 'acquire! "another profile or installation is already active"))
    (interaction:flush!)
    (let-values ([(r rows) (composition:acquire! profile)])
      (set! binding r)
      (set! acquisition rows)
      (finish!)
      (current)))

  (edoc "Read the locally acquired composition binding, or false before startup. The snapshot is an explicit guard for install!; reading it performs no wire request."
        (returns (or list #f)) (public))
  (define (current) (datum:copy binding))

  (define (finish!)
    (when (and binding (pair? (field (field binding 'value) 'cleanup)))
      (let-values ([(status r) (composition:finish! binding)])
        (unless (eq? status 'applied) (error 'root "cleanup binding became stale" status))
        (set! binding r))))

  (define (redraw! coalesce?)
    ;; Admission has made the old canonical tree unavailable. Its already
    ;; published terminal shadow stays pinned until the command unwinds.
    (unless pending?
      (tui:render!
        (lambda () (tui:terminal-size!) (head:before-frame!))
        (lambda () (tui:draw-root! (and mounted (widget:prepare! mounted (tui:screen-cols) (tui:screen-rows)))))
        coalesce?)))

  (define (key! event)
    (cond [(eof-object? event) (head:quit!)]
      [(or pending? (not mounted)
           (not (exists (lambda (p) (equal? mounted (widget:frame-id (car p)))) (widget:shown)))) (routing:cancel!)]
      [(equal? event "MOUSE-HANDLED") (routing:cancel!)]
      [else
       (let ([key (if (char? event) (tty:character-event event) event)])
         (routing:input! mounted
           (if (equal? key "PASTE") (list 'text (head:read-paste) 'paste)
             (list 'key key (let ([c (tty:key-event-character key)]) (and c (string c)))))))]))
  (define click #f)
  (define (mouse! handle? phase bits x y)
    (when (and handle? (not pending?))
      (let* ([press? (and (char=? phase #\M) (zero? (bitwise-and bits 96)) (< (bitwise-and bits 3) 3))]
             [now (real-time)] [at (list bits x y)]
             [double? (and press? click (equal? (car click) at) (< (- now (cdr click)) 500))])
        (when press? (set! click (and (not double?) (cons at now))))
        (widget:pointer! (tty:pointer-event phase bits (if double? 2 1)) (- x 1) (- y 1))))
    (if (and (not (zero? (bitwise-and bits 32))) (= (bitwise-and bits 3) 3)) 'ignore "MOUSE-HANDLED"))

  (edoc "Prepare and admit a root, then install it at the next head command boundary. Failure leaves the old display usable and frees candidate demand. Disposition is retire or a persistent owner already retaining the previous root; false is allowed for initial or unchanged roots. Return status and the admitted binding. No startup script is run here."
        (expected list "snapshot from current or acquire!") (candidate (or model #f) "unparented view or empty root")
        (disposition (or model #f (one-of retire)) "explicit previous-root lifetime")
        (returns (values symbol datum)) (public))
  (define (install! expected candidate disposition)
    (require-main!)
    (unless (and binding (equal? expected binding)) (error 'install! "expected the current binding"))
    (when pending? (error 'install! "installation is pending its command boundary"))
    (interaction:flush!)
    (head:finish-frame!)
    (let* ([same? (and mounted (equal? mounted candidate))]
           [tree (if (and acquisition (equal? candidate (root-of binding))) acquisition
                   (if candidate (view:tree candidate) '()))]
           [old mounted] [old-rows (if old (view:tree old) '())]
           [stage #f] [admitted? #f])
      (set! acquisition #f)
      (dynamic-wind void
        (lambda ()
          (when (and candidate (not same?))
            (set! stage (widget:stage! tree 'composition (tui:screen-cols) (tui:screen-rows))))
          (let-values ([(status next rows) (composition:admit! expected candidate tree disposition)])
            (when (eq? status 'applied)
              (set! admitted? #t) (set! pending? #t) (set! binding next)
              ;; No teardown runs in the installing command's continuation.
              (head:set-frame-hook! redraw!)
              (head:set-key-handler! key!) (head:set-mouse-handler! mouse!)
              (head:run-on-main!
                (lambda ()
                  (guard (ex [else (widget:invalidate!) (head:quit!) (raise ex)])
                    (routing:cancel!) (widget:invalidate!) (set! click #f)
                    (interaction:adopt!
                      (append (map (lambda (row) (cons (car row) #f)) old-rows) rows))
                    (unless same? (when old (widget:detach! old)))
                    (when stage (widget:adopt! stage))
                    (set! mounted candidate)
                    (head:set-prepare-hook! void) (head:set-after-key! void)
                    (head:set-idle-hook! (lambda (fence?) (void))) (head:set-report-handler! #f)
                    (set! pending? #f))
                  (finish!)
                  (redraw! #f))))
            (values status (datum:copy next))))
        (lambda () (when (and stage (not admitted?)) (widget:discard! stage))))))

  (edoc "Load the composition protocol and primitive widget definitions, without allocating a root. Release local demand on head shutdown.")
  (define (init!)
    (kernel:load-module! "composition") (kernel:load-module! "widget")
    (head:add-shutdown-hook!
      (lambda ()
        (when mounted (widget:unmount! mounted) (set! mounted #f))
        (finish!))))
)
