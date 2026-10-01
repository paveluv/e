;; Head runtime: one input mailbox, presentation clock and lifecycle.
;; Importing it allocates no document, window, popup or composition.
(import (only (foundation edoc) elibrary))
(elibrary (head head)
  (export add-color-scheme-hook! add-pre-redraw-hook!
    add-publication-hook! add-shutdown-hook! after-key!
    before-frame! call-uninterrupted call-with-interrupt
    current-keys defer-frame! finish-frame! frame-presented!
    host-color-scheme in-main-pump input-live? interrupted?
    key! last-command make-interrupted mouse-position publish! quit!
    quitting? read-key-event read-paste (rename (frame! redraw!)) report!
    request-frame-at! run-deferred! run-on-main!
    run-shutdown-hooks! set-after-key! set-current-keys!
    set-frame-hook! set-idle-hook! set-key-handler! set-last-command!
    set-mouse-handler! set-mouse-position! set-prepare-hook!
    set-report-handler! start-input-reader! typed-text ui-actor
    wait-for-frame! wake-main!)
  (import (chezscheme)
          (prefix (core endpoint) endpoint:)
          (prefix (core kernel) kernel:)
          (prefix (core startup) startup:)
          (prefix (head interaction) interaction:)
          (prefix (head pacing) pacing:)
          (prefix (head suspension) suspension:)
          (prefix (service log) log:)
          (prefix (state actor) actor:)
          (prefix (state surface) surface:)
          (prefix (only (sys sys) terminal-isig! duplicate-standard-input-port) sys:)
          (prefix (sys tty) tty:))

  ;; The text of the bracketed paste just consumed: the pump's paste
  ;; handler stashes it, the PASTE key's command reads it.
  (define pending-paste "")

  (edoc "The text of the bracketed paste just consumed."
        (returns string))
  (define (read-paste)
    pending-paste)

  ;;; The seat's loop -------------------------------------------------------------

  ;; The scheduling substrate: a dedicated thread owns the terminal
  ;; input (through a private dup'd port, so its blocking reads never
  ;; hold a console lock) and posts parsed events to the seat's
  ;; mailbox; any thread may post a wake or a thunk.  read-key-event --
  ;; called synchronously by the main loop, prompts, i-search,
  ;; everything -- is the mailbox pump: between keys it services
  ;; wake-ups and posted thunks.  The seat services its own side
  ;; effects -- a paste's text, a host's color report, a posted thunk's
  ;; error, the store's news before a frame; two hooks reach up: the
  ;; frame (the painter's) and the mouse (the commands').  A remote
  ;; head runs the same loop with a socket reader posting in place of
  ;; the tty.

  (define mailbox (kernel:make-mailbox))
  (define main-thread (get-thread-id))
  (define deferred '())          ; thunks posted during a nested pump

  ;; Main-thread presentation state: each frame derives its next deadline
  ;; anew. Providers request only still-live work while preparing or painting
  ;; a frame, so expiry, eviction and replacement need no alarm cancellation.
  (define frame-deadline #f)
  (define frame-pending? #f)

  (edoc "Publish any deferred full frame before a partial update or interaction; whether a frame was needed."
        (returns boolean))
  (define (finish-frame!)
    (and frame-pending?
      ;; A hook may itself prompt or redraw. Do not recursively flush the
      ;; same pending frame; an unsuccessful flush remains pending.
      (let ([complete? #f])
        (dynamic-wind
          (lambda () (set! complete? #f) (set! frame-pending? #f))
          (lambda () (frame!) (set! complete? #t) #t)
          (lambda () (unless complete? (set! frame-pending? #t)))))))

  (edoc "Ask for a frame by a monotonic deadline, the earliest request winning."
        (deadline any "a monotonic time"))
  (define (request-frame-at! deadline)
    (unless (and (time? deadline) (eq? (time-type deadline) 'time-monotonic))
      (error 'request-frame-at! "expected a monotonic deadline" deadline))
    (when (or (not frame-deadline) (time<? deadline frame-deadline))
      (set! frame-deadline (copy-time deadline))))

  (edoc "Queue a thunk for the next main-thread command boundary. Work posted by the UI runs before its next input; other threads enter through the mailbox. Nested readers defer it."
        (thunk thunk "what to run"))
  (define (run-on-main! thunk)
    (if (= main-thread (get-thread-id))
      (begin (set! deferred (cons thunk deferred)) (wake-main!))
      (kernel:mailbox-post! mailbox (cons 'run thunk))))

  ;; A burst of foreign edits (an agent's tight loop, a chatty PTY)
  ;; must not queue one repaint per event: a wake is posted only when
  ;; none is outstanding, so a burst collapses into one frame.  The
  ;; claim happens before the frame is painted, never after -- a wake
  ;; arriving mid-paint queues the next frame instead of being lost.
  (define wake-lock (make-mutex))
  (define wake-queued #f)

  (edoc "Wake the main loop for a frame, once per burst of events.")
  (define (wake-main!)
    (when (with-mutex wake-lock
            (and (not wake-queued) (begin (set! wake-queued #t) #t)))
      (kernel:mailbox-post! mailbox '(wake))))

  (define (claim-wake!)
    (with-mutex wake-lock (set! wake-queued #f)))

  (define deliver-endpoint! (endpoint:start! wake-main!))

  ;; #t while the main loop itself pumps the mailbox: posted thunks
  ;; may run right away.  Nested pumps (prompts, i-search, key
  ;; describers) leave it #f and defer them, so a foreign thunk never
  ;; runs in the middle of a modal read.
  (edoc "Whether the main loop itself pumps the mailbox, so posted thunks may run right away; nested pumps defer them."
        (value boolean))
  (define in-main-pump (make-parameter #f))

  (define (run-posted! thunk)
    ;; a posted thunk's error is news, not a crash
    (guard (ex [else (log:add! 'head:run-posted! (kernel:condition-text ex))])
      (parameterize ([in-main-pump #f]) (thunk))))

  (edoc "Run the thunks a nested pump set aside, oldest first.")
  (define (run-deferred!)
    ;; Complete the causal chain before admitting another key, including a
    ;; prompt outcome followed by its parked caller's resumption.
    (let loop ()
      (let ([runs (reverse deferred)])
        (set! deferred '())
        (unless (null? runs)
          (finish-frame!) (for-each run-posted! runs) (loop)))))

  ;; The pump's hooks: the frame hook prepares and paints a frame (the
  ;; painter's, above); the mouse handler applies a report -- (handler handle? c b
  ;; x y) -> an event string, #f, or the symbol ignore for a report the
  ;; loop need not hear at all, such as pointer motion (the commands', above).
  (define frame-hook (lambda (coalesce?) (before-frame!)))
  (define key-handler (lambda (event) (when (eof-object? event) (quit!))))

  (edoc "Install the composition's keyboard adapter. The pump itself supplies no editor bindings."
        (proc procedure "normalized key handler"))
  (define (set-key-handler! proc) (set! key-handler proc))

  (edoc "Deliver a pump key to the installed composition. EOF quits even with an empty composition."
        (event any "normalized key, paste marker or EOF"))
  (define (key! event) (key-handler event))
  (define mouse-handler (lambda (handle? c b x y) #f))
  ;; Last reported pointer cell (1-based x . y). Keyboard input retires
  ;; mouse emphasis; the next report restores it without moving point.
  (define the-mouse-position #f)

  (edoc "The pointer's last reported (column . row), or #f."
        (returns (or pair #f)))
  (define (mouse-position)
    the-mouse-position)

  (edoc "Record the pointer's position, or #f when unknown."
        (position (or pair #f) "(column . row)"))
  (define (set-mouse-position! position)
    (set! the-mouse-position position))

  (edoc "Install the frame hook, which prepares and paints one complete composition."
        (proc procedure "(proc coalesce?), using the existing backend transaction"))
  (define (set-frame-hook! proc)
    (set! frame-hook proc))

  (edoc "Install the mouse handler: (handler handle? c b x y) applies a decoded mouse report."
        (proc procedure "the handler"))
  (define (set-mouse-handler! proc)
    (set! mouse-handler proc))
  (define after-key-hook void)

  (edoc "Install what runs after every key."
        (proc thunk "the hook"))
  (define (set-after-key! proc)
    (set! after-key-hook proc))

  (edoc "Run the installed after-key hook.")
  (define (after-key!)
    (after-key-hook))

  (edoc "Prepare and present through the installed composition using the same pump as wakeups and deadlines."
        (coalesce (list-of boolean) "optional permission to defer expired keyboard frames"))
  (define (frame! . coalesce)
    ;; Wakes and deadlines use the same preparation as direct redraws.
    (parameterize ([in-main-pump #f]) (frame-hook (and (pair? coalesce) (car coalesce))))
    ;; Nested prompts can temporarily borrow windows. Only an outer pump
    ;; frame checkpoints the screen the user will return to.
    (when (in-main-pump) (idle-hook #f)))

  (define idle-hook (lambda (fence?) (void)))

  (edoc "Install the composition's idle publication callback. A true fence requests acknowledged publication after remote evaluation."
        (proc procedure "(proc fence?)"))
  (define (set-idle-hook! proc) (set! idle-hook proc))

  (define report-handler #f)

  (edoc "Install an optional composition diagnostic display; false restores stderr. Diagnostics always reach the log."
        (proc (or procedure #f) "(proc text), or false"))
  (define (set-report-handler! proc) (set! report-handler proc))

  (edoc "Report a head diagnostic to the log and the composition's display, or stderr when no display is installed."
        (text string "the diagnostic"))
  (define (report! text)
    (guard (ex [else (void)]) (log:add! 'head:report! text))
    (unless (and report-handler (guard (ex [else #f]) (report-handler text) #t))
      (display text (current-error-port)) (newline (current-error-port))))

  ;; The host's color scheme: prefer DSR 997 reports, with the OSC 11
  ;; background as a fallback for older terminals. Hooks run on the main
  ;; thread when the scheme changes, updating faces and terminal children.
  (define host-color-scheme-value #f)
  (define host-color-scheme-reported? #f)

  (edoc "The terminal's color scheme as detected: dark, light or #f."
        (returns (or (one-of dark light) #f)))
  (define (host-color-scheme)
    host-color-scheme-value)

  (define color-scheme-hooks (kernel:make-registry))

  (edoc "Register a hook run with the scheme when the terminal reports one."
        (hook procedure "(hook scheme)"))
  (define (add-color-scheme-hook! hook)
    (unless (procedure? hook)
      (error 'add-color-scheme-hook! "expected a procedure" hook))
    (kernel:registry-add! color-scheme-hooks hook))

  (define (note-color-scheme! scheme)
    (unless (eq? scheme host-color-scheme-value)
      (set! host-color-scheme-value scheme)
      (for-each (lambda (hook) (guard (ex [else (void)]) (hook scheme)))
                (kernel:registry-items color-scheme-hooks))
      (wake-main!)))

  ;; The seat's lifetime, and the command the dispatcher ran last (kill
  ;; chaining and typed runs ask).
  (define quit-requested #f)

  (edoc "Request that the main loop end.")
  (define (quit!)
    (set! quit-requested #t))

  (edoc "Whether quitting was requested."
        (returns boolean))
  (define (quitting?)
    quit-requested)

  (define the-last-command #f)

  (edoc "The command the last key ran, for commands that chain, such as consecutive kills."
        (returns any))
  (define (last-command)
    the-last-command)

  (edoc "Record the command the last key ran."
        (c any "the command"))
  (define (set-last-command! c)
    (set! the-last-command c))

  ;; the key sequence being dispatched -- the self-inserting command
  ;; reads its character here
  (define the-current-keys '())

  (edoc "The key sequence being dispatched; the self-inserting command reads its character here."
        (returns list))
  (define (current-keys)
    the-current-keys)

  (edoc "The text the key being dispatched types, a one-character string, or #f for a key that is no character: what a SELF-INSERT binding's command receives, (keymap:call edit:type! head:typed-text) say."
        (returns (or string #f)))
  (define (typed-text)
    (let ([keys (current-keys)])
      (and (pair? keys) (string? (car keys))
           (let ([c (tty:key-event-character (car keys))]) (and c (string c))))))

  (edoc "Record the key sequence being dispatched."
        (keys list "the events"))
  (define (set-current-keys! keys)
    (set! the-current-keys keys))

  ;; Whether the reader runs: a question asked before it does would wait
  ;; forever on a mailbox nothing feeds, so the prompts ask first.
  (define input-reader-started? #f)

  (define presentation-clock
    (pacing:make (lambda () (current-time 'time-monotonic)) sleep))

  (edoc "Wait for the current input's presentation deadline without pumping another event."
        (milliseconds integer "input-to-presentation budget, from 0 to 50 milliseconds"))
  (define (wait-for-frame! milliseconds)
    (pacing:wait! presentation-clock milliseconds))

  (define (keyboard-message? message)
    (and (pair? message) (eq? (car message) 'key)
         (let ([event (caddr message)]) (or (char? event) (string? event)))))

  (edoc "Defer this prepared frame only for an already-expired keyboard event at the front of the queue, within the presentation clock's bound."
        (milliseconds integer "the input-to-presentation budget")
        (returns boolean))
  (define (defer-frame! milliseconds)
    (let ([message (kernel:mailbox-peek mailbox)])
      (and (keyboard-message? message)
           (or (not frame-deadline) (time<? (current-time 'time-monotonic) frame-deadline))
           (pacing:defer? presentation-clock (cadr message) milliseconds)
           (begin (set! frame-pending? #t) #t))))

  (edoc "Record successful terminal publication; the prepared geometry is now displayed.")
  (define (frame-presented!)
    (set! frame-pending? #f)
    (pacing:presented! presentation-clock)
    (publish! #f))

  (edoc "Whether terminal input reaches the pump: the input reader has started, so a prompt can be answered."
        (returns boolean))
  (define (input-live?) input-reader-started?)

  (edoc "Start the thread that reads terminal events into the pump's mailbox.")
  (define (start-input-reader!)
    (set! input-reader-started? #t)
    (let ([stdin (sys:duplicate-standard-input-port)])
      (fork-thread
        (lambda ()
          (let loop ()
            (let ([event (guard (ex [else (eof-object)])
                           (tty:read-event stdin))])
              (kernel:mailbox-post! mailbox
                (list 'key (current-time 'time-monotonic) event))
              (unless (eof-object? event) (loop))))))))

  (edoc "Read the next key from the pump, applying mouse reports unless handle-mouse? is #f, in which case they are consumed without being applied, for a context that must not change focus; frames and posted thunks run while waiting."
        (handle-mouse? boolean "whether to apply mouse reports")
        (returns (or char string any) "a character, an event string, or eof")
        (effects internal)
        (prompts))
  (define read-key-event
    ;; Consumers see the same names whether they are the main editor,
    ;; I-search, a prompt, or a key describer.  A context that must not
    ;; change editor focus passes #f: mouse reports are consumed
    ;; without being applied.
    (case-lambda
      [()
       (read-key-event #t)]
      [(handle-mouse?)
       (let pump ()
         (when (in-main-pump) (run-deferred!))
         ;; Fence before dequeueing: a rendering hook can itself read input.
         ;; Only ordinary keys in the outer pump may use prepared geometry;
         ;; mouse events, callbacks and modal readers require publication.
         (when frame-pending?
           (unless (and (in-main-pump)
                        (keyboard-message? (kernel:mailbox-peek mailbox)))
             (finish-frame!)))
         (let ([message (kernel:mailbox-receive! mailbox frame-deadline #t)])
           (case (and message (car message))
             [(#f)
              (frame!)
              (pump)]
             [(key)
              (let ([event (caddr message)])
                (unless (and (pair? event)
                             (memq (car event) '(host-color-scheme host-background)))
                  (pacing:input! presentation-clock (cadr message)))
                (cond
                  [(not (pair? event)) (set-mouse-position! #f) event]
                  [(eq? (car event) 'mouse)
                   (set-mouse-position! (and handle-mouse?
                                             (cons (list-ref event 3) (list-ref event 4))))
                   (let ([result (apply mouse-handler handle-mouse? (cdr event))])
                     (cond [(eq? result 'ignore)
                            ;; Swallowed, but it may have moved hover state:
                            ;; frame once the burst of reports has drained.
                            (request-frame-at! (current-time 'time-monotonic))
                            (pump)]
                           [else (or result "MOUSE-HANDLED")]))]
                  [(eq? (car event) 'paste)
                   (set-mouse-position! #f)
                   (set! pending-paste (cdr event))
                   "PASTE"]
                  [(eq? (car event) 'host-color-scheme)
                   (set! host-color-scheme-reported? #t)
                   (note-color-scheme! (cadr event))
                   (pump)]
                  [(eq? (car event) 'host-background)
                   (unless host-color-scheme-reported?
                     ;; Approximate brightness on the reported RGB scale.
                     (note-color-scheme!
                       (if (< (apply + (map * '(299 587 114) (cdr event))) 127500)
                           'dark 'light)))
                   (pump)]
                  [else (pump)]))]
             [(wake)
              (claim-wake!)
              (frame!)
              (pump)]
             [(run)
              (cond [(in-main-pump)
                     (run-posted! (cdr message))
                     (frame!)]
                    [else
                     (set! deferred (cons (cdr message) deferred))])
              (pump)]
             [else (pump)])))]))
  (define publication-hooks (kernel:make-registry))

  (edoc "Register head state publication after presentation and before checkpoints. The hook must queue without waiting unless fence? is true before a lifecycle checkpoint."
        (hook procedure "(hook fence?)"))
  (define (add-publication-hook! hook)
    (unless (procedure? hook) (error 'add-publication-hook! "expected a procedure"))
    (kernel:registry-add! publication-hooks hook))

  (edoc "Publish deferred head interaction state, waiting for acknowledgement only at a lifecycle fence."
        (fence? boolean "whether publication must complete"))
  (define (publish! fence?)
    (for-each (lambda (hook) (hook fence?)) (kernel:registry-items publication-hooks)))

  (define shutdown-hook-registry (kernel:make-registry))
  (define pre-redraw-hook-registry (kernel:make-registry))

  (edoc "Register a hook run before every frame, after foreign edits are adopted."
        (proc thunk "the hook"))
  (define (add-pre-redraw-hook! proc)
    (unless (procedure? proc)
      (error 'add-pre-redraw-hook! "expected a procedure" proc))
    (kernel:registry-add! pre-redraw-hook-registry proc))

  (define prepare-hook void)

  (edoc "Install composition preparation before widget and application frame hooks."
        (proc thunk "the preparation callback"))
  (define (set-prepare-hook! proc) (set! prepare-hook proc))

  (edoc "Begin frame preparation, clearing the previous deadline and running composition and registered preparation hooks.")
  (define (before-frame!)
    (set! frame-deadline #f)
    (deliver-endpoint!)
    (prepare-hook)
    (for-each (lambda (hook) (guard (ex [else (void)]) (hook)))
              (kernel:registry-items pre-redraw-hook-registry)))

  (edoc "Register a hook run when the head shuts down."
        (proc thunk "the hook"))
  (define (add-shutdown-hook! proc)
    (unless (procedure? proc)
      (error 'add-shutdown-hook! "expected a procedure" proc))
    (kernel:registry-add! shutdown-hook-registry proc))

  (edoc "Run the shutdown hooks, ignoring their errors.")
  (define (run-shutdown-hooks!)
    (for-each (lambda (hook) (guard (ex [else (void)]) (hook)))
              (kernel:registry-items shutdown-hook-registry)))

  ;;; Interruptible execution -----------------------------------------------------------

  ;; A runaway computation run on the user's behalf (an M-x expression, a
  ;; shell command, ...) would freeze the editor, so for its duration the
  ;; terminal turns C-g into SIGINT (outside it the editor runs with
  ;; signals off), and SIGINT becomes a raised condition, answering #t to
  ;; interrupted?, that unwinds the computation -- C-g aborts an
  ;; evaluation just as it cancels a prompt.  Limitation: only running
  ;; Scheme can be interrupted this way -- a blocking foreign call runs
  ;; to completion.
  (edoc "A computation was interrupted by C-g.")
  (define-condition-type &interrupted &serious make-interrupted interrupted?)

  ;; Interaction owns C-g; interruption applies to computation.  While
  ;; the editor waits for the user -- a prompt, a key query, a search --
  ;; isig is off and C-g arrives as an ordinary key the interaction
  ;; handles, so a command cancels the same way however it was invoked;
  ;; between interactions an evaluation is interruptible.
  (define isig-on? #f)

  (define (set-isig! on)
    (unless (eq? on isig-on?)
      (set! isig-on? on)
      (sys:terminal-isig! on)))

  (edoc "Run an interaction during which C-g is an ordinary key rather than an interrupt."
        (thunk thunk "the interaction")
        (returns any "what the thunk returns")
        (effects internal))
  (define (call-uninterrupted thunk)
    ;; run thunk as an interaction: C-g is a key while it lasts
    (let ([old isig-on?])
      (dynamic-wind
        (lambda () (set-isig! #f))
        thunk
        (lambda () (set-isig! old)))))

  (edoc "Run a computation that C-g interrupts, raising an interrupted condition."
        (thunk thunk "the computation")
        (returns any "what the thunk returns"))
  (define (call-with-interrupt thunk)
    ;; Run thunk interruptibly by C-g.
    (let ([saved (keyboard-interrupt-handler)]
          [old isig-on?])
      (dynamic-wind
        (lambda ()
          (keyboard-interrupt-handler
            (lambda () (raise (make-interrupted))))
          (set-isig! #t))
        thunk
        (lambda ()
          (set-isig! old)
          (keyboard-interrupt-handler saved)))))

  ;;; The seat as an actor -----------------------------------------------------------

  ;; Under E_TEST_EVAL another actor may drive this head through mail: an
  ;; (evaluate token text) payload evaluates text at the top level on the main
  ;; thread -- the environment M-x sees -- and answers the sender with
  ;; (evaluated token printed) or (evaluated token error text). Like a key,
  ;; the evaluation is followed by a frame and a checkpoint, so what it showed
  ;; is what a resumed screen restores. Tests use it instead of typing into
  ;; the prompt; it is off unless the variable is set.
  (define evaluation-mail? (and (getenv "E_TEST_EVAL") #t))
  (define (deliver-evaluation-mail! message)
    (when (and evaluation-mail? (list? message) (= (length message) 3) (eq? (car message) 'message))
      (let ([from (cadr message)] [payload (caddr message)])
        (when (and (list? payload) (= (length payload) 3) (eq? (car payload) 'evaluate)
                   (string? (caddr payload)))
          (run-on-main!
            (lambda ()
              (suspension:call! ui-actor
                (lambda () (run-on-main! (lambda () (suspension:drain!
                                                      (lambda (ex) (log:add! 'head:deliver-evaluation-mail! (kernel:condition-text ex)))))))
                (lambda ()
                  (let ([reply (guard (ex [else (list 'evaluated (cadr payload) 'error (kernel:condition-text ex))])
                                 (let ([value (kernel:evaluate! (read (open-input-string (caddr payload))) (interaction-environment))])
                                   (list 'evaluated (cadr payload) (format "~s" value))))])
                    (guard (ex [else (void)]) (frame!) (idle-hook #t))
                    (actor:send! from reply))))))))))

  ;; another actor's message to this head wakes its loop; the question
  ;; is presented before the next frame
  (edoc "This head's actor identity in the store and the interaction protocol."
        (value head))
  (define ui-actor ;; Claim process identity independently of any composition.
    ;; Runtime registrations outlive the extension that first imports the
    ;; head. The default host subscribes to text before taking its inventory.
    (kernel:call-with-runtime-registrations
      (lambda ()
        (let* ([requested (startup:name)]
               [seed (or requested (startup:default-name))])
          (let claim ([name seed] [suffix 2])
            (guard (ex [(kernel:registration-conflict? ex)
                        (if requested
                            (error 'e "head name already in use" requested)
                            (claim (string-append seed " " (number->string suffix))
                                   (+ suffix 1)))])
              (kernel:call-with-registration-update
                (lambda ()
                  (let ([identity (actor:register! (list 'head name)
                                    (lambda (message) (deliver-evaluation-mail! message) (wake-main!))
                                    'all)])
                    ;; Surface events are wakeups. The head prepares current
                    ;; demanded rows on its pump, never on a publisher thread.
                    (surface:subscribe! #f (lambda (event) (wake-main!)))
                    identity)))))))))

  ;;; The seat's first state ---------------------------------------------------------

  (define interaction-started
    (begin
      (interaction:start! ui-actor wake-main!)
      (kernel:call-with-runtime-registrations
        (lambda () (add-publication-hook! (lambda (fence?) (if fence? (interaction:flush!) (interaction:publish!))))))))
)
