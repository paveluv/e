;; core.e -- the e editor's core: the library (core).
;;
;; Imported (and thereby compiled, when stale) by the `e` loader script
;; before the extension modules, which import it in turn.  Internals
;; (including all mutable state) are invisible outside the library and
;; open to compiler optimization; the exports -- the editor's public API,
;; which is all that extension modules and M-x can see -- are immutable.
;; Both guarantees are enforced by the language.

(library (core)
  (export
    ;; state, read-only
    current-buffer buffer-list point mark
    buffer? buffer-name buffer-file buffer-text buffer-clean? buffer-modified
    buffer-read-only buffer-mode-name buffer-line-numbers
    buffer-line buffer-lines buffer-line-count buffer-line-styles
    buffer-state-id
    new-buffer buffer-named editor-symbol?
    (rename (lookup-buffer buffer))   ; buffers print as (buffer "name")
    ;; buffers, windows, files
    visit-file! save-file! save!! save-as!! find-file!! data-directory
    show-buffer! kill-buffer! display-buffer! pop-up-or-reuse! buffer-append!
    fresh-buffer
    set-buffer-mode! set-buffer-read-only! set-buffer-wrap! set-buffer-name!
    call-with-buffer
    switch-buffer!! kill-buffer!!
    split-window! split-window-right!
    delete-window! delete-other-windows! other-window!
    focus-window-up! focus-window-down! focus-window-left! focus-window-right!
    resize-window! wrap! wrap-lines scroll-margin
    scrollbar scrollbar-position line-numbers line-numbers!
    ;; editing and movement
    insert-text! replace-region-text! newline! delete-forward! backspace!
    kill-line! kill-region! copy-region! yank! undo! redo!
    copy-to-kill-buffer! current-kill-ring
    forward-kill-ring-to-system-clipboard
    set-mark-command! beginning-of-line! end-of-line! keyboard-quit!
    redraw-command! open-line! page-up! page-down!
    page-window-fraction! set-point-without-scroll! point-visible?
    reset-buffer-viewports! set-buffer-viewports!
    previous-line! next-line! beginning-of-buffer! end-of-buffer!
    move-left! move-right! indent-tab!
    call-as-one-edit!
    indent-line! indent-region! indent-buffer! format-region! format-buffer!
    move-horizontal! move-vertical! goto-point!
    quit!!
    ;; extending the editor
    bind-key! bind-default-key! unbind-key! key-binding key-event-binding
    command-key command-keys command-hint describe-key!!
    register-mode! add-mode-extension! find-mode mode-styles
    memoize-buffer-analysis add-highlighter! add-hyperlinker!
    detect-hyperlinks buffer-line-hyperlinks
    register-indenter! register-formatter!
    add-status-hint! add-buffer-status-hint!
    host-color-scheme add-color-scheme-hook!
    load-module! reload-module! modules-reload-on-save config-reload-on-save
    load-config! indent-on-tab!
    add-pre-save-hook! add-post-save-hook! add-buffer-kill-hook!
    add-shutdown-hook! add-pre-redraw-hook! set-startup-page!
    prompt! confirm? prompt-ghost prompt-inspector prompt-multiline
    prompt-edge-motion prompt-reindent
    completion-highlight
    prompt-styler completion-styler
    min-window-lines
    complete! show-completions! dismiss-completions!
    read-key-event key-event-character read-paste run-on-main! wake-main!
    cursor-in-echo
    (rename (handle-key! dispatch-key!))
    selected-window select-window! quitting?
    set-message! show-message! show-prompt-message!
    current-message prompt-active? redraw! error-text mouse!
    log! present-log-entry! present-log-entries!
    log-entries log-length log-record log-styler
    format-log-entry
    message-source message-progress
    echo-highlight visual-bell!
    register-app! register-view! set-app-presentation! set-app-capture!
    app-capture-escaped? set-app-cursor-visible! set-app-manages-viewport!
    set-app-status-position! detach-app!
    app-event-position app-event-buffer-position app-event-button
    escape-app-capture! display-app! display-app-here!
    buffer-window-size buffer-narrowest-width
    target-window target-buffer show-buffer-in-target!
    view-append! view-replace! view-invalidate!
    register-log-formatter! log-history
    publish-descriptions! published-descriptions
    call-with-interrupt call-uninterrupted interrupted?
    vector-fill-range! string-search compile-style set-style!
    string-tail string-prefix? string-suffix? string-join split-lines
    ;; the editor itself
    main)
  ;; The editor defines a few names Chez also exports (the buffer record's
  ;; buffer-mode accessor vs the port option, ...); a library body may not
  ;; shadow an import, so those imports are excluded.  The system-specific
  ;; layer -- libc, termios, signals -- comes from (sys).
  (import (except (chezscheme) buffer-mode) (sys) (diff)
          (prefix (state) state:) (prefix (text) text:)
          (prefix (kernel) kernel:) (prefix (actors) actors:)
          (prefix (log) log:) (prefix (styles) styles:)
          (prefix (keymap) keymap:) (prefix (tty) tty:)
          (prefix (echo) echo:) (prefix (head) head:)
          (prefix (paint) paint:))

  ;; The bindings Chez itself provides, so that the editor's public API
  ;; (and module definitions) can be told apart from builtins -- M-x
  ;; completion highlights them.
  (define baseline-bindings
    (let ([table (make-eq-hashtable)])
      (for-each (lambda (sym) (eq-hashtable-set! table sym #t))
                (environment-symbols (scheme-environment)))
      table))

  (define (editor-symbol? sym)
    (and (top-level-bound? sym)
         (not (eq-hashtable-ref baseline-bindings sym #f))))

  ;;; Buffers and windows ----------------------------------------------------

  ;; The seat's buffer record lives in (head) now -- the client-side
  ;; cache of a store buffer plus per-seat presentation -- reached
  ;; through these facade aliases; buffer-lines-set! below stays here:
  ;; it is the store transaction, not the raw field write.
  (define make-buffer head:make-buffer)
  (define buffer? head:buffer?)
  (define buffer-name head:buffer-name)
  (define buffer-name-set! head:buffer-name-set!)
  (define buffer-lines head:buffer-lines)
  (define buffer-lines-raw-set! head:buffer-lines-raw-set!)
  (define buffer-revision head:buffer-revision)
  (define buffer-revision-set! head:buffer-revision-set!)
  (define buffer-history head:buffer-history)
  (define buffer-history-set! head:buffer-history-set!)
  (define buffer-mark-row head:buffer-mark-row)
  (define buffer-mark-row-set! head:buffer-mark-row-set!)
  (define buffer-mark-col head:buffer-mark-col)
  (define buffer-mark-col-set! head:buffer-mark-col-set!)
  (define buffer-marked head:buffer-marked)
  (define buffer-marked-set! head:buffer-marked-set!)
  (define buffer-spot-row head:buffer-spot-row)
  (define buffer-spot-row-set! head:buffer-spot-row-set!)
  (define buffer-spot-col head:buffer-spot-col)
  (define buffer-spot-col-set! head:buffer-spot-col-set!)
  (define buffer-spot-top head:buffer-spot-top)
  (define buffer-spot-top-set! head:buffer-spot-top-set!)
  (define buffer-line-numbers-setting head:buffer-line-numbers-setting)
  (define buffer-line-numbers-setting-set!
    head:buffer-line-numbers-setting-set!)
  (define buffer-wrap-setting head:buffer-wrap-setting)
  (define buffer-wrap-setting-set! head:buffer-wrap-setting-set!)
  (define buffer-state-id head:buffer-state-id)
  (define buffer-state-id-set! head:buffer-state-id-set!)
  (define buffer-state-rev head:buffer-state-rev)
  (define buffer-state-rev-set! head:buffer-state-rev-set!)
  (define-syntax buffers
    (identifier-syntax [id (head:buffers)]
      [(set! id v) (head:set-buffers! v)]))

  ;; Shared facts, read and written through the store.  The fallbacks
  ;; only cover a buffer whose twin is missing (a store outage, a
  ;; failed mirror creation); every created buffer initializes its
  ;; managed facts, so an absent property reads honestly as #f.
  ;;
  ;;   file    the visited path, or #f
  ;;   trailing whether the file ends in a newline
  ;;   modified unsaved changes (any actor's)
  ;;   mode    the buffer's mode NAME -- the registry record never
  ;;           crosses the seam; find-mode resolves it on read, so a
  ;;           reloaded mode module is picked up live
  ;;   mode-auto whether the mode came from detection
  ;;   read-only
  ;;   stamp/base the disk state last agreed with: the mtime raising
  ;;           suspicion cheaply, and the content as loaded or last
  ;;           saved -- the base for comparisons and three-way merges
  ;;   stale   a detected external change, worn as a red !! until a
  ;;           save settles it

  (define (buffer-fact b key fallback)
    (let ([id (buffer-state-id b)])
      (if id
          (guard (ex [else fallback]) (state:property id key))
          fallback)))

  (define (buffer-fact-set! b key value)
    (guard (ex [else (void)])
      (when (buffer-state-id b)
        (state:set-property! ui-actor (buffer-state-id b) key value))))

  (define (buffer-file b) (buffer-fact b 'file #f))
  (define (buffer-file-set! b v) (buffer-fact-set! b 'file v))
  (define (buffer-trailing b) (buffer-fact b 'trailing #t))
  (define (buffer-trailing-set! b v) (buffer-fact-set! b 'trailing v))
  (define (buffer-modified b) (buffer-fact b 'modified #f))
  (define (buffer-modified-set! b v) (buffer-fact-set! b 'modified v))
  (define (buffer-mode b)
    (let ([n (buffer-fact b 'mode #f)]) (and n (find-mode n))))
  (define (buffer-mode-set! b m)
    (buffer-fact-set! b 'mode (and m (mode-name m))))
  (define (buffer-mode-auto b) (buffer-fact b 'mode-auto #t))
  (define (buffer-mode-auto-set! b v) (buffer-fact-set! b 'mode-auto v))
  (define (buffer-read-only b) (buffer-fact b 'read-only #f))
  (define (buffer-read-only-set! b v) (buffer-fact-set! b 'read-only v))
  (define (buffer-stamp b) (buffer-fact b 'stamp #f))
  (define (buffer-stamp-set! b v) (buffer-fact-set! b 'stamp v))
  (define (buffer-base b) (buffer-fact b 'base #f))
  (define (buffer-base-set! b v) (buffer-fact-set! b 'base v))
  (define (buffer-stale b) (buffer-fact b 'stale #f))
  (define (buffer-stale-set! b v) (buffer-fact-set! b 'stale v))

  ;; The head's window tree lives in the (head) seam module now: the
  ;; records, the split geometry, and the seat state (windows, the
  ;; layout root, the selected window, the layout's divider output).
  ;; Facade aliases and identifier-syntax facades below; app-aware
  ;; layout surgery (set-layout-root!), wrap policy, and the main loop
  ;; stay here until the rest of the head moves.
  (define make-window head:make-window)
  (define window? head:window?)
  (define window-buffer head:window-buffer)
  (define window-buffer-set! head:window-buffer-set!)
  (define window-top head:window-top)
  (define window-top-set! head:window-top-set!)
  (define window-topseg head:window-topseg)
  (define window-topseg-set! head:window-topseg-set!)
  (define window-left head:window-left)
  (define window-left-set! head:window-left-set!)
  (define window-prow head:window-prow)
  (define window-prow-set! head:window-prow-set!)
  (define window-pcol head:window-pcol)
  (define window-pcol-set! head:window-pcol-set!)
  (define window-shown-top head:window-shown-top)
  (define window-shown-top-set! head:window-shown-top-set!)
  (define window-size head:window-size)
  (define window-size-set! head:window-size-set!)
  (define window-goal head:window-goal)
  (define window-goal-set! head:window-goal-set!)
  (define window-xoff head:window-xoff)
  (define window-xoff-set! head:window-xoff-set!)
  (define window-width head:window-width)
  (define window-width-set! head:window-width-set!)
  (define window-wgoal head:window-wgoal)
  (define window-wgoal-set! head:window-wgoal-set!)
  (define window-wrap head:window-wrap)
  (define window-wrap-set! head:window-wrap-set!)
  (define make-layout-split head:make-layout-split)
  (define layout-split? head:layout-split?)
  (define layout-split-orientation head:layout-split-orientation)
  (define layout-split-first head:layout-split-first)
  (define layout-split-first-set! head:layout-split-first-set!)
  (define layout-split-second head:layout-split-second)
  (define layout-split-second-set! head:layout-split-second-set!)
  (define layout-split-first-weight head:layout-split-first-weight)
  (define layout-split-first-weight-set!
    head:layout-split-first-weight-set!)
  (define layout-split-second-weight head:layout-split-second-weight)
  (define layout-split-second-weight-set!
    head:layout-split-second-weight-set!)
  (define layout-leaves head:layout-leaves)
  (define layout-replace head:layout-replace)
  (define layout-parent head:layout-parent)
  (define layout-min-width head:layout-min-width)
  (define layout-min-height head:layout-min-height)
  (define weighted-first head:weighted-first)
  (define layout-node! head:layout-node!)
  (define min-window-lines head:min-window-lines)
  (define-syntax windows
    (identifier-syntax [id (head:windows)]
      [(set! id v) (head:set-windows! v)]))
  (define-syntax layout-root
    (identifier-syntax [id (head:root)]
      [(set! id v) (head:set-root! v)]))
  (define-syntax current-window
    (identifier-syntax [id (head:current)]
      [(set! id v) (head:set-current! v)]))
  (define-syntax layout-dividers
    (identifier-syntax [id (head:dividers)]
      [(set! id v) (head:set-dividers! v)]))

  ;; The (state) store is the master copy of every buffer's text; the
  ;; core is its first privileged client.  A core buffer's lines field
  ;; is a cache of the store's immutable text vector, adopted after
  ;; every operation -- the core never mutates a line vector in place.
  ;; Core edits enter the store transactionally (whole-line and
  ;; line-splice granularity), wholesale replacements are resets, and
  ;; foreign actors' edits flow back before each frame
  ;; (sync-foreign-edits!).  If the store refuses (a foreign edit
  ;; overlapped mid-command) or breaks, the core keeps editing:
  ;; content wins locally and the store is reset to match.

  (define ui-actor '(head main))

  (define (mirror-create! b)
    (guard (ex [else (void)])
      (buffer-state-id-set!
        b (state:create! ui-actor (buffer-name b)
                         (vector->list (buffer-lines b))))
      (buffer-state-rev-set! b 0)))

  (define (adopt-state! b)
    ;; make the cache the store's current text -- the vectors are
    ;; immutable, so adoption is reference sharing, never a copy
    (let-values ([(text revision) (state:snapshot (buffer-state-id b))])
      (buffer-lines-raw-set! b text)
      (buffer-state-rev-set! b revision)
      (bump-buffer-revision! b)))

  ;; Store outage: buffers whose cache forked from the store because a
  ;; store call failed.  Each fork is logged once, edits stay local
  ;; (never half-and-half), and every frame re-converges what it can.
  (define forked-buffers '())

  (define (adopt-local! b text)
    ;; the store is unreachable: keep editing on the local cache
    ;; alone -- on the record, and queued for re-convergence
    (buffer-lines-raw-set! b text)
    (bump-buffer-revision! b)
    (when (and (buffer-state-id b) (not (memq b forked-buffers)))
      (set! forked-buffers (cons b forked-buffers))
      (guard (ex [else (void)])
        (log! 'state
              (format "store outage: ~s forked from the store"
                      (buffer-name b))))))

  (define (reconverge-forked!)
    ;; recovery, at frame time: re-baseline each forked buffer from
    ;; its cache; a store still down keeps the buffer queued, a dead
    ;; buffer drops out.  A twin that no longer exists means another
    ;; actor deleted the buffer: the head forgets it rather than
    ;; resurrecting what someone killed (the lifecycle sync normally
    ;; gets there first).
    (when (pair? forked-buffers)
      (set! forked-buffers
        (filter
          (lambda (b)
            (guard (ex [else #t])
              (cond
                [(not (memq b (buffer-list))) #f]
                [(not (state:exists? (buffer-state-id b)))
                 (buffer-state-id-set! b #f)
                 (forget-buffer! b)
                 #f]
                [else
                 (state:reset! ui-actor (buffer-state-id b)
                               (buffer-lines b))
                 (adopt-state! b)
                 (log-reconvergence! b)
                 #f])))
          forked-buffers))))

  (define (log-reconvergence! b)
    (guard (ex [else (void)])
      (log! 'state
            (format "store recovered: ~s re-baselined from the editor"
                    (buffer-name b)))))

  (define (state-reset! b new-lines)
    ;; wholesale replacement: a new store baseline, adopted back --
    ;; which is exactly re-convergence, so a success unforks
    (if (buffer-state-id b)
        (guard (ex [else (adopt-local! b new-lines)])
          (state:reset! ui-actor (buffer-state-id b) new-lines)
          (adopt-state! b)
          (set! forked-buffers (remq b forked-buffers)))
        (adopt-local! b new-lines)))

  (define (state-edit! b span replacement)
    ;; The ui's text edits go through the store first and the cache
    ;; adopts the result.  A stale refusal means a foreign edit
    ;; overlapped mid-command: core content wins -- the edit applies
    ;; to the cache's coordinates and resets the store (the conflict
    ;; is in the audit log; see the tech debt ledger).
    (define (local-text)
      (let-values ([(new-text delta)
                    (text:apply-edit (buffer-lines b) span replacement)])
        new-text))
    (if (and (buffer-state-id b) (not (memq b forked-buffers)))
        (guard (ex [else (adopt-local! b (local-text))])
          (let-values ([(status info)
                        (state:edit! ui-actor (buffer-state-id b)
                                     (buffer-state-rev b)
                                     span replacement)])
            (if (eq? status 'applied)
                (begin (adopt-state! b) (note-ui-edit! b))
                (let ([foreign
                       (guard (ex [else #f])
                         (find (lambda (entry)
                                 (not (equal? (cadr entry) ui-actor)))
                               (state:history (buffer-state-id b) 8)))])
                  ;; the conflict is on the record before core wins
                  (guard (ex [else (void)])
                    (log! 'state
                          (format "conflict: ui overrode ~a in ~s"
                                  (if foreign (cadr foreign) "another actor")
                                  (buffer-name b))))
                  (state-reset! b (local-text))
                  ;; ... and the losing actor is told, after the reset
                  ;; settles, so a re-read sees the truth:
                  ;; (conflict buffer-id buffer-name winning-actor)
                  (when foreign
                    (guard (ex [else (void)])
                      (actors:send! (cadr foreign)
                                    (list 'conflict (buffer-state-id b)
                                          (buffer-name b)
                                          ui-actor))))))))
        (adopt-local! b (local-text))))

  (define (mirror-rename! b)
    (when (buffer-state-id b)
      (guard (ex [else (void)])
        (state:rename! ui-actor (buffer-state-id b) (buffer-name b)))))

  (define (new-buffer name)
    (let ([b (make-buffer name (vector "") 0 (vector '() '())
                          0 0 #f 0 0 0 'default 'default #f 0)])
      (mirror-create! b)
      ;; the managed facts start explicit, so absence stays honest
      (buffer-trailing-set! b #t)
      (buffer-mode-auto-set! b #t)
      b))

  (define (bump-buffer-revision! b)
    (buffer-revision-set! b (+ (buffer-revision b) 1)))

  (define (buffer-of-state-id id)
    (find (lambda (b) (eqv? (buffer-state-id b) id)) buffers))

  (define (adopt-store-buffer! id)
    ;; Another actor created a store buffer: give this head a record
    ;; for it, so it shows in the buffer list like any other -- unless
    ;; its creator marked it ephemeral (a head's own pop-ups).  It
    ;; joins at the end: this seat did not ask for it.  A buffer with
    ;; no mode yet gets detection, recorded as the shared fact.
    (unless (or (buffer-of-state-id id)
                (state:property id 'ephemeral))
      (let-values ([(text revision) (state:snapshot id)])
        (let ([b (make-buffer (state:buffer-name id) text 0
                              (vector '() '()) 0 0 #f 0 0 0
                              'default 'default id revision)])
          (unless (buffer-mode b) (assign-mode! b))
          (set! buffers (append buffers (list b)))))))

  (define (buffer-lines-set! b new-lines)
    (state-reset! b new-lines))

  (define buffers-initialized                              ; most recent first
    (set! buffers (list (new-buffer "*scratch*"))))
  (define head-seat-initialized
    (let ([w (make-window (car buffers) 0 0 0 0 0 #f 0 0 0 0 1 'default)])
      (set! windows (list w))
      (set! layout-root w)
      (set! current-window w)))

  ;; A persistent layout is a binary tree.  Leaves are windows; an internal
  ;; node splits its rectangle into `below` (stacked) or `right`
  ;; (side-by-side) children.  Weights retain the user's proportions across
  ;; terminal and echo-area size changes.
  (define (set-layout-root! root)
    (let* ([old windows]
           [new-windows (layout-leaves root)])
      ;; A removed app target becomes ephemeral. Preserve the buffer it last
      ;; displayed; the app will materialize a fresh target window on demand.
      (for-each
        (lambda (a)
          (let ([target (app-target-window a)])
            (when (and target (memq target old) (not (memq target new-windows)))
              (app-target-buffer-set! a (window-buffer target))
              (app-target-window-set! a #f))))
        (registered-apps))
      (set! layout-root root)
      (set! windows new-windows)))

  (define (replace-layout-window! old replacement)
    (set-layout-root! (layout-replace layout-root old replacement)))

  ;; Whether windows soft-wrap by default -- for config.e; a window
  ;; toggled by hand (wrap!, C-x t) keeps its own setting.
  (define wrap-lines (make-parameter #t))

  (define (window-wrapped? w)
    (let* ([a (app-of (window-buffer w))]
           [choice (and a (app-wrap a))]
           [buffer-choice (buffer-wrap-setting (window-buffer w))]
           [x (cond
                [(and a (not (eq? choice 'default))) choice]
                [(not (eq? buffer-choice 'default)) buffer-choice]
                [else (window-wrap w)])])
      (if (eq? x 'default) (wrap-lines) x)))

  (define (clean-wrap? w)
    (let ([x (buffer-wrap-setting (window-buffer w))])
      (or (eq? x 'clean) (and (pair? x) (eq? (car x) 'clean)))))

  (define (wrap-width w)
    ;; a wrapped row keeps its last column for the \ continuation mark;
    ;; a clean wrap draws none and uses the full width -- or its own
    ;; cap: (clean . n) wraps at n columns inside a wider window
    (let ([x (buffer-wrap-setting (window-buffer w))])
      (max 1 (cond
               [(and (pair? x) (eq? (car x) 'clean))
                (min (cdr x) (window-content-width w))]
               [(eq? x 'clean) (window-content-width w)]
               [else (- (window-content-width w) 1)]))))

  ;; The rest of the editor is written against simple state names: `lines`,
  ;; `point-row`, and so on.  Each name is an identifier macro reading and
  ;; writing through current-window, so every command transparently follows
  ;; buffer and window switches.
  (define-syntax define-state
    (syntax-rules ()
      [(_ name place get put)
       (define-syntax name
         (identifier-syntax
           [id (get place)]
           [(set! id v) (put place v)]))]))

  (define-state lines (window-buffer current-window)
    buffer-lines buffer-lines-set!)
  (define-state file-name (window-buffer current-window)
    buffer-file buffer-file-set!)
  (define-state trailing-newline? (window-buffer current-window)
    buffer-trailing buffer-trailing-set!)
  (define-state modified? (window-buffer current-window)
    buffer-modified buffer-modified-set!)
  (define-state history (window-buffer current-window)
    buffer-history buffer-history-set!)
  (define-state mark-row (window-buffer current-window)
    buffer-mark-row buffer-mark-row-set!)
  (define-state mark-col (window-buffer current-window)
    buffer-mark-col buffer-mark-col-set!)
  (define-state mark-active? (window-buffer current-window)
    buffer-marked buffer-marked-set!)
  (define-state point-row current-window window-prow window-prow-set!)
  (define-state point-col current-window window-pcol window-pcol-set!)
  (define-state top-row current-window window-top window-top-set!)
  (define-state left-col current-window window-left window-left-set!)

  ;;; Editor state ------------------------------------------------------------

  (define (startup-greeting)
    (let* ([date (current-date)]
           [hour (date-hour date)]
           [hour12 (let ([h (mod hour 12)]) (if (= h 0) 12 h))]
           [weekdays '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
                        "Friday" "Saturday")]
           [months '#("January" "February" "March" "April" "May" "June"
                      "July" "August" "September" "October" "November"
                      "December")]
           [salutation (cond [(< hour 12) "Good morning!"]
                             [(< hour 18) "Good afternoon!"]
                             [else "Good evening!"])])
      (format "Today is ~a, ~a ~a, ~a. It's ~2,'0d:~2,'0d ~a. ~a"
              (vector-ref weekdays (date-week-day date))
              (vector-ref months (- (date-month date) 1))
              (date-day date)
              (date-year date)
              hour12
              (date-minute date)
              (if (< hour 12) "AM" "PM")
              salutation)))

  (define editor-name "e")
  ;; The echo area's model lives in the (echo) seam module now: the
  ;; names below are identifier-syntax facades, so half a hundred
  ;; (set! message ...) sites land there unchanged.  Painting, the
  ;; prompts, and the geometry driver stay here until head.e.
  (define-syntax message
    (identifier-syntax [id (echo:text)] [(set! id v) (echo:set-text! v)]))
  (define-syntax message-ghost
    (identifier-syntax [id (echo:ghost)] [(set! id v) (echo:set-ghost! v)]))
  (define-syntax message-styles
    (identifier-syntax [id (echo:styles)] [(set! id v) (echo:set-styles! v)]))
  (define-syntax echo-pending
    (identifier-syntax [id (echo:pending)] [(set! id v) (echo:set-pending! v)]))
  (define-syntax echo-cursor
    (identifier-syntax [id (echo:cursor)] [(set! id v) (echo:set-cursor! v)]))
  (define-syntax echo-indent
    (identifier-syntax [id (echo:indent)] [(set! id v) (echo:set-indent! v)]))
  (define-syntax echo-input-end
    (identifier-syntax [id (echo:input-end)] [(set! id v) (echo:set-input-end! v)]))
  (define-syntax echo-height
    (identifier-syntax [id (echo:height)] [(set! id v) (echo:set-height! v)]))
  (define-syntax echo-scroll
    (identifier-syntax [id (echo:scroll)] [(set! id v) (echo:set-scroll! v)]))
  (define-syntax echo-spans
    (identifier-syntax [id (echo:spans)] [(set! id v) (echo:set-spans! v)]))
  (define-syntax echo-live-height
    (identifier-syntax [id (echo:live-height)] [(set! id v) (echo:set-live-height! v)]))
  (define echo-greeting-shown (echo:set-text! (startup-greeting)))

  (define kill-ring "")
  (define last-command #f)
  (define suppress-history (make-parameter #f))
  (define quit? #f)
  ;; The echo area is normally one line; during a prompt it grows with
  ;; the input, wrapping at the right edge Emacs-style with a trailing
  ;; backslash and continuation lines indented to the prompt text, up to
  ;; eight lines, after which it scrolls.  The windows above share what
  ;; remains of the screen.
  (define rows 24)
  (define cols 80)
  ;;; Terminal size ---------------------------------------------------------

  ;; The system-specific work (termios, ioctl, SIGWINCH) lives in (sys);
  ;; here only the editor's idea of its size.  Without a terminal, sizes
  ;; fall back to LINES/COLUMNS.

  (define size-dirty? #t)

  ;; C-l also forces a size refresh in case resize events are unavailable.
  (define sigwinch-registered
    (watch-terminal-resize! (lambda () (set! size-dirty? #t))))

  (define (env-number name fallback)
    (let* ([s (getenv name)]
           [n (and s (string->number s))])
      (if (and n (exact? n) (integer? n) (> n 0)) n fallback)))

  (define (terminal-size!)
    (when size-dirty?
      (set! size-dirty? #f)
      (set! rows (max 4 (env-number "LINES" 24)))
      (set! cols (max 20 (env-number "COLUMNS" 80)))
      (let ([size (terminal-size)])
        (when size
          (set! rows (max 4 (car size)))
          (set! cols (max 20 (cdr size)))))))

  ;;; Small utilities -------------------------------------------------------

  (define (string-tail s i) (substring s i (string-length s)))

  (define (string-insert s at addition)
    (string-append (substring s 0 at) addition (string-tail s at)))

  (define (string-delete s from to)
    (string-append (substring s 0 from) (string-tail s to)))

  (define (error-text ex)
    (if (condition? ex)
        (with-output-to-string (lambda () (display-condition ex)))
        (format "~a" ex)))

  (define (string-suffix? suffix s)
    (let ([n (string-length s)] [m (string-length suffix)])
      (and (>= n m) (string=? (substring s (- n m) n) suffix))))

  (define (string-prefix? prefix s)
    (let ([n (string-length s)] [m (string-length prefix)])
      (and (>= n m) (string=? (substring s 0 m) prefix))))

  (define (string-join xs sep)
    ;; Build small separator-prefixed pieces, then concatenate them all once;
    ;; repeatedly extending the complete prefix would copy it quadratically.
    (if (null? xs)
        ""
        (apply string-append
               (cons (car xs)
                     (map (lambda (x) (string-append sep x)) (cdr xs))))))

  (define (common-prefix strs)
    ;; The longest prefix shared by every string in the non-empty list.
    (fold-left (lambda (acc s)
                 (let loop ([i 0])
                   (if (and (< i (string-length acc)) (< i (string-length s))
                            (char=? (string-ref acc i) (string-ref s i)))
                       (loop (+ i 1))
                       (substring acc 0 i))))
               (car strs) (cdr strs)))

  (define string-search
    ;; Index of the first occurrence of needle inside s[start, limit),
    ;; or #f.  Exact by default; the optional fold? matches case
    ;; insensitively (incremental search offers that -- lexers, mode
    ;; detection, and replace! must not).
    (case-lambda
      [(s needle start limit) (string-search s needle start limit #f)]
      [(s needle start limit fold?)
       (let ([eq? (if fold? char-ci=? char=?)]
             [len (string-length needle)])
         (if (= len 0)
             start
             (let ([failure (make-vector len 0)])
               ;; KMP prefix table: the longest proper prefix ending here.
               (let build ([i 1] [matched 0])
                 (when (< i len)
                   (cond
                     [(eq? (string-ref needle i) (string-ref needle matched))
                      (let ([matched (+ matched 1)])
                        (vector-set! failure i matched)
                        (build (+ i 1) matched))]
                     [(> matched 0)
                      (build i (vector-ref failure (- matched 1)))]
                     [else (build (+ i 1) 0)])))
               (let scan ([i start] [matched 0])
                 (cond
                   [(>= i limit) #f]
                   [(eq? (string-ref s i) (string-ref needle matched))
                    (let ([matched (+ matched 1)])
                      (if (= matched len)
                          (+ (- i len) 1)
                          (scan (+ i 1) matched)))]
                   [(> matched 0)
                    (scan i (vector-ref failure (- matched 1)))]
                   [else (scan (+ i 1) 0)])))))]))

  (define (split-lines s)
    (let loop ([start 0] [i 0] [acc '()])
      (cond [(= i (string-length s))
             (reverse (cons (substring s start i) acc))]
            [(char=? (string-ref s i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
            [else (loop start (+ i 1) acc)])))

  (define (vector-splice v from to inserted)
    ;; A copy of v with elements [from, to) replaced by the list `inserted`.
    (vector-append (vector-copy v 0 from)
                   (list->vector inserted)
                   (vector-copy v to (- (vector-length v) to))))

  (define (vector-fill-range! v from to x)
    (let loop ([i from])
      (when (< i to) (vector-set! v i x) (loop (+ i 1)))))

  (define (insert-before lst x y)
    ;; A copy of lst with y inserted right before x (or at the end).
    (cond [(null? lst) (list y)]
          [(eq? (car lst) x) (cons y lst)]
          [else (cons (car lst) (insert-before (cdr lst) x y))]))

  (define (insert-after lst x y)
    ;; A copy of lst with y inserted right after x (or at the end).
    (cond [(null? lst) (list y)]
          [(eq? (car lst) x) (cons x (cons y (cdr lst)))]
          [else (cons (car lst) (insert-after (cdr lst) x y))]))

  ;;; Buffer access and undo ------------------------------------------------

  (define (vlen) (vector-length lines))
  (define (line-at n) (vector-ref lines n))
  (define (current-line) (line-at point-row))
  (define (set-line! n s)
    ;; the store is the master: the edit goes there, the cache adopts
    (let ([b (window-buffer current-window)])
      (state-edit! b (text:make-span n 0 n
                                     (string-length (vector-ref lines n)))
                   (list s))))

  (define (splice-lines! from to inserted)
    ;; Replace whole lines [from, to) of the current buffer as one
    ;; transactional store edit -- so other actors' marks and bases
    ;; survive ordinary typing.
    (let* ([b (window-buffer current-window)]
           [old lines]
           [count (vector-length old)])
      (state-edit!
        b
        (cond
          [(< to count) (text:make-span from 0 to 0)]
          [(> from 0)
           (text:make-span (- from 1)
                           (string-length (vector-ref old (- from 1)))
                           (- count 1)
                           (string-length (vector-ref old (- count 1))))]
          [else
           (text:make-span 0 0 (- count 1)
                           (string-length (vector-ref old (- count 1))))])
        (cond
          [(< to count) (append inserted '(""))]
          [(> from 0) (cons "" inserted)]
          [(null? inserted) '("")]
          [else inserted]))))

  (define (editor-snapshot)
    ;; the cache vectors are immutable now: snapshots share, never copy
    (list lines point-row point-col trailing-newline? modified?
          (buffer-state-rev (window-buffer current-window))))

  (define (restore-snapshot! snapshot)
    ;; The snapshot was just popped off a history stack, so nothing else
    ;; references its line vector and it can be adopted without copying.
    (set! lines (car snapshot))
    (set! point-row (cadr snapshot))
    (set! point-col (caddr snapshot))
    (set! trailing-newline? (cadddr snapshot))
    ;; The buffer may have been saved or merged since this snapshot was
    ;; taken, changing its current disk base.  For a file buffer, derive
    ;; modified state from that base instead of restoring a stale flag.
    (let* ([b (window-buffer current-window)]
           [base (buffer-base b)])
      (set! modified?
        (if base
            (not (string=? (buffer-text b) base))
            (list-ref snapshot 4))))
    (set! mark-active? #f)
    (invalidate-screen-cache!))

  ;; Undo entries are labeled with the user-level action that made them
  ;; -- "insert \"hello\"", "(replace-all! \"xx\" \"yy\")" -- and undo
  ;; and redo report the label.  Inside a call-as-one-edit! group, the
  ;; box holds (label . buffers-snapshotted): one entry per buffer the
  ;; group touches, labeled with the group's label (or, lacking one,
  ;; that buffer's first edit's).
  (define edit-group (make-parameter #f))

  (define (push-undo! label)
    (vector-set! history 0 (cons (cons label (editor-snapshot))
                                 (vector-ref history 0)))
    (vector-set! history 1 '()))

  ;; Refused edits raise this distinguished condition: the main loop
  ;; shows it as a plain message rather than an exception report.
  (define-condition-type &read-only &error make-read-only-error
    read-only-error?)

  ;; Likewise for a command the user declined mid-flight -- an edit
  ;; in a buffer whose file changed on disk, say.
  (define-condition-type &refused &error make-refusal refusal?)

  (define (disk-stamp path)
    ;; The file's mtime as (seconds . nanoseconds), or #f.
    (guard (ex [else #f])
      (and (file-exists? path)
           (let ([t (file-modification-time path)])
             (cons (time-second t) (time-nanosecond t))))))

  (define (string-lines s)
    ;; s split at newlines, a trailing newline yielding no empty last
    ;; line: the shape comparisons and merges run on.
    (let* ([n (string-length s)]
           [body (if (and (> n 0)
                          (char=? (string-ref s (- n 1)) #\newline))
                     (substring s 0 (- n 1))
                     s)])
      (list->vector (split-lines body))))

  (define (ends-in-newline? s)
    (and (> (string-length s) 0)
         (char=? (string-ref s (- (string-length s) 1)) #\newline)))

  (define (merge-trailing-newline base mine theirs)
    ;; Three-way merge for the one bit line vectors do not carry.  With a
    ;; boolean, two sides that both differ from base necessarily agree.
    (cond [(eq? mine base) theirs]
          [(eq? theirs base) mine]
          [else mine]))

  (define visual-bell-generation 0)
  (define visual-bell-active? #f)

  (define (visual-bell!)
    ;; Arm an overlay but let the caller's ordinary frame paint it. A PTY
    ;; reader invokes this between decoding output and its scheduled redraw;
    ;; starting a nested frame here would stop it at BEL before the diagnostic
    ;; bytes which commonly follow.
    (when screen-live?
      (let ([display (terminal-output-port)]
            [generation
             (with-mutex redraw-lock
               (set! visual-bell-generation (+ visual-bell-generation 1))
               (set! visual-bell-active? #t)
               visual-bell-generation)])
        (fork-thread
          (lambda ()
            (sleep (make-time 'time-duration 50000000 0))
            (when screen-live?
              (parameterize ([terminal-output-port display])
                (with-mutex redraw-lock
                  ;; A newer bell owns the deadline and must not be cleared by
                  ;; an older animation's expiry.
                  (when (= generation visual-bell-generation)
                    (set! visual-bell-active? #f)
                    (invalidate-screen-cache!)
                    (update-terminal-title!)
                    (redraw-frame!))))))))))

  (define (query-key! question allowed . rest)
    ;; A focused single-key question. Decode complete terminal events so an
    ;; arrow's leading ESC cannot cancel the question and leave its remaining
    ;; bytes to move point. Callers mark an option as m)erge internally; the
    ;; marker is removed for display and its first letter uses the bold choice
    ;; face. This keeps option structure separate from presentation.
    (define (render-question text)
      (let loop ([i 0] [chars '()] [marked '()])
        (if (= i (string-length text))
            (let* ([out (list->string (reverse chars))]
                   [styles (make-vector (string-length out) 'plain)])
              (let mark ([flags (reverse marked)] [j 0])
                (unless (null? flags)
                  (when (car flags) (vector-set! styles j 'choice))
                  (mark (cdr flags) (+ j 1))))
              (cons out styles))
            (let* ([ch (string-ref text i)]
                   [marker? (and (< (+ i 2) (string-length text))
                                 (char=? (string-ref text (+ i 1)) #\))
                                 (char-alphabetic?
                                   (string-ref text (+ i 2)))
                                 (string-search allowed
                                   (string (char-downcase ch)) 0
                                   (string-length allowed)))])
              (loop (+ i (if marker? 2 1))
                    (cons ch chars) (cons marker? marked))))))
    (let* ([rendered (render-question question)]
           [shown (string-append (car rendered) " ")]
           [shown-styles (let* ([source (cdr rendered)]
                                [v (make-vector (string-length shown) 'plain)])
                           (let copy ([i 0])
                             (when (< i (vector-length source))
                               (vector-set! v i (vector-ref source i))
                               (copy (+ i 1))))
                           v)]
           [repaint (and (pair? rest) (car rest))])
      (define (repaint-extra!)
        (when repaint
          (repaint)
          (place-cursor!)))
      (call-uninterrupted
        (lambda ()
          (dynamic-wind
            (lambda ()
              (set! message shown)
              (set! message-ghost "")
              (set! message-styles (cons shown (lambda (_) shown-styles)))
              (set! echo-indent 0)
              (set! echo-input-end (string-length shown))
              (set! echo-cursor (string-length shown))
              (redraw!)
              (repaint-extra!))
            (lambda ()
              (let wait ()
                (let ([event (read-key-event #f)])
                  (cond [(eof-object? event) #f]
                    [(string=? event "C-g") #\alarm]
                    [(string=? event "ESC") #\esc]
                    [(key-event-character event)
                     => (lambda (choice)
                          (if (string-search allowed
                                (string (char-downcase choice))
                                0 (string-length allowed))
                              choice
                              (begin
                                (visual-bell!)
                                (repaint-extra!)
                                (wait))))]
                    [else
                     (visual-bell!)
                     (repaint-extra!)
                     (wait)]))))
            (lambda ()
              (set! echo-cursor #f)
              (set! echo-indent #f)
              (set! echo-input-end #f)
              (set! message-styles #f)
              (set! message "")
              (set! message-ghost "")))))))

  (define (check-disk-before-edit!)
    ;; The start of an edit session -- one undo entry; chained typing
    ;; checks once: if the file changed on disk meanwhile, mark the
    ;; buffer stale -- a red !! in the status bar -- and let the edit
    ;; proceed; the save guard still compares contents.  The mtime
    ;; raises the suspicion cheaply; the content confirms it, so a
    ;; mere touch passes silently.
    (let ([b (window-buffer current-window)])
      (when (and file-name (buffer-base b))
        (let ([stamp (disk-stamp file-name)])
          (unless (equal? stamp (buffer-stamp b))
            (let ([disk (guard (ex [else #f])
                          (and (file-exists? file-name)
                               (read-file file-name)))])
              (unless (and disk (string=? disk (buffer-base b)))
                (buffer-stale-set! b #t))
              (buffer-stamp-set! b stamp)))))))

  (define (record-edit! label)
    ;; Every editing command passes through here before touching the
    ;; buffer, so this is also where read-only buffers are protected:
    ;; #t forbids all edits, and a procedure decides per edit.
    (let ([guard (buffer-read-only (window-buffer current-window))])
      (when (if (procedure? guard) (not (guard)) guard)
        (raise (condition (make-read-only-error)
                          (make-message-condition "buffer is read-only")))))
    (unless (suppress-history)
      (check-disk-before-edit!)
      (let ([group (edit-group)]
            [b (window-buffer current-window)])
        (cond [(not group) (push-undo! label)]
              [(memq b (cdr (unbox group))) (void)]
              [else (push-undo! (or (car (unbox group)) label))
                    (set-box! group (cons (car (unbox group))
                                          (cons b (cdr (unbox group)))))]))))

  (define (relabel-last-edit! label)
    (let ([h (vector-ref history 0)])
      (when (pair? h) (set-car! (car h) label))))

  (define (call-as-one-edit! label thunk)
    ;; Bundle every edit thunk makes into one labeled undo step per
    ;; buffer it touches -- and none for buffers it does not edit.
    ;; Nested groups defer to the outermost.
    (if (edit-group)
        (thunk)
        (parameterize ([edit-group (box (cons label '()))]) (thunk))))

  (define (elide s width)
    ;; s shortened to about width with an elided middle, for messages.
    (if (<= (string-length s) width)
        s
        (let ([keep (max 4 (quotient (- width 5) 2))])
          (string-append (substring s 0 keep) " ... "
                         (string-tail s (- (string-length s) keep))))))

  (define (foreign-edits-since? b rev)
    ;; did another actor edit this buffer's store copy after rev?
    (and (buffer-state-id b)
         (guard (ex [else #f])
           (exists (lambda (entry)
                     (and (> (car entry) rev)
                          (not (equal? (cadr entry) ui-actor))))
                   (state:history (buffer-state-id b) 256)))))

  (define (history-shift! from to verb)
    ;; The report -- what was undone or redone -- is also returned, so
    ;; M-x (undo!) shows it as its result.  Restoring a snapshot from
    ;; before another actor's edit would silently erase their work, so
    ;; that refuses instead, like state:undo! reports 'blocked.
    (set! message
      (cond
        [(null? (vector-ref history from))
         (format "No further ~a information" (string-downcase verb))]
        [(foreign-edits-since?
           (window-buffer current-window)
           (let ([snapshot (cdr (car (vector-ref history from)))])
             (if (> (length snapshot) 5) (list-ref snapshot 5) 0)))
         (format "~a blocked: another actor edited this buffer since"
                 verb)]
        [else
         (let ([entry (car (vector-ref history from))])
           (vector-set! history from (cdr (vector-ref history from)))
           (vector-set! history to
             (cons (cons (car entry) (editor-snapshot))
                   (vector-ref history to)))
           (restore-snapshot! (cdr entry))
           (elide (if (car entry) (format "~a ~a" verb (car entry)) verb)
                  cols))]))
    message)

  (define (undo!) (history-shift! 0 1 "Undo"))

  (define (redo!) (history-shift! 1 0 "Redo"))


  ;;; Point, mark, and editing ----------------------------------------------

  (define (changed!)
    (set! modified? #t) (set! message "") (set! mark-active? #f)
    (set! goal-pos #f))

  (define (ordered-region) ; -> start-row start-col end-row end-col
    (if (or (< point-row mark-row)
            (and (= point-row mark-row) (< point-col mark-col)))
        (values point-row point-col mark-row mark-col)
        (values mark-row mark-col point-row point-col)))

  (define (clamp-point!)
    (set! point-row (max 0 (min point-row (- (vlen) 1))))
    (set! point-col (max 0 (min point-col (string-length (current-line))))))

  (define (move-left!)
    (cond [(> point-col 0) (set! point-col (- point-col 1))]
          [(> point-row 0)
           (set! point-row (- point-row 1))
           (set! point-col (string-length (current-line)))]))

  (define (move-right!)
    (cond [(< point-col (string-length (current-line)))
           (set! point-col (+ point-col 1))]
          [(< point-row (- (vlen) 1))
           (set! point-row (+ point-row 1)) (set! point-col 0)]))

  (define (move-horizontal! delta)
    ;; Move point delta characters, negative to the left, crossing line
    ;; ends the way repeated single steps do.
    (if (< delta 0)
        (do ([i 0 (- i 1)]) ((= i delta)) (move-left!))
        (do ([i 0 (+ i 1)]) ((= i delta)) (move-right!))))

  (define (goto-point! p)
    ;; Move point straight to (row . col), clamped into the buffer.
    (set! point-row (max 0 (min (car p) (- (vlen) 1))))
    (set! point-col (max 0 (min (cdr p) (string-length (current-line)))))
    ;; App interaction state belongs to the buffer: every window showing the
    ;; same app mirrors its cursor and selection row.
    (when (app-buffer? (current-buffer))
      (buffer-spot-row-set! (current-buffer) point-row)
      (buffer-spot-col-set! (current-buffer) point-col)
      (for-each
        (lambda (w)
          (when (eq? (window-buffer w) (current-buffer))
            (window-prow-set! w point-row)
            (window-pcol-set! w point-col)))
        windows)))

  ;; Vertical moves aim for a goal column, so point comes back to it after
  ;; passing through shorter lines (as in Emacs).  The goal survives exactly
  ;; as long as each command finds point where the previous vertical move
  ;; left it; anything else that moves point starts a fresh goal.
  (define goal-col 0)
  (define goal-pos #f)

  (define (move-vertical! delta)
    ;; By buffer lines -- or by visual rows in a soft-wrapping window,
    ;; where up and down walk a long line's segments (C-a and C-e
    ;; still treat it as one line).  The goal column is visual when
    ;; wrapped.
    (define wrapped? (window-wrapped? current-window))
    (define (land! breaks k)
      ;; the goal column within segment k, clamped into it
      (set! point-col
        (min (+ (segment-start breaks k) goal-col)
             (segment-close breaks k (string-length (current-line))))))
    (unless (equal? goal-pos (cons point-row point-col))
      (set! goal-col
        (if wrapped?
            (let ([breaks (line-breaks current-window (current-line))])
              (- point-col
                 (segment-start breaks (segment-of breaks point-col))))
            point-col)))
    (if wrapped?
        (let step ([n delta])
          (cond
            [(zero? n) (void)]
            [(negative? n)
             (let* ([breaks (line-breaks current-window (current-line))]
                    [seg (segment-of breaks point-col)])
               (cond
                 [(> seg 0)                ; up, within the same line
                  (land! breaks (- seg 1))]
                 [(> point-row 0)          ; onto the line above's last row
                  (set! point-row (- point-row 1))
                  (let ([breaks (line-breaks current-window
                                             (current-line))])
                    (land! breaks (- (vector-length breaks) 1)))]))
             (step (+ n 1))]
            [else
             (let* ([breaks (line-breaks current-window (current-line))]
                    [seg (segment-of breaks point-col)])
               (cond
                 [(< (+ seg 1) (vector-length breaks))
                  (land! breaks (+ seg 1))]  ; down, within the same line
                 [(< point-row (- (vlen) 1))
                  (set! point-row (+ point-row 1))
                  (land! (line-breaks current-window (current-line)) 0)]))
             (step (- n 1))]))
        (begin
          (set! point-row (max 0 (min (+ point-row delta) (- (vlen) 1))))
          (set! point-col (min goal-col (string-length (current-line))))))
    (set! goal-pos (cons point-row point-col)))

  (define (split-inserted-lines s)
    ;; Unlike split-lines, retain an empty final part: inserting "a\n"
    ;; creates a new empty row and leaves point on it.
    (let ([n (string-length s)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond [(= i n) (reverse (cons (substring s start i) acc))]
              [(char=? (string-ref s i) #\newline)
               (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
              [else (loop (+ i 1) start acc)]))))

  (define (insert-text! s)
    ;; Buffer rows never contain newline characters.  Programmatic inserts
    ;; get the same structural treatment as a paste or repeated newline!.
    (unless (string=? s "")
      (record-edit! (format "insert ~s" s))
      (let* ([row point-row]
             [col point-col]
             [old (current-line)]
             [parts (split-inserted-lines s)])
        (if (null? (cdr parts))
            (begin
              (set-line! row (string-insert old col s))
              (set! point-col (+ col (string-length s))))
            (let* ([last (car (reverse parts))]
                   [replacement
                    (append
                      (list (string-append (substring old 0 col) (car parts)))
                      (reverse (cdr (reverse (cdr parts))))
                      (list (string-append last (string-tail old col))))])
              (splice-lines! row (+ row 1) replacement)
              (set! point-row (+ row (- (length parts) 1)))
              (set! point-col (string-length last))))
        (changed!))))

  (define (newline!)
    (record-edit! "newline")
    (let ([s (current-line)])
      (set-line! point-row (substring s 0 point-col))
      (splice-lines! (+ point-row 1) (+ point-row 1)
                     (list (string-tail s point-col)))
      (set! point-row (+ point-row 1)) (set! point-col 0)
      (changed!)))

  (define (delete-forward!)
    (cond [(< point-col (string-length (current-line)))
           (record-edit!
             (format "delete ~s"
                     (string (string-ref (current-line) point-col))))
           (set-line! point-row
             (string-delete (current-line) point-col (+ point-col 1)))
           (changed!)]
          [(< point-row (- (vlen) 1))
           (record-edit! "delete newline")
           (set-line! point-row
             (string-append (current-line) (line-at (+ point-row 1))))
           (splice-lines! (+ point-row 1) (+ point-row 2) '())
           (changed!)]))

  (define (backspace!)
    (when (or (> point-col 0) (> point-row 0))
      (record-edit!
        (if (> point-col 0)
            (format "delete ~s"
                    (string (string-ref (current-line) (- point-col 1))))
            "delete newline"))
      (parameterize ([suppress-history #t])
        (move-left!) (delete-forward!))))

  ;;; Kill and yank ---------------------------------------------------------

  (define forward-kill-ring-to-system-clipboard
    (make-parameter
      #f
      (lambda (enabled?)
        (unless (boolean? enabled?)
          (error 'forward-kill-ring-to-system-clipboard
                 "expected a boolean" enabled?))
        enabled?)))

  (define base64-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  (define (base64-encode bytes)
    (let ([length (bytevector-length bytes)])
      (let loop ([at 0] [parts '()])
        (if (= at length)
            (apply string-append (reverse parts))
            (let* ([remaining (- length at)]
                   [a (bytevector-u8-ref bytes at)]
                   [b (if (> remaining 1)
                          (bytevector-u8-ref bytes (+ at 1)) 0)]
                   [c (if (> remaining 2)
                          (bytevector-u8-ref bytes (+ at 2)) 0)]
                   [bits (+ (bitwise-arithmetic-shift-left a 16)
                            (bitwise-arithmetic-shift-left b 8) c)]
                   [digit (lambda (shift)
                            (string
                              (string-ref
                                base64-alphabet
                                (bitwise-and
                                  (bitwise-arithmetic-shift-right bits shift)
                                  63))))]
                   [chunk (string-append
                            (digit 18) (digit 12)
                            (if (> remaining 1) (digit 6) "=")
                            (if (> remaining 2) (digit 0) "="))])
              (loop (+ at (min 3 remaining)) (cons chunk parts)))))))

  (define (publish-system-clipboard! text)
    ;; OSC 52 lets the host terminal own the clipboard, which also works when
    ;; e is several SSH or multiplexer layers away from the desktop. The
    ;; payload is base64, so buffer contents cannot terminate the sequence.
    (when (and (forward-kill-ring-to-system-clipboard) screen-live?)
      (with-mutex redraw-lock
        (ansi "\x1b;]52;c;" (base64-encode (string->utf8 text)) "\x1b;\\")
        (flush-output-port (terminal-output-port)))))

  (define (kill! text)
    ;; Consecutive kill commands accumulate into a single kill-ring entry.
    (set! kill-ring
      (if (eq? last-command 'kill) (string-append kill-ring text) text))
    (set! last-command 'kill)
    (publish-system-clipboard! kill-ring))

  (define (copy-to-kill-buffer! text)
    ;; Replace the text yanked by C-y without changing a buffer or point.
    (unless (string? text)
      (error 'copy-to-kill-buffer! "expected a string" text))
    (set! kill-ring text)
    (publish-system-clipboard! kill-ring)
    (void))

  (define (kill-line!)
    (let* ([s (current-line)] [n (string-length s)])
      (cond [(< point-col n)
             (let ([text (substring s point-col n)])
               (record-edit! (format "kill ~s" text))
               (kill! text)
               (set-line! point-row (substring s 0 point-col))
               (changed!))]
            [(< point-row (- (vlen) 1))
             (kill! "\n")
             (delete-forward!)])))

  (define (current-kill-ring)
    ;; The kill ring's text, for consumers outside the buffer -- the
    ;; terminal's yank, a future clipboard bridge.
    kill-ring)

  (define (yank!)
    ;; Kill-ring entries can span lines after consecutive C-k commands.  Insert
    ;; newlines as buffer structure rather than embedding them in a line string.
    (unless (string=? kill-ring "")
      (record-edit! (format "yank ~s" kill-ring))
      (parameterize ([suppress-history #t])
        (let ([parts (split-lines kill-ring)])
          (insert-text! (car parts))
          (for-each (lambda (part) (newline!) (insert-text! part))
                    (cdr parts))))))

  (define (region-text sr sc er ec)
    (if (= sr er)
        (substring (line-at sr) sc ec)
        (let loop ([row (- er 1)] [acc (list (substring (line-at er) 0 ec))])
          (if (< row sr)
              (apply string-append acc)
              (loop (- row 1)
                    (cons (if (= row sr)
                              (string-tail (line-at sr) sc)
                              (line-at row))
                          (cons "\n" acc)))))))

  (define (delete-region! sr sc er ec)
    (if (= sr er)
        (set-line! sr (string-delete (line-at sr) sc ec))
        (let ([joined (string-append (substring (line-at sr) 0 sc)
                                     (string-tail (line-at er) ec))])
          (splice-lines! sr (+ er 1) (list joined)))))

  (define (replace-region-text! start end text)
    ;; Replace one ordered buffer range in a single structural operation.
    ;; Bulk editors use this instead of rebuilding a line once per match.
    (let ([sr (car start)] [sc (cdr start)]
          [er (car end)] [ec (cdr end)])
      (record-edit! "replace region")
      (parameterize ([suppress-history #t])
        (delete-region! sr sc er ec)
        (set! point-row sr)
        (set! point-col sc)
        (insert-text! text))
      (changed!)))

  (define (copy-region!)
    ;; Save the region to the kill ring without deleting it -- M-w, as
    ;; in Emacs.  The mark deactivates; C-y reinserts.
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let-values ([(sr sc er ec) (ordered-region)])
          (if (and (= sr er) (= sc ec))
              (set! message "Empty region")
              (begin
                (copy-to-kill-buffer! (region-text sr sc er ec))
                (set! mark-active? #f)
                (set! message "Copied"))))))

  (define (kill-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let-values ([(sr sc er ec) (ordered-region)])
          (if (and (= sr er) (= sc ec))
              (set! message "Empty region")
              (let ([text (region-text sr sc er ec)])
                (record-edit! (format "kill ~s" text))
                (kill! text)
                (delete-region! sr sc er ec)
                (set! point-row sr) (set! point-col sc)
                (changed!)
                (set! last-command 'kill))))))

  ;;; Files -----------------------------------------------------------------

  (define (read-file path)
    (call-with-input-file path
      (lambda (p)
        (let ([s (get-string-all p)])
          (if (eof-object? s) "" s)))))

  (define (directory-part path)
    ;; Everything up to and including the last slash, or #f without one.
    (let loop ([i (- (string-length path) 1)])
      (cond [(< i 0) #f]
            [(char=? (string-ref path i) #\/) (substring path 0 (+ i 1))]
            [else (loop (- i 1))])))

  (define (base-name path)
    (let ([dir (directory-part path)])
      (if dir (string-tail path (string-length dir)) path)))

  (define (expand-path path)
    ;; Expand a leading ~ to the home directory.
    (let ([home (getenv "HOME")])
      (cond [(not home) path]
            [(string=? path "~") home]
            [(string-prefix? "~/" path) (string-append home (string-tail path 1))]
            [else path])))

  (define (abbreviate-path path)
    ;; The inverse of expand-path, for display: home becomes ~.
    (let ([home (getenv "HOME")])
      (if (and home (string-prefix? (string-append home "/") path))
          (string-append "~" (string-tail path (string-length home)))
          path)))

  (define (absolute-path path)
    ;; A relative path is relative to the process working directory,
    ;; which never changes.
    (if (or (string-prefix? "/" path) (string-prefix? "~" path))
        path
        (string-append (current-directory) "/" path)))

  (define (canonical-visit-path path)
    ;; One stable identity for visited files. Existing paths chase symbolic
    ;; links; for a new file, chase its existing parent and retain the final
    ;; component. Textual normalization is the portable fallback.
    (let* ([full (canonical-path (expand-path path))]
           [real (canonical-file-path full)])
      (or real
          (let* ([dir (or (directory-part full) "/")]
                 [parent (if (and (> (string-length dir) 1)
                                  (string-suffix? "/" dir))
                             (substring dir 0 (- (string-length dir) 1))
                             dir)]
                 [real-parent (canonical-file-path parent)])
            (if real-parent
                (string-append real-parent "/" (base-name full))
                full)))))

  (define (default-directory)
    ;; The directory of the current buffer's file (or the working
    ;; directory), with a trailing slash, absolute -- a file visited
    ;; by a relative path has a relative directory-part, useless as a
    ;; prompt offer on its own -- and abbreviated for display.
    (abbreviate-path
      (absolute-path
        (or (and file-name (directory-part file-name))
            (string-append (current-directory) "/")))))

  (define (complete-file-name s)
    ;; Completion candidates for the partial path s: the entries of its
    ;; directory whose names extend its final component, as full paths, with
    ;; a trailing slash on directories so completion can descend into them.
    ;; A leading ~ is kept in the candidates but expanded for the lookups.
    ;; Dotfiles are offered only once the component starts with a dot.
    (guard (ex [else '()])
      (let* ([dir (or (directory-part s) "")]
             [part (string-tail s (string-length dir))]
             [listing (directory-list
                        (expand-path
                          (cond [(string=? dir "") "."]
                                [(string=? dir "/") "/"]
                                [else (substring dir 0 (- (string-length dir) 1))])))])
        (map (lambda (name)
               (let ([full (string-append dir name)])
                 (if (file-directory? (expand-path full))
                     (string-append full "/")
                     full)))
             (sort string<?
                   (filter (lambda (name)
                             (and (string-prefix? part name)
                                  (or (not (string=? part ""))
                                      (not (string-prefix? "." name)))))
                           listing))))))

  (define (unique-name base self)
    ;; base, or base<2>, base<3>, ... -- whichever no other buffer uses.
    (let ([used (make-hashtable string-hash string=?)])
      (for-each (lambda (b)
                  (unless (eq? b self)
                    (hashtable-set! used (buffer-name b) #t)))
                buffers)
      (let loop ([k 1])
        (let ([name (if (= k 1) base (format "~a<~a>" base k))])
          (if (hashtable-ref used name #f)
              (loop (+ k 1))
              name)))))

  (define (set-buffer-name! b name)
    (unless (and (buffer? b) (string? name) (> (string-length name) 0))
      (error 'set-buffer-name! "expected a buffer and nonempty name" b name))
    (buffer-name-set! b (unique-name name b))
    (mirror-rename! b)
    b)

  (define (file-buffer path)
    ;; A fresh buffer visiting path; #f (with a message) when it cannot be read.
    (if (file-exists? path)
        (guard (ex [else (parameterize ([message-source 'visit-file!])
                           (set-message! (format "Cannot open ~a: ~a"
                                                 path (error-text ex))))
                         #f])
          (let* ([content (read-file path)]
                 [n (string-length content)]
                 [ends? (and (> n 0)
                             (char=? (string-ref content (- n 1)) #\newline))]
                 [body (if ends? (substring content 0 (- n 1)) content)]
                 [b (new-buffer (unique-name (base-name path) #f))])
            (buffer-lines-set! b (list->vector (split-lines body)))
            (buffer-trailing-set! b ends?)
            (buffer-file-set! b path)
            (buffer-base-set! b content)
            (buffer-stamp-set! b (disk-stamp path))
            (assign-mode! b)
            (log! 'visit-file! (cons "Loaded" path))
            b))
        (let ([b (new-buffer (unique-name (base-name path) #f))])
          (buffer-file-set! b path)
          (assign-mode! b)
          (log! 'visit-file! (cons "New file:" path))
          b)))

  (define (visit-file! path)
    ;; Switch to the buffer visiting path, creating it if necessary.
    ;; Reopening a buffer whose file changed on disk meanwhile raises
    ;; a buffer-only dialog: merge, reread, cancel.  Reopening never writes.
    (let ([path (canonical-visit-path path)])
      (cond [(find (lambda (b) (equal? (buffer-file b) path)) buffers)
             => (lambda (b)
                  (show-buffer! b)
                  (when (buffer-base b)
                    ;; Reopening is explicit and uncommon, so compare content
                    ;; every time. This catches preserved timestamps and a
                    ;; stale buffer whose cached stamp was already refreshed.
                    (let ([disk (guard (ex [else #f])
                                  (and (file-exists? path)
                                       (read-file path)))])
                      (cond
                        [(and disk (string=? disk (buffer-base b)))
                         (buffer-stamp-set! b (disk-stamp path))
                         (buffer-stale-set! b #f)]
                        [disk (reopen-changed-file! b path disk)]
                        [else
                         (parameterize ([message-source 'visit-file!])
                           (set-message!
                             (format "Cannot reread ~a" path)))]))))]
            [(file-buffer path) => show-buffer!])))

  (define (read-disk-for-save path)
    ;; #f means genuinely absent.  An existing file that cannot be read
    ;; cannot be compared with the buffer's base, so fail closed instead
    ;; of treating it as a new destination and replacing it unchecked.
    (and (file-exists? path)
         (guard (ex [else
                     (raise
                       (condition
                         (make-refusal)
                         (make-message-condition
                           (format "Cannot verify ~a before saving: ~a"
                                   path (error-text ex)))))])
           (read-file path))))
  (define (save-file! path*)
    ;; Saving is guarded by content, not clocks: the disk is read and
    ;; compared with the buffer's base (what it loaded or last saved).
    ;; A mismatch means somebody changed the file meanwhile -- the
    ;; save stops and asks: overwrite, merge three-way, or cancel.
    (define path (canonical-visit-path path*))
    (define adopted? (not (equal? path file-name)))  ; saving under a new name
    (define b (window-buffer current-window))
    (define disk (read-disk-for-save path))
    (define (write!)
      ;; Rewriting recreates the file: remember its permissions (the
      ;; exec bit on a script, say) and put them back after.
      (define mode (and (file-exists? path)
                        (guard (ex [else #f]) (get-mode path))))
      (guard (ex [else (parameterize ([message-source 'save-file!])
                         (set-message!
                           (format "Save failed: ~a" (error-text ex))))
                       #f])
        (call-with-output-file path
          (lambda (p)
            (let loop ([i 0])
              (when (< i (vlen))
                (display (line-at i) p)
                (when (or (< i (- (vlen) 1)) trailing-newline?) (newline p))
                (loop (+ i 1)))))
          'replace)
        (set! file-name path) (set! modified? #f)
        (begin
          (buffer-name-set! b (unique-name (base-name path) b))
          (mirror-rename! b))
        ;; re-detect the mode only when the name changed: a plain
        ;; re-save must not clobber a mode chosen by hand; adoption
        ;; also lifts read-only -- the buffer visits an ordinary
        ;; file now, whatever protected its previous life
        (when adopted? (assign-mode! b) (buffer-read-only-set! b #f))
        (when mode (guard (ex [else (void)]) (chmod path mode)))
        (buffer-base-set! b (buffer-text b))
        (buffer-stamp-set! b (disk-stamp path))
        (buffer-stale-set! b #f)
        ;; a conflicted merge reports its details once resolved --
        ;; saved with no markers left; the resolution preceded the
        ;; write, so its record does too
        (let ([pending (assq b merge-reports)])
          (when (and pending (not (buffer-has-conflicts? b)))
            (set! merge-reports (remq pending merge-reports))
            (log! 'save-file!
                  (format "Merge resolved -- details in ~a" (cdr pending)))))
        (log! 'save-file! (cons "Wrote" path))
        (run-save-hooks! post-save-hooks path)
        #t))
    (run-save-hooks! pre-save-hooks path)
    (cond
      [(and disk (not adopted?) (not modified?)
            (buffer-base b) (string=? disk (buffer-base b)))
       ;; nothing to do, and the mtime stays untouched
       (set! message "No changes to save")
       #f]
      [(and disk (not adopted?)
            (not (and (buffer-base b) (string=? disk (buffer-base b)))))
       (stale-save! b path disk write!)]
      [(and disk adopted?)
       ;; saving under a new name onto an existing file
       (let ask ()
         (let* ([k (query-key! (format "~a exists; overwrite? y)es or n)o"
                                       (base-name path))
                               "yn")]
                [n (and k (char->integer k))])
           (cond [(memv n '(121 89)) (write!)]
                 [(or (not n) (memv n '(110 78 7 27)))
                  (set! message "Save cancelled") #f]
                 [else (ask)])))]
      [else (write!)]))

  (define (merge-report! b path base report conflicts)
    ;; The merge's paper trail: a read-only *merge-<buffer>* holding
    ;; diff's unified-diff-style rendering -- built quietly, never
    ;; displayed; the echo names it.  -> the report buffer's name.
    (let* ([name (format "*merge-~a*" (buffer-name b))]
           [rb (fresh-buffer name)]
           [lines (merge-report-lines path base report conflicts)])
      (when (pair? lines) (apply buffer-append! rb lines))
      (buffer-read-only-set! rb #t)
      name))

  (define (merge-from-disk! b path disk)
    ;; Replace the buffer with the three-way merge of its base, its
    ;; text, and the disk; -> the conflict count and the report
    ;; buffer's name.  The buffer adopts the disk as its new base
    ;; either way -- the external change is incorporated, so the next
    ;; save writes cleanly.  One undo entry.
    (let* ([base-text (buffer-base b)]
           [base-trailing (ends-in-newline? base-text)]
           [mine-trailing (buffer-trailing b)]
           [disk-trailing (ends-in-newline? disk)]
           [base (string-lines base-text)])
      (let-values ([(merged conflicts report)
                    (merge3 base
                            (string-lines (buffer-text b))
                            (string-lines disk))])
        (define merged-trailing
          (merge-trailing-newline base-trailing mine-trailing disk-trailing))
        (buffer-base-set! b disk)
        (buffer-stamp-set! b (disk-stamp path))
        (record-edit! "merge from disk")
        (buffer-lines-set! b (if (null? merged)
                               (vector "")
                               (list->vector merged)))
        (buffer-trailing-set! b merged-trailing)
        (changed!)
        (values conflicts (merge-report! b path base report conflicts)))))

  (define (reread-from-disk! b path disk)
    ;; Discard the buffer's copy and adopt the disk verbatim.  Rereading is a
    ;; new baseline, not an edit: it clears modification and undo state.
    (let* ([lines (string-lines disk)]
           [last (- (vector-length lines) 1)])
      (buffer-lines-set! b lines)
      (buffer-trailing-set! b (ends-in-newline? disk))
      (buffer-base-set! b disk)
      (buffer-stamp-set! b (disk-stamp path))
      (buffer-stale-set! b #f)
      (buffer-modified-set! b #f)
      (buffer-history-set! b (vector '() '()))
      (buffer-marked-set! b #f)
      (set! merge-reports (remp (lambda (p) (eq? (car p) b)) merge-reports))
      (for-each
        (lambda (w)
          (when (eq? (window-buffer w) b)
            (let ([row (min (window-prow w) last)])
              (window-prow-set! w row)
              (window-pcol-set! w
                (min (window-pcol w)
                     (string-length (vector-ref lines row))))
              (window-top-set! w (min (window-top w) last)))))
        windows)
      (parameterize ([message-source 'visit-file!])
        (set-message! (format "Reread ~a" path)))
      #t))

  (define (reopen-changed-file! b path disk)
    (let ask ()
      (let* ([k (query-key!
                  (format "~a changed on disk: m)erge, r)eread, c)ancel"
                          (base-name path))
                  "mrc")]
             [n (and k (char->integer k))])
        (cond
          [(memv n '(109 77))                                 ; m
           (let-values ([(conflicts report-name)
                         (merge-from-disk! b path disk)])
             ;; The merge incorporated this disk version into the buffer's
             ;; baseline.  It remains modified only when it differs from disk.
             (buffer-stale-set! b #f)
             (buffer-modified-set! b (not (string=? (buffer-text b) disk)))
             (when (> conflicts 0)
               (set! merge-reports
                 (cons (cons b report-name)
                       (remp (lambda (p) (eq? (car p) b)) merge-reports))))
             (parameterize ([message-source 'visit-file!])
               (set-message!
                 (if (zero? conflicts)
                     (format "Merged from disk -- details in ~a" report-name)
                     (format "Merged with ~a conflict~a -- resolve (~a)"
                             conflicts (if (= conflicts 1) "" "s")
                             (command-hint
                               '(next-conflict! keep-mine! keep-disk!))))))
             #t)]
          [(memv n '(114 82)) (reread-from-disk! b path disk)] ; r
          [(or (not n) (memv n '(99 67 7 27)))                ; c, C-g, ESC
           (keyboard-quit!)
           #f]
          [else (ask)]))))

  ;; Merge reports awaiting resolution -- (buffer . report-name): a
  ;; conflicted merge does not announce its report buffer up front;
  ;; the save that carries the resolved text does, separately.
  (define merge-reports '())

  (define (buffer-conflict-count b)
    ;; How many merge conflict markers are left in b.
    (let ([v (buffer-lines b)])
      (let loop ([i 0] [n 0])
        (if (= i (vector-length v))
            n
            (loop (+ i 1)
                  (if (string-prefix? "<<<<<<<" (vector-ref v i))
                      (+ n 1)
                      n))))))

  (define (buffer-has-conflicts? b)
    (> (buffer-conflict-count b) 0))

  (define (stale-save! b path disk write!)
    (buffer-stale-set! b #t)   ; worn until a write settles it
    (let ask ()
      (let* ([k (query-key!
                  (format "~a changed on disk: o)verwrite, m)erge, c)ancel"
                          (base-name path))
                  "omc")]
             [n (and k (char->integer k))])
        (cond
          [(memv n '(111 79)) (write!)]                       ; o
          [(memv n '(109 77))                                 ; m
           (let-values ([(conflicts report-name)
                         (merge-from-disk! b path disk)])
             (if (zero? conflicts)
                 (begin
                   (write!)
                   (parameterize ([message-source 'save-file!])
                     (set-message!
                       (format "Merged and saved -- details in ~a"
                               report-name)))
                   #t)
                 (begin
                   (set! merge-reports
                     (cons (cons b report-name)
                           (remp (lambda (p) (eq? (car p) b))
                                 merge-reports)))
                   (parameterize ([message-source 'save-file!])
                     (set-message!
                       (format "Merged with ~a conflict~a -- resolve (~a), then save"
                               conflicts (if (= conflicts 1) "" "s")
                               (command-hint
                                 '(next-conflict! keep-mine! keep-disk!)))))
                   #f)))]
          [(memv n '(99 67 7 27)) (set! message "Save cancelled") #f]
          [(not n) #f]
          [else (ask)]))))

  (define (buffer-text b)
    (let* ([v (buffer-lines b)] [n (vector-length v)])
      (let loop ([i (- n 1)] [acc (if (buffer-trailing b) (list "\n") '())])
        (let ([acc (cons (vector-ref v i) acc)])
          (if (= i 0)
              (apply string-append acc)
              (loop (- i 1) (cons "\n" acc)))))))

  (define (buffer-clean? b)
    ;; Nothing is lost by discarding b: it was never modified, it is
    ;; read-only (a view, a report), its text is identical to what is
    ;; on disk again, or it is an empty file-less buffer.
    (or (not (buffer-modified b))
        (buffer-read-only b)
        (let ([path (buffer-file b)])
          (if path
              (and (file-exists? path)
                   (guard (ex [else #f])
                     (string=? (buffer-text b) (read-file path))))
              (let ([v (buffer-lines b)])
                (and (= (vector-length v) 1)
                     (string=? (vector-ref v 0) "")))))))

  ;;; Buffer and window commands ---------------------------------------------

  (define (set-window-buffer! w b)
    ;; Display b in w, remembering where point was in the old buffer and
    ;; restoring where it last was in the new one.
    (let ([old (window-buffer w)])
      (unless (eq? old b)
        (buffer-spot-row-set! old (window-prow w))
        (buffer-spot-col-set! old (window-pcol w))
        (buffer-spot-top-set! old (window-top w))
        ;; Buffer identity is part of every content and status row, even when
        ;; the new buffer happens to have equal text and presentation chrome.
        ;; This is especially important when an asynchronous app paints its
        ;; final frame while the main thread replaces it.
        (invalidate-screen-cache!)))
    (window-buffer-set! w b)
    (window-prow-set! w (buffer-spot-row b))
    (window-pcol-set! w (buffer-spot-col b))
    (window-top-set! w (buffer-spot-top b))
    (window-topseg-set! w 0)
    (window-left-set! w 0))

  (define (show-buffer! b)
    (when (and (app-buffer? b) (not (eq? b (window-buffer current-window))))
      (set-app-target! b current-window (window-buffer current-window)))
    (set! buffers (cons b (remq b buffers)))   ; most recently used first
    (set-window-buffer! current-window b))

  ;; Read-only views of the editor's state, for M-x and modules; mutation
  ;; goes through the command API.
  (define (current-buffer) (window-buffer current-window))
  (define (buffer-list) (list-copy buffers))
  (define (set-message! s)
    ;; A stamped message is a log entry -- recorded and shown; with
    ;; (message-source #f) it is an indicator, shown and forgotten,
    ;; like a CapsLock light, and an empty message merely clears the
    ;; indicator.  Either way it presents the moment it is set,
    ;; mid-command included, and never before the screen is the
    ;; editor's.
    (let ([src (message-source)])
      (if (and src (> (string-length s) 0))
          (log! src s)
          (show-message! s #f))))
  (define (current-message) message)

  (define (prompt-active?)
    ;; True while a prompt owns the echo area (cursor parked there).
    (and echo-cursor #t))
  (define (point) (cons point-row point-col))
  (define (mark) (and mark-active? (cons mark-row mark-col)))
  (define (buffer-line-count b) (vector-length (buffer-lines b)))
  (define (buffer-line b n) (vector-ref (buffer-lines b) n))

  (define (memoize-buffer-analysis analyze)
    ;; Turn a whole-buffer analyzer into a row provider.  Buffer content has
    ;; one revision stamp, so validation is O(1) and analysis runs at most
    ;; once between edits, however many visible rows ask for its result.
    (let ([cache (make-weak-eq-hashtable)])
      (lambda (b row)
        (let* ([revision (buffer-revision b)]
               [hit (eq-hashtable-ref cache b #f)])
          (unless (and hit (= (car hit) revision))
            (set! hit
              (cons revision (analyze (vector-copy (buffer-lines b)))))
            (eq-hashtable-set! cache b hit))
          (let ([product (cdr hit)])
            (and (< row (vector-length product))
                 (vector-ref product row)))))))

  (define (call-with-buffer b thunk)
    ;; Run thunk with b temporarily the current buffer: in the window
    ;; already showing it when there is one -- point moves where the
    ;; user sees them -- else invisibly in the current window with the
    ;; usual spot saving; the MRU order is untouched either way.
    (cond
      [(eq? b (window-buffer current-window)) (thunk)]
      [(find (lambda (w) (eq? (window-buffer w) b)) windows)
       => (lambda (w)
            (let ([prev current-window])
              (dynamic-wind
                (lambda () (set! current-window w))
                thunk
                (lambda () (set! current-window prev)))))]
      [else
       (let ([old (window-buffer current-window)])
         (dynamic-wind
           (lambda () (set-window-buffer! current-window b))
           thunk
           (lambda () (set-window-buffer! current-window old))))]))

  (define (data-directory)
    ;; Where commands and apps keep built or fetched data, out of git:
    ;; the data directory next to lib, created on first use.  Each
    ;; concern takes a subdirectory -- the describe corpus lives in
    ;; data/describe.
    (let ([dir (string-append (caar (library-directories)) "/../data")])
      (unless (file-directory? dir) (mkdir dir))
      dir))

  (define (buffer-named name)
    (find (lambda (b) (string=? (buffer-name b) name)) buffers))

  (define (fresh-buffer name)
    ;; A named snapshot-style tool buffer, emptied for rebuilding. Live tools
    ;; use register-view! instead. An existing buffer is reused: the
    ;; windows showing it keep showing it and display-buffer! finds it
    ;; on screen -- no kill, no second window, no duplication.
    (let ([b (or (buffer-named name) (new-buffer name))])
      (buffer-read-only-set! b #f)
      (buffer-lines-set! b (vector ""))
      (buffer-history-set! b (vector '() '()))
      (buffer-modified-set! b #f)
      (for-each (lambda (w)
                  (when (eq? (window-buffer w) b)
                    (window-top-set! w 0)
                    (window-topseg-set! w 0)
                    (window-prow-set! w 0)
                    (window-pcol-set! w 0)))
                windows)
      b))

  ;;; Apps and views ------------------------------------------------------------

  ;; An app is a dynamic read-only buffer with a renderer and, optionally, an
  ;; event handler. A view is the degenerate app with no handler. Apps remember
  ;; the window and buffer that were active before entry, so controls can act
  ;; on that target while the app window itself remains current.
  (define-record-type app
    (fields buffer refresh! handle-event!
            (mutable target-window) (mutable target-buffer)
            (mutable refresh-error)
            (mutable sticky-lines) (mutable scrollbar) (mutable wrap)
            (mutable cursor-style) (mutable capture)
            (mutable cursor-visible?) (mutable manages-viewport)
            (mutable status-position)))

  ;; Created lazily because the general registry machinery is initialized
  ;; later in this library. Once created it participates in module retraction
  ;; and transactional reload rollback like every other extension registry.
  (define app-registry #f)
  (define buffer-kill-hook-registry #f)
  (define shutdown-hook-registry #f)
  ;; The latest record for each app buffer also persists outside the registry.
  ;; Module retraction removes executable callbacks, while target/selection
  ;; state survives and is inherited by the replacement registration.
  (define known-apps '())

  (define (ensure-app-registry!)
    (unless app-registry (set! app-registry (make-registry)))
    app-registry)

  (define (add-buffer-kill-hook! proc)
    (unless (procedure? proc)
      (error 'add-buffer-kill-hook! "expected a procedure" proc))
    (unless buffer-kill-hook-registry
      (set! buffer-kill-hook-registry (make-registry)))
    (registry-add! buffer-kill-hook-registry proc))

  (define pre-redraw-hook-registry #f)

  (define (add-pre-redraw-hook! proc)
    ;; Run before each main-loop frame, after the command that changed
    ;; the layout -- where a view can re-fit itself to its window.
    (unless (procedure? proc)
      (error 'add-pre-redraw-hook! "expected a procedure" proc))
    (unless pre-redraw-hook-registry
      (set! pre-redraw-hook-registry (make-registry)))
    (registry-add! pre-redraw-hook-registry proc))

  (define (run-pre-redraw-hooks!)
    (when pre-redraw-hook-registry
      ;; the command that just ran may have changed the split tree;
      ;; window geometry is otherwise only refreshed while painting
      (window-layout)
      (for-each (lambda (hook) (guard (ex [else (void)]) (hook)))
                (registry-items pre-redraw-hook-registry))))

  (define (add-shutdown-hook! proc)
    (unless (procedure? proc)
      (error 'add-shutdown-hook! "expected a procedure" proc))
    (unless shutdown-hook-registry
      (set! shutdown-hook-registry (make-registry)))
    (registry-add! shutdown-hook-registry proc))

  (define (run-shutdown-hooks!)
    (when shutdown-hook-registry
      (for-each (lambda (hook) (guard (ex [else (void)]) (hook)))
                (registry-items shutdown-hook-registry))))

  (define (registered-apps)
    (if app-registry (registry-items app-registry) '()))

  (define (app-of b)
    (find (lambda (a) (eq? (app-buffer a) b)) (registered-apps)))

  (define (known-app-of b)
    (find (lambda (a) (eq? (app-buffer a) b)) known-apps))

  (define (app-buffer? b) (and (app-of b) #t))

  (define (detach-app! b)
    ;; Preserve the app's current buffer contents while removing dynamic
    ;; refresh, event capture, target-window behavior, and app presentation.
    ;; The buffer thereafter behaves like an ordinary read-only buffer.
    (let ([a (app-of b)])
      (when a
        (when (eq? a capture-bypass-app) (clear-capture-bypass!))
        (set-box! app-registry
          (remp (lambda (entry) (eq? (app-buffer (cdr entry)) b))
                (unbox app-registry))))
      (buffer-read-only-set! b #t)
      b))

  (define (set-app-target! b w prior)
    (let ([a (app-of b)])
      (when a
        (app-target-window-set! a w)
        (app-target-buffer-set! a prior))))

  (define (register-app! name refresh! . handler)
    (let* ([named (buffer-named name)]
           [_ (when (and named (not (known-app-of named)))
                (error 'register-app! "buffer name is already in use" name))]
           [b (or named (new-buffer name))]
           [old (or (app-of b) (known-app-of b))]
           [a (make-app b refresh! (and (pair? handler) (car handler))
                        (and old (app-target-window old))
                        (and old (app-target-buffer old)) #f
                        (if old (app-sticky-lines old) 0)
                        (and old (app-scrollbar old))
                        (if old (app-wrap old) 'default)
                        (if old (app-cursor-style old) 'default)
                        (and old (app-capture old))
                        (if old (app-cursor-visible? old) 'default)
                        (and old (app-manages-viewport old))
                        (and old (app-status-position old)))]
           [registry (ensure-app-registry!)])
      (unless (procedure? refresh!)
        (error 'register-app! "refresh must be a procedure" refresh!))
      (when (and (pair? handler) (not (procedure? (car handler))))
        (error 'register-app! "event handler must be a procedure" (car handler)))
      (buffer-read-only-set! b #t)
      (unless (memq b buffers) (set! buffers (append buffers (list b))))
      (set! known-apps
        (cons a (remp (lambda (old) (eq? (app-buffer old) b)) known-apps)))
      ;; Re-registration in one init replaces rather than duplicates refreshes.
      (set-box! registry
        (remp (lambda (entry) (eq? (app-buffer (cdr entry)) b))
              (unbox registry)))
      (registry-add! registry a)
      b))

  (define capture-bypass-app #f)
  (define capture-escape-event #f)
  (define capture-literal! #f)

  (define (set-app-capture! b capture?)
    (let ([a (app-of b)])
      (unless a (error 'set-app-capture! "not an app buffer" b))
      (unless (boolean? capture?)
        (error 'set-app-capture! "capture must be #t or #f" capture?))
      (app-capture-set! a capture?)
      (when (and (not capture?) (eq? a capture-bypass-app))
        (clear-capture-bypass!))
      b))

  (define (app-capture-escaped? b)
    (let ([a (app-of b)])
      (and a (eq? a capture-bypass-app))))

  (define (set-app-cursor-visible! b visible?)
    (let ([a (app-of b)])
      (unless a (error 'set-app-cursor-visible! "not an app buffer" b))
      (unless (or (boolean? visible?) (procedure? visible?))
        (error 'set-app-cursor-visible!
               "visibility must be a boolean or procedure" visible?))
      (app-cursor-visible?-set! a visible?)
      b))

  (define (set-app-manages-viewport! b manages?)
    (let ([a (app-of b)])
      (unless a (error 'set-app-manages-viewport! "not an app buffer" b))
      (unless (boolean? manages?)
        (error 'set-app-manages-viewport! "manages must be #t or #f" manages?))
      (app-manages-viewport-set! a manages?)
      b))

  (define (set-app-status-position! b position)
    (let ([a (app-of b)])
      (unless a (error 'set-app-status-position! "not an app buffer" b))
      (unless (or (not position) (procedure? position))
        (error 'set-app-status-position!
               "position must be #f or a procedure" position))
      (app-status-position-set! a position)
      b))

  (define (app-cursor-visible-in? w)
    (let* ([a (app-of (window-buffer w))]
           [visibility (and a (app-cursor-visible? a))])
      (cond [(not a) #t]
            [(eq? visibility 'default) #t]
            [(procedure? visibility)
             (guard (ex [else #t]) (visibility w))]
            [(boolean? visibility) visibility]
            [else #t])))

  (define (app-manages-window-viewport? w)
    (let ([a (app-of (window-buffer w))])
      (and a (app-manages-viewport a))))

  (define (escape-app-capture! escape-event literal!)
    ;; Suspend a fully capturing app for the next complete global command.
    ;; dispatch-sequence! owns multi-key prefixes and commands own their
    ;; prompts synchronously, so the bypass naturally lasts through both.
    (let ([a (app-of (current-buffer))])
      (unless (and a (app-capture a))
        (error 'escape-app-capture! "current app does not capture input"))
      (unless (and (string? escape-event) (procedure? literal!))
        (error 'escape-app-capture! "expected an event and literal procedure"))
      (set! capture-bypass-app a)
      (set! capture-escape-event escape-event)
      (set! capture-literal! literal!)
      (redraw!)
      (void)))

  (define (set-app-presentation! b sticky-lines scrollbar . options)
    ;; Configure buffer-level presentation shared by every window showing the
    ;; app. Sticky rows stay above the scrollable body; scrollbar is #f, #t
    ;; (enabled using the configured side), left, or right.
    (let ([a (app-of b)])
      (unless a (error 'set-app-presentation! "not an app buffer" b))
      (unless (and (integer? sticky-lines) (exact? sticky-lines)
                   (>= sticky-lines 0))
        (error 'set-app-presentation! "sticky line count must be nonnegative"
               sticky-lines))
      (unless (memq scrollbar '(#f #t left right))
        (error 'set-app-presentation!
               "scrollbar must be #f, #t, left, or right" scrollbar))
      (let ([wrap (if (pair? options) (car options) 'default)]
            [cursor-style (if (and (pair? options) (pair? (cdr options)))
                              (cadr options) 'default)])
        (unless (memq wrap '(default #t #f))
          (error 'set-app-presentation!
                 "wrap must be default, #t, or #f" wrap))
        (unless (memq cursor-style
                      '(default block underline bar
                                blinking-block blinking-underline blinking-bar))
          (error 'set-app-presentation!
                 "invalid cursor style"
                 cursor-style))
        (app-wrap-set! a wrap)
        (app-cursor-style-set! a cursor-style))
      (app-sticky-lines-set! a sticky-lines)
      (app-scrollbar-set! a scrollbar)
      (invalidate-screen-cache!)
      b))

  (define (buffer-sticky-lines b)
    (let ([a (app-of b)])
      (if a (min (app-sticky-lines a) (buffer-line-count b)) 0)))

  ;; Ordinary buffers use the global setting. An app can force the bar on
  ;; with #t, force a particular side, or otherwise inherit the global choice.
  (define scrollbar
    (make-parameter #f
      (lambda (visible?)
        (unless (boolean? visible?)
          (error 'scrollbar "must be #t or #f" visible?))
        visible?)))
  (define scrollbar-position
    (make-parameter 'right
      (lambda (side)
        (unless (memq side '(left right))
          (error 'scrollbar-position "must be left or right" side))
        side)))

  (define line-numbers
    (make-parameter #f
      (lambda (visible?)
        (unless (boolean? visible?)
          (error 'line-numbers "must be #t or #f" visible?))
        visible?)))

  (define (buffer-line-numbers b)
    (let ([setting (buffer-line-numbers-setting b)])
      (if (eq? setting 'default) (line-numbers) setting)))

  (define (line-numbers!)
    (let ([b (current-buffer)])
      (buffer-line-numbers-setting-set! b (not (buffer-line-numbers b)))
      (invalidate-screen-cache!)
      (set-message!
        (format "Line numbers ~a" (if (buffer-line-numbers b) "on" "off")))))

  (define (window-line-number-width w)
    (if (buffer-line-numbers (window-buffer w))
        (+ 1 (string-length
               (number->string (buffer-line-count (window-buffer w)))))
        0))

  (define (window-scrollbar? w)
    (let* ([a (app-of (window-buffer w))]
           [choice (and a (app-scrollbar a))])
      (cond [(memq choice '(left right)) choice]
            [(or choice (scrollbar)) (scrollbar-position)]
            [else #f])))

  (define (window-content-width w)
    (max 1 (- (window-width w)
              (if (window-scrollbar? w) 1 0)
              (window-line-number-width w))))

  (define (buffer-narrowest-width b)
    ;; The smallest content width among the windows showing b, or #f
    ;; -- what a rendering shared by every window must fit.
    (let ([ws (filter (lambda (w) (eq? (window-buffer w) b)) windows)])
      (and (pair? ws)
           (fold-left (lambda (m w) (min m (window-content-width w)))
                      (window-content-width (car ws))
                      (cdr ws)))))

  (define (buffer-window-size b)
    ;; The text grid of the preferred window displaying b.  App-owned terminal
    ;; state uses one grid per buffer, so the focused window wins when several
    ;; windows mirror it.
    (let ([w (if (eq? (window-buffer current-window) b)
                 current-window
                 (find (lambda (candidate)
                         (eq? (window-buffer candidate) b))
                       windows))])
      (and w (cons (window-size w) (window-content-width w)))))

  (define (window-scrollbar-column w)
    (case (window-scrollbar? w)
      [(left) (window-xoff w)]
      [(right) (+ (window-xoff w) (window-width w) -1)]
      [else #f]))

  (define (register-view! name refresh!)
    (register-app! name refresh!))

  (define (view-buffer? b)
    (app-buffer? b))

  (define (target-window)
    (let ([a (app-of (current-buffer))])
      (cond [(not a) current-window]
            [(memq (app-target-window a) windows) (app-target-window a)]
            [else #f])))

  (define (target-buffer)
    (let* ([a (app-of (current-buffer))]
           [w (and a (app-target-window a))])
      (cond [(not a) (current-buffer)]
            [(and w (memq w windows)
                  (not (eq? (window-buffer w) (app-buffer a))))
             (window-buffer w)]
            [(app-target-buffer a)]
            [else (current-buffer)])))

  (define (show-buffer-in-target! b)
    (let* ([a (app-of (current-buffer))]
           [w (or (target-window)
                  (and a (create-ephemeral-target-window! b)))])
      (unless (memq b buffers) (set! buffers (append buffers (list b))))
      (set! buffers (cons b (remq b buffers)))
      (when a (app-target-buffer-set! a b))
      (if w
          (begin
            (when a (app-target-window-set! a w))
            (set-window-buffer! w b))
          (parameterize ([message-source 'app])
            (set-message! "Cannot create a target window: the screen is too small")))
      b))

  (define (display-app! b)
    (unless (app-buffer? b)
      (error 'display-app! "not an app buffer" b))
    (let* ([origin current-window]
           [prior (window-buffer origin)]
           [w (display-buffer! b)])
      (and w
           (begin
             (unless (eq? prior b)
               (set-app-target! b origin prior))
             (set! current-window w)
             w))))

  (define (display-app-here! b)
    ;; Display an app in the selected window and make that same window its
    ;; target. If it is already current, preserve the buffer the app was
    ;; targeting rather than replacing that memory with the app itself.
    (unless (app-buffer? b)
      (error 'display-app-here! "not an app buffer" b))
    (let ([prior (if (eq? b (current-buffer))
                     (target-buffer)
                     (current-buffer))])
      (set-app-target! b current-window prior)
      (set! buffers (cons b (remq b buffers)))
      (set-window-buffer! current-window b)
      current-window))

  (define (refresh-visible-views!)
    (for-each (lambda (a)
                (when (find (lambda (w) (eq? (window-buffer w) (app-buffer a)))
                            windows)
                  (guard (ex [else
                              (let ([text
                                     (format "App ~a refresh failed: ~a"
                                             (buffer-name (app-buffer a))
                                             (error-text ex))])
                                (unless (equal? text (app-refresh-error a))
                                  (app-refresh-error-set! a text)
                                  (log! 'app text)))])
                    ((app-refresh! a))
                    (app-refresh-error-set! a #f))))
              (filter (lambda (a) (memq (app-buffer a) buffers))
                      (registered-apps))))

  (define (view-append! b lines)
    ;; Append lines to view b: windows whose point was at the very end
    ;; follow the tail; others hold their viewport still.
    (when (pair? lines)
      (let* ([v (buffer-lines b)]
             [n (vector-length v)]
             [virgin? (and (= n 1) (string=? (vector-ref v 0) ""))]
             [tail? (lambda (w)
                      (and (eq? (window-buffer w) b)
                           (= (window-prow w) (- n 1))
                           (= (window-pcol w)
                              (string-length (vector-ref v (- n 1))))))]
             [tails (filter tail? windows)])
        (buffer-lines-set! b
          (if virgin?
              (list->vector lines)
              (vector-splice v n n lines)))
        (let* ([nv (buffer-lines b)]
               [last (- (vector-length nv) 1)])
          (for-each (lambda (w)
                      (window-prow-set! w last)
                      (window-pcol-set! w
                        (string-length (vector-ref nv last))))
                    tails)))))

  (define (view-replace! b lines)
    ;; Replace a view's rendering without disturbing windows when it has not
    ;; changed. On a real change, keep point and the viewport where possible,
    ;; clamping them only when the new rendering is shorter.
    (let ([new (if (null? lines) (vector "") (list->vector lines))])
      (unless (equal? (buffer-lines b) new)
        (buffer-lines-set! b new)
        ;; A view may be refreshed by a worker thread while the main input
        ;; loop is between frames.  Its old row keys can otherwise survive a
        ;; racing redraw even though the buffer revision changed.  Dynamic
        ;; view replacement is comparatively rare (terminal emulation is the
        ;; demanding case), so prefer a guaranteed coherent frame.
        (invalidate-screen-cache!)
        (clamp-buffer-positions! b))))

  (define (clamp-buffer-positions! b)
    ;; keep the buffer's spot and every window's point inside the
    ;; (possibly shorter) current lines
    (let* ([v (buffer-lines b)]
           [last (- (vector-length v) 1)])
      (buffer-spot-row-set! b (min (buffer-spot-row b) last))
      (buffer-spot-col-set!
        b (min (buffer-spot-col b)
               (string-length (vector-ref v (buffer-spot-row b)))))
      (for-each
        (lambda (w)
          (when (eq? (window-buffer w) b)
            (window-prow-set! w (min (window-prow w) last))
            (window-pcol-set!
              w (min (window-pcol w)
                     (string-length (vector-ref v (window-prow w)))))
            (window-top-set! w (min (window-top w) last))))
        windows)))

  ;;; The log -----------------------------------------------------------------

  ;; The editor's syslog lives in the (log) seam module now -- the
  ;; structured records and the formatter registry are state, not UI.
  ;; The core keeps the echo-area presentation (installed below as the
  ;; log's presenter) and these facade aliases until its call sites
  ;; and the extension modules migrate to log: prefixes.
  (define log! log:log!)
  (define log-record log:log-record)
  (define log-length log:log-length)
  (define log-entries log:log-entries)
  (define log-history log:log-history)
  (define register-log-formatter! log:register-log-formatter!)
  (define log-styler log:log-styler)
  (define format-log-entry log:format-log-entry)

  (define message-source
    ;; Who a message came from, for the log's attribution: components
    ;; parameterize it around their messages.  #f makes the message a
    ;; plain indicator -- shown, never logged.
    (make-parameter 'e))

  (define message-progress
    ;; When true, a logged message supersedes its component's newest
    ;; line in the echo area -- progress redrawn in place rather than
    ;; stacked -- never a line from another component.  The log
    ;; records every step regardless.
    (make-parameter #f))

  (define (present-log-entry! e)
    ;; Present an existing record in the echo area without logging it again.
    (present-log-entries! (list e)))

  (define (present-log-entries! entries . tail)
    ;; Queue several existing records and repaint once, avoiding a full echo
    ;; geometry change and terminal redraw for every streamed line.
    (let loop ([left entries])
      (when (pair? left)
        (let* ([e (car left)]
               [text (format-log-entry e)]
               [styler (log-styler (cadr e))]
               [ghost (if (and (null? (cdr left)) (pair? tail))
                          (car tail)
                          "")])
          (echo-queue! (cadr e) text styler #f ghost)
          (loop (cdr left)))))
    (when (pair? entries) (present-echo!)))

  ;; The head's side of every log! -- present the fresh record
  ;; transiently in the echo area, styled by its component's styler.
  ;; Visible log views catch up at the next redraw.
  (define log-presenter-installed
    (log:set-presenter!
      (lambda (e show?)
        (when show?
          (if (message-progress)
              (echo-append! (cadr e) (format-log-entry e)
                            (log-styler (cadr e)) #t)
              (present-log-entry! e))))))


  ;; A buffer's printed form is the expression that looks it up again, so
  ;; results shown in *eval* can be pasted straight into the next
  ;; expression: (buffer-line-count (buffer "e")).  The lookup is by
  ;; name at evaluation time -- a killed buffer's form reports itself.
  (define (lookup-buffer name)
    (or (buffer-named name) (error 'buffer "no buffer named" name)))

  (define buffer-printing
    (record-writer (record-type-descriptor head:buffer)
      (lambda (r p wr)
        (display "(buffer " p)
        (wr (buffer-name r) p)
        (display ")" p))))

  (define (complete-buffer-name s)
    (sort string<? (filter (lambda (n) (string-prefix? s n))
                           (map buffer-name buffers))))

  (define (switch-buffer!!)
    (let* ([current (window-buffer current-window)]
           [default (find (lambda (b) (not (eq? b current))) buffers)]
           [s (prompt! (if default
                           (format "Switch to buffer (default ~a): "
                                   (buffer-name default))
                           "Switch to buffer: ")
                       complete-buffer-name)])
      (when s
        (cond [(string=? s "") (when default (show-buffer! default))]
              [(buffer-named s) => show-buffer!]
              [else (show-buffer! (new-buffer s))
                    (set! message (format "New buffer ~a" s))]))))

  (define (kill-buffer! b)
    (when (buffer-state-id b)
      (guard (ex [else (void)])
        (state:delete! ui-actor (buffer-state-id b))
        (buffer-state-id-set! b #f)))
    (forget-buffer! b)
    (parameterize ([message-source 'kill-buffer!])
      (set-message! (format "Killed ~a" (buffer-name b)))))

  (define (forget-buffer! b)
    ;; drop this head's record of a buffer whose twin is gone -- kill
    ;; hooks, the buffer list, apps, and every window showing it
    (when buffer-kill-hook-registry
      (for-each
        (lambda (hook)
          (guard (ex [else
                      (log! 'kill-buffer!
                            (format "Buffer cleanup failed for ~a: ~a"
                                    (buffer-name b) (error-text ex)))])
            (hook b)))
        (registry-items buffer-kill-hook-registry)))
    (set! buffers (remq b buffers))
    (set! known-apps
      (remp (lambda (a) (eq? (app-buffer a) b)) known-apps))
    (when app-registry
      (set-box! app-registry
        (remp (lambda (entry) (eq? (app-buffer (cdr entry)) b))
              (unbox app-registry))))
    (when (null? buffers) (set! buffers (list (new-buffer "*scratch*"))))
    (for-each (lambda (w)
                (when (eq? (window-buffer w) b)
                  (set-window-buffer! w (car buffers))))
              windows))

  (define (kill-buffer!!)
    (let* ([current (window-buffer current-window)]
           [s (prompt! (format "Kill buffer (default ~a): "
                               (buffer-name current))
                       complete-buffer-name)])
      (when s
        (let ([b (if (string=? s "") current (buffer-named s))])
          (cond [(not b) (set! message (format "No buffer named ~a" s))]
                [(or (buffer-clean? b)
                     (confirm? (format "Buffer ~a modified; kill anyway?"
                                       (buffer-name b))))
                 (kill-buffer! b)])))))

  (define (next-window w)
    (let ([tail (cdr (memq w windows))])
      (if (pair? tail) (car tail) (car windows))))

  (define (focus-window! w)
    ;; All user-visible focus changes pass here so entering an app by keyboard
    ;; or mouse records the window and buffer being left as its target.
    (when (and (memq w windows) (not (eq? w current-window)))
      (let ([old current-window])
        (dispatch-app-event! "BLUR")
        (when (app-buffer? (window-buffer w))
          (set-app-target! (window-buffer w) old (window-buffer old)))
        (set! current-window w)
        (dispatch-app-event! "FOCUS")))
    current-window)

  (define (other-window!)
    (focus-window! (next-window current-window)))

  (define (focus-window-direction! direction)
    (let* ([layout (window-layout)]
           [cursor (window-screen-position current-window
                                           point-row point-col)]
           [cx (- (cdr cursor) 1)]
           [cy (- (car cursor) 1)])
      ;; Cast a ray from point. This matters in asymmetric trees: from a tall
      ;; right-hand window, for example, the cursor row chooses which of two
      ;; stacked windows on the left receives focus.
      (define (distance entry)
        (let* ([w (car entry)]
               [x0 (window-xoff w)] [x1 (+ x0 (window-width w) -1)]
               [y0 (cadr entry)] [y1 (+ y0 (caddr entry))])
          (case direction
            [(left) (and (< x1 cx) (<= y0 cy y1) (- cx x1))]
            [(right) (and (> x0 cx) (<= y0 cy y1) (- x0 cx))]
            [(up) (and (< y1 cy) (<= x0 cx x1) (- cy y1))]
            [(down) (and (> y0 cy) (<= x0 cx x1) (- y0 cy))])))
      (let loop ([entries layout] [best #f] [best-distance #f])
        (if (null? entries)
            (when best (focus-window! (car best)))
            (let ([d (and (not (eq? (caar entries) current-window))
                          (distance (car entries)))])
              (if (and d (or (not best-distance) (< d best-distance)))
                  (loop (cdr entries) (car entries) d)
                  (loop (cdr entries) best best-distance)))))))

  (define (focus-window-up!) (focus-window-direction! 'up))
  (define (focus-window-down!) (focus-window-direction! 'down))
  (define (focus-window-left!) (focus-window-direction! 'left))
  (define (focus-window-right!) (focus-window-direction! 'right))

  (define (selected-window)
    ;; The current window, an opaque token: hold it, compare it, give
    ;; it back to select-window!.
    current-window)

  (define (select-window! w)
    ;; Make w current when it is still on screen; -> whether it was.
    (and (memq w windows) (begin (focus-window! w) #t)))

  (define (quitting?)
    ;; Has a quit been requested?  A module driving its own key loop
    ;; (the search) checks this after dispatching a key through.
    quit?)

  (define (split-current-window! orientation b)
    (let* ([vertical? (eq? orientation 'below)]
           [extent (if vertical?
                       (+ (window-size current-window) 1)
                       (window-width current-window))]
           [minimum (if vertical? (+ (min-window-lines) 1) 20)]
           [usable (- extent (if vertical? 0 1))])
      (and (>= usable (* 2 minimum))
           (let* ([second (quotient usable 2)]
                  [first (- usable second)]
                  [w (make-window b top-row (window-topseg current-window)
                                  left-col point-row point-col #f
                                  (max 1 (- second 1))
                                  (max 1 (- second 1)) 0 0 second
                                  (window-wrap current-window))]
                  [node (make-layout-split orientation current-window w
                                           first second)])
             (replace-layout-window! current-window node)
             w))))

  (define (split-window!)
    ;; Split only the selected leaf, as in Emacs.
    (unless (split-current-window! 'below (window-buffer current-window))
      (set! message "Not enough room to split")))

  (define (split-window-right!)
    (unless (split-current-window! 'right (window-buffer current-window))
      (set! message "Not enough room to split"))
    (void))

  (define (wrap! . on)
    ;; Toggle (or set) soft-wrapping of long lines in the current window.
    (window-wrap-set! current-window
                      (if (pair? on) (car on)
                          (not (window-wrapped? current-window))))
    (window-left-set! current-window 0)
    (set! goal-pos #f)              ; the goal column changes meaning
    (set! message (format "Wrap ~a"
                          (if (window-wrapped? current-window) "on" "off")))
    (void))

  (define (resize-window! delta)
    ;; Resize at the nearest enclosing stacked split.
    (let loop ([child current-window])
      (let ([parent (layout-parent layout-root child)])
        (cond
          [(not parent) (set! message "No vertical split")]
          [(eq? (layout-split-orientation parent) 'below)
           (let ([signed (if (eq? child (layout-split-first parent))
                             delta (- delta))])
             (layout-split-first-weight-set!
               parent (max 1 (+ (layout-split-first-weight parent) signed)))
             (layout-split-second-weight-set!
               parent (max 1 (- (layout-split-second-weight parent) signed))))]
          [else (loop parent)]))))

  (define (delete-window!)
    (if (null? (cdr (layout-leaves layout-root)))
        (set! message "Only one window")
        (let* ([next (next-window current-window)]
               [parent (layout-parent layout-root current-window)]
               [sibling (if (eq? current-window (layout-split-first parent))
                            (layout-split-second parent)
                            (layout-split-first parent))])
          (replace-layout-window! parent sibling)
          (focus-window! next))))

  (define (delete-other-windows!)
    (set-layout-root! current-window))

  (define (create-ephemeral-target-window! b)
    ;; Materialize an app target that was removed. Unlike display-buffer!, this
    ;; always creates a new window and never appropriates an unrelated one.
    (split-current-window! 'below b))

  (define (display-buffer! b)
    ;; Show b without leaving the current window: in the window already
    ;; showing it, else the next window, else a fresh split.  The window,
    ;; or #f when the screen has no room for one.
    (unless (memq b buffers) (set! buffers (append buffers (list b))))
    (cond
      [(find (lambda (w) (eq? (window-buffer w) b)) windows)]
      [(pair? (cdr (layout-leaves layout-root)))
       (let ([w (next-window current-window)])
         (set-window-buffer! w b)
         w)]
      [(split-current-window! 'below b)]
      [else #f]))

  (define (pop-up-or-reuse! b)
    ;; Help-like buffers never appropriate another leaf: reuse an existing
    ;; window displaying b, otherwise create a new tile below the current one.
    ;; Focus stays where it was so the popup remains a reference alongside the
    ;; command that requested it.
    (unless (memq b buffers) (set! buffers (append buffers (list b))))
    (or (find (lambda (w) (eq? (window-buffer w) b)) windows)
        (split-current-window! 'below b)))

  (define (buffer-append! b . new-lines)
    ;; Append lines to b, transcript style: a fresh buffer's single empty
    ;; line is replaced, and the display follows -- point moves to the
    ;; last line in every window showing b, and in ones that show it later.
    ;; A transcript belongs in the buffer list even before it is shown.
    (unless (memq b buffers) (set! buffers (append buffers (list b))))
    (let ([v (buffer-lines b)]
          [add (list->vector new-lines)])
      (buffer-lines-set! b
        (if (and (= (vector-length v) 1) (string=? (vector-ref v 0) ""))
            add
            (vector-append v add))))
    (let ([last (- (vector-length (buffer-lines b)) 1)])
      (buffer-spot-row-set! b last)
      (buffer-spot-col-set! b 0)
      (for-each (lambda (w)
                  (when (eq? (window-buffer w) b)
                    (window-prow-set! w last)
                    (window-pcol-set! w 0)))
                windows)))

  ;;; Module registries -------------------------------------------------------

  ;; Everything a module registers with the core -- key bindings, modes,
  ;; highlighters, whatever a future hook adds -- goes through a registry
  ;; and is tagged with the module whose init! is running.  Reloading a
  ;; module retracts its entries wholesale before running its init!
  ;; afresh, so registration is replace-by-module by construction: a new
  ;; hook gets it by using make-registry, with nothing to remember.
  ;; Entries registered outside any module (M-x, say) have owner #f and
  ;; survive reloads.  Lookups prefer newer entries.

  ;; The machinery itself lives in the kernel now; these are the
  ;; facade aliases the core's own call sites keep using until they
  ;; migrate to kernel: prefixes.
  (define registering-module kernel:registering-module)
  (define make-registry kernel:make-registry)
  (define registry-add! kernel:registry-add!)
  (define registry-items kernel:registry-items)
  (define registry-entries kernel:registry-entries)
  (define registry-find kernel:registry-find)
  (define retract-module! kernel:retract-module!)
  (define registration-snapshot kernel:registration-snapshot)
  (define restore-registrations! kernel:restore-registrations!)

  ;; Describe keeps the presentation and record format in its own module;
  ;; the core only owns these opaque batches so normal module retraction also
  ;; removes documentation when a registration disappears on reload.
  (define description-registry (make-registry))

  (define (publish-descriptions! entries)
    (registry-add! description-registry entries))

  (define (published-descriptions)
    (apply append (reverse (registry-items description-registry))))

  ;; Modules may hook the save: pre-save hooks run before anything is
  ;; checked or written (formatting, say), post-save hooks after a
  ;; successful write (the module reload lives there).  Each receives
  ;; the path being written; a raising hook reports and the save goes
  ;; on.
  (define pre-save-hooks (make-registry))
  (define post-save-hooks (make-registry))

  (define (add-pre-save-hook! proc) (registry-add! pre-save-hooks proc))
  (define (add-post-save-hook! proc) (registry-add! post-save-hooks proc))

  (define (run-save-hooks! hooks path)
    (for-each (lambda (p)
                (guard (ex [else (parameterize ([message-source 'save-file!])
                                   (set-message!
                                     (format "Save hook failed: ~a"
                                             (error-text ex))))])
                  (p path)))
              (registry-items hooks)))

  ;; The file commands' formatters: their entries are (verb . path),
  ;; formatted "verb path", their histories the paths (see
  ;; log-history).
  (define log-formatters-init
    (let ([fmt (lambda (d)
                 (if (pair? d)
                     (format "~a ~a" (car d) (cdr d))
                     (format "~a" d)))])
      (log:register-log-formatter! 'visit-file! fmt)
      (log:register-log-formatter! 'save-file! fmt)))

  ;;; Modes -------------------------------------------------------------------

  ;; A mode provides syntax highlighting for the buffers it matches.
  ;; Extension modules call register-mode! with the mode's name,
  ;; the file-name endings it claims, the interpreter names recognized in a
  ;; #! first line (for files without a matching extension), and a styles
  ;; function mapping a line to a vector of per-column style symbols
  ;; understood by style-code, or #f for an unstyled line.  Brackets styled
  ;; 'delimiter take part in bracket matching; in a buffer without a mode
  ;; every bracket counts.

  (define-record-type mode
    (fields name extensions interpreters styles
            ;; optional display transform: (render buffer row line) ->
            ;; a string of the SAME length, or a same-length vector containing
            ;; one display string per logical cell. The latter permits a cell
            ;; to contain a grapheme and represents a wide glyph's continuation
            ;; with "". The buffer text is untouched and columns stay 1:1.
            render
            ;; optional buffer-aware styling: (row-styles buffer row
            ;; line) -> a styles vector, or #f for the plain styles
            ;; function.  Uncached by the core -- the mode memoizes.
            row-styles)
    (protocol (lambda (new)
                (case-lambda
                  [(n e i s) (new n e i s #f #f)]
                  [(n e i s r) (new n e i s r #f)]
                  [(n e i s r rs) (new n e i s r rs)]))))

  (define modes (make-registry))
  (define mode-extension-additions (make-registry))

  (define (register-mode! name extensions interpreters styles . extra)
    ;; extra: an optional render transform, then an optional
    ;; buffer-aware row-styles procedure (see the mode record).
    (registry-add! modes
      (make-mode name extensions interpreters styles
                 (and (pair? extra) (car extra))
                 (and (pair? extra) (pair? (cdr extra)) (cadr extra)))))

  (define (add-mode-extension! name extension)
    ;; Add a suffix to an existing mode without replacing its implementation.
    ;; This is a registry so config-owned additions disappear on config reload.
    (unless (and (string? extension) (> (string-length extension) 1)
                 (char=? (string-ref extension 0) #\.))
      (error 'add-mode-extension! "expected an extension beginning with ."
             extension))
    (unless (find-mode name)
      (error 'add-mode-extension! "no such mode" name))
    (registry-add! mode-extension-additions (cons extension name))
    (for-each (lambda (b) (when (buffer-mode-auto b) (assign-mode! b)))
              buffers)
    (void))

  (define (detect-mode path first-line)
    ;; The mode for a file: by extension, then by the #! interpreter line.
    (or (and path
             (let ([addition
                    (find (lambda (entry)
                            (string-suffix? (car entry) path))
                          (registry-items mode-extension-additions))])
               (and addition (find-mode (cdr addition)))))
        (and path
             (registry-find modes
               (lambda (m)
                 (exists (lambda (ext) (string-suffix? ext path))
                         (mode-extensions m)))))
        (and (string-prefix? "#!" first-line)
             (registry-find modes
               (lambda (m)
                 (exists (lambda (name)
                           (string-search first-line name 0
                                          (string-length first-line)))
                         (mode-interpreters m)))))))

  (define (assign-mode! b)
    (buffer-mode-set! b
      (detect-mode (buffer-file b) (vector-ref (buffer-lines b) 0)))
    (buffer-mode-auto-set! b #t))

  (define (find-mode name)
    (registry-find modes (lambda (m) (string=? (mode-name m) name))))

  (define (set-buffer-mode! b name)
    ;; Give b the registered mode called name (#f for none), regardless of
    ;; its file name -- how transcript buffers get their highlighting.
    (buffer-mode-set! b (and name (find-mode name)))
    (buffer-mode-auto-set! b #f))

  (define (set-buffer-read-only! b flag)
    (buffer-read-only-set! b flag))

  (define (set-buffer-wrap! b setting)
    ;; clean wraps like #t but draws no continuation marks and lets the
    ;; text use the full width -- for formatted read-only presentations;
    ;; (clean . n) additionally caps the wrapping width at n columns.
    (unless (or (memq setting '(default #t #f clean))
                (and (pair? setting) (eq? (car setting) 'clean)
                     (fixnum? (cdr setting)) (>= (cdr setting) 20)))
      (error 'set-buffer-wrap!
             "expected default, #t, #f, clean, or (clean . columns)"
             setting))
    (buffer-wrap-setting-set! b setting)
    b)

  (define (buffer-mode-name b)
    ;; The name of b's mode, or #f without one.
    (let ([m (buffer-mode b)]) (and m (mode-name m))))

  ;;; Indentation and formatting ------------------------------------------------

  ;; Both are provided per mode by modules.  An indenter maps rows to
  ;; where their text should start: (proc buffer from to) -> one entry
  ;; per row of from..to -- #f leaving a row alone (a multi-line
  ;; string's interior, say), a column, or an ascending list of
  ;; columns when several indentations are valid (its stops) --
  ;; computed as if each row settles on the stop nearest its current
  ;; indentation, top to bottom.  The commands settle likewise; TAB
  ;; instead cycles: the nearest stop to the right, wrapping around.
  ;; A formatter rewrites rows wholesale: (proc buffer from to) -> the
  ;; replacement lines, or #f when the rows cannot be formatted.  TAB
  ;; indents the current line when the mode registered its indenter
  ;; with the tab flag on (the default).
  (define indenters (make-registry))   ; entries (mode-name proc tab?)
  (define formatters (make-registry))  ; entries (mode-name proc)

  (define (register-indenter! name proc . tab)
    (registry-add! indenters (list name proc (or (null? tab) (car tab)))))

  (define (register-formatter! name proc)
    (registry-add! formatters (list name proc)))

  (define (mode-entry registry)
    (let ([m (buffer-mode-name (window-buffer current-window))])
      (and m (registry-find registry (lambda (x) (string=? (car x) m))))))

  (define (leading-blanks s)
    (let loop ([i 0])
      (if (and (< i (string-length s))
               (memv (string-ref s i) '(#\space #\tab)))
          (loop (+ i 1))
          i)))

  (define (settle-stops col cur)
    ;; An indenter entry resolved for a line currently at cur: the
    ;; nearest stop (ties leftward); a bare column stands.
    (if (pair? col)
        (fold-left (lambda (best s)
                     (if (< (abs (- s cur)) (abs (- best cur))) s best))
                   (car col) col)
        col))

  (define (cycle-stops col cur)
    ;; TAB's resolution: the nearest stop right of cur, wrapping back
    ;; to the first past the last.
    (if (pair? col)
        (or (find (lambda (s) (> s cur)) col) (car col))
        col))

  (define (apply-indent! from cols pad?)
    ;; Rewrite the leading whitespace of rows from.. to the given
    ;; columns (#f leaves a row, as does a whitespace-only row --
    ;; except with pad?, which pads it out to the column: TAB on a
    ;; blank line).  One undo entry; point and mark follow their
    ;; line's text, landing on the indentation when they sat inside
    ;; the old one.  -> whether anything changed.
    (define b (window-buffer current-window))
    (define v (buffer-lines b))
    (define n (vector-length v))
    (define (retabbed s col)
      (let ([rest (string-tail s (leading-blanks s))])
        (if (string=? rest "")
            (if pad? (make-string col #\space) s)
            (string-append (make-string col #\space) rest))))
    (let ([changes
           (let loop ([r from] [cs cols] [acc '()])
             (if (or (null? cs) (>= r n))
                 (reverse acc)
                 (loop (+ r 1) (cdr cs)
                       (if (and (car cs)
                                (not (string=? (retabbed (vector-ref v r)
                                                         (car cs))
                                               (vector-ref v r))))
                           (cons (cons r (car cs)) acc)
                           acc))))])
      (when (pair? changes)
        (record-edit! "indent")
        (let ([nv (let ([o (make-vector n)])
                    (do ([i 0 (+ i 1)]) ((= i n) o)
                      (vector-set! o i (vector-ref v i))))])
          (for-each
            (lambda (change)
              (let* ([row (car change)] [col (cdr change)]
                     [old (vector-ref v row)]
                     [lead (leading-blanks old)]
                     [follow (lambda (c)
                               (if (<= c lead) col (+ c (- col lead))))])
                (vector-set! nv row (retabbed old col))
                (when (= row point-row)
                  (set! point-col (follow point-col)))
                (when (and mark-active? (= row mark-row))
                  (set! mark-col (follow mark-col)))))
            changes)
          (buffer-lines-set! b nv))
        (changed!))
      (pair? changes)))

  (define (indent-rows! from to)
    ;; Indent rows [from, to] by the mode's indenter, each settling on
    ;; the stop nearest its current indentation; -> #f without one.
    (let ([entry (mode-entry indenters)])
      (if (not entry)
          (begin (set! message "No indenter for this mode") #f)
          (let* ([b (window-buffer current-window)]
                 [v (buffer-lines b)]
                 [last (min to (- (vector-length v) 1))]
                 [cols (let settle ([r from]
                                    [cs ((cadr entry) b from last)]
                                    [acc '()])
                         (if (null? cs)
                             (reverse acc)
                             (settle (+ r 1) (cdr cs)
                                     (cons (settle-stops
                                             (car cs)
                                             (leading-blanks
                                               (vector-ref v r)))
                                           acc))))])
            (apply-indent! from cols #f)
            #t))))

  (define (indent-line!)
    ;; TAB's work: indent the current line, cycling through its stops
    ;; -- the nearest stop right of the current indentation, wrapping
    ;; -- and land on the indentation (a blank line pads out to it);
    ;; point already past it stays with its text.
    (let ([entry (mode-entry indenters)])
      (if (not entry)
          (set! message "No indenter for this mode")
          (let* ([b (window-buffer current-window)]
                 [cols ((cadr entry) b point-row point-row)]
                 [col (and (pair? cols)
                           (cycle-stops (car cols)
                                        (leading-blanks
                                          (line-at point-row))))])
            (when col
              (apply-indent! point-row (list col) #t)
              (when (< point-col col) (set! point-col col))))))
    (void))

  (define (indent-tab!)
    ;; TAB: the mode indents when it asked to; otherwise nothing.
    (let ([entry (mode-entry indenters)])
      (when (and entry (caddr entry))
        (indent-line!))))

  (define (indent-on-tab! name flag)
    ;; Configuration: whether TAB auto-indents in the named mode,
    ;; overriding the flag its indenter registered with.
    (let ([entry (registry-find indenters
                                (lambda (x) (string=? (car x) name)))])
      (unless entry (error 'indent-on-tab! "no indenter for mode" name))
      (registry-add! indenters (list name (cadr entry) flag))))

  (define (indent-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let ([from (min mark-row point-row)]
              [to (max mark-row point-row)])
          (when (indent-rows! from to)
            (set! message (format "Indented ~a line~a" (+ (- to from) 1)
                                  (if (= from to) "" "s"))))))
    (void))

  (define (indent-buffer!)
    (let ([n (vector-length (buffer-lines (window-buffer current-window)))])
      (when (indent-rows! 0 (- n 1))
        (set! message (format "Indented ~a lines" n))))
    (void))

  (define (replace-rows! from to lines)
    ;; Replace rows [from, to] of the current buffer with lines (a
    ;; list), one undo entry; point keeps its row when it can.
    (define b (window-buffer current-window))
    (define v (buffer-lines b))
    (define n (vector-length v))
    (let ([nv (list->vector
                (let loop ([r 0] [acc '()])
                  (cond [(= r from)
                         (append (reverse acc) lines
                                 (let tail ([r (+ to 1)] [acc '()])
                                   (if (>= r n)
                                       (reverse acc)
                                       (tail (+ r 1)
                                             (cons (vector-ref v r) acc)))))]
                        [else (loop (+ r 1)
                                    (cons (vector-ref v r) acc))])))])
      (record-edit! "format")
      (buffer-lines-set! b (if (zero? (vector-length nv)) (vector "") nv))
      (set! point-row (max 0 (min point-row
                                  (- (vector-length (buffer-lines b)) 1))))
      (changed!)))

  (define (format-rows! from to)
    ;; Format rows [from, to] by the mode's formatter; -> whether the
    ;; buffer changed.
    (let ([entry (mode-entry formatters)])
      (cond
        [(not entry) (set! message "No formatter for this mode") #f]
        [else
         (let* ([b (window-buffer current-window)]
                [v (buffer-lines b)]
                [last (min to (- (vector-length v) 1))]
                [lines ((cadr entry) b from last)])
           (cond
             [(not lines) (set! message "Cannot format these lines") #f]
             [(let same ([r from] [ls lines])
                (if (null? ls)
                    (> r last)
                    (and (<= r last)
                         (string=? (car ls) (vector-ref v r))
                         (same (+ r 1) (cdr ls)))))
              (set! message "Already formatted") #f]
             [else
              (replace-rows! from last lines)
              ;; formatted through the end: the file ends with exactly
              ;; one newline
              (when (= last (- (vector-length v) 1))
                (set! trailing-newline? #t))
              #t]))])))

  (define (format-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let ([from (min mark-row point-row)]
              [to (max mark-row point-row)])
          (when (format-rows! from to)
            (set! message "Formatted region"))))
    (void))

  (define (format-buffer!)
    (let ([n (vector-length (buffer-lines (window-buffer current-window)))])
      (when (format-rows! 0 (- n 1))
        (set! message (format "Formatted ~a lines" n))))
    (void))

  (define (no-styles s) #f)

  ;; Computed styles, memoized per line string.  Edits replace line
  ;; strings (never mutate them), so string identity keys the cache and
  ;; can never go stale; weak keys keep it bounded by the live lines.
  ;; Each entry remembers its mode, in case an identical string is shared
  ;; between buffers of different modes.
  (define style-cache (make-weak-eq-hashtable))

  (define (buffer-line-styles b)
    ;; The line-styles function of b's mode; unstyled without one.
    (let ([m (buffer-mode b)])
      (if m
          (lambda (s)
            (let ([hit (eq-hashtable-ref style-cache s #f)])
              (if (and hit (eq? (car hit) m))
                  (cdr hit)
                  ;; a raising mode styles the line plain rather than
                  ;; taking the redraw (and the editor) down
                  (let ([styles (guard (ex [else #f])
                                  ((mode-styles m) s))])
                    (eq-hashtable-set! style-cache s (cons m styles))
                    styles))))
          no-styles)))

  ;; Faces may be recolored from config.e. Overrides are owned registrations,
  ;; so dropping the line from config.e and reloading restores the default.
  ;; Faces and the style DSL live in the (styles) seam module now;
  ;; the core keeps these facade aliases until its call sites and the
  ;; extension modules migrate to styles: prefixes, and installs the
  ;; repaint trigger for face redefinitions (painted rows are cached
  ;; by content, not by face definitions).
  (define compile-style styles:compile-style)
  (define style-escape styles:style-escape)
  (define set-style! styles:set-style!)
  (define style-code styles:style-code)

  (define styles-hook-installed
    (styles:set-styles-changed-hook!
      (lambda () (invalidate-screen-cache!))))

  ;;; Rendering -------------------------------------------------------------

  ;; The row painter lives in the (paint) seam module now: given a
  ;; line, its styles, marks, links, and selection, it emits minimal
  ;; styled runs.  Frame composition (layout, scrolling, the screen
  ;; cache, redraw!) stays here until head.e.  Facade aliases until
  ;; call sites migrate to paint: prefixes.
  (define ansi paint:ansi)
  (define goto paint:goto)
  (define fit paint:fit)
  (define display-editor-line paint:display-editor-line)
  (define emit-runs paint:emit-runs)
  (define detect-hyperlinks paint:detect-hyperlinks)
  (define valid-hyperlink? paint:valid-hyperlink?)
  (define compute-breaks paint:compute-breaks)

  (define (region-span row line-length)
    ;; The columns of `row` inside the active region, as (start . end), or #f.
    (and mark-active?
         (let-values ([(sr sc er ec) (ordered-region)])
           (cond [(or (< row sr) (> row er)) #f]
                 [(= sr er) (cons sc ec)]
                 [(= row sr) (cons sc line-length)]
                 [(= row er) (cons 0 ec)]
                 [else (cons 0 line-length)]))))

  ;; Context highlighting is provided by modules: a highlighter, registered
  ;; with add-highlighter!, is called at every redraw and returns ranges
  ;; of the current buffer to mark up -- a list of (row start end) or
  ;; (row start end style) entries, drawn in the current window on top
  ;; of the syntax styles. A scoped (buffer row start end style) or
  ;; (window row start end style) entry may decorate inactive buffers or one
  ;; particular window. Styles: mark (the default) underlines --
  ;; the paren module matches brackets this way -- while match and
  ;; match-point are the search's cyan and yellow backgrounds, and active is
  ;; the selected row in an app. A
  ;; broken highlighter is ignored for that redraw rather than taking
  ;; the editor down.
  ;; Modules may add a status hint: a thunk returning a short string
  ;; (or #f) appended to the current window's status line -- the
  ;; pretty-parens mode shows the source paren under point this way.
  (define status-hints (make-registry))
  (define buffer-status-hints (make-registry))

  (define (add-status-hint! proc)
    (registry-add! status-hints proc))

  (define (add-buffer-status-hint! proc)
    ;; Unlike a conventional status hint, this is evaluated for every painted
    ;; window as (proc buffer active?) and can therefore describe passive
    ;; windows too.
    (registry-add! buffer-status-hints proc))

  (define (status-hint-values b active?)
    (let loop ([procs (append (if active? (registry-items status-hints) '())
                              (registry-items buffer-status-hints))]
               [ordinary (and active? (length (registry-items status-hints)))]
               [out '()])
      (if (null? procs)
          (reverse out)
          (let ([value
                 (guard (ex [else #f])
                   (let ([v (if (and ordinary (> ordinary 0))
                                ((car procs))
                                ((car procs) b active?))])
                     (cond
                       [(string? v) (list (cons v #f))]
                       [(and (pair? v) (string? (car v))) (list v)]
                       [(and (list? v)
                             (for-all (lambda (span)
                                        (and (pair? span)
                                             (string? (car span))))
                                      v))
                        v]
                       [else #f])))])
            (loop (cdr procs)
                  (and ordinary (> ordinary 1) (- ordinary 1))
                  (if value (append (reverse value) out) out))))))

  (define highlighters (make-registry))

  (define (add-highlighter! proc)
    (registry-add! highlighters proc))

  (define (highlight-ranges)
    (fold-left (lambda (acc h) (append (guard (ex [else '()]) (h)) acc))
               '() (registry-items highlighters)))

  ;; Hyperlinkers produce (start end URI [id]) ranges for one buffer line.
  ;; They are deliberately separate from visual highlighters: links carry a
  ;; payload, participate in hit testing, and are also exposed to an upstream
  ;; terminal through OSC 8. Newer providers take precedence on overlap.
  (define hyperlinkers (make-registry))

  (define (add-hyperlinker! proc)
    (registry-add! hyperlinkers proc))

  (define (buffer-line-hyperlinks buffer row)
    (let ([line (buffer-line buffer row)])
      (fold-left
        (lambda (links proc)
          (append
            (filter (lambda (link)
                      (valid-hyperlink? link (string-length line)))
                    (guard (ex [else '()]) (proc buffer row line)))
            links))
        (detect-hyperlinks line) (registry-items hyperlinkers))))

  (define (ranges-on-row ranges w b row current?)
    (fold-left (lambda (acc r)
                 (let* ([buffer-scoped? (and (pair? r) (buffer? (car r)))]
                        [window-scoped? (and (pair? r) (window? (car r)))]
                        [scoped? (or buffer-scoped? window-scoped?)]
                        [range (if scoped? (cdr r) r)])
                   (if (and (or (and buffer-scoped? (eq? (car r) b))
                                (and window-scoped? (eq? (car r) w))
                                (and (not scoped?) current?))
                            (= (car range) row))
                       (cons (cdr range) acc)
                       acc)))
               '() ranges))


  (define (window-layout)
    ;; Recursively tile the persistent split tree.
    ;; -> list of (window start text-height), start 0-based.
    (set! layout-dividers '())
    (layout-node! layout-root 0 0 cols (max 2 (- rows echo-height))))

  (define (page-size)
    ;; The scrollable body height. Sticky app rows are fixed chrome and do not
    ;; form part of a page.
    (let ([height (caddr (assq current-window (window-layout)))])
      (max 1 (- height
                (min height
                     (buffer-sticky-lines (current-buffer)))))))

  ;; Soft wrap breaks at word boundaries: each line has a break table
  ;; -- the start position of every visual segment -- computed
  ;; greedily (the last space that fits; a word longer than the width
  ;; breaks mid-word) and memoized per line string and width, like the
  ;; style cache: edits replace line strings, so identity keys it.
  (define wrap-cache (make-weak-eq-hashtable))

  (define (line-breaks w line)
    ;; The break table for line in w: a vector of segment starts.
    (let* ([width (wrap-width w)]
           [hit (eq-hashtable-ref wrap-cache line '())]
           [found (assv width hit)])
      (if found
          (cdr found)
          (let ([breaks (compute-breaks line width)])
            (eq-hashtable-set! wrap-cache line
                               (cons (cons width breaks) hit))
            breaks))))

  (define (segment-of breaks col)
    ;; The segment holding column col.
    (let loop ([k (- (vector-length breaks) 1)])
      (if (or (= k 0) (>= col (vector-ref breaks k)))
          k
          (loop (- k 1)))))

  (define (segment-start breaks k) (vector-ref breaks k))

  (define (segment-close breaks k len)
    ;; The last column the cursor may occupy within segment k.
    (if (< (+ k 1) (vector-length breaks))
        (- (vector-ref breaks (+ k 1)) 1)
        len))

  (define (line-segments w line)
    ;; How many screen rows the line takes in w: 1, or its soft-wrapped
    ;; segment count.
    (if (window-wrapped? w)
        (vector-length (line-breaks w line))
        1))

  (define (page-window! direction fraction)
    ;; Pagination is a viewport operation. Shift its top by the requested
    ;; fraction of the body height in visual rows, clamp at either end, then
    ;; put point in the middle.
    ;; A second outward page at an already-clamped edge moves point to that
    ;; edge. Wrapped segments count as rows; the visual column is preserved.
    (let* ([w current-window]
           [v (buffer-lines (current-buffer))]
           [n (vector-length v)]
           [sticky (min (buffer-sticky-lines (current-buffer)) (- n 1))]
           [height (page-size)]
           [wrapped? (window-wrapped? w)]
           [visual-col (if wrapped?
                           (let* ([line (vector-ref v point-row)]
                                  [breaks (line-breaks w line)])
                             (- point-col
                                (segment-start breaks
                                  (segment-of breaks point-col))))
                           point-col)])
      (define (offset-at target segment)
        (let loop ([row sticky] [offset 0])
          (if (>= row target)
              (+ offset segment)
              (loop (+ row 1)
                    (+ offset (line-segments w (vector-ref v row)))))))
      (define (position-at offset)
        (let loop ([row sticky] [left offset])
          (let ([segments (line-segments w (vector-ref v row))])
            (if (or (= row (- n 1)) (< left segments))
                (cons row (min left (- segments 1)))
                (loop (+ row 1) (- left segments))))))
      (define (column-at position)
        (let* ([row (car position)]
               [line (vector-ref v row)])
          (if wrapped?
              (let ([breaks (line-breaks w line)] [seg (cdr position)])
                (min (+ (segment-start breaks seg) visual-col)
                     (segment-close breaks seg (string-length line))))
              (min visual-col (string-length line)))))
      (define (land! top-offset point-offset)
        (let ([top (position-at top-offset)]
              [point (position-at point-offset)])
          (goto-point! (cons (car point) (column-at point)))
          (window-top-set! w (car top))
          (window-topseg-set! w (cdr top))))
      (let* ([total (max 1 (offset-at n 0))]
             [last-top (max 0 (- total height))]
             [old-top (min last-top
                           (max 0 (offset-at (window-top w)
                                             (window-topseg w))))]
             [up? (negative? direction)]
             [step (max 1 (quotient height fraction))]
             [at-edge? (= old-top (if up? 0 last-top))]
             [top (cond [(<= total height) 0]
                        [up? (max 0 (- old-top step))]
                        [else (min last-top (+ old-top step))])]
             [middle (+ top (quotient (- height 1) 2))]
             [point (cond [(<= total height) (if up? 0 (- total 1))]
                          [at-edge? (if up? 0 (- total 1))]
                          [else middle])])
        (land! top point))))

  (define (page-window-fraction! direction fraction)
    (page-window! direction fraction))

  (define (set-point-without-scroll! position)
    (let* ([v (buffer-lines (current-buffer))]
           [row (max 0 (min (car position) (- (vector-length v) 1)))])
      (window-prow-set! current-window row)
      (window-pcol-set! current-window
                        (max 0 (min (cdr position)
                                    (string-length (vector-ref v row)))))))

  (define (set-buffer-viewports! b position top excluded-windows)
    (let* ([v (buffer-lines b)]
           [row (max 0 (min (car position) (- (vector-length v) 1)))]
           [col (max 0 (min (cdr position)
                            (string-length (vector-ref v row))))]
           [top (max 0 (min top (- (vector-length v) 1)))])
      (buffer-spot-row-set! b row)
      (buffer-spot-col-set! b col)
      (buffer-spot-top-set! b top)
      (for-each
        (lambda (w)
          (when (and (eq? (window-buffer w) b)
                     (not (memq w excluded-windows)))
            (window-top-set! w top)
            (window-topseg-set! w 0)
            (window-left-set! w 0)
            (window-prow-set! w row)
            (window-pcol-set! w col)))
        windows)
      b))

  (define (reset-buffer-viewports! b position)
    (set-buffer-viewports! b position 0 '()))

  (define (view-invalidate! b)
    ;; Dynamic row renderers can change their presentation while the view's
    ;; structural placeholder lines remain equal. Mark the display stale
    ;; explicitly so the next frame asks the renderer for every visible row.
    (unless (app-buffer? b)
      (error 'view-invalidate! "not an app or view buffer" b))
    (invalidate-screen-cache!)
    b)

  (define (point-visible?)
    (let* ([entry (assq current-window (window-layout))]
           [p (window-screen-position current-window point-row point-col)])
      (and entry (< (cadr entry) (car p))
           (<= (car p) (+ (cadr entry) (caddr entry))))))

  (define (rows-before w prow pcol)
    ;; Screen rows between w's top -- its first visible segment -- and
    ;; point, wrap-aware.
    (let* ([v (buffer-lines (window-buffer w))]
           [sticky (buffer-sticky-lines (window-buffer w))])
      (let loop ([i (max sticky (window-top w))]
                 [n (- (window-topseg w))])
        (if (>= i prow)
            (+ n (if (window-wrapped? w)
                     (segment-of (line-breaks w (vector-ref v prow)) pcol)
                     0))
            (loop (+ i 1) (+ n (line-segments w (vector-ref v i))))))))

  ;; The minimal visual distance kept between the cursor and the
  ;; window's top and bottom edges: scrolling starts that early, and
  ;; the cursor enters the zone only where the view cannot scroll any
  ;; further (the ends of the buffer).  Configurable in config.e.
  (define scroll-margin
    (make-parameter 8 (lambda (v) (max 0 v))))

  (define (view-overflows? w v height)
    ;; Is there more content than the window holds, counting from its
    ;; top segment?
    (let loop ([i (max (buffer-sticky-lines (window-buffer w))
                       (window-top w))]
               [n (- (window-topseg w))])
      (cond [(> n height) #t]
            [(>= i (vector-length v)) #f]
            [else (loop (+ i 1)
                        (+ n (line-segments w (vector-ref v i))))])))

  (define (scroll-window! w height)
    ;; Clamp w's point to its buffer (edits in another window may have moved
    ;; the ground under it) and scroll so point stays visible -- at
    ;; least scroll-margin rows from the edges, where the buffer's
    ;; ends allow.
    (let* ([v (buffer-lines (window-buffer w))]
           [sticky (buffer-sticky-lines (window-buffer w))]
           [height (max 1 (- height sticky))]
           [prow (max 0 (min (window-prow w) (- (vector-length v) 1)))]
           [pcol (max 0 (min (window-pcol w)
                             (string-length (vector-ref v prow))))]
           [m (min (scroll-margin) (div (max 0 (- height 1)) 2))])
      (window-prow-set! w prow)
      (window-pcol-set! w pcol)
      (unless (or (not (app-cursor-visible-in? w))
                  (app-manages-window-viewport? w))
        (if (window-wrapped? w)
          (let ([pseg (segment-of (line-breaks w (vector-ref v prow))
                                  pcol)])
            ;; a stale top (edits, toggles) clamps into the buffer
            (window-top-set! w
              (max sticky
                   (min (window-top w) (- (vector-length v) 1))))
            (window-topseg-set!
              w (min (window-topseg w)
                     (- (line-segments w (vector-ref v (window-top w)))
                        1)))
            ;; point above the view: its own segment row becomes the
            ;; top, so moving up scrolls by one visual row, not by the
            ;; whole wrapped line
            (when (and (>= prow sticky)
                       (or (< prow (window-top w))
                         (and (= prow (window-top w))
                           (< pseg (window-topseg w)))))
              (window-top-set! w prow)
              (window-topseg-set! w pseg))
            ;; the margin above: retreat while the top of the buffer
            ;; still allows
            (let retreat ()
              (when (and (< (rows-before w prow pcol) m)
                         (or (> (window-top w) sticky)
                             (> (window-topseg w) 0)))
                (if (> (window-topseg w) 0)
                    (window-topseg-set! w (- (window-topseg w) 1))
                    (begin
                      (window-top-set! w (- (window-top w) 1))
                      (window-topseg-set!
                        w (- (line-segments
                               w (vector-ref v (window-top w)))
                             1))))
                (retreat)))
            ;; and below: advance one visual row at a time, only while
            ;; content actually overflows the window.  Each step reduces
            ;; distance by exactly one; carrying it avoids rescanning from
            ;; top to point at every step on a large jump.
            (let advance ([distance (rows-before w prow pcol)])
              (when (and (>= distance (- height m))
                         (or (< (window-top w) prow)
                             (< (window-topseg w) pseg))
                         (view-overflows? w v height))
                (if (< (+ (window-topseg w) 1)
                       (line-segments w (vector-ref v (window-top w))))
                    (window-topseg-set! w (+ (window-topseg w) 1))
                    (begin (window-top-set! w (+ (window-top w) 1))
                           (window-topseg-set! w 0)))
                (advance (- distance 1)))))
          (begin
            (window-topseg-set! w 0)
            (when (< prow (+ (window-top w) m))
              (window-top-set! w (max sticky (- prow m))))
            (when (>= prow (+ (window-top w) height (- m)))
              (window-top-set! w
                (min (- prow (- height 1 m))
                     (max sticky (- (vector-length v) height)))))
            (when (< pcol (window-left w)) (window-left-set! w pcol))
            (when (>= pcol (+ (window-left w) (window-content-width w)))
              (window-left-set! w
                (- pcol (window-content-width w) -1))))))))

  ;; The cache holds, per screen row, the key describing what that row
  ;; currently shows; a row is repainted only when its key changes.  Any
  ;; change of view (size, search highlight, window arrangement) discards
  ;; the whole cache.
  (define screen-cache '#())
  (define screen-live? #f) ; the terminal is ours only between main's
                           ; alternate-screen enter and exit
  (define cached-view #f)
  (define cursor-style-shown "\x1b;[0 q")   ; DECSCUSR last emitted

  (define (invalidate-screen-cache!) (set! cached-view #f))

  (define (erase-screen!)
    ;; Blank the terminal and schedule the full repaint -- an actual
    ;; erase, which also clears the terminal's own selection highlight
    ;; where an identical overwrite would not.
    (ansi "\x1b;[2J")
    (invalidate-screen-cache!))

  (define (shift-screen-cache! delta start height)
    ;; Mirror a native delta-row terminal scroll in cache rows
    ;; [start, start+height); rows the scroll uncovered become #f.
    (let ([end (+ start height)])
      (if (> delta 0)
          (let loop ([i start])
            (when (< i end)
              (vector-set! screen-cache i
                (and (< (+ i delta) end) (vector-ref screen-cache (+ i delta))))
              (loop (+ i 1))))
          (let loop ([i (- end 1)])
            (when (>= i start)
              (vector-set! screen-cache i
                (and (>= (+ i delta) start) (vector-ref screen-cache (+ i delta))))
              (loop (- i 1)))))))

  (define (native-scroll! w start height)
    ;; When w's top moved since it was last drawn, and mostly the same lines
    ;; remain visible, let the terminal shift them: restrict the scrolling
    ;; region to this window's text rows, scroll, and mirror it in the cache.
    ;; The rows the scroll uncovered then repaint through the usual path.
    ;; In a wrapped window the shift counts visual rows -- the segments
    ;; of the lines that crossed the top.  (An edit alongside the
    ;; scroll can make either count stale; the row keys then miss and
    ;; those rows repaint, so the shift is only ever an economy.)
    (define (rows-between from to)
      ;; visual rows spanned by lines [from, to); #f out of range
      (let ([v (buffer-lines (window-buffer w))])
        (and (<= 0 from) (<= to (vector-length v))
             (let loop ([i from] [n 0])
               (if (>= i to)
                   n
                   (loop (+ i 1)
                         (+ n (line-segments w (vector-ref v i)))))))))
    (let* ([shown (window-shown-top w)]
           [t (window-top w)]
           [ts (window-topseg w)]
           [vdelta (and (pair? shown)
                        (let ([s (car shown)] [ss (cdr shown)])
                          (if (window-wrapped? w)
                              (let ([d (if (>= t s)
                                           (rows-between s t)
                                           (let ([n (rows-between t s)])
                                             (and n (- n))))])
                                (and d (+ d (- ts ss))))
                              (- t s))))])
      ;; Worth it only while most rows survive the shift: a page-sized
      ;; scroll visibly flings the window's content before overwriting
      ;; nearly all of it anyway, where an in-place repaint sits still.
      ;; Only a full-width window scrolls natively: a side-by-side
      ;; column would need VT420 left/right margins, which too few
      ;; terminals support -- it repaints in place instead, flicker
      ;; suppressed by the synchronized update (mode 2026) around the
      ;; frame.
      (when (and vdelta (not (= vdelta 0)) (<= (* 2 (abs vdelta)) height)
                 (= (window-width w) cols))
        (ansi "\x1b;[?25l"
              "\x1b;[" (number->string (+ start 1)) ";"
              (number->string (+ start height)) "r"
              (format "\x1b;[~a~a" (abs vdelta) (if (> vdelta 0) "S" "T"))
              "\x1b;[r")
        (shift-screen-cache! vdelta start height))))

  (define (paint-dividers! layout)
    ;; Paint vertical boundaries from the same recursive geometry used for
    ;; hit testing. Stacked boundaries are the upper leaves' status bars. At
    ;; an intersection, a spanning stacked split owns the cell and connects
    ;; its thin horizontal stroke to the divider above with a light `┴`;
    ;; otherwise the vertical split continues through as a thin stroke.
    (define (stacked-divider-crosses? x row)
      (exists (lambda (d)
                (and (eq? (car d) 'below)
                     (= row (cadddr d))
                     (<= (caddr d) x)
                     (< x (+ (caddr d) (list-ref d 4)))))
              layout-dividers))
    (for-each
      (lambda (divider)
        (when (eq? (car divider) 'right)
          (let ([x (caddr divider)] [start (cadddr divider)]
                [height (list-ref divider 4)])
            (do ([r start (+ r 1)]) ((>= r (+ start height)))
              ;; The final row always meets whatever full-width region
              ;; ends the divider -- the echo area, or a transient
              ;; pop-up such as completions -- and connects to it.
              (let ([junction? (or (stacked-divider-crosses? x r)
                                   (= r (+ start height -1)))])
                (paint! r x (list 'divider junction?)
                        (lambda ()
                          (if junction?
                              (ansi (style-code 'chrome)
                                    "\x2534;\x1b;[0m")
                              (ansi (style-code 'chrome)
                                    "\x2502;\x1b;[0m")))))))))
      layout-dividers))

  (define (paint! row xoff key draw)
    ;; Repaint the segment of the 0-based screen row starting at
    ;; column xoff unless it already shows key; a row shared by
    ;; side-by-side windows caches one key per segment.
    (let* ([entry (vector-ref screen-cache row)]
           [hit (and (pair? entry) (assv xoff entry))])
      (unless (and hit (equal? (cdr hit) key))
        (ansi "\x1b;[?25l") (goto (+ row 1) (+ xoff 1))
        (draw)
        (vector-set! screen-cache row
          (cons (cons xoff key)
                (if hit (remq hit entry) (or entry '())))))))

  (define (echo-indent-now) (echo:indent-now cols))

  (define (compute-echo-spans content len)
    (echo:compute-spans content len cols))

  (define (echo-position k)
    ;; Visual (line . column) of content index k, per echo-spans.
    (let loop ([spans echo-spans] [line 0])
      (let ([span (car spans)])
        (if (or (null? (cdr spans)) (< k (cdr span))
                (and (= k (cdr span)) (< k (string-length message))
                     (char=? (string-ref message k) #\newline)))
            (cons line (+ (if (= line 0) 0 (echo-indent-now))
                          (- k (car span))))
            (loop (cdr spans) (+ line 1))))))

  ;; Parameterized on (by eval, around an evaluation), the cursor parks
  ;; at the end of the echo area's content -- and is drawn as a blinking
  ;; underline, so a running evaluation is visible at a glance.
  (define cursor-in-echo (make-parameter #f))

  ;; Prompts may parameterize this to style the echo content -- M-x
  ;; gives the expression Scheme highlighting.  A procedure from the
  ;; content string to a styles vector (as modes produce), or #f to
  ;; style nothing; a raising styler paints plain.
  (define echo-highlight (make-parameter #f))

  (define (prompt-styler label input-styler)
    ;; Lift a styler for the editable input into one for the complete echo
    ;; content. The prompt label and any note stay grey; only the input is
    ;; delegated. Shared by file, symbol, and expression prompts.
    (let ([llen (string-length label)])
      (lambda (content)
        (and (string-prefix? label content)
             (let* ([styles (make-vector (string-length content) 'comment)]
                    [end (min (or echo-input-end (string-length content))
                              (string-length content))]
                    [input (substring content (min llen end) end)]
                    [inner (input-styler input)])
               (and inner
                    (begin
                      (let loop ([i llen])
                        (when (< i end)
                          (vector-set! styles i (vector-ref inner (- i llen)))
                          (loop (+ i 1))))
                      styles)))))))

  (define (completion-styler match? highlight?)
    ;; Style one completion input by its semantic state: an incomplete or
    ;; unknown value is italic, an exact match is plain, and a distinguished
    ;; match (an editor symbol, for example) uses the editor face.
    (lambda (input)
      (make-vector (string-length input)
                   (cond [(highlight? input) 'editor]
                         [(match? input) 'plain]
                         [else 'italic]))))

  (define (echo-cursor-now)
    (or echo-cursor
        (and (cursor-in-echo)
             (+ (string-length message) (string-length message-ghost)))
        (and capture-bypass-app
             (eq? (app-of (current-buffer)) capture-bypass-app)
             (string-length message))))

                              ; applied while the text still matches

  (define (show-message! s styles-pair)
    ;; Put s in the echo area and paint right away (once the screen is
    ;; the editor's).
    (set! echo-indent #f)
    (set! echo-input-end #f)
    (set! message s)
    (set! message-ghost "")
    (set! message-styles styles-pair)
    (present-echo!))

  (define (show-prompt-message! label input styler)
    ;; Preserve a completed prompt's exact layout and styling while its
    ;; command runs.  In particular, hard-newline continuations retain the
    ;; prompt indentation instead of becoming an unrelated plain message.
    (let ([content (string-append label input)])
      (set! echo-indent (string-length label))
      (set! echo-input-end (string-length content))
      (set! message content)
      (set! message-ghost "")
      (set! message-styles (and styler (cons content styler)))
      (present-echo!)))

  (define (echo-append! component text styler replace?)
    ;; Append one line to the echo area's transient log: every logged
    ;; message stacks up there, component-prefixed, until the next key
    ;; settles the area.  With replace? true the component's newest
    ;; line is superseded when it is also the newest overall --
    ;; progress redrawn in place -- never another component's.  A
    ;; stale indicator gives way; a prompt's input line stays put
    ;; below, and so does a running evaluation's kept query -- the
    ;; user sees what is running.
    (echo-queue! component text styler replace?)
    (present-echo!))

  (define (echo-queue! component text styler replace? . rest)
    ;; Update transient echo state without painting it; batch publishers use
    ;; this before one final present-echo!.
    (echo:queue! component text styler replace?
                 (if (pair? rest) (car rest) "")
                 (and (echo-cursor-now) #t)))

  (define (present-echo!)
    ;; Present the echo area now, mid-command included (once the
    ;; screen is the editor's).  Grown or shrunk it takes a full
    ;; redraw -- the windows above shift, their status bars with them
    ;; -- otherwise painting the area suffices.
    (when screen-live?
      (let ([h echo-height])
        (update-echo-geometry!)
        (if (= h echo-height)
            (paint-echo-area!)
            (redraw!)))
      (flush-output-port (terminal-output-port))))

  (define (echo-log-prefix e) (echo:log-prefix e cols))
  (define (echo-log-spans prefix-len content)
    (echo:log-spans prefix-len content cols))
  (define (echo-log-rows e) (echo:log-rows e cols))

  (define (display-echo-log-row prefix text styler ghost k span wrapped?)
    ;; One visual row of a transient-log entry: the grey prefix on the
    ;; first, its indent on continuations, the slice under the
    ;; component's styler, a mark closing every wrapped row.
    (let* ([lead (if (= k 0)
                     prefix
                     (make-string (min (string-length prefix)
                                       (quotient cols 2))
                                  #\space))]
           [start (car span)]
           [end (cdr span)]
           [styles (and styler (guard (ex [else #f]) (styler text)))]
           [text-end (min end (string-length text))]
           [ghost-start (max start (string-length text))]
           [content (string-append text ghost)])
      (ansi "\x1b;[0m" (style-code 'chrome) lead)
      (when (< start text-end)
        (if styles
            (emit-runs text styles start text-end)
            (ansi "\x1b;[0m" (substring text start text-end))))
      (when (< ghost-start end)
        (ansi "\x1b;[0m" (style-code 'chrome)
              (substring content ghost-start end)))
      (ansi "\x1b;[0m"
            (make-string (max 0 (- cols (string-length lead) (- end start)
                                   (if wrapped? 1 0)))
                         #\space)
            (if wrapped? "\\" ""))))

  (define (paint-echo-area!)
    ;; Paint the pending transient-log lines, then the visible
    ;; (wrapped) live line under them.  Recompute the geometry first:
    ;; set-message! and echo-append! come here directly, with the
    ;; content just changed (from redraw! it is a no-op).
    (update-echo-geometry!)
    (let loop ([es echo-pending] [row (- rows echo-height)])
      (when (pair? es)
        (let* ([e (car es)]
               [prefix (echo-log-prefix e)]
               [text (cadr e)]
               [ghost (cadddr e)]
               [spans (echo-log-spans (string-length prefix)
                                      (string-append text ghost))]
               [limit (- rows echo-live-height)])
          ;; the entry's rows in turn; clipped at the area's edge when
          ;; a single entry alone overflows the cap (the tail is in
          ;; *log*)
          (let rloop ([spans spans] [k 0] [row row])
            (if (or (null? spans) (>= row limit))
                (loop (cdr es) row)
                (let ([span (car spans)]
                      [wrapped? (pair? (cdr spans))])
                  (paint! row 0 (list 'echo-log e k span wrapped?)
                          (lambda ()
                            (display-echo-log-row prefix text (caddr e) ghost
                                                  k span wrapped?)))
                  (rloop (cdr spans) (+ k 1) (+ row 1))))))))
    (when (> echo-live-height 0)
      (let* ([content (string-append message message-ghost)]
             [ghost-at (string-length message)]
             [total (length echo-spans)]
             [indent (echo-indent-now)])
        (let loop ([line echo-scroll] [row (- rows echo-live-height)])
          (when (< row rows)
            (let* ([span (list-ref echo-spans line)]
                   [start (car span)]
                   [end (min (cdr span) (string-length content))]
                   [end (max end start)]
                   [lead (if (= line 0) 0 indent)]
                   [wrapped? (< line (- total 1))]
                   [cut (min (max (- ghost-at start) 0) (- end start))]
                   ;; a prompt's label -- content up to echo-indent on
                   ;; the first visual line -- is painted grey, the
                   ;; transient log's shade: quiet chrome, the input
                   ;; carries the emphasis
                   [lb (if (= line 0)
                           (min (or echo-indent 0) (+ start cut))
                           0)])
              (paint! row 0
                      (list 'echo line (substring content start end)
                        cut lead lb wrapped? (and (echo-highlight) #t)
                        (and message-styles #t))
                      (lambda ()
                        (let ([styles
                               (or (and (echo-highlight)
                                     (guard (ex [else #f])
                                       ((echo-highlight) content)))
                                 (and message-styles
                                      (string-prefix? (car message-styles)
                                                      content)
                                      (guard (ex [else #f])
                                        ((cdr message-styles)
                                         (car message-styles)))))])
                          (ansi (make-string lead #\space))
                          (when (> lb 0)
                            (ansi (style-code 'chrome)
                                  (substring content 0 lb) "\x1b;[0m"))
                          (if styles
                            ;; styled runs for the typed part
                            (emit-runs content styles (+ start lb)
                                       (+ start cut))
                            (ansi (substring content (+ start lb)
                                             (+ start cut))))
                          (ansi "\x1b;[0m" (style-code 'chrome)
                            (substring content (+ start cut) end)
                            "\x1b;[0m"
                            (make-string
                              (max 0 (- cols lead (- end start)
                                        (if wrapped? 1 0)))
                              #\space)
                            (if wrapped? "\\" ""))))))
            (loop (+ line 1) (+ row 1)))))))

  (define (paint-scrollbar! w row k height sticky top total)
    (let ([side (window-scrollbar? w)])
      (when side
        (let* ([body-height (max 1 (- height sticky))]
               [body-total (max 0 (- total sticky))]
               [thumb-size (if (<= body-total body-height)
                             body-height
                             (max 1 (quotient (* body-height body-height)
                                              body-total)))]
               [travel (max 0 (- body-height thumb-size))]
               [scrollable (max 1 (- body-total body-height))]
               [thumb-start (if (= travel 0) 0
                              (quotient (* (max 0 (- top sticky)) travel)
                                        scrollable))]
               [j (- k sticky)]
               [thumb? (and (>= j thumb-start)
                            (< j (+ thumb-start thumb-size)))]
               [glyph (cond [(< k sticky) " "]
                        [thumb?
                         ;; Heavy box drawing stays centered and joins adjacent
                         ;; thumb rows without seams.
                         "\x2503;"]
                        [else "\x2502;"])])
          (paint! row
                  (+ (window-xoff w)
                    (if (eq? side 'right) (- (window-width w) 1) 0))
                  (list 'scrollbar glyph)
                  (lambda ()
                    (ansi (style-code 'chrome) glyph "\x1b;[0m")))))))

  (define (paint-line-number! row x width line first-segment?)
    (when (> width 0)
      (let* ([label (if (and line first-segment?)
                        (number->string (+ line 1))
                        "")]
             [text (string-append
                     (make-string (max 0 (- width 1 (string-length label)))
                                  #\space)
                     label " ")])
        (paint! row x (list 'line-number text)
                (lambda ()
                  (ansi (style-code 'chrome) text "\x1b;[0m"))))))

  (define (paint-window! w start height ranges)
    (let* ([b (window-buffer w)]
           [v (buffer-lines b)]
           [n (vector-length v)]
           [sticky (min height (buffer-sticky-lines b))]
           [top (max sticky (window-top w))]
           [left (window-left w)]
           [gutter-width (window-line-number-width w)]
           [gutter-x (+ (window-xoff w)
                        (if (eq? (window-scrollbar? w) 'left) 1 0))]
           [content-x (+ gutter-x gutter-width)]
           [content-width (window-content-width w)]
           [styles-of (buffer-line-styles b)]
           [mode-tag (let ([m (buffer-mode b)]) (and m (mode-name m)))]
           [current? (eq? w current-window)])
      ;; Walk buffer lines from the top -- its first visible segment --
      ;; a soft-wrapping window painting a long line as successive
      ;; slices (the same line at successive left offsets), others one
      ;; row per line.
      (let loop ([k 0] [i (if (> sticky 0) 0 top)]
                 [seg (if (> sticky 0) 0 (window-topseg w))])
        (when (< k height)
          (let ([row (+ start k)])
            (paint-scrollbar! w row k height sticky top n)
            (paint-line-number! row gutter-x gutter-width
                                (and (< i n) i) (= seg 0))
            (if (< i n)
                (let* ([line (vector-ref v i)]
                       [shown (let ([r (let ([m (buffer-mode b)])
                                         (and m (mode-render m)))])
                                (or (and r (guard (ex [else #f])
                                             (let ([t (r b i line)])
                                               (and (or (and (string? t)
                                                             (= (string-length t)
                                                                (string-length line)))
                                                        (and (vector? t)
                                                             (= (vector-length t)
                                                                (string-length line))
                                                             (for-all string?
                                                                      (vector->list t))))
                                                    t))))
                                    line))]
                       [wrapped? (and (>= i sticky) (window-wrapped? w))]
                       [breaks (and wrapped? (line-breaks w line))]
                       [slice-left (if wrapped?
                                       (segment-start breaks seg)
                                       left)]
                       [bound (if (and wrapped?
                                       (< (+ seg 1)
                                          (vector-length breaks)))
                                  (segment-start breaks (+ seg 1))
                                  (string-length line))]
                       [edge (cond
                               [(and wrapped?
                                     (< (+ seg 1) (vector-length breaks)))
                                (if (clean-wrap? w) #f 'wrap)]
                               [(and (not wrapped?)
                                     (> (string-length line)
                                        (+ left content-width)))
                                'trunc]    ; it continues past the edge: $
                               [else #f])]
                       [span (and current? (region-span i (string-length line)))]
                       [marks (ranges-on-row ranges w b i current?)]
                       [links (buffer-line-hyperlinks b i)])
                  (let ([row-styles
                         (let ([m (buffer-mode b)])
                           (and m (mode-row-styles m)
                                (guard (ex [else #f])
                                  ((mode-row-styles m) b i line))))])
                    (paint! row content-x
                            (list i line shown span marks links slice-left
                                  mode-tag row-styles edge)
                            (lambda ()
                              (display-editor-line line shown span marks links
                                                   slice-left
                                                   (or row-styles
                                                       (styles-of line))
                                                   edge
                                                   content-width
                                                   bound))))
                  (if (and (>= i sticky)
                           (< (+ seg 1) (line-segments w line)))
                      (loop (+ k 1) i (+ seg 1))
                      (loop (+ k 1)
                            (if (= (+ k 1) sticky) top (+ i 1)) 0)))
                (begin
                  (paint! row content-x '(empty)
                          (lambda () (ansi (fit "" content-width))))
                  (loop (+ k 1) (+ i 1) 0))))))
      (let* ([active-app (app-of (current-buffer))]
             [target? (and active-app
                           (eq? (app-target-window active-app) w)
                           (memq w windows))]
             [conflicts (and (assq b merge-reports)
                             (let ([n (buffer-conflict-count b)])
                               (and (> n 0) n)))]
             [head-prefix
              (format "~a~a~a  "
                      (if target? ">" " ")
                      (cond [(buffer-stale b) "!!"]
                            [(view-buffer? b) "[]"]
                            [(buffer-read-only b) "%%"]
                            [(buffer-modified b) "**"]
                            [else "--"])
                      editor-name)]
             [name (buffer-name b)]
             [app-position
              (let* ([a (app-of b)]
                     [position (and a (app-status-position a))])
                (and position
                     (guard (ex [else #f]) (position b))))]
             [status-row (if (pair? app-position)
                             (car app-position) (window-prow w))]
             [status-col (if (pair? app-position)
                             (cdr app-position) (window-pcol w))]
             [head (format "~a~a  L~a C~a"
                           head-prefix name
                           (+ status-row 1) (+ status-col 1))]
             [conf (if conflicts
                       (format "  ~a conflict~a"
                               conflicts (if (= conflicts 1) "" "s"))
                       "")]
             [mode-text (if mode-tag (format "  (~a)" mode-tag) "")]
             [hint-values (status-hint-values b current?)]
             [hint-text (apply string-append (map car hint-values))]
             [hint-wide-extra
              (fold-left
                (lambda (extra character)
                  (+ extra
                     (max 0 (- (terminal-character-width character) 1))))
                0 (string->list hint-text))]
             [page-text
              (if (and (eq? b completions-buffer)
                       (> completions-pages 1))
                  (format "  page ~a/~a"
                          (+ completions-page 1)
                          completions-pages)
                  "")]
             [status (format "~a~a~a~a~a "
                             head conf mode-text hint-text page-text)]
             [window-buttons " [↕][↔][×]"])
        (let ([stale? (buffer-stale b)])
          (paint! (+ start height) (window-xoff w)
                  (list 'status status current? target? stale?)
                  (lambda ()
                    ;; Reversed cells take the bar's shade from the
                    ;; foreground color, so full reverse tracks the
                    ;; terminal's scheme (dark bar on light, light on
                    ;; dark) and an explicit mid grey marks inactive
                    ;; on either -- dim, the old marker, vanishes in
                    ;; reverse on light schemes.
                    (let* ([bar (cond [current? "\x1b;[7m"]
                                      [else "\x1b;[7;38;5;245m"])]
                           [fg (cond [current? "\x1b;[39m"]
                                     [else "\x1b;[38;5;245m"])]
                           [content-width
                            (max 0 (- (window-width w)
                                      (string-length window-buttons)
                                      hint-wide-extra))]
                           [text (string-append (fit status content-width)
                                                window-buttons)]
                           [n (string-length text)]
                           [cs (min (string-length head) content-width)]
                           [ce (min (+ cs (string-length conf)) content-width)]
                           [ns (min (string-length head-prefix) content-width)]
                           [ne (min (+ ns (string-length name)) content-width)]
                           [hs (min (+ (string-length head)
                                       (string-length conf)
                                       (string-length mode-text))
                                    content-width)]
                           [he (min (+ hs (string-length hint-text))
                                    content-width)]
                           [normal-start
                            (if stale? (min 3 content-width) 0)])
                      (ansi bar)
                      (when stale?
                        ;; the !! flag in red, the rest as usual
                        (ansi "\x1b;[31m" (substring text 0 normal-start)
                              fg))
                      (ansi (substring text normal-start ns)
                            "\x1b;[1m" (substring text ns ne)
                            "\x1b;[22m" (substring text ne cs))
                      (unless (= cs ce)
                        ;; the conflict count in red too
                        (ansi "\x1b;[31m" (substring text cs ce)
                              fg))
                      (ansi (substring text ce hs))
                      (let loop ([values hint-values] [at hs])
                        (when (and (pair? values) (< at he))
                          (let* ([value (car values)]
                                 [end (min (+ at (string-length (car value)))
                                           he)])
                            (case (cdr value)
                              [(italic) (ansi "\x1b;[3m")]
                              [(red) (ansi "\x1b;[31m")])
                            (ansi (substring text at end))
                            (case (cdr value)
                              [(italic) (ansi "\x1b;[23m")]
                              [(red) (ansi fg)])
                            (loop (cdr values) end))))
                      (ansi (substring text he content-width)
                            "\x1b;[1m"
                            (substring text content-width n)
                            "\x1b;[0m"))))))))

  (define (echo-cap)
    ;; How tall the whole echo area may grow: everything but each
    ;; window's minimum -- min-window-lines of text (at least 2,
    ;; redraw!'s collapse threshold) plus its status line.
    (max 1 (- rows (layout-min-height layout-root))))

  (define (update-echo-geometry!)
    ;; The echo area stacks the pending transient-log lines above the
    ;; live line.  The live line's height follows its wrapped content
    ;; (the grey suggestion included): prompt input wraps with
    ;; continuations indented to the prompt text, and a plain message
    ;; that overflows the width wraps the same way at indent zero --
    ;; up to eight lines, after which it scrolls, keeping the prompt
    ;; cursor's line visible; empty behind pending lines it folds
    ;; away.  The whole area grows until the windows above hit their
    ;; minimum; past that the oldest pending lines are evicted -- they
    ;; remain in *log*.
    (let* ([content (string-append message message-ghost)]
           [len (string-length content)]
           [cursor (echo-cursor-now)]
           [padded (max len (if cursor (+ cursor 1) 1))])
      (set! echo-spans (compute-echo-spans content padded))
      (let* ([total (length echo-spans)]
             [live (if (or cursor (> len 0) (null? echo-pending))
                       (min total (max 1 (min 8 (- rows 3))))
                       0)]
             [room (max (if (= live 0) 1 0) (- (echo-cap) live))]
             [pending-rows (lambda ()
                             (fold-left + 0 (map echo-log-rows
                                                 echo-pending)))])
        ;; a long entry wraps over several rows, so eviction counts
        ;; rows, whole oldest entries first; a lone entry past the cap
        ;; stays, clipped by the painter
        (let drop ()
          (when (and (pair? echo-pending) (pair? (cdr echo-pending))
                     (> (pending-rows) room))
            (set! echo-pending (cdr echo-pending))
            (drop)))
        (set! echo-live-height live)
        (set! echo-height (+ live (min room (pending-rows))))
        (when cursor
          (let ([line (car (echo-position cursor))])
            (when (< line echo-scroll) (set! echo-scroll line))
            (when (>= line (+ echo-scroll live))
              (set! echo-scroll (- line (- live 1))))))
        (set! echo-scroll
          (max 0 (min echo-scroll (- total (max live 1))))))))

  (define redraw-lock (make-mutex))

  (define (paint-visual-bell!)
    (when visual-bell-active?
      (let loop ([row (- rows echo-height)])
        (when (< row rows)
          (goto (+ row 1) 1)
          (ansi "\x1b;[7m" (make-string cols #\space) "\x1b;[0m")
          (loop (+ row 1))))))

  ;; Foreign actors edit the (state) store directly; their changes
  ;; flow back into the core's line caches before each frame.  The
  ;; subscription callback runs on whichever thread edited, so it only
  ;; records the buffer id; the main loop does the adoption.
  (define foreign-lock (make-mutex))
  (define foreign-pending '())

  (define (event-actor event)
    ;; every store event names its actor last: (delete id actor) is
    ;; the one three-element shape
    (if (eq? (car event) 'delete) (caddr event) (list-ref event 3)))

  (define (note-foreign-event event)
    (when (and (memq (car event) '(edit reset property create rename delete))
               (not (equal? (event-actor event) ui-actor)))
      (with-mutex foreign-lock
        (set! foreign-pending (cons event foreign-pending)))
      (wake-main!)))

  (define state-subscription (state:subscribe! #f note-foreign-event))

  ;; The ui's own side of the audit stream, coalesced: keystrokes are
  ;; too many to log one by one, so consecutive ui edits to a buffer
  ;; batch into one entry -- flushed before a foreign actor's
  ;; operation on the same buffer (so the record reads in true
  ;; order), when a burst goes stale, and at shutdown.
  (define ui-audit-bursts '())  ; (id . #(name first-rev last-rev n time))

  (define (note-ui-edit! b)
    (guard (ex [else (void)])
      (let* ([id (buffer-state-id b)]
             [rev (buffer-state-rev b)]
             [hit (assv id ui-audit-bursts)]
             [now (time-second (current-time 'time-monotonic))])
        (if hit
            (let ([v (cdr hit)])
              (vector-set! v 2 rev)
              (vector-set! v 3 (+ (vector-ref v 3) 1))
              (vector-set! v 4 now))
            (set! ui-audit-bursts
              (cons (cons id (vector (buffer-name b) rev rev 1 now))
                    ui-audit-bursts))))))

  (define (flush-ui-audit! which)
    ;; which: a buffer id, 'stale (idle bursts), or 'all
    (let ([now (time-second (current-time 'time-monotonic))])
      (let-values ([(flushed kept)
                    (partition
                      (lambda (entry)
                        (case which
                          [(all) #t]
                          [(stale)
                           (> (- now (vector-ref (cdr entry) 4)) 3)]
                          [else (eqv? (car entry) which)]))
                      ui-audit-bursts)])
        (set! ui-audit-bursts kept)
        (for-each
          (lambda (entry)
            (guard (ex [else (void)])
              (let ([v (cdr entry)])
                (log! 'state
                      (format "ui: ~a edit~a in ~s (revisions ~a-~a)"
                              (vector-ref v 3)
                              (if (= (vector-ref v 3) 1) "" "s")
                              (vector-ref v 0)
                              (vector-ref v 1) (vector-ref v 2))
                      #f))))
          (reverse flushed)))))

  (define ui-audit-flushed-at-exit
    (add-shutdown-hook! (lambda () (flush-ui-audit! 'all))))

  (define (sync-foreign-edits!)
    (let ([events (with-mutex foreign-lock
                    (let ([pending foreign-pending])
                      (set! foreign-pending '())
                      (reverse pending)))])
      ;; the audit stream: every foreign operation is on the record --
      ;; (log-view 'state) shows what other actors did
      (for-each
        (lambda (event)
          (guard (ex [else (void)])
            (let ([id (cadr event)] [actor (event-actor event)])
              ;; the modified flag flips on every edit: audit the
              ;; edits, not their bookkeeping shadow
              (unless (and (eq? (car event) 'property)
                           (eq? (caddr event) 'modified))
                (flush-ui-audit! id)
                (log! 'state
                      (case (car event)
                        [(create)
                         (format "~a created ~s" actor (caddr event))]
                        [(rename)
                         (format "~a renamed ~s to ~s" actor
                                 (let ([b (buffer-of-state-id id)])
                                   (if b (buffer-name b) id))
                                 (caddr event))]
                        [(delete)
                         (format "~a deleted ~s" actor
                                 (let ([b (buffer-of-state-id id)])
                                   (if b (buffer-name b) id)))]
                        [(property)
                         (format "~a set ~a of ~s"
                                 actor (caddr event)
                                 (state:buffer-name id))]
                        [else
                         (format "~a ~a ~s~a"
                                 actor
                                 (if (eq? (car event) 'reset)
                                     "reset" "edited")
                                 (state:buffer-name id)
                                 (if (eq? (car event) 'edit)
                                     (format " at ~a"
                                             (text:span-start
                                               (text:delta-span
                                                 (list-ref event 4))))
                                     ""))]))))))
        events)
      ;; the buffer lifecycle across heads: another actor's buffers
      ;; appear in this head's list, renames follow, and a deletion
      ;; drops the record -- any window showing it moves on
      (for-each
        (lambda (event)
          (guard (ex [else (void)])
            (case (car event)
              [(create) (adopt-store-buffer! (cadr event))]
              [(rename)
               (let ([b (buffer-of-state-id (cadr event))])
                 (when b (buffer-name-set! b (caddr event))))]
              [(delete)
               (let ([b (buffer-of-state-id (cadr event))])
                 (when b
                   (buffer-state-id-set! b #f)   ; the twin is gone
                   (forget-buffer! b)))]
              [else (void)])))
        events)
      ;; a foreign fact changed (mode, file, read-only): the status
      ;; line must repaint even though no text moved
      (for-each
        (lambda (event)
          (when (eq? (car event) 'property)
            (let ([b (find (lambda (b)
                             (eqv? (buffer-state-id b) (cadr event)))
                           buffers)])
              (when b
                (bump-buffer-revision! b)
                (invalidate-screen-cache!)))))
        events)
      ;; carry every view's point across the foreign deltas, so a
      ;; cursor keeps its content when an agent edits above it
      (for-each
        (lambda (event)
          (when (eq? (car event) 'edit)
            (let ([b (find (lambda (b)
                             (eqv? (buffer-state-id b) (cadr event)))
                           buffers)])
              (when b
                (guard (ex [else (void)])
                  (let ([d (list-ref event 4)])
                    (for-each
                      (lambda (w)
                        (when (eq? (window-buffer w) b)
                          (let ([p (text:rebase-position
                                     (cons (window-prow w)
                                           (window-pcol w))
                                     d)])
                            (window-prow-set! w (car p))
                            (window-pcol-set! w (cdr p)))
                          (window-top-set!
                            w (car (text:rebase-position
                                     (cons (window-top w) 0) d)))))
                      windows)
                    (let ([p (text:rebase-position
                               (cons (buffer-spot-row b)
                                     (buffer-spot-col b))
                               d)])
                      (buffer-spot-row-set! b (car p))
                      (buffer-spot-col-set! b (cdr p)))))))))
        events)
      (for-each
        (lambda (id)
          (let ([b (find (lambda (b) (eqv? (buffer-state-id b) id))
                         buffers)])
            (when b
              (guard (ex [else (void)])
                (let-values ([(text revision) (state:snapshot id)])
                  (unless (= revision (buffer-state-rev b))
                    ;; adoption is sharing: nothing mutates in place
                    (buffer-lines-raw-set! b text)
                    (bump-buffer-revision! b)
                    (buffer-state-rev-set! b revision)
                    (when (buffer-file b) (buffer-modified-set! b #t))
                    (clamp-buffer-positions! b)
                    (invalidate-screen-cache!)))))))
        (let dedupe ([ids (map cadr events)] [seen '()])
          (cond [(null? ids) (reverse seen)]
                [(memv (car ids) seen) (dedupe (cdr ids) seen)]
                [else (dedupe (cdr ids) (cons (car ids) seen))])))))

  ;; Stage 1's tail: the human's cursor is a first-class mark other
  ;; actors can see.  Published once per frame, for the selected
  ;; window's buffer, under the ui actor's 'point.
  ;; What the head looks at, published as state marks other actors can
  ;; read, refreshed per frame by a desired-versus-published diff:
  ;; every window's cursor as (point . serial), the selected window's
  ;; additionally as plain 'point, and the active region as 'region
  ;; and (region . serial).  A mark drops when its window closes,
  ;; looks at another buffer, or the selection deactivates.  Serials
  ;; ride a weak table, so closed windows carry theirs to the grave.

  (define window-serial-counter 0)
  (define window-serials (make-weak-eq-hashtable))

  (define (window-serial w)
    (or (hashtable-ref window-serials w #f)
        (begin
          (set! window-serial-counter (+ window-serial-counter 1))
          (hashtable-set! window-serials w window-serial-counter)
          window-serial-counter)))

  ;; (((id . name) . value) ...): value is (row . col) for a point,
  ;; ((row . col) . (row . col)) for a region -- plain data, so frames
  ;; without changes are equal? and publish nothing
  (define published-marks '())

  (define (desired-head-marks)
    (fold-left
      (lambda (acc w)
        (let ([id (buffer-state-id (window-buffer w))])
          (if (not id)
              acc
              (let* ([serial (window-serial w)]
                     [selected? (eq? w current-window)]
                     [p (cons (window-prow w) (window-pcol w))]
                     [acc (cons (cons (cons id (cons 'point serial)) p)
                                acc)]
                     [acc (if selected?
                              (cons (cons (cons id 'point) p) acc)
                              acc)])
                (if (and selected? mark-active?)
                    (let ([region (cons (cons mark-row mark-col) p)])
                      (cons* (cons (cons id (cons 'region serial)) region)
                             (cons (cons id 'region) region)
                             acc))
                    acc)))))
      '() windows))

  (define (mark-value value)
    ;; a region value becomes a normalized span; a point stays a pair
    (if (pair? (car value))
        (text:normalize-span
          (text:make-span (caar value) (cdar value)
                          (cadr value) (cddr value)))
        value))

  (define (publish-head-marks!)
    (guard (ex [else (void)])
      (let ([desired (desired-head-marks)])
        (unless (equal? desired published-marks)
          (for-each
            (lambda (entry)
              (unless (assoc (car entry) desired)
                (guard (ex [else (void)])
                  (state:drop-mark! ui-actor (caar entry) (cdar entry)))))
            published-marks)
          (for-each
            (lambda (entry)
              (let ([old (assoc (car entry) published-marks)])
                (unless (and old (equal? (cdr old) (cdr entry)))
                  (guard (ex [else (void)])
                    (state:set-mark! ui-actor (caar entry) (cdar entry)
                                     (mark-value (cdr entry)))))))
            desired)
          (set! published-marks desired)))))

  (define (state-frame-sync!)
    ;; lifecycle first: a foreign deletion forgets the buffer before
    ;; outage recovery could mistake its missing twin for a store fault
    (sync-foreign-edits!)
    (reconverge-forked!)
    (flush-ui-audit! 'stale)
    (publish-head-marks!)
    (present-pending-ask!))

  ;; The head's side of the interaction protocol: another actor's
  ;; question waits in the echo area as an unlogged indicator until
  ;; C-c a answers it -- nobody's keyboard is stolen mid-thought.
  (define ui-actor-registered
    (actors:register! ui-actor (lambda (message) (wake-main!))))

  (define (present-pending-ask!)
    (let ([asks (actors:pending ui-actor)])
      (when (and (pair? asks)
                 (not (prompt-active?))
                 (string=? message ""))
        (let ([ask (car asks)])
          (set! message
            (elide (format "~a asks: ~a -- C-c a answers~a"
                           (cadr ask) (caddr ask)
                           (if (> (length asks) 1)
                               (format " (~a waiting)" (length asks))
                               ""))
                   cols))))))

  (define (answer!!)
    ;; Answer the oldest question another actor posed (see
    ;; docs/DESIGN2.md, the interaction protocol).
    (let ([asks (actors:pending ui-actor)])
      (if (null? asks)
          (set! message "Nothing to answer")
          (let* ([ask (car asks)]
                 [choices (cadddr ask)]
                 [reply
                  (prompt! (format "~a [~a] "
                                   (caddr ask)
                                   (if (null? choices)
                                       "..."
                                       (string-join choices "/")))
                           (and (pair? choices)
                                (lambda (s)
                                  (filter
                                    (lambda (choice)
                                      (string-prefix? s choice))
                                    choices))))])
            (when (and reply (> (string-length reply) 0))
              (if (actors:answer! (car ask) reply)
                  (set! message "Answered")
                  (set! message "That question was withdrawn")))))))

  (define foreign-sync-hooked (add-pre-redraw-hook! state-frame-sync!))

  (define (redraw-frame!)
    ;; The frame goes out inside a synchronized update (mode 2026):
    ;; a supporting terminal holds rendering until the closing pair,
    ;; so a scroll and the repaint over it appear as one; others
    ;; ignore the mode.
    (ansi "\x1b;[?2026h")
    (terminal-size!)
    (update-echo-geometry!)
    ;; window geometry is otherwise set while painting, one frame
    ;; stale from here -- refresh views against the current layout
    (window-layout)
    (refresh-visible-views!)
    (update-completions-size!)
    ;; A terminal too small for the splits collapses back to one window.
    (when (and (pair? (cdr (layout-leaves layout-root)))
               (or (< cols (layout-min-width layout-root))
                   (< (- rows echo-height) (layout-min-height layout-root))))
      (set-layout-root!
        (if (memq current-window (layout-leaves layout-root))
            current-window
            (car (layout-leaves layout-root)))))
    (let* ([layout (window-layout)]
           [view (list rows cols
                       (map (lambda (entry)
                              (list (cadr entry) (caddr entry)
                                    (window-xoff (car entry))
                                    (window-width (car entry))))
                            layout)
                       ;; A scrollbar changes one window row from a single
                       ;; full-width cached segment into two overlapping
                       ;; segments (the bar and the content).  Row cache
                       ;; entries are keyed by their starting column, so a
                       ;; later full-width paint cannot selectively evict a
                       ;; covered content segment.  Treat presentation
                       ;; topology as part of the view and discard those
                       ;; incompatible segment keys when buffers are switched.
                       (map (lambda (w)
                              (list (window-wrapped? w)
                                    (window-scrollbar? w)
                                    (window-line-number-width w)
                                    (buffer-sticky-lines (window-buffer w))))
                            windows))])
      (for-each (lambda (entry) (scroll-window! (car entry) (caddr entry)))
                layout)
      (if (not (equal? view cached-view))
          (begin (set! screen-cache (make-vector rows #f))
                 (set! cached-view view))
          (for-each (lambda (entry)
                      ;; Native terminal scrolling moves the whole window
                      ;; rectangle. Sticky rows and scrollbar columns must
                      ;; remain fixed, so those presentations use ordinary
                      ;; cached repainting instead.
                      (unless (or (> (buffer-sticky-lines
                                       (window-buffer (car entry))) 0)
                                  (window-scrollbar? (car entry))
                                  (> (window-line-number-width (car entry)) 0))
                        (native-scroll! (car entry) (cadr entry) (caddr entry))))
                    layout))
      (paint-dividers! layout)
      (let ([ranges (highlight-ranges)])
        (for-each (lambda (entry)
                    (paint-window! (car entry) (cadr entry) (caddr entry) ranges)
                    (window-shown-top-set! (car entry)
                                           (cons (window-top (car entry))
                                                 (window-topseg (car entry)))))
                  layout))
      (paint-echo-area!)
      (paint-visual-bell!))
    (place-cursor!)
    (ansi "\x1b;[?2026l")
    (flush-output-port (terminal-output-port)))

  (define (redraw!)
    ;; PTY readers may request a frame while the main thread waits for input.
    ;; Keep the cache and terminal output as one indivisible transaction.
    (with-mutex redraw-lock
      (update-terminal-title!)
      (redraw-frame!)))

  (define terminal-title-shown #f)

  (define (safe-terminal-title s)
    ;; OSC is terminated by BEL or ST. Do not let a buffer name inject either
    ;; terminator (or another terminal control) into the host terminal.
    (list->string
      (map (lambda (c)
             (let ([n (char->integer c)])
               (if (or (< n 32) (= n 127)) #\space c)))
           (string->list s))))

  (define (update-terminal-title!)
    ;; OSC 2 is understood by GNOME Terminal, xterm, and nested e terminals.
    (let ([title (string-append "e: " (buffer-name (current-buffer)))])
      (unless (equal? title terminal-title-shown)
        (set! terminal-title-shown title)
        (ansi "\x1b;]2;" (safe-terminal-title title) "\x1b;\\"))))

  (define (window-screen-position w prow pcol)
    ;; 1-based screen (row . col) of a buffer position in w, wrap-aware.
    (let* ([entry (assq w (window-layout))]
           [sticky (buffer-sticky-lines (window-buffer w))]
           [x (+ (window-xoff w)
                 (if (eq? (window-scrollbar? w) 'left) 1 0)
                 (window-line-number-width w))]
           [screen-row (if (< prow sticky)
                           (+ (cadr entry) prow 1)
                           (+ (cadr entry) sticky
                              (rows-before w prow pcol) 1))])
      (if (and (>= prow sticky) (window-wrapped? w))
          (cons screen-row
                (let ([breaks (line-breaks
                                w (vector-ref
                                    (buffer-lines (window-buffer w)) prow))])
                  (+ x
                     (- pcol (segment-start breaks (segment-of breaks pcol)))
                     1)))
          (cons screen-row (+ x (- pcol (window-left w)) 1)))))

  (define (place-cursor!)
    ;; Park the cursor in the echo area (a prompt, or a running
    ;; evaluation -- the latter drawn as a blinking underline), else
    ;; put it at point in the current window.  Also called on its own
    ;; when an interaction is about to wait for a key, so its cursor
    ;; rules take effect without a repaint.
    (let* ([cursor (echo-cursor-now)]
           [a (app-of (window-buffer current-window))]
           [visible? (or cursor (app-cursor-visible-in? current-window))])
      (if cursor
          (let ([p (echo-position cursor)])
            (goto (+ (- rows echo-live-height) (- (car p) echo-scroll) 1)
                  (min (+ (cdr p) 1) cols)))
          (when visible?
            (let ([p (window-screen-position current-window
                                             point-row point-col)])
              (goto (min (car p) rows) (min (cdr p) cols)))))
      (let* ([a (app-of (window-buffer current-window))]
             [app-style (and a (app-cursor-style a))]
             [style (cond
                      [(cursor-in-echo) "\x1b;[3 q"]
                      ;; a prompt: the cursor is in the echo area's input,
                      ;; which is editable whatever the buffer behind it
                      [echo-cursor "\x1b;[0 q"]
                      [(app-capture-escaped? (current-buffer)) "\x1b;[0 q"]
                      [(and app-style (not (eq? app-style 'default)))
                       (case app-style
                         [(block) "\x1b;[2 q"]
                         [(underline) "\x1b;[4 q"]
                         [(bar) "\x1b;[6 q"]
                         [(blinking-block) "\x1b;[1 q"]
                         [(blinking-underline) "\x1b;[3 q"]
                         [(blinking-bar) "\x1b;[5 q"])]
                      ;; a bar where typing cannot land: a read-only buffer
                      [(buffer-read-only (window-buffer current-window))
                       "\x1b;[5 q"]
                      [else "\x1b;[0 q"])])
        (unless (string=? style cursor-style-shown)
          (set! cursor-style-shown style)
          (ansi style)))
      (ansi (if visible? "\x1b;[?25h" "\x1b;[?25l"))
      (flush-output-port (terminal-output-port))))

  ;;; Prompts and commands --------------------------------------------------

  (define (completion-label c)
    ;; A candidate as shown in the completions list: the part after the last
    ;; separator -- a path's last component (with the trailing slash kept on
    ;; directories), an expression's trailing symbol; plain names unchanged.
    ;; A label that comes out empty (a view name like "[log]" ends in a
    ;; separator) falls back to the whole candidate.
    (if (string-suffix? "/" c)
        (string-append (base-name (substring c 0 (- (string-length c) 1))) "/")
        (let loop ([i (- (string-length c) 1)])
          (cond [(< i 0) c]
                [(memv (string-ref c i) '(#\/ #\space #\( #\) #\[ #\]))
                 (let ([tail (string-tail c (+ i 1))])
                   (if (string=? tail "") c tail))]
                [else (loop (- i 1))]))))

  (define (format-columns labels width)
    ;; The labels laid out in columns across width, one string per line.
    (let* ([w (+ 2 (fold-left max 0 (map string-length labels)))]
           [ncols (max 1 (quotient width w))])
      (let loop ([xs labels] [acc '()])
        (if (null? xs)
            (reverse acc)
            (let row ([i 0] [xs xs] [line ""])
              (if (or (= i ncols) (null? xs))
                  (loop xs (cons line acc))
                  (row (+ i 1) (cdr xs) (string-append line (fit (car xs) w)))))))))

  ;; The *completions* pop-up: a temporary full-width overlay above the echo
  ;; area. It is deliberately outside the persistent split tree.
  (define completions-buffer #f)
  (define completions-restore #f)

  ;; Prompts may parameterize this to make candidates stand out in the
  ;; pop-up -- M-x highlights the symbols the editor itself defines.
  (define completion-highlight (make-parameter (lambda (label) #f)))

  (define (completions-mode)
    ;; A mode for the pop-up highlighting the labels the current
    ;; completion-highlight predicate selects.
    (let ([highlight? (completion-highlight)])
      (make-mode "completions" '() '()
        (lambda (s)
          (let ([styles (make-vector (string-length s) 'plain)]
                [n (string-length s)])
            (let loop ([i 0])
              (cond [(>= i n) styles]
                    [(char=? (string-ref s i) #\space) (loop (+ i 1))]
                    [else
                     (let ([j (let end ([j i])
                                (if (or (>= j n)
                                        (char=? (string-ref s j) #\space))
                                    j
                                    (end (+ j 1))))])
                       (when (highlight? (substring s i j))
                         (vector-fill-range! styles i j 'editor))
                       (loop j))])))))))

  (define completions-labels #f)   ; the labels shown: repeat detection
  (define completions-rows '#())   ; the full column layout
  (define completions-cols 0)      ; the width the layout was built for
  (define completions-page 0)
  (define completions-pages 1)
  (define completions-filled #f)   ; (page size) the buffer holds

  (define (completions-window)
    (and completions-buffer
         (find (lambda (w) (eq? (window-buffer w) completions-buffer))
               windows)))

  (define (completions-layout! labels width)
    (set! completions-rows (list->vector (format-columns labels width)))
    (set! completions-cols width)
    (set! completions-page 0)
    (set! completions-filled #f))

  (define (show-completions! labels)
    ;; The candidate list borrows the prompt's target window: it shows
    ;; *completions* until the prompt ends, then gets its buffer back
    ;; with point and viewport intact.  A list taller than the window
    ;; is paged, and repeated TAB on the same candidates cycles the
    ;; pages.  No pop-ups: the layout tree is the only source of
    ;; windows, so every seam sees one kind of window.
    (cond
      [(and completions-restore (equal? labels completions-labels))
       (set! completions-page (mod (+ completions-page 1)
                                   (max 1 completions-pages)))
       (set! completions-filled #f)
       #t]
      [completions-restore                       ; already up: refresh it
       (set! completions-labels labels)
       (completions-layout! labels (window-content-width current-window))
       #t]
      [else
       (set! completions-buffer (new-buffer "*completions*"))
       ;; a head's own chrome: other heads do not adopt it
       (buffer-fact-set! completions-buffer 'ephemeral #t)
       (buffer-read-only-set! completions-buffer #t)
       (buffer-mode-set! completions-buffer (completions-mode))
       (set! completions-labels labels)
       (completions-layout! labels (window-content-width current-window))
       (let ([target current-window]
             [shown (window-buffer current-window)])
         (set-window-buffer! target completions-buffer)
         (set! completions-restore
           (lambda ()
             (when (and (memq target windows)
                        (eq? (window-buffer target) completions-buffer))
               (set-window-buffer! target (if (memq shown buffers)
                                              shown
                                              (car buffers)))))))
       #t]))

  (define (update-completions-size!)
    ;; Page the list to the window it borrowed: the whole list when it
    ;; fits, the largest possible page otherwise -- the buffer holds
    ;; the current page.
    (let ([w (completions-window)])
      (when w
        (unless (= completions-cols (window-content-width w)) ; resized
          (completions-layout! completions-labels (window-content-width w)))
        (let* ([all (max 1 (vector-length completions-rows))]
               [size (max 1 (min all (window-size w)))])
          (set! completions-pages (div (+ all size -1) size))
          (when (>= completions-page completions-pages)
            (set! completions-page 0))
          (unless (equal? completions-filled (list completions-page size))
            (set! completions-filled (list completions-page size))
            (let* ([from (* completions-page size)]
                   [to (min (vector-length completions-rows) (+ from size))]
                   [out (make-vector (max 1 (- to from)) "")])
              (do ([i from (+ i 1)]) ((>= i to))
                (vector-set! out (- i from)
                             (vector-ref completions-rows i)))
              (buffer-lines-set! completions-buffer out)
              (window-top-set! w 0)
              (window-prow-set! w 0) (window-pcol-set! w 0)))))))

  (define (dismiss-completions!)
    (when completions-restore
      (completions-restore)
      (when (buffer-state-id completions-buffer)
        (guard (ex [else (void)])
          (state:delete! ui-actor (buffer-state-id completions-buffer))))
      (set! buffers (remq completions-buffer buffers))
      (set! completions-buffer #f)
      (set! completions-restore #f)
      (set! completions-labels #f)))

  ;; Prompts may parameterize this to suggest what could follow the input --
  ;; M-x uses it to show the pending parameters of the call being typed.
  ;; The suggestion is drawn in grey after the cursor; #f for none.
  (define prompt-ghost (make-parameter (lambda (s) #f)))

  ;; M-. at a prompt hands the input and cursor position here --
  ;; describe wires it to pop the reference page for the symbol at
  ;; (or just before) the cursor.  A procedure (text pos), or #f.
  (define prompt-inspector (make-parameter #f))

  ;; A structured multiline prompt supplies (text position inserted-text ->
  ;; (new-text . new-position)). M-x uses this for M-RET and bracketed paste;
  ;; ordinary prompts retain compact single-line paste behavior.
  (define prompt-multiline (make-parameter #f))

  ;; Optional prompt-specific C-a/C-e behavior: (action text position second?
  ;; -> new-position). second? records the immediately preceding edge command,
  ;; independently of where the cursor happened to be.
  (define prompt-edge-motion (make-parameter #f))

  ;; Optional whole-input normalization after every prompt edit:
  ;; (text position -> (new-text . new-position)). M-x uses this to reindent
  ;; all logical lines after each character, deletion, completion, or paste.
  (define prompt-reindent (make-parameter #f))

  (define (complete! s complete k)
    ;; TAB in a prompt, as in Emacs: extend s to the longest common prefix
    ;; of its completions; when it cannot be extended, pop up the candidate
    ;; list.  k continues the prompt loop as (k new-s note).
    (let ([cands (complete s)])
      (cond
        [(null? cands) (dismiss-completions!) (k s " [No match]")]
        [(null? (cdr cands))
         (dismiss-completions!)
         (if (string=? (car cands) s)
             (k s " [Sole completion]")
             (k (car cands) ""))]
        [else
         (let ([lcp (common-prefix cands)])
           (cond [(> (string-length lcp) (string-length s)) (k lcp "")]
                 [(show-completions! (map completion-label cands)) (k s "")]
                 [else (k s (format " {~a}"
                                    (string-join (map completion-label cands)
                                                 " ")))]))])))

  (define (prompt-window-commands)
    ;; The global commands a prompt may run without losing its input:
    ;; pure window management. Resolution goes through the live keymap,
    ;; so rebinding these commands -- or binding new chords to them --
    ;; works inside every prompt as well.
    (list focus-window-up! focus-window-down!
          focus-window-left! focus-window-right!
          other-window! split-window! split-window-right!
          delete-window! delete-other-windows!))

  (define (prompt-kill-buffer!)
    ;; kill-buffer!!'s prompt-safe stand-in: no nested prompt, and a
    ;; file-backed buffer with unsaved changes is refused with a note.
    (let ([b (current-buffer)])
      (if (and (buffer-file b) (not (buffer-clean? b)))
          (format "  ~a has unsaved changes" (buffer-name b))
          (guard (ex [else (string-append "  " (error-text ex))])
            (kill-buffer! b)
            ""))))

  (define (prompt-window-command event)
    ;; Resolve the event, and any chord it opens, through the global
    ;; keymap while a prompt runs. A window-management command yields a
    ;; thunk that runs it and returns the note to show; any other
    ;; complete chord is consumed whole so its tail cannot leak into the
    ;; input; a plain key or self-inserting character that is not a
    ;; window command stays with the prompt.
    (define (action-thunk action)
      (cond
        [(memq action (prompt-window-commands))
         (lambda ()
           (guard (ex [else (string-append "  " (error-text ex))])
             (action)
             ""))]
        [(eq? action kill-buffer!!) prompt-kill-buffer!]
        [else #f]))
    (and (not (key-event-character event))
         (let loop ([sequence (list event)])
           (cond
             [(binding-prefix? 'global sequence)
              (let ([next (read-key-event #f)])
                (if (eof-object? next)
                    (lambda () "")
                    (loop (append sequence (list next)))))]
             [(resolved-binding 'global sequence)
              => (lambda (hit)
                   (or (action-thunk (binding-action (cdr hit)))
                       (and (> (length sequence) 1) (lambda () ""))))]
             [(> (length sequence) 1) (lambda () "")]
             [else #f]))))

  (define (prompt! label . rest)
    ;; Read input in the echo area, with the cursor parked there. Optional
    ;; arguments: a completer (string -> list of candidate strings) enabling
    ;; TAB completion, initial input (pre-filled, editable), a history box
    ;; (a list of previous inputs, newest first) navigated with the up and
    ;; down arrows -- accepting an input records it there -- an
    ;; alternative completer bound to Shift-TAB, and a normalizer
    ;; applied to the accepted input before recording and returning.
    ;; Whichever way the prompt ends, the completions pop-up is taken
    ;; down.
    (define (optional n)
      (let loop ([r rest] [n n])
        (cond [(null? r) #f]
              [(= n 0) (car r)]
              [else (loop (cdr r) (- n 1))])))
    (define complete (optional 0))
    (define initial (or (optional 1) ""))
    (define history (optional 2))
    (define alt-complete (optional 3))
    ;; applied to the accepted input before it is recorded and
    ;; returned: eval closes forgiven parentheses here, so the history
    ;; carries the completed expression
    (define normalize (optional 4))
    (define hist-pos -1)   ; -1: editing; 0..: showing that history entry
    (define stash "")      ; the in-progress input while browsing history
    (define last-edge #f)  ; beginning/end, only across consecutive presses
    (define (record-history! s)
      (when (and history (> (string-length s) 0))
        (let ([h (unbox history)])
          (unless (and (pair? h) (string=? (car h) s))
            (set-box! history (cons s h))))))
    (define (run-prompt)
      (let loop ([s initial] [pos (string-length initial)] [note ""])
        (define len (string-length s))
        (define (edited new-s new-pos) ; an edit restarts history browsing
          (set! hist-pos -1)
          (let ([reindent (prompt-reindent)])
            (if reindent
                (let ([result (guard (ex [else (cons new-s new-pos)])
                                (reindent new-s new-pos))])
                  (loop (car result) (cdr result) ""))
                (loop new-s new-pos ""))))
        (define (history-show entry)
          (loop entry (string-length entry) ""))
        (define (history-up)
          (let ([h (if history (unbox history) '())])
            (if (< (+ hist-pos 1) (length h))
                (begin
                  (when (= hist-pos -1) (set! stash s))
                  (set! hist-pos (+ hist-pos 1))
                  (history-show (list-ref h hist-pos)))
                (loop s pos note))))
        (define (history-down)
          (cond [(= hist-pos -1) (loop s pos note)]
                [(= hist-pos 0) (set! hist-pos -1) (history-show stash)]
                [else (set! hist-pos (- hist-pos 1))
                      (history-show (list-ref (unbox history) hist-pos))]))
        (define (vertical-move delta)
          ;; Move the cursor between the prompt's visual lines, keeping
          ;; the column, clamped into the editable input.
          (let* ([p (echo-position echo-cursor)]
                 [target (list-ref echo-spans (+ (car p) delta))]
                 [indent (echo-indent-now)]
                 [col (cdr p)]
                 [k (if (= (+ (car p) delta) 0)
                        (min col (cdr target))
                        (+ (car target) (max 0 (- col indent))))]
                 [k (min k (cdr target))]
                 [new-pos (min (max 0 (- k (string-length label)))
                               (string-length s))])
            (loop s new-pos note)))
        (define (cursor-on-top?)
          (= (car (echo-position echo-cursor)) 0))
        (define (cursor-on-bottom?)
          (= (car (echo-position echo-cursor)) (- (length echo-spans) 1)))
        (set! message (string-append label s note))
        (set! echo-input-end (+ (string-length label) len))
        (set! message-ghost
          (if (string=? note "") (or ((prompt-ghost) s) "") ""))
        (set! echo-indent (string-length label))
        (set! echo-cursor (+ (string-length label) pos))
        (redraw!)
        ;; Mouse reports are live here: clicks focus windows and work the
        ;; window controls without canceling the prompt.
        (let* ([event (read-key-event #t)]
               [action (and (not (eof-object? event))
                            (key-event-binding 'prompt event))]
               [previous-edge last-edge])
          (set! last-edge #f)
          (cond
            [(eof-object? event) #f]
            [(eq? action 'cancel) (set! message "Quit") #f]
            [(eq? action 'accept)
             (let ([out (if normalize (normalize s) s)])
               (record-history! out) (set! message "") out)]
            [(eq? action 'beginning)
             (set! last-edge 'beginning)
             (let ([move (prompt-edge-motion)])
               (loop s (if move
                           (move 'beginning s pos
                                 (eq? previous-edge 'beginning))
                           0)
                     ""))]
            [(eq? action 'backward) (loop s (max 0 (- pos 1)) "")]
            [(eq? action 'end)
             (set! last-edge 'end)
             (let ([move (prompt-edge-motion)])
               (loop s (if move
                           (move 'end s pos (eq? previous-edge 'end))
                           len)
                     ""))]
            [(eq? action 'forward) (loop s (min len (+ pos 1)) "")]
            [(eq? action 'up)
             (if (cursor-on-top?) (history-up) (vertical-move -1))]
            [(eq? action 'down)
             (if (cursor-on-bottom?) (history-down) (vertical-move 1))]
            [(eq? action 'delete-forward)
             (if (< pos len)
                 (edited (string-delete s pos (+ pos 1)) pos)
                 (loop s pos ""))]
            [(eq? action 'delete-backward)
             (if (= pos 0)
                 (loop s pos "")
                 (edited (string-delete s (- pos 1) pos) (- pos 1)))]
            [(eq? action 'kill)
             (set! kill-ring (string-tail s pos))
             (edited (substring s 0 pos) pos)]
            [(eq? action 'yank)
             (edited (string-insert s pos kill-ring)
                     (+ pos (string-length kill-ring)))]
            [(eq? action 'complete)
             (set! hist-pos -1)
             (if complete
                 (complete! s complete
                            (lambda (new-s note)
                              (if (string=? note "")
                                  (edited new-s (string-length new-s))
                                  (loop new-s (string-length new-s) note))))
                 (loop s pos ""))]
            [(eq? action 'alternate-complete)
             (set! hist-pos -1)
             (if alt-complete
                 (complete! s alt-complete
                            (lambda (new-s note)
                              (if (string=? note "")
                                  (edited new-s (string-length new-s))
                                  (loop new-s (string-length new-s) note))))
                 (loop s pos ""))]
            [(eq? action 'inspect)
             (let ([p (prompt-inspector)])
               (when p (guard (ex [else (void)]) (p s pos))))
             (loop s pos "")]
            [(eq? action 'newline)
             (let ([insert (prompt-multiline)])
               (if insert
                   (let ([result (insert s pos "\n")])
                     (edited (car result) (cdr result)))
                   (loop s pos "")))]
            [(eq? action 'paste)
             (let* ([lines (split-pasted-lines (read-paste))]
                    [insert (prompt-multiline)])
               (if insert
                   (let ([result (insert s pos (string-join lines "\n"))])
                     (edited (car result) (cdr result)))
                   (let ([text (string-join lines " ")])
                     (edited (string-insert s pos text)
                             (+ pos (string-length text))))))]
            [(prompt-window-command event)
             => (lambda (run) (loop s pos (run)))]
            [(key-event-character event)
             => (lambda (c)
                  (edited (string-insert s pos (string c)) (+ pos 1)))]
            [else (loop s pos "")]))))
    ;; The prompt owns C-g while it runs, and its echo-area state is
    ;; restored however it exits -- an error unwinding through it
    ;; included.
    (call-uninterrupted
      (lambda ()
        (dynamic-wind
          void
          run-prompt
          (lambda ()
            (set! echo-cursor #f)
            (set! echo-indent #f)
            (set! echo-input-end #f)
            (set! echo-scroll 0)
            (set! message-ghost "")
            (dismiss-completions!))))))

  (define (confirm? label)
    ;; Ordinary yes/no questions share the focused, highlighted, visual-bell
    ;; choice engine used by file conflict decisions.
    (let ([answer (query-key! (string-append label " y)es or n)o") "yn")])
      (and answer (memv (char->integer answer) '(121 89)))))

  (define (file-prompt-styler label)
    ;; Existence shown in the face, component-wise: the typed path's
    ;; longest leading run of components that exists on disk stays
    ;; upright, the rest leans italic -- so a TAB that landed on a
    ;; mere common prefix (no such file yet) is telling at a glance,
    ;; without another TAB to ask.
    (define (exists? p)
      (guard (ex [else #f]) (file-exists? (expand-path p))))
    (prompt-styler label
      (lambda (path)
        (let* ([v (make-vector (string-length path) 'plain)]
               [split                    ; length of the existing prefix
                (let loop ([k (string-length path)])
                  (cond [(= k 0) 0]
                        [(exists? (substring path 0 k)) k]
                        [else (loop (let prev ([i (- k 2)])
                                      (cond [(< i 0) 0]
                                            [(char=? (string-ref path i) #\/)
                                             (+ i 1)]
                                            [else (prev (- i 1))])))]))])
          (vector-fill-range! v split (string-length path) 'italic)
          v))))

  (define (save!!)
    (if file-name
        (save-file! file-name)
        (let ([s (parameterize ([echo-highlight
                                 (file-prompt-styler "Write file: ")])
                   (prompt! "Write file: " complete-file-name
                            (default-directory)))])
          (when (and s (> (string-length s) 0)) (save-file! s))))
    (void))

  (define (save-as!!)
    ;; Prompt for a path -- prefilled with the current file, ready to
    ;; edit -- and save the buffer there: the buffer visits the new
    ;; file from then on, its name and mode following.
    (let ([s (parameterize ([echo-highlight (file-prompt-styler "Save as: ")])
               (prompt! "Save as: " complete-file-name
                        (if file-name
                            (abbreviate-path (absolute-path file-name))
                            (default-directory))
                        (box (log-history 'save-file! cdr))))])
      (when (and s (> (string-length s) 0)) (save-file! s)))
    (void))

  (define (find-file!!)
    ;; Visiting a file never loses the old buffer, so no confirmation
    ;; needed.  Up and down browse the paths visited before, off the
    ;; log.
    (let ([s (parameterize ([echo-highlight
                             (file-prompt-styler "Find file: ")])
               (prompt! "Find file: " complete-file-name (default-directory)
                        (box (log-history 'visit-file! cdr))))])
      (when (and s (> (string-length s) 0)) (visit-file! s))))

  (define (quit!!)
    (if (for-all buffer-clean? buffers)
        (set! quit? #t)
        (let ([answer (query-key!
                        "Modified buffers exist; quit anyway? y)es, n)o, v)iew"
                        "ynv")])
          (case (and answer (char-downcase answer))
            [(#\y) (set! quit? #t)]
            [(#\v)
             (let ([b (buffer-named "*buffers*")])
               (if b
                   (when (display-app! b)
                     ;; A direct app entry still receives the same
                     ;; initialization opportunity as its ordinary command.
                     (dispatch-app-event! "FOCUS")
                     (set! message ""))
                   (set-message! "The *buffers* app is not available")))]
            [else (void)]))))

  ;;; Interruptible execution -------------------------------------------------

  ;; A runaway computation run on the user's behalf (an M-x expression, a
  ;; shell command, ...) would freeze the editor, so for its duration the
  ;; terminal turns C-g into SIGINT (outside it the editor runs with
  ;; signals off), and SIGINT becomes a raised condition, answering #t to
  ;; interrupted?, that unwinds the computation -- C-g aborts an
  ;; evaluation just as it cancels a prompt.  Limitation: only running
  ;; Scheme can be interrupted this way -- a blocking foreign call runs
  ;; to completion.
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
      (terminal-isig! on)))

  (define (call-uninterrupted thunk)
    ;; Interaction also owns the cursor: while it runs, the cursor
    ;; follows the interaction's rules, not a parked evaluation's.
    (let ([old isig-on?])
      (dynamic-wind
        (lambda () (set-isig! #f))
        (lambda () (parameterize ([cursor-in-echo #f]) (thunk)))
        (lambda () (set-isig! old)))))

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

  ;;; Pasting and typed runs --------------------------------------------------

  (define (split-pasted-lines s)
    ;; Pasted text split at newlines, whichever convention the terminal
    ;; delivered: \n, \r\n, or bare \r.
    (let ([n (string-length s)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond [(= i n) (reverse (cons (substring s start i) acc))]
              [(char=? (string-ref s i) #\newline)
               (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
              [(char=? (string-ref s i) #\return)
               (let ([next (if (and (< (+ i 1) n)
                                    (char=? (string-ref s (+ i 1)) #\newline))
                               (+ i 2) (+ i 1))])
                 (loop next next (cons (substring s start i) acc)))]
              [else (loop (+ i 1) start acc)]))))

  (define pending-paste "")

  (define (read-paste)
    ;; The text of the paste event just consumed.
    pending-paste)

  (define (paste-into-buffer!)
    ;; A bracketed paste: the whole text becomes one labeled edit, its
    ;; newlines becoming real line breaks.
    (let ([text (read-paste)])
      (unless (string=? text "")
        (call-as-one-edit! (format "insert ~s" text)
          (lambda ()
            (let ([parts (split-pasted-lines text)])
              (insert-text! (car parts))
              (for-each (lambda (part) (newline!) (insert-text! part))
                        (cdr parts))))))))

  ;; Consecutive typed characters coalesce into one undo entry (up to
  ;; twenty, as in Emacs), so undo removes the run, not one character.
  ;; The chain is (buffer row col run-length text): where the next typed
  ;; character must land to continue the run.  Any other key breaks it.
  (define insert-chain #f)

  (define (self-insert! ch chain)
    (let ([b (window-buffer current-window)]
          [s (string ch)])
      (if (and chain
               (eq? (car chain) b)
               (= (cadr chain) point-row)
               (= (caddr chain) point-col)
               (< (cadddr chain) 20))
          (let ([text (string-append (list-ref chain 4) s)])
            (parameterize ([suppress-history #t]) (insert-text! s))
            (relabel-last-edit! (format "insert ~s" text))
            (set! insert-chain
              (list b point-row point-col (+ (cadddr chain) 1) text)))
          (begin
            (insert-text! s)
            (set! insert-chain (list b point-row point-col 1 s))))))

  ;;; Mouse -------------------------------------------------------------------

  ;; SGR mouse tracking: clicks focus the window under the pointer and
  ;; place point at the clicked cell, dragging selects as though the
  ;; mark were set at the press (C-Space) and point moved, and the
  ;; wheel scrolls the window under the pointer, wherever the focus is.
  ;; The cost is the terminal's native mouse selection -- hold Shift
  ;; for that -- so mouse! turns the whole thing on or off at run time.
  (define mouse-on? #f)

  (define (set-mouse! on)
    (set! mouse-on? on)
    (ansi (if on "\x1b;[?1002;1006h" "\x1b;[?1002;1006l"))
    (flush-output-port (terminal-output-port)))

  (define (mouse! on)
    ;; Turn mouse tracking on or off (off restores native selection).
    (set-mouse! on)
    (set! message (format "Mouse ~a" (if on "on" "off")))
    (void))

  (define (window-at x0 r0 receiver)
    ;; Call receiver with the layout entry containing 0-based screen
    ;; position (x0, r0) (text rows or the status line); #f in the
    ;; echo area or on a divider.
    (let loop ([entries (window-layout)])
      (cond [(null? entries) #f]
            [(and (<= (cadr (car entries)) r0
                      (+ (cadr (car entries)) (caddr (car entries))))
                  (<= (window-xoff (caar entries)) x0
                      (+ (window-xoff (caar entries))
                         (window-width (caar entries))
                         -1)))
             (receiver (car entries))]
            [else (loop (cdr entries))])))

  (define last-press #f)   ; (x y ms) of the previous button press

  (define (window-button-at x0 r0)
    ;; The three bracketed controls occupy the last nine status columns;
    ;; a pop-up window shows only its close control in the last three.
    (window-at x0 r0
      (lambda (entry)
        (let ([w (car entry)])
          (and (= r0 (+ (cadr entry) (caddr entry)))
               (let ([from-end (- (+ (window-xoff w) (window-width w)) x0)])
                 (cond [(<= 1 from-end 3) (cons 'close w)]
                       [(<= 4 from-end 6) (cons 'right w)]
                       [(<= 7 from-end 9) (cons 'below w)]
                       [else #f])))))))

  (define (word-char? c)
    (not (or (char-whitespace? c)
             (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\, #\.)))))

  (define (select-word!)
    ;; Select the word point is on (or just after): mark at its start,
    ;; point at its end.
    (let* ([s (current-line)]
           [n (string-length s)]
           [on? (lambda (i)
                  (and (>= i 0) (< i n) (word-char? (string-ref s i))))]
           [col (cond [(on? point-col) point-col]
                      [(on? (- point-col 1)) (- point-col 1)]
                      [else #f])])
      (when col
        (set! mark-row point-row)
        (set! mark-col (let back ([i col])
                         (if (on? (- i 1)) (back (- i 1)) i)))
        (set! point-col (let fwd ([i col])
                          (if (on? i) (fwd (+ i 1)) i)))
        (set! mark-active? #t))))

  ;; The window whose status bar is being dragged to resize it, or #f.
  ;; 1-based cell coordinates within the app's text viewport while a mouse
  ;; event is dispatched, or #f for keyboard events.
  (define app-event-position (make-parameter #f))
  ;; Raw xterm button code (including modifier/motion bits) for an app mouse
  ;; event, or #f for keyboard events.
  (define app-event-button (make-parameter #f))
  ;; The unclamped (row . column) addressed by an app content click. This can
  ;; lie beyond the buffer and lets apps distinguish empty viewport space from
  ;; their last rendered line.
  (define app-event-buffer-position (make-parameter #f))
  (define drag-divider #f)   ; recursive divider descriptor
  (define (divider-at x0 r0)
    ;; Divider metadata comes directly from window-layout. A descriptor is
    ;; (orientation split x y span).
    (define (hit? orientation d)
      (and (eq? (car d) orientation)
           (if (eq? orientation 'right)
               (and (= x0 (caddr d))
                    (<= (cadddr d) r0)
                    (< r0 (+ (cadddr d) (list-ref d 4))))
               (and (= r0 (cadddr d))
                    (<= (caddr d) x0)
                    (< x0 (+ (caddr d) (list-ref d 4)))))))
    (window-layout)
    ;; A `┴` crossing visually belongs to the spanning horizontal split.
    (or (find (lambda (d) (hit? 'below d)) layout-dividers)
        (find (lambda (d) (hit? 'right d)) layout-dividers)))

  (define (transfer-split! split delta)
    ;; Normalize stale ratio weights to the currently realized cell extents,
    ;; then move the boundary. Thus one keyboard step is one cell even after a
    ;; terminal resize, and mouse dragging uses the same exact arithmetic.
    (unless (= delta 0)
      (let* ([layout (window-layout)]
             [orientation (layout-split-orientation split)]
             [first (layout-split-first split)]
             [second (layout-split-second split)])
        (define (extent node)
          (let ([entries
                 (map (lambda (w) (assq w layout)) (layout-leaves node))])
            (if (eq? orientation 'right)
                (- (apply max
                          (map (lambda (entry)
                                 (+ (window-xoff (car entry))
                                    (window-width (car entry))))
                               entries))
                   (apply min
                          (map (lambda (entry) (window-xoff (car entry)))
                               entries)))
                (- (apply max
                          (map (lambda (entry)
                                 (+ (cadr entry) (caddr entry) 1))
                               entries))
                   (apply min (map cadr entries))))))
        (let* ([one (extent first)] [two (extent second)]
               [m1 (if (eq? orientation 'right)
                       (layout-min-width first)
                       (layout-min-height first))]
               [m2 (if (eq? orientation 'right)
                       (layout-min-width second)
                       (layout-min-height second))]
               [delta (min delta (- two m2))]
               [delta (max delta (- m1 one))])
          (layout-split-first-weight-set! split (+ one delta))
          (layout-split-second-weight-set! split (- two delta))))))

  (define (window-position w start height x y)
    ;; The buffer (row . col) at 1-based screen (x, y) inside w's text
    ;; band, wrap-aware: wrapped lines occupy successive screen rows,
    ;; so the band row is walked through the segment counts.
    (let* ([v (buffer-lines (window-buffer w))]
           [sticky (buffer-sticky-lines (window-buffer w))]
           [k (max 0 (- y 1 start))]
           [col (max 0 (- x 1 (window-xoff w)
                          (if (eq? (window-scrollbar? w) 'left) 1 0)
                          (window-line-number-width w)))])
      (cond
        [(< k sticky)
         (cons (min k (- (vector-length v) 1)) col)]
        [(window-wrapped? w)
         (let loop ([i (max sticky (window-top w))]
                    [k (+ (- k sticky) (window-topseg w))])
           (if (>= i (vector-length v))
               ;; Preserve the addressed row outside the buffer. The point
               ;; setter clamps ordinary clicks; app handlers also receive
               ;; this raw position so blank viewport space stays distinct
               ;; from the final rendered line.
               (cons i col)
               (let* ([line (vector-ref v i)]
                      [breaks (line-breaks w line)]
                      [segs (vector-length breaks)])
                 (if (< k segs)
                     (cons i (min (+ (segment-start breaks k) col)
                                  (segment-close breaks k
                                                 (string-length line))))
                     (loop (+ i 1) (- k segs))))))]
        [else
         (cons (+ (max sticky (window-top w)) (- k sticky))
               (+ (window-left w) col))])))

  (define (mouse-press! x y button)
    ;; A normal-buffer press focuses its window and places point. An app text
    ;; press instead updates and invokes the app without stealing focus; only
    ;; an app status-bar press focuses that window. A text press also arms the
    ;; mark there -- dragging activates it, a motionless click does not;
    ;; a second press on the same cell within half a second is a double
    ;; click, selecting the word there.  A press on a status bar (other
    ;; than the lowest) arms a resize drag instead.
    ;; The terminal's own Shift-selection highlight is not touched here
    ;; (erasing on every press flickers); C-l clears it.
    (set! drag-divider #f)
    (let ([prev last-press]
          [now (real-time)])
      (define (arm-text-selection!)
        (set! mark-row point-row)
        (set! mark-col point-col)
        (set! mark-active? #f)
        (when (and prev
                   (= (car prev) x) (= (cadr prev) y)
                   (< (- now (caddr prev)) 450))
          (select-word!)))
      (set! last-press (list x y now))
      (cond
        [(window-button-at (- x 1) (- y 1)) =>
         (lambda (button)
           (let ([action (car button)] [w (cdr button)])
             (cond
               [else
                (focus-window! w)
                (case action
                  [(below) (split-window!)]
                  [(right) (split-window-right!)]
                  [(close) (delete-window!)])]))
           "MOUSE-HANDLED")]
        [(divider-at (- x 1) (- y 1)) =>
         (lambda (divider)
           ;; a below divider doubles as the upper window's status bar:
           ;; pressing it focuses that window, as any status bar does,
           ;; and still arms the drag
           (when (eq? (car divider) 'below)
             (window-at (- x 1) (- y 1)
               (lambda (entry) (focus-window! (car entry)))))
           (set! drag-divider divider)
           "MOUSE-HANDLED")]
        [else
         (window-at (- x 1) (- y 1)
           (lambda (entry)
             (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
               (cond
                 [(= (- y 1) (+ start height))        ; the status bar
                  (focus-window! w)
                  "MOUSE-HANDLED"]
                 [(and (window-scrollbar-column w)
                       (= (- x 1) (window-scrollbar-column w)))
                  ;; App bars navigate like their wheel controls: they do not
                  ;; take focus and do not invoke the row's click action.
                  (let ([old current-window])
                    (unless (app-buffer? (window-buffer w))
                      (focus-window! w))
                    (set! current-window w)
                    (when (and (app-buffer? (window-buffer w))
                               (memq old windows))
                      (set! current-window old)))
                  "MOUSE-HANDLED"]
                 [(app-buffer? (window-buffer w))
                  (let ([old current-window])
                    (unless (eq? w old)
                      (set-app-target! (window-buffer w) old
                                       (window-buffer old)))
                    (set! current-window w)
                    (let ([old-point (point)]
                          [clicked (window-position w start height x y)])
                      (goto-point! clicked)
                      (set! mark-active? #f)
                      ;; Focusing the clicked window is the default. An app may
                      ;; perform a target action and explicitly preserve the
                      ;; old focus by returning keep-focus for MOUSE-CLICK.
                      (let ([result
                             (parameterize
                               ([app-event-buffer-position clicked]
                                [app-event-button button])
                               (dispatch-app-event! "MOUSE-CLICK"))])
                        (cond [(eq? result 'ignore-click)
                               (goto-point! old-point)
                               (when (memq old windows)
                                 (set! current-window old))]
                              [(and (eq? result 'keep-focus) (memq old windows))
                               (set! current-window old)]
                              [(not result)
                               ;; Views and unhandled app text select like
                               ;; ordinary read-only buffer text. Arm the mark at
                               ;; this press instead of reusing stale state.
                               (arm-text-selection!)])))
                    "MOUSE-HANDLED")]
                 [else                                ; a text row
                  (focus-window! w)
                  (goto-point! (window-position w start height x y))
                  (arm-text-selection!)
                  ;; A mode may act on the click -- following a link,
                  ;; say -- through a MOUSE-CLICK binding in its keymap.
                  (let ([context (mode-key-context)])
                    (when context
                      (let ([action (key-event-binding context
                                                       "MOUSE-CLICK")])
                        (when (procedure? action)
                          (guard (ex [else
                                      (set! message (error-text ex))])
                            (action))))))
                  "MOUSE-HANDLED"]))))])))

  (define (mouse-drag! x y button)
    ;; A split-divider drag resizes its two subtrees; otherwise extend
    ;; the selection armed by the press --
    ;; the mark activates and point follows the pointer within the
    ;; focused window's text area.
    (cond
      [drag-divider
       (let* ([orientation (car drag-divider)]
              [split (cadr drag-divider)]
              [old (if (eq? orientation 'right)
                       (caddr drag-divider)
                       (cadddr drag-divider))]
              [now (if (eq? orientation 'right) (- x 1) (- y 1))]
              [delta (- now old)])
         (unless (= delta 0)
           (transfer-split! split delta)
           (if (eq? orientation 'right)
               (set-car! (cddr drag-divider) now)
               (set-car! (cdddr drag-divider) now))))]
      [else
       (window-at (- x 1) (- y 1)
         (lambda (entry)
           (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
             (when (and (eq? w current-window)
                        (< (- y 1) (+ start height)))
               (goto-point! (window-position w start height x y))
               (if (app-buffer? (window-buffer w))
                   (unless (parameterize ([app-event-button button])
                             (dispatch-app-event! "MOUSE-DRAG"))
                     (set! mark-active? #t))
                   (set! mark-active? #t))))))]))

  (define (mouse-release! x y button)
    (window-at (- x 1) (- y 1)
      (lambda (entry)
        (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
          (when (and (eq? w current-window)
                     (< (- y 1) (+ start height))
                     (app-buffer? (window-buffer w)))
            (goto-point! (window-position w start height x y))
            (parameterize ([app-event-button button])
              (dispatch-app-event! "MOUSE-RELEASE")))))))

  (define (mouse-wheel! x y button dir meta? shift?)
    ;; Scroll the window under the pointer; the focused window stays focused.
    ;; Meta-wheel
    ;; applies the corresponding global buffer-switch binding to the hovered
    ;; window instead. Apps get an ordinary directional tick first so list
    ;; controls can choose their wheel step.
    (window-at (- x 1) (- y 1)
      (lambda (entry)
        (let ([old current-window]
              [w (car entry)])
          (set! current-window w)
          (if (and meta? (memv dir '(0 1)))
              (dispatch-sequence! (if (= dir 0) "M-S-UP" "M-S-DOWN") #f)
              (begin
                (when (and (app-buffer? (window-buffer w)) (not (eq? w old)))
                  (set-app-target! (window-buffer w) old (window-buffer old)))
                (unless (parameterize
                          ([app-event-position
                            (cons (max 1 (- x (window-xoff w)))
                                  (max 1 (- y (cadr entry))))]
                           [app-event-button button])
                          (dispatch-app-event!
                            (string-append
                              (if shift? "S-" "")
                              (case dir
                                [(0) "WHEEL-UP"]
                                [(1) "WHEEL-DOWN"]
                                [(2) "WHEEL-LEFT"]
                                [(3) "WHEEL-RIGHT"]
                                [else "WHEEL"]))))
                  ((wheel-mover dir)))))
          (when (memq old windows) (set! current-window old))
          "MOUSE-HANDLED"))))

  (define (wheel-mover dir)
    ;; Wheel direction (the low bits of a 64-flagged button): up, down,
    ;; left, right. Vertical ticks move the hovered viewport by one eighth
    ;; of its height; horizontal ones move point sideways within its line.
    (case dir
      [(0) (lambda () (page-window! -1 8))]
      [(1) (lambda () (page-window! 1 8))]
      [(2) (lambda () (goto-point! (cons point-row (- point-col 3))))]
      [(3) (lambda () (goto-point! (cons point-row (+ point-col 3))))]
      [else (lambda () (void))]))

  ;; Input decoding lives in the (tty) seam module now: the reader
  ;; thread calls (tty:read-event stdin); the main thread applies the
  ;; parsed data below.
  (define character-event tty:character-event)
  (define key-event-character tty:key-event-character)

  (define (apply-mouse-event! handle? c b x y)
    ;; Wheel is button 64/65; releases are ignored.  A context that
    ;; must not change editor focus passes handle? #f: the report is
    ;; consumed without being applied.
    (and handle?
         (cond [(char=? c #\m)                         ; release
                (mouse-release! x y b)
                (set! drag-divider #f)
                "MOUSE-HANDLED"]
               [(= (bitwise-and b 64) 64)               ; wheel
                (mouse-wheel! x y b (bitwise-and b 3)
                              (= (bitwise-and b 8) 8)
                              (= (bitwise-and b 4) 4))]
               [(= (bitwise-and b 32) 32)               ; drag
                (when (< (bitwise-and b 3) 3)
                  (mouse-drag! x y b))
                "MOUSE-HANDLED"]
               [(< (bitwise-and b 3) 3)                 ; a press
                (mouse-press! x y b)]
               [else "MOUSE-HANDLED"])))

  ;;; Key handling ----------------------------------------------------------

  ;; Key syntax and the binding tables live in the (keymap) seam
  ;; module now; dispatch stays here, and these facade aliases stay
  ;; until call sites and extension modules migrate to keymap:
  ;; prefixes.
  (define key-spec keymap:key-spec)
  (define sequence-text keymap:sequence-text)
  (define bind-key! keymap:bind-key!)
  (define bind-default-key! keymap:bind-default-key!)
  (define unbind-key! keymap:unbind-key!)
  (define key-binding keymap:key-binding)
  (define key-event-binding keymap:key-event-binding)
  (define binding-prefix? keymap:binding-prefix?)
  (define command-keys keymap:command-keys)
  (define command-key keymap:command-key)
  (define command-hint keymap:command-hint)
  (define resolved-binding keymap:resolved-binding)
  (define binding-context keymap:binding-context)
  (define binding-action keymap:binding-action)
  (define binding-kind keymap:binding-kind)

  (define (settle-echo!) (echo:settle!))

  ;; The host's color scheme, learned from its DSR 997 reports (mode 2031
  ;; subscribes to them at startup): #f until the host says, then 'dark or
  ;; 'light. Hooks run on the main thread whenever a report arrives, so
  ;; the terminal module can forward the change to subscribed children.
  (define host-color-scheme-value #f)

  (define (host-color-scheme) host-color-scheme-value)

  (define color-scheme-hooks '())

  (define (add-color-scheme-hook! hook)
    (unless (procedure? hook)
      (error 'add-color-scheme-hook! "expected a procedure" hook))
    (set! color-scheme-hooks (cons hook color-scheme-hooks)))

  (define (note-color-scheme! scheme)
    (set! host-color-scheme-value scheme)
    (for-each (lambda (hook) (guard (ex [else (void)]) (hook scheme)))
              color-scheme-hooks))

  ;; The seat's loop -- the mailbox, the wake dedupe, posted thunks,
  ;; the reader thread, and the read-key-event pump -- lives in (head)
  ;; now.  The core installs what a frame does and how side effects
  ;; are consumed, and keeps these facade aliases.
  (define read-key-event head:read-key-event)
  (define run-on-main! head:run-on-main!)
  (define wake-main! head:wake-main!)
  (define in-main-pump head:in-main-pump)
  (define start-input-reader! head:start-input-reader!)

  (define (run-posted-thunk! thunk)
    (guard (ex [else (parameterize ([message-source 'run-on-main!])
                       (set-message! (error-text ex)))])
      (thunk)))

  (define pump-handlers-installed
    (head:set-pump-handlers!
      (lambda () (state-frame-sync!) (redraw!))
      run-posted-thunk!
      (lambda (handle? c b x y) (apply-mouse-event! handle? c b x y))
      (lambda (text) (set! pending-paste text))
      (lambda (scheme) (note-color-scheme! scheme))))

  (define (set-mark-command!)
    (set! mark-row point-row) (set! mark-col point-col)
    (set! mark-active? #t) (set! message "Mark set"))
  (define (beginning-of-line!) (set! point-col 0))
  (define (end-of-line!) (set! point-col (string-length (current-line))))
  (define (keyboard-quit!) (set! mark-active? #f) (set! message "Quit"))
  (define (redraw-command!)
    (set! size-dirty? #t) (erase-screen!) (set! message "Screen redrawn"))
  (define (open-line!)
    (let ([row point-row] [col point-col])
      (newline!) (set! point-row row) (set! point-col col)))
  (define (page-up!) (page-window! -1 1))
  (define (page-down!) (page-window! 1 1))
  (define (previous-line!) (move-vertical! -1))
  (define (next-line!) (move-vertical! 1))
  (define (beginning-of-buffer!) (set! point-row 0) (set! point-col 0))
  (define (end-of-buffer!)
    (set! point-row (- (vlen) 1))
    (set! point-col (string-length (current-line))))

  (define (run-key-action! action)
    (cond [(procedure? action) (action)]
          [(not action) (set! message "Key is unbound")]
          [else (error 'dispatch-key! "context action used globally" action)]))

  (define (mode-key-context)
    ;; A buffer mode may carry its own key bindings under a context
    ;; named after the mode; they take precedence over the global map
    ;; while a buffer of that mode is current.
    (let ([name (buffer-mode-name (current-buffer))])
      (and name (string->symbol name))))

  (define (dispatch-sequence! first chain)
    (let ([mode-context (mode-key-context)])
      (let loop ([sequence (list first)])
        (let ([hit (or (and mode-context
                            (resolved-binding mode-context sequence))
                       (resolved-binding 'global sequence))]
              [prefix? (or (and mode-context
                                (binding-prefix? mode-context sequence))
                           (binding-prefix? 'global sequence))])
          (cond
            [prefix?
             (set! message (string-append (sequence-text sequence) "-"))
             (set! echo-pending '())
             (redraw!)
             (let ([next (read-key-event)])
               (if (eof-object? next)
                 (set! quit? #t)
                 (loop (append sequence (list next)))))]
            [hit
             ;; A prefix is only a waiting indicator. Once its complete binding
             ;; is known, remove it before the command runs; commands that have
             ;; something useful to report will publish their own message.
             (when (> (length sequence) 1) (settle-echo!))
             (run-key-action! (binding-action (cdr hit)))]
            [(and (= (length sequence) 1)
               (key-event-character first))
             => (lambda (c) (self-insert! c chain))]
            [else
             (set! message
               (format "~a is undefined" (sequence-text sequence)))])))))

  (define (dispatch-app-event! event)
    (let* ([a (app-of (current-buffer))]
           [handler (and a (app-handle-event! a))]
           [result (and handler (handler event))])
      (or result (and a (app-capture a)))))

  (define (clear-capture-bypass!)
    (set! capture-bypass-app #f)
    (set! capture-escape-event #f)
    (set! capture-literal! #f))

  (define (handle-key! input)
    (define chain insert-chain)
    (set! insert-chain #f)
    (let ([event (cond [(eof-object? input) input]
                       [(char? input) (character-event input)]
                       [else input])])
      (cond
        [(eof-object? event) (set! quit? #t)]
        [(string=? event "MOUSE-HANDLED")
         (settle-echo!)
         (void)]
        [else
         (unless (string=? event "C-k") (set! last-command #f))
         (settle-echo!)
         (let ([a (app-of (current-buffer))])
           (if (and capture-bypass-app (eq? a capture-bypass-app))
               (let ([escape capture-escape-event]
                     [literal! capture-literal!])
                 (if (string=? event escape)
                     (begin (clear-capture-bypass!) (literal!))
                     ;; Keep the escaped state visible throughout prefixes and
                     ;; synchronous prompts. Capture resumes when the complete
                     ;; global command returns.
                     (dynamic-wind
                       (lambda () (void))
                       (lambda () (dispatch-sequence! event chain))
                       clear-capture-bypass!)))
               (begin
                 ;; Focus may have changed since an app requested escape.
                 (when capture-bypass-app (clear-capture-bypass!))
                 (unless (dispatch-app-event! event)
                   (dispatch-sequence! event chain)))))])))

  (define (action-name action)
    (cond
      [(not action) "unbound"]
      [(symbol? action) (symbol->string action)]
      [else
       (let ([sym
              (find
                (lambda (s)
                  (and (top-level-bound? s)
                       (guard (ex [else #f])
                         (eq? (top-level-value s) action))))
                (environment-symbols (interaction-environment)))])
         (if sym (symbol->string sym) "anonymous command"))]))

  (define (binding-origin owned)
    (let ([owner (car owned)] [kind (binding-kind (cdr owned))])
      (cond [(eq? owner 'config) "config.e (user override)"]
            [owner (format "module ~a (~a)" owner kind)]
            [(eq? kind 'default) "core default"]
            [else "current session (user override)"])))

  (define (read-described-sequence)
    (let loop ([sequence (list (read-key-event #f))])
      (if (binding-prefix? 'global sequence)
          (begin
            (set! message (format "Describe key: ~a-" (sequence-text sequence)))
            (redraw!)
            (loop (append sequence (list (read-key-event #f)))))
          sequence)))

  (define (describe-key!!)
    (parameterize ([message-source #f])
      (set-message! "Describe key: "))
    (redraw!)
    (let* ([sequence (read-described-sequence)]
           [all (keymap:sequence-bindings sequence)]
           [entries (filter
                      (lambda (owned)
                        (eq? (keymap:binding-context (cdr owned)) 'global))
                      all)]
           [resolved (keymap:choose-binding entries)]
           [b (fresh-buffer "*help*")])
      (buffer-append! b
        (sequence-text sequence)
        ""
        (if resolved
            (format "Resolved to: ~a" (action-name (binding-action (cdr resolved))))
            "Resolved to: self-insert or undefined")
        "Keymap: global"
        (if resolved
            (format "Defined by: ~a" (binding-origin resolved))
            "Defined by: fallback"))
      (when (> (length entries) 1)
        (buffer-append! b "" "Shadowed bindings:")
        (for-each
          (lambda (owned)
            (unless (eq? owned resolved)
              (buffer-append! b
                (format "  ~a — ~a"
                        (action-name (binding-action (cdr owned)))
                        (binding-origin owned)))))
          entries))
      (let ([contexts
             (fold-left
               (lambda (acc owned)
                 (let ([context (binding-context (cdr owned))])
                   (if (or (eq? context 'global) (memq context acc))
                       acc
                       (append acc (list context)))))
               '() all)])
        (when (pair? contexts)
          (buffer-append! b "" "Contextual bindings:")
          (for-each
            (lambda (context)
              (let ([hit (resolved-binding context sequence)])
                (when hit
                  (buffer-append! b
                    (format "  ~a: ~a — ~a"
                            context
                            (action-name (binding-action (cdr hit)))
                            (binding-origin hit))))))
            contexts)))
      (buffer-read-only-set! b #t)
      (set! message "")
      (unless (pop-up-or-reuse! b)
        (set-message! "The *help* buffer could not be displayed"))))

  ;; Core defaults are data, just like module and config bindings.
  (define core-keys-bound
    (begin
      (for-each
        (lambda (entry) (bind-default-key! (car entry) (cadr entry)))
        `(("C-@" ,set-mark-command!) ("C-a" ,beginning-of-line!)
          ("C-b" ,move-left!) ("C-d" ,delete-forward!)
          ("C-e" ,end-of-line!) ("C-f" ,move-right!)
          ("C-g" ,keyboard-quit!) ("ESC" ,keyboard-quit!)
          ("BACKSPACE" ,backspace!)
          ("TAB" ,indent-tab!) ("RET" ,newline!) ("C-k" ,kill-line!)
          ("C-l" ,redraw-command!) ("C-n" ,next-line!)
          ("C-o" ,open-line!) ("C-p" ,previous-line!)
          ("C-v" ,page-down!) ("C-w" ,kill-region!) ("C-y" ,yank!)
          ("C-_" ,undo!) ("C-M-_" ,redo!) ("M-w" ,copy-region!)
          ("M-v" ,page-up!) ("M-<" ,beginning-of-buffer!)
          ("M-UP" ,focus-window-up!) ("M-DOWN" ,focus-window-down!)
          ("M-LEFT" ,focus-window-left!) ("M-RIGHT" ,focus-window-right!)
          ("M->" ,end-of-buffer!) ("UP" ,previous-line!)
          ("DOWN" ,next-line!) ("LEFT" ,move-left!)
          ("RIGHT" ,move-right!) ("HOME" ,beginning-of-line!)
          ("END" ,end-of-line!) ("DELETE" ,delete-forward!)
          ("PAGEUP" ,page-up!) ("PAGEDOWN" ,page-down!)
          ("PASTE" ,paste-into-buffer!) ("MOUSE" ,void)
          ("C-x C-g" ,keyboard-quit!) ("C-x C-s" ,save!!)
          ("C-x C-w" ,save-as!!) ("C-x C-c" ,quit!!)
          ("C-x C-f" ,find-file!!) ("C-x b" ,switch-buffer!!)
          ("C-x k" ,kill-buffer!!) ("C-x o" ,other-window!)
          ("C-x 0" ,delete-window!) ("C-x 1" ,delete-other-windows!)
          ("C-x 2" ,split-window!) ("C-x 3" ,split-window-right!)
          ("C-x l" ,line-numbers!) ("C-x t" ,wrap!)
          ("C-h k" ,describe-key!!)
          ("C-c a" ,answer!!)))
      (for-each
        (lambda (entry)
          (bind-default-key! 'prompt (car entry) (cadr entry)))
        '(("C-g" cancel) ("ESC" cancel) ("RET" accept)
          ("C-a" beginning) ("HOME" beginning)
          ("C-b" backward) ("LEFT" backward)
          ("C-e" end) ("END" end) ("C-f" forward) ("RIGHT" forward)
          ("UP" up) ("DOWN" down) ("C-d" delete-forward)
          ("DELETE" delete-forward) ("C-h" delete-backward)
          ("BACKSPACE" delete-backward) ("C-k" kill) ("C-y" yank)
          ("TAB" complete) ("S-TAB" alternate-complete)
          ("M-." inspect) ("M-RET" newline) ("PASTE" paste)))
      #t))

  ;;; Modules -----------------------------------------------------------------

  ;; Extension modules are libraries in the lib directory, loaded through
  ;; here -- by the loader at startup, or later by hand -- so the core
  ;; knows which modules exist and owns their registrations (see the
  ;; module registries above).

  ;; The lifecycle lives in the kernel now; the exported names stay
  ;; usable through these facade aliases, and the core hangs its
  ;; after-reload work on the kernel's hook.
  (define load-module! kernel:load-module!)
  (define reload-module! kernel:reload-module!)

  (define reload-tail-hooked
    (kernel:add-after-reload-hook!
      (lambda (name)
        (load-config!)              ; the settings reapply on top
        (refresh-buffer-modes!)
        (invalidate-screen-cache!)
        (set! message (format "Reloaded ~a" name)))))

  (define (refresh-buffer-modes!)
    ;; Re-resolve every buffer's mode by name, so buffers pick up a
    ;; reloaded mode's new styles (or lose a mode that is gone).
    (for-each (lambda (b)
                (if (buffer-mode-auto b)
                    (assign-mode! b)
                    (let ([m (buffer-mode b)])
                      (when m
                        (buffer-mode-set! b (find-mode (mode-name m)))))))
              buffers))

  ;; Saving a module's source reloads it on the spot (a fresh .e file
  ;; in the lib directory is loaded for the first time), and saving
  ;; config.e applies it, so editing the editor from inside itself
  ;; takes effect on save.  Both on by default; (modules-reload-on-save
  ;; #f) or (config-reload-on-save #f) -- in config.e for an
  ;; installation, at M-x for a session -- turns either off.
  (define modules-reload-on-save (make-parameter #t))
  (define config-reload-on-save (make-parameter #t))

  (define (canonical-path path*)
    ;; path made absolute, with ".", "..", and empty segments resolved
    ;; textually (symbolic links are not chased) -- enough to recognize
    ;; the editor's own files whichever way they are named.
    (let* ([path (if (string-prefix? "/" path*)
                     path*
                     (string-append (current-directory) "/" path*))]
           [n (string-length path)])
      (let loop ([i 0] [start 0] [stack '()])
        (define (push seg)
          (cond [(or (string=? seg "") (string=? seg ".")) stack]
                [(string=? seg "..") (if (pair? stack) (cdr stack) stack)]
                [else (cons seg stack)]))
        (cond [(> i n) (string-append "/" (string-join (reverse stack) "/"))]
              [(or (= i n) (char=? (string-ref path i) #\/))
               (loop (+ i 1) (+ i 1) (push (substring path start i)))]
              [else (loop (+ i 1) start stack)]))))

  (define (module-name-of-path path)
    ;; The module name a saved path denotes: a .e file directly in the
    ;; editor's lib directory; #f for anything else -- the core included,
    ;; which cannot be reloaded.
    (let ([full (canonical-path path)]
          [lib (string-append (canonical-path (caar (library-directories)))
                              "/")])
      (and (string-prefix? lib full)
           (string-suffix? ".e" full)
           (let ([base (string-tail full (string-length lib))])
             (and (not (string-search base "/" 0 (string-length base)))
                  (not (string=? base "core.e"))
                  (substring base 0 (- (string-length base) 2)))))))

  (define (config-file)
    ;; config.e next to the loader script: the lib directory's parent.
    (string-append (caar (library-directories)) "/../config.e"))

  (define (load-config!)
    ;; The user's configuration: config.e, plain expressions evaluated
    ;; in the editor's top level (the M-x environment).  Loaded at
    ;; startup once the modules are up, and again after every module
    ;; reload so its settings reapply on top of fresh registrations --
    ;; it must tolerate being loaded any number of times.  Its own
    ;; registrations are owned like a module's, retracted before each
    ;; load, so nothing accumulates.  -> whether it loaded cleanly; an
    ;; error reports and leaves the rest of the file unread.
    (let ([path (config-file)])
      (and (file-exists? path)
           (begin
             (retract-module! 'config)
             ;; a recolor must repaint rows cached under the old codes
             (invalidate-screen-cache!)
             (guard (ex [else (parameterize ([message-source 'config])
                                (set-message!
                                  (format "Error in config.e: ~a"
                                    (error-text ex))))
                              #f])
               (parameterize ([registering-module 'config])
                 (load path))
               (refresh-buffer-modes!)
               (invalidate-screen-cache!)
               #t)))))

  (define (reload-on-save! path)
    ;; The post-save hook.  A reload that fails (a module saved mid-edit,
    ;; say) reports itself without disturbing the save -- or the editor,
    ;; which keeps running the module's old version.  A saved config.e
    ;; applies on the spot the same way.
    (let ([name (and (modules-reload-on-save) (module-name-of-path path))])
      (cond
        [name
         (guard (ex [else (parameterize ([message-source 'reload-module!])
                            (set-message!
                              (format "Reload of ~a failed: ~a"
                                      name (error-text ex))))])
           (reload-module! name)
           (parameterize ([message-source 'reload-module!])
             (set-message! (format "Reloaded ~a" name))))]
        [(and (config-reload-on-save)
              (string=? (canonical-path path) (canonical-path (config-file))))
         (when (load-config!)
           (parameterize ([message-source 'config])
             (set-message! "Applied config.e")))])))

  ;; the core's own post-save hook: the reload lives there like any
  ;; module's
  (define reload-hooked (add-post-save-hook! reload-on-save!))

  ;;; Main ------------------------------------------------------------------

  (define (usage)
    (display "Usage: e [file]\n")
    (display "A tiny Emacs-like terminal editor. Set LINES/COLUMNS if needed.\n")
    (display "Extension modules are loaded from the lib directory at startup.\n"))

  (define startup-page #f)

  (define (set-startup-page! proc)
    ;; A module (or config.e) may present a welcome page when e starts
    ;; without a file argument; #f restores the plain scratch buffer.
    (unless (or (not proc) (procedure? proc))
      (error 'set-startup-page! "expected a procedure or #f" proc))
    (set! startup-page proc))

  (define (main)
    ;; The loader script is pure bootstrap; the extension modules are
    ;; loaded here, before the file argument needs their modes.
    (let ([args (command-line-arguments)])
      (when (and (pair? args) (member (car args) '("-h" "--help"))) (usage) (exit 0))
      ;; the log-view module lists *log* from startup
      (for-each
        (lambda (failure)
          (let ([msg (format "Error in ~a: ~a"
                             (car failure) (error-text (cdr failure)))])
            (display (format "e: ~a\n" msg) (current-error-port))
            (set! message msg)))
        (reverse (kernel:load-modules!)))
      (load-config!)
      (if (pair? args)
          (visit-file! (car args))
          (when startup-page
            (guard (ex [else (void)]) (startup-page))
            ;; the greeting outlives the page's own load chatter
            (set! message (startup-greeting)))))
    (unless (and (getenv "TERM") (not (string=? (getenv "TERM") "dumb")))
      (display "e: an interactive terminal is required\n" (current-error-port))
      (exit 1))
    ;; A stray SIGINT outside an evaluation must not drop into Chez's break
    ;; prompt underneath the editor's screen.
    (keyboard-interrupt-handler void)
    ;; A stray (exit) or (abort) evaluated at the prompt must not kill
    ;; the process past the modified-buffers check: they run the
    ;; editor's quit and unwind the evaluation instead.
    (let ([safe-quit (lambda args
                       (quit!!)
                       (raise (make-interrupted)))])
      (exit-handler safe-quit)
      (abort-handler safe-quit)
      (reset-handler safe-quit))
    (dynamic-wind
      ;; The alternate screen, plus bracketed paste: terminals that
      ;; support it (virtually all) wrap pastes in ESC[200~ / ESC[201~,
      ;; making a paste one identifiable edit; others ignore the mode.
      ;; Mouse tracking likewise (see mouse!).
      ;; Mode 2031 subscribes to the host's color-scheme change reports
      ;; and DSR 996 asks for the current one; hosts without the feature
      ;; ignore both.
      (lambda () (terminal-raw!)
        (ansi "\x1b;[?1049h\x1b;[2J\x1b;[?2004h\x1b;[?2031h\x1b;[?996n")
        (set-mouse! #t)
        (set! screen-live? #t)
        (start-input-reader!))
      (lambda ()
        (let loop ()
          (unless quit?
            (for-each run-posted-thunk! (head:take-deferred!))
            (run-pre-redraw-hooks!)
            (redraw!)
            ;; A command that raises (a read-only buffer, a bug in an
            ;; extension module) reports itself instead of killing the
            ;; editor.
            (guard (ex [(read-only-error? ex)
                        (set! message "Buffer is read-only")]
                       [(refusal? ex)
                        (set! message (condition-message ex))]
                       [else (parameterize ([message-source 'error])
                               (set-message! (error-text ex)))])
              (handle-key! (parameterize ([in-main-pump #t])
                             (read-key-event))))
            (clamp-point!)
            (loop))))
      (lambda ()
        (run-shutdown-hooks!)
        (set! screen-live? #f)
        (unless (string=? cursor-style-shown "\x1b;[0 q")
          (ansi "\x1b;[0 q"))
        (ansi "\x1b;[?1002;1006l\x1b;[?2031l\x1b;[?2004l\x1b;[?25h\x1b;[?1049l\x1b;[0m")
        (flush-output-port (terminal-output-port))
        (terminal-restore!))))

) ;; library (core)
