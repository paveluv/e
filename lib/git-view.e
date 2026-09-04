;; git-view.e -- interactive Git history and patch views.

(library (git-view)
  (export init! git-log!! git-log-refresh!)
  (import (chezscheme) (core)
          (prefix (modes) modes:)
          (prefix (strings) strings:)
          (prefix (paint) paint:)
          (prefix (head) head:)
          (prefix (keymap) keymap:) (except (git) init!)
          (only (describe) register-descriptions!))

  (define log-buffer #f)
  (define diff-buffer #f)
  (define repository #f)
  (define log-rows '())
  (define log-lines '())
  (define selected-patch #f)
  (define log-dirty? #t)
  (define diff-dirty? #t)
  (define refresh-label "[refresh]")
  (define refresh-column 0)
  (define refresh-pressed? #f)

  (define (pad text width)
    (string-append text (make-string (max 0 (- width (string-length text)))
                                     #\space)))

  (define (state-letter state)
    (case state
      [(modified) "M"] [(added) "A"] [(deleted) "D"]
      [(renamed) "R"] [(copied) "C"] [(type-changed) "T"]
      [(unmerged) "U"] [else "?"]))

  (define (short-hash commit)
    (substring (git-commit-hash commit) 0
               (min 10 (string-length (git-commit-hash commit)))))

  (define (commit-date commit)
    (let ([date (time-utc->date
                  (make-time 'time-utc 0 (git-commit-time commit)))])
      (format "~4,'0d-~2,'0d-~2,'0d"
              (date-year date) (date-month date) (date-day date))))

  (define (change-label change)
    (let ([path (git-diff-path change)]
          [old (git-diff-original-path change)])
      (if old (format "~a -> ~a" old path) path)))

  (define (load-log! repo)
    (let ([rows '()] [lines '()])
      (for-each
        (lambda (commit)
          (set! rows (cons (list 'commit commit) rows))
          (set! lines
            (cons (format "~a  ~a  ~a  ~a"
                          (short-hash commit) (commit-date commit)
                          (pad (git-commit-author-name commit) 20)
                          (git-commit-subject commit))
                  lines))
          (for-each
            (lambda (change)
              (set! rows (cons (list 'file commit change) rows))
              (set! lines
                (cons (format "    ~a  ~a"
                              (state-letter (git-diff-status change))
                              (change-label change))
                      lines)))
            (git-commit-files repo commit)))
        (git-log repo 20))
      (set! log-rows (reverse rows))
      (let ([prefix (format "Git log: ~a  " (git-repository-path repo))])
        (set! refresh-column (string-length prefix))
        (set! log-lines
          (cons (string-append prefix refresh-label) (reverse lines))))
      (set! log-dirty? #t)))

  (define (refresh-log!)
    (when log-dirty?
      (head:view-replace! log-buffer log-lines)
      (set! log-dirty? #f)))

  (define (refresh-diff!)
    (when diff-dirty?
      (head:view-replace!
        diff-buffer
        (if selected-patch
            (cons (format "~a  ~a  ~a"
                          (git-patch-path selected-patch)
                          (substring (git-patch-commit selected-patch) 0 10)
                          (git-repository-path repository))
                  (map git-patch-line-text
                       (git-patch-lines selected-patch)))
            '("No patch selected")))
      (set! diff-dirty? #f)))

  (define (row-at-point)
    (let ([row (car (point))])
      (and (<= 1 row (length log-rows))
           (list-ref log-rows (- row 1)))))

  (define (move-row! delta)
    (when (pair? log-rows)
      (goto-point!
        (cons (min (length log-rows)
                   (max 1 (+ (max 1 (car (point))) delta)))
              0))))

  (define (show-row-diff!)
    (let ([row (row-at-point)])
      (when (and row (eq? (car row) 'file))
        (set! selected-patch
          (git-file-patch repository (cadr row)
                          (git-diff-path (caddr row))))
        (set! diff-dirty? #t)
        (refresh-diff!)
        (show-buffer! diff-buffer))))

  (define (reload-log!)
    (unless repository (error 'git-log-refresh! "Git log is not open"))
    (load-log! repository)
    (refresh-log!)
    (when (eq? (current-buffer) log-buffer)
      (goto-point! (cons (if (null? log-rows) 0 1) 0)))
    (set-message! "Git log refreshed"))

  (define (git-log-refresh!)
    (let ([visible? (eq? (current-buffer) log-buffer)]
          [started (real-time)])
      (dynamic-wind
        (lambda ()
          (when visible?
            (set! refresh-pressed? #t)
            (paint:redraw!)))
        reload-log!
        (lambda ()
          (when visible?
            ;; Keep a very fast refresh visible as a press instead of a
            ;; one-frame color flicker.
            (let ([remaining (- 80 (- (real-time) started))])
              (when (> remaining 0)
                (sleep (make-time 'time-duration (* remaining 1000000) 0))))
            (set! refresh-pressed? #f))))))

  (define (refresh-button?)
    (and (= (car (point)) 0)
         (<= refresh-column (cdr (point)))
         (< (cdr (point)) (+ refresh-column (string-length refresh-label)))))

  (define (handle-log-event! event)
    (cond [(member event '("UP" "C-p")) (move-row! -1) #t]
          [(member event '("DOWN" "C-n")) (move-row! 1) #t]
          [(string=? event "WHEEL-UP") (move-row! -1) #t]
          [(string=? event "WHEEL-DOWN") (move-row! 1) #t]
          [(member event '("r" "R")) (git-log-refresh!) #t]
          [(string=? event "RET") (show-row-diff!) #t]
          [(string=? event "MOUSE-CLICK")
           (if (refresh-button?) (git-log-refresh!) (show-row-diff!))
           'keep-focus]
          [else #f]))

  (define (fill-style line style)
    (make-vector (string-length line) style))

  (define (log-styles line)
    (cond [(strings:prefix? "Git log:" line)
           (let ([styles (fill-style line 'bold)])
             (when (<= (+ refresh-column (string-length refresh-label))
                       (string-length line))
               (vector-fill-range!
                 styles refresh-column
                 (+ refresh-column (string-length refresh-label))
                 (if refresh-pressed? 'active 'editor)))
             styles)]
          [(strings:prefix? "    " line)
           (let ([styles (fill-style line 'plain)])
             (when (> (string-length line) 5)
               (vector-set! styles 4 'keyword))
             styles)]
          [else
           (let ([styles (fill-style line 'plain)])
             (vector-fill-range! styles 0 (min 10 (string-length line))
                                 'keyword)
             (when (> (string-length line) 12)
               (vector-fill-range! styles 12
                                   (min 22 (string-length line)) 'comment))
             styles)]))

  (define (diff-styles line)
    (cond [(or (strings:prefix? "@@" line)
               (strings:prefix? "+++ " line)
               (strings:prefix? "--- " line))
           (fill-style line 'keyword)]
          [(strings:prefix? "+" line) (fill-style line 'string)]
          [(strings:prefix? "-" line) (fill-style line 'rainbow1)]
          [(or (strings:prefix? "diff --git " line)
               (strings:prefix? "index " line))
           (fill-style line 'comment)]
          [else (fill-style line 'plain)]))

  (define (ensure-git-buffers!)
    ;; Git views are application state, not startup furniture. Create them
    ;; together on first use; re-register existing buffers after a hot reload
    ;; without making fresh sessions expose empty Git buffers.
    (unless log-buffer
      (set! log-buffer
        (head:register-app! "*git-log*" refresh-log! handle-log-event!))
      (head:set-app-presentation! log-buffer 1 #t)
      (modes:choose! log-buffer "git-log"))
    (unless diff-buffer
      (set! diff-buffer (head:register-view! "*git-diff*" refresh-diff!))
      (head:set-app-presentation! diff-buffer 1 #t)
      (modes:choose! diff-buffer "git-diff")))

  (define (git-log!! . path)
    (let ([source (if (pair? path) (car path)
                      (or (head:buffer-file (current-buffer)) "."))])
      (ensure-git-buffers!)
      (set! repository (git-open source))
      (load-log! repository)
      (refresh-log!)
      (let ([w (display-buffer! log-buffer)])
        (when w
          (select-window! w)
          (goto-point! '(1 . 0))))
      (void)))

  (define (init!)
    (modes:register! "git-log" '() '() log-styles)
    (modes:register! "git-diff" '() '() diff-styles)
    ;; A reload after Git was opened reconnects its surviving app buffers;
    ;; ordinary startup remains lazy.
    (when (or (head:buffer-named "*git-log*") (head:buffer-named "*git-diff*"))
      (ensure-git-buffers!))
    (register-descriptions!
      '(((git-log!!) (("procedure" . "(git-log!! [path])")) "void"
         ("(git-view)") git-view "Git" #f
         "Open the interactive `*git-log*` app for the repository containing `path` or the current file. Navigate commits and changed files with Up and Down; press Enter on a file to show its read-only patch in the target window.")
        ((git-log-refresh!)
         (("procedure" . "(git-log-refresh!)")) "void"
         ("(git-view)") git-view "Git" #f
         "Reload commits and changed files in the open `*git-log*` app. The header's `[refresh]` button and the app's `r` key invoke this command.")))
    (paint:add-highlighter!
      (lambda ()
        (if (and log-buffer (memq log-buffer (buffer-list)))
            (let ([row (call-with-buffer log-buffer
                         (lambda () (car (point))))])
              (if (<= 1 row (- (buffer-line-count log-buffer) 1))
                  (let ([shadow
                         (list log-buffer row 0
                               (string-length (buffer-line log-buffer row))
                               'active-shadow)])
                    (if (eq? (current-buffer) log-buffer)
                        (list shadow
                              (list (selected-window) row 0
                                    (string-length
                                      (buffer-line log-buffer row))
                                    'active))
                        (list shadow)))
                  '()))
            '())))
    (keymap:bind-default-key! "C-x g" git-log!!)))
