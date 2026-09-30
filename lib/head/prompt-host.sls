;; Default outer placement; prompt controls themselves know no windows.
(import (only (foundation edoc) elibrary))
(elibrary (head prompt-host)
  (export init!)
  (import (chezscheme) (prefix (head head) head:)
          (prefix (head interaction) interaction:) (prefix (head prompt-control) prompt-control:)
          (prefix (head widget) widget:) (prefix (head window) window:)
          (prefix (state model) model:) (prefix (state view) view:))

  (define-record-type surface
    (fields root (mutable buffer) window origin previous rows (mutable prompts)))
  (define current #f)
  (define (field r k) (cdr (assq k r)))
  (define (visible? s)
    (and s (eq? (head:window-buffer (head:popup)) (surface-buffer s))))
  (define (children prompts)
    (map (lambda (p n) (list (string->symbol (format "prompt-~a" n)) (cadr p) '(grow 1))) prompts (iota (length prompts))))
  (define (arrange! s prompts)
    ;; The envelope revision also covers focus publication. Read the structural
    ;; guard after publishing our own preceding input, never before that fence.
    (interaction:flush!)
    (let* ([root (surface-root s)] [r (model:snapshot root)]
           [change (list root (field r 'revision) (children prompts) '((name . "prompt")))])
      (let-values ([(status rows)
                    (if (surface-buffer s) (widget:arrange! (list change))
                      (view:arrange! head:ui-actor (list change) '()))])
        (unless (eq? status 'applied) (error 'arrange! "prompt host changed" status)))
      (surface-prompts-set! s prompts)))
  (define (prepare-frame! s)
    (widget:prepare! (surface-root s) (max 1 (head:window-content-width (head:popup)))
      (max 1 (head:popup-rows))))
  (define (detach! s request)
    (let ([remaining (remp (lambda (p) (equal? request (car p))) (surface-prompts s))])
      (if (pair? remaining)
        (when (model:snapshot (surface-root s)) (arrange! s remaining) (prepare-frame! s))
        (begin
          (when (visible? s)
            (if (and (> (surface-rows s) 0) (memq (surface-previous s) (head:buffers)))
              (begin (head:set-window-buffer! (head:popup) (surface-previous s)) (head:show-popup! (surface-rows s)))
              (head:hide-popup!))
            (when (and (memq (surface-window s) (head:windows))
                    (eq? (head:window-buffer (surface-window s)) (surface-origin s)))
              (window:focus! (surface-window s))))
          (widget:unmount! (surface-root s))
          (when (surface-buffer s) (head:forget-buffer! (surface-buffer s)))
          (when (eq? s current) (set! current #f))))))
  (define (prepare)
    (let* ([w (head:current-window)] [b (head:window-buffer w)]
           [root (head:window-widget w)] [d (and root (interaction:snapshot root))]
           [s (and (visible? current) (eq? w (head:popup)) current)]
           [parent (and s (car (car (reverse (surface-prompts s)))))]
           [origin (list (cons 'view root) (cons 'generation (and d (view:generation d)))
                     (cons 'focus (and d (view:focus d))) (cons 'source (and d (view:source d))))])
      (values parent origin
        (lambda (request receiver)
          (let ([s (or s (make-surface
                           (view:create! head:ui-actor #f 'overlay 1 '((name . "prompt")) '() request)
                           #f w b (head:window-buffer (head:popup)) (head:popup-rows) '()))])
            (guard (ex [else (detach! s request) (raise ex)])
              (arrange! s (append (surface-prompts s) (list (list request receiver))))
              (unless (surface-buffer s)
                (surface-buffer-set! s (window:show-widget! (head:popup) (surface-root s)))
                (head:buffer-fact-set! (surface-buffer s) 'resume-kind #f)
                (head:buffer-fact-set! (surface-buffer s) 'internal #t))
              (set! current s)
              (head:show-popup! (head:popup-default-rows))
              (prepare-frame! s)
              (window:focus! (head:popup))
              (lambda () (detach! s request))))))))

  (edoc "Install the default prompt placement in the outer pop-up. Nested input shares one overlay tree; removing or replacing the host cancels its waiting callers.")
  (define (init!) (prompt-control:register-host! prepare)))
