;; Optional interactive departure policy; the generic pump owns teardown.
(import (only (foundation edoc) elibrary))
(elibrary (head lifecycle)
  (export quit! shutdown! shutdown-on-exit)
  (import (chezscheme) (prefix (core client) client:) (prefix (foundation string) string:)
    (prefix (head head) head:) (prefix (head interaction) interaction:)
    (prefix (head prompt) prompt:) (prefix (head widget) widget:))

  (edoc "Whether quitting the last head offers to stop the base. Cancelling keeps this head attached."
    (value boolean))
  (define shutdown-on-exit (make-parameter #f
                             (lambda (value) (unless (boolean? value) (error 'shutdown-on-exit "expected a boolean")) value)))

  (edoc "Save the base and stop every head, reviewing live terminals, other heads, agents and pending interactions. Documents and persistent views survive. A composition may supply a review command to inspect work. Acceptance uses the same save/stop path as SIGTERM."
    (prompts))
  (define (shutdown!)
    (let ([token #f] [viewer (widget:command-owner (or (widget:target) (widget:focused)) 'review)])
      (define (finish!)
        (when token (guard (ex [else (void)]) (client:request 'cancel-review token)) (set! token #f)))
      (interaction:flush!)
      (guard (ex [else (finish!) (raise ex)])
        (let review ([remote (client:request 'prepare-close)] [changed? #f])
          (set! token (cadr remote))
          (let* ([status (cadddr remote)] [count (lambda (key) (cdr (assq key status)))]
                 [risks (filter values
                          (map (lambda (n noun) (and (> n 0) (format "~a ~a~a" n noun (if (= n 1) "" "s"))))
                            (list (count 'terminals) (max 0 (- (count 'heads) 1)) (count 'agents) (count 'pending))
                            '("terminal" "other head" "agent session" "pending interaction")))]
                 [answer (if (null? risks) #\y
                           (prompt:key!
                             (format "~aStop the base? ~a. Text and views are saved. y)es, n)o~a"
                               (if changed? "Work changed; " "") (string:join risks ", ") (if viewer ", v)iew" ""))
                             (if viewer "ynv" "yn")))])
            (case (and answer (char-downcase answer))
              [(#\y) (interaction:flush!) (review (client:request 'shutdown token) #t)]
              [(#\v) (finish!) (widget:invoke! viewer 'review)]
              [else (void)])))
        (finish!))))

  (edoc "Detach this head after publishing logical interaction, keeping its documents and persistent composition in the base. If shutdown-on-exit is enabled, the base decides whether this is the last head and reviews stopping it."
    (prompts))
  (define (quit!)
    (interaction:flush!)
    (if (not (shutdown-on-exit)) (head:quit!)
      (let ([result (client:leave! #t)])
        (if (and (pair? result) (eq? (car result) 'last)) (shutdown!) (head:quit!)))))
)
