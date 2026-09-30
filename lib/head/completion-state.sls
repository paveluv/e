;; Completion normalization and list state, independent of editing and placement.
(import (only (foundation edoc) elibrary))
(elibrary (head completion-state)
  (export choose! create dismiss! finish! normalize! refresh! snapshot)
  (import (chezscheme) (prefix (foundation string) string:)
          (prefix (head completion) completion:))

  (define-record-type state
    (fields primary transform (mutable text) (mutable position) (mutable generation)
      (mutable source) (mutable range) (mutable prepared) (mutable options)
      (mutable matches) (mutable index) (mutable candidates) (mutable page)
      (mutable note) (mutable preview) (mutable undo) (mutable preview-text)
      (mutable searcher) (mutable maker) (mutable needle) (mutable hit)))

  (edoc "Create a head-local completion session. The optional text transformer applies only to completion edits; text ownership and submission remain with the host."
        (primary any "primary source; its track procedure may supply a live search")
        (transform (or procedure #f) "text and character caret -> (text . caret)"))
  (define (create primary transform)
    (make-state primary transform "" 0 0 #f #f #f '() '() 0 #f 0 "" #f #f #f #f #f #f #f))

  (edoc "Read a prepared session as (generation text caret candidates note page-sequence source preview). False candidates means the list is hidden. This never invokes a provider."
        (s any "completion session") (returns list))
  (define (snapshot s)
    (list (state-generation s) (state-text s) (state-position s) (state-candidates s)
      (if (and (string=? (state-note s) "") (state-hit s))
        (cond [(car (state-hit s)) (format " [~a of ~a]" (car (state-hit s)) (cdr (state-hit s)))]
          [(> (string-length (or (state-needle s) "")) 0) " [no match]"] [else ""])
        (state-note s))
      (state-page s) (state-source s) (state-preview s)))

  (define (end-preview! s)
    (when (state-undo s) (guard (ex [else (void)]) ((state-undo s))))
    (state-preview-set! s #f) (state-undo-set! s #f) (state-preview-text-set! s #f))
  (define (end-search! s accepted?)
    (when (state-searcher s) (guard (ex [else (void)]) ((completion:searcher-done (state-searcher s)) accepted?)))
    (state-searcher-set! s #f) (state-maker-set! s #f) (state-needle-set! s #f) (state-hit-set! s #f))
  (define (track! s text position)
    (let* ([primary (state-primary s)]
           [wanted (and (completion:source? primary) (completion:source-track primary)
                     (guard (ex [else #f]) ((completion:source-track primary) text position)))])
      (cond
        [(not wanted) (end-search! s #f)]
        [else
         (unless (and (state-searcher s) (eq? (car wanted) (state-maker s)))
           (end-search! s #f) (state-maker-set! s (car wanted)) (state-searcher-set! s ((car wanted))))
         (unless (equal? (cdr wanted) (state-needle s))
           (state-needle-set! s (cdr wanted))
           (state-hit-set! s ((completion:searcher-find (state-searcher s)) (cdr wanted))))])))

  (edoc "Close reversible candidate and search previews. Only acceptance keeps a live search's chosen position; repeated cleanup is harmless."
        (s any "completion session") (accepted? boolean "whether input was accepted"))
  (define (finish! s accepted?) (end-preview! s) (end-search! s accepted?))

  (define (value candidate)
    (if (completion:candidate? candidate) (completion:candidate-value candidate) candidate))
  (define (prepared? s source text position)
    (equal? (state-prepared s) (list source text position)))
  (define (candidates! s candidates)
    (unless (equal? candidates (state-candidates s))
      (state-candidates-set! s candidates) (state-page-set! s 0)))
  (define (show! s candidates)
    (if (equal? candidates (state-candidates s)) (state-page-set! s (+ 1 (state-page s)))
      (candidates! s candidates)))

  (edoc "Forget normalization and the visible candidate page without changing authored input."
        (s any "completion session"))
  (define (dismiss! s)
    (state-source-set! s #f) (state-range-set! s #f) (state-prepared-set! s #f)
    (state-options-set! s '()) (state-matches-set! s '()) (state-index-set! s 0)
    (candidates! s #f))

  (edoc "Refresh completion against explicit authored text and caret. An already visible source updates its matches without calculating normalization alternatives. Call from input/service work, never painting."
        (s any "completion session") (text string "current authored input")
        (position integer "character caret"))
  (define (refresh! s text position)
    (unless (and (<= 0 position (string-length text)) (exact? position) (integer? position))
      (error 'refresh! "invalid completion caret" position))
    (track! s text position)
    (when (and (state-preview s) (not (equal? text (state-preview-text s)))) (end-preview! s))
    (unless (and (string=? text (state-text s)) (= position (state-position s)))
      (unless (prepared? s (state-source s) text position) (state-prepared-set! s #f))
      (if (state-source s)
        (let-values ([(start end options candidates)
                      ((completion:source-lookup (state-source s)) text position)])
          (if (and start (= start (car (state-range s))))
            (begin (state-range-set! s (cons start end)) (state-matches-set! s candidates)
              (when (state-candidates s) (candidates! s candidates)))
            (dismiss! s)))
        (unless (string=? text (state-text s)) (dismiss! s)))
      (state-text-set! s text) (state-position-set! s position)
      (state-generation-set! s (+ 1 (state-generation s))) (state-note-set! s "")))

  (define (replace-range s text)
    (let ([range (state-range s)] [input (state-text s)])
      (cons (string-append (substring input 0 (car range)) text (string:tail input (cdr range)))
        (+ (car range) (string-length text)))))
  (define (edit! s next prepared)
    (let* ([transform (state-transform s)]
           [next (if transform (transform (car next) (cdr next)) next)])
      (when prepared (state-prepared-set! s (list prepared (car next) (cdr next))))
      (refresh! s (car next) (cdr next))))
  (define (continue! s source next)
    (let-values ([(start end options candidates) ((completion:source-lookup source) (car next) (cdr next))])
      (if (and start (state-range s) (<= (car (state-range s)) start (cdr next)) (pair? candidates)
            (not (and (null? (cdr candidates)) (string=? (value (car candidates)) (substring (car next) start end)))))
        (begin (state-range-set! s (cons start end)) (state-matches-set! s candidates) (candidates! s candidates))
        (dismiss! s))))
  (define (preview! s candidate)
    (unless (eq? candidate (state-preview s))
      (end-preview! s)
      (when (and (completion:candidate? candidate) (completion:candidate-preview candidate))
        (state-preview-set! s candidate) (state-preview-text-set! s (state-text s))
        (state-undo-set! s (guard (ex [else #f]) ((completion:candidate-preview candidate)))))))
  (define (preview-value! s text)
    (preview! s (find (lambda (candidate) (string=? (value candidate) text)) (state-matches s))))

  (edoc "Normalize once, then cycle equivalent spellings or request another page. The source owns matching and set-preserving extensions; deferred extensions are evaluated only for new normalization."
        (s any "completion session") (source any "cursor-aware source, legacy prefix procedure or false")
        (backwards? boolean "visit the previous live-search match"))
  (define (normalize! s source backwards?)
    (state-note-set! s "")
    (state-generation-set! s (+ 1 (state-generation s)))
    (let ([text (state-text s)] [position (state-position s)])
      (cond
        [(state-searcher s)
         (state-hit-set! s ((if backwards? (completion:searcher-previous (state-searcher s)) (completion:searcher-next (state-searcher s)))))]
        [(not source) (void)]
        [(completion:source? source)
         (let-values ([(start end options candidates) ((completion:source-lookup source) text position)])
           (cond
             [(not start)
              (let ([next (if (completion:source-settle source) ((completion:source-settle source) text position) (cons text position))])
                (if (equal? next (cons text position))
                  (begin (dismiss! s) (state-note-set! s " [No symbol]"))
                  (begin (state-source-set! s source) (state-range-set! s (cons position position))
                    (continue! s source next) (edit! s next #f))))]
             [else
              (state-source-set! s source) (state-range-set! s (cons start end))
              (cond
                [(null? candidates)
                 (when (state-candidates s) (candidates! s candidates)) (state-note-set! s " [No match]")]
                [(and (null? (cdr candidates)) (completion:source-settle source))
                 (let* ([options (if (procedure? options) (options) options)] [candidate (car candidates)]
                        [next (replace-range s (if (pair? options) (car options) (value candidate)))]
                        [next ((completion:source-settle source) (car next) (cdr next))])
                   (if (equal? next (cons text position)) (dismiss! s)
                     (begin (continue! s source next) (edit! s next #f) (preview! s candidate))))]
                [(prepared? s source text position)
                 (if (<= (length (state-options s)) 1) (show! s (state-matches s))
                   (begin
                     (state-index-set! s (mod (+ 1 (state-index s)) (length (state-options s))))
                     (candidates! s (state-matches s))
                     (let ([text (list-ref (state-options s) (state-index s))])
                       (edit! s (replace-range s text) source) (preview-value! s text))))]
                [else
                 (state-options-set! s (if (procedure? options) (options) options)) (state-index-set! s 0)
                 (state-matches-set! s candidates)
                 (when (state-candidates s) (candidates! s candidates))
                 (unless (pair? (state-options s)) (error 'normalize! "source returned matches without extensions"))
                 (let* ([option (car (state-options s))] [next (replace-range s option)]
                        [unchanged? (string=? text (car next))])
                   (edit! s next source) (preview-value! s option)
                   (when (and unchanged? (not (state-candidates s)))
                     (state-note-set! s (format " [~a matches; Tab to list]" (length candidates)))))])]))]
        [else
         (when (state-source s) (dismiss! s))
         (let ([candidates (source text)])
           (cond
             [(null? candidates) (dismiss! s) (state-note-set! s " [No match]")]
             [(null? (cdr candidates))
              (dismiss! s)
              (if (string=? (car candidates) text)
                (begin (state-position-set! s (string-length text)) (state-note-set! s " [Sole completion]"))
                (edit! s (cons (car candidates) (string-length (car candidates))) #f))]
             [else (let ([prefix (string:common-prefix candidates)])
                     (if (> (string-length prefix) (string-length text))
                       (edit! s (cons prefix (string-length prefix)) #f)
                       (show! s candidates)))]))])))

  (edoc "Choose a displayed insertion value only while the captured session generation still matches. Refuse stale or absent candidates without editing."
        (s any "completion session") (generation integer "prepared page generation")
        (text string "candidate insertion value") (returns boolean))
  (define (choose! s generation text)
    (and (= generation (state-generation s))
      (state-candidates s) (exists (lambda (candidate) (string=? text (value candidate))) (state-candidates s))
      (let ([next (if (state-source s) (replace-range s text) (cons text (string-length text)))])
        (dismiss! s) (edit! s next #f) #t))))
