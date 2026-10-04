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
      (mutable note) (mutable selected)
      (mutable basis)))

  (edoc "Create a head-local completion session. The optional text transformer applies only to completion edits; text ownership and submission remain with the host."
        (primary any "primary source")
        (transform (or procedure #f) "text and character caret -> (text . caret)"))
  (define (create primary transform)
    (make-state primary transform "" 0 0 #f #f #f '() '() 0 #f 0 "" #f #f))
  (define (basis source) (and (completion:source? source) ((completion:source-basis source))))

  (edoc "Read a prepared session as (generation text caret candidates note page-sequence source selected-candidate). False candidates means the list is hidden. This never invokes a provider."
        (s any "completion session") (returns list))
  (define (snapshot s)
    (list (state-generation s) (state-text s) (state-position s) (state-candidates s)
      (state-note s)
      (state-page s) (state-source s) (state-selected s)))

  (edoc "Release the completion source. Scoped presentations own their own view lifetimes."
        (s any "completion session"))
  (define (finish! s)
    (state-selected-set! s #f)
    (when (completion:source? (state-primary s)) ((completion:source-release (state-primary s)))))

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
    (unless (and (string=? text (state-text s)) (= position (state-position s))
              (equal? (state-basis s) (basis (state-primary s))))
      (state-selected-set! s #f)
      (unless (equal? (state-basis s) (basis (state-primary s))) (state-prepared-set! s #f))
      (state-basis-set! s (basis (state-primary s)))
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
  (define (remember! s candidate)
    (state-selected-set! s (and (completion:candidate? candidate) (completion:candidate-context candidate) candidate)))
  (define (remember-value! s text)
    (remember! s (find (lambda (candidate) (string=? (value candidate) text)) (state-matches s))))

  (edoc "Normalize once, then cycle equivalent spellings or request another page. The source owns matching and set-preserving extensions; deferred extensions are evaluated only for new normalization."
        (s any "completion session") (source any "cursor-aware source, legacy prefix procedure or false")
  )
  (define (normalize! s source)
    (refresh! s (state-text s) (state-position s))
    (state-note-set! s "")
    (state-generation-set! s (+ 1 (state-generation s)))
    (let ([text (state-text s)] [position (state-position s)])
      (cond
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
                     (begin (continue! s source next) (edit! s next #f) (remember! s candidate))))]
                [(prepared? s source text position)
                 (if (<= (length (state-options s)) 1) (show! s (state-matches s))
                   (begin
                     (state-index-set! s (mod (+ 1 (state-index s)) (length (state-options s))))
                     (candidates! s (state-matches s))
                     (let ([text (list-ref (state-options s) (state-index s))])
                       (edit! s (replace-range s text) source) (remember-value! s text))))]
                [else
                 (state-options-set! s (if (procedure? options) (options) options)) (state-index-set! s 0)
                 (state-matches-set! s candidates)
                 (when (state-candidates s) (candidates! s candidates))
                 (unless (pair? (state-options s)) (error 'normalize! "source returned matches without extensions"))
                 (let* ([option (car (state-options s))] [next (replace-range s option)]
                        [unchanged? (string=? text (car next))])
                   (edit! s next source) (remember-value! s option)
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

  (edoc "Choose a displayed insertion value only while the captured session generation still matches. Apply the source's continuation just as for a sole Tab completion. Refuse stale or absent candidates without editing."
        (s any "completion session") (generation integer "prepared page generation")
        (text string "candidate insertion value") (returns boolean))
  (define (choose! s generation text)
    (and (= generation (state-generation s))
      (equal? (state-basis s) (basis (state-primary s)))
      (state-candidates s) (exists (lambda (candidate) (string=? text (value candidate))) (state-candidates s))
      (let* ([candidate (find (lambda (c) (string=? text (value c))) (state-candidates s))]
             [source (state-source s)]
             [next (if source (replace-range s text) (cons text (string-length text)))]
             [next (if (and source (completion:source-settle source))
                     ((completion:source-settle source) (car next) (cdr next)) next)])
        (dismiss! s) (edit! s next #f) (remember! s candidate) #t))))
