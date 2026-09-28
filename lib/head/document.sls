;; Temporary host bridge for shared documents and legacy local buffers.
(import (only (foundation edoc) elibrary))
(elibrary (head document)
  (export create-source! reference resolve! retire!)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (core row) row:)
          (prefix (foundation datum) datum:) (prefix (foundation wire) wire:)
          (prefix (head head) head:) (prefix (service file) file:)
          (prefix (service log) log:) (prefix (state catalogue) catalogue:) (prefix (state view) view:))

  (define token #f)
  (define serial 0)
  (define entries (make-eq-hashtable)) ; pump-owned buffer -> (key version facts text-revision)
  (define lock (make-mutex))
  (define ready (make-condition))
  (define pending (make-hashtable equal-hash equal?)) ; coalesced wire facts only

  (define (queue! key facts)
    (with-mutex lock
      (hashtable-set! pending key facts)
      (condition-signal ready)))

  (define (facts b)
    (append (list (cons 'name (head:buffer-name b)) (cons 'flags (head:buffer-flags b)))
      (if (head:buffer-fact b 'app #f) '()
        (append (list (cons 'lines (head:buffer-line-count b)))
          (if (and (head:buffer-modified b) (head:buffer-modified-at b))
            (list (cons 'modified-at (head:buffer-modified-at b))) '())))
      (filter values (map (lambda (key) (let ([v (head:buffer-fact b key #f)]) (and (string? v) (cons key v)))) '(file mode)))))

  (define (changed! b present?)
    ;; This callback runs on the pump. Its cost is independent of the number
    ;; of other buffers; only immutable raw facts cross to the wire worker.
    (when token
      (let ([old (hashtable-ref entries b #f)])
        (if (or (not present?) (head:buffer-fact b 'internal #f))
          (when old (hashtable-delete! entries b) (queue! (car old) #f))
          (let* ([model (head:buffer-fact b 'widget-id #f)]
                 [key (or model (and old (not (row:source? (car old))) (car old))
                        (begin (set! serial (+ 1 serial)) serial))]
                 [raw (if model '() (facts b))]
                 [text-revision (and (not model) (not (head:buffer-fact b 'app #f)) (head:content-revision b))])
            (unless (and old (equal? key (car old)) (equal? raw (caddr old)) (equal? text-revision (cadddr old)))
              (let* ([version (if old (+ 1 (cadr old)) 0)]
                     [value (if model '() (cons (cons 'version version) raw))])
                ;; The receiver bounds each record as well as each batch.
                ;; Refuse an oversized fact set locally rather than wedge
                ;; the entire contribution stream behind it.
                (when old (unless (equal? key (car old)) (queue! (car old) #f)))
                (hashtable-set! entries b (list key version (datum:copy raw) text-revision))
                (if (> (bytevector-length (wire:encode (list (list key value)))) 65536)
                  (begin (queue! key #f)
                    (log:add! 'document:changed! (format "Local metadata too large: ~a" (head:buffer-name b))))
                  (queue! key (datum:copy value))))))))))

  (define (send!)
    (guard (ex [else (log:add! 'document:send! (kernel:condition-text ex))])
      (let loop ()
        (let ([batch
               (with-mutex lock
                 (let wait () (when (= (hashtable-size pending) 0) (condition-wait ready lock) (wait)))
                 (let-values ([(ks vs) (hashtable-entries pending)])
                   (let take ([i 0] [bytes 5] [out '()])
                     (if (or (= i (vector-length ks)) (= (length out) 256)) out
                       (let* ([entry (list (vector-ref ks i) (vector-ref vs i))]
                              [size (- (bytevector-length (wire:encode entry)) 3)])
                         (if (> (+ bytes size) 65536) out
                           (begin (hashtable-delete! pending (car entry))
                             (take (+ i 1) (+ bytes size) (cons entry out)))))))))])
          (when (catalogue:contribute! head:ui-actor token batch) (loop))))))

  (edoc "Create this head's buffer catalogue source, including its transient legacy local entries. Publications are coalesced off the UI path; the base owns shared buffer and widget metadata."
        (persistence (one-of transient persistent) "source restart policy") (returns row-source))
  (define (create-source! persistence)
    (unless token
      (set! token (catalogue:attach! head:ui-actor))
      (fork-thread send!)
      (for-each (lambda (b) (unless (head:buffer-store-id b) (changed! b #t))) (head:buffers)))
    (catalogue:create-source! head:ui-actor (file:expand "~/") persistence))

  (edoc "A stable catalogue row reference for a listed buffer, or false. Local references exist after create-source! establishes this attachment. Widget hosts return their base view reference."
        (b buffer "head buffer") (returns any))
  (define (reference b)
    (and (memq b (head:buffers))
      (cond [(head:buffer-store-id b) => (lambda (id) (list 'buffer id))]
        [(head:buffer-fact b 'widget-id #f) => values]
        [(hashtable-ref entries b #f) => (lambda (entry) (list 'local head:ui-actor token (car entry)))]
        [else #f])))

  (edoc "Resolve a live catalogue reference in this head, adopting shared text when needed. Foreign or retired local references refuse with false. Widget references resolve existing host buffers; window:show-widget! can mount a retained base view."
        (ref any "buffer, local or model row reference") (returns (or buffer #f)))
  (define (resolve! ref)
    (and (list? ref)
      (cond [(and (= (length ref) 2) (eq? (car ref) 'buffer) (integer? (cadr ref)))
             (head:adopt-store-buffer! (cadr ref))]
        [(or (row:source? ref)
           (and (= (length ref) 4) (eq? (car ref) 'local) (equal? (cadr ref) head:ui-actor) (equal? (caddr ref) token)))
         (find (lambda (b) (equal? ref (reference b))) (head:buffers))]
        [else #f])))

  (edoc "Retire an attachment-local document only if its displayed metadata version still matches. Shared text must use store:archive!; every affected window follows ordinary buffer retirement."
        (ref datum "local or widget reference") (version integer "shown metadata version") (returns boolean))
  (define (retire! ref version)
    (let* ([b (resolve! ref)]
           [current (and b (not (head:buffer-store-id b))
                      (if (row:source? ref)
                        (let ([d (view:snapshot ref)]) (and d (view:generation d)))
                        (let ([e (hashtable-ref entries b #f)]) (and e (cadr e)))))])
      (and current (equal? current version) (begin (head:forget-buffer! b) #t))))

  (define local-hook (head:add-local-buffer-hook! changed!)))
