;; Shared bounded row/rank cache. Requests belong to visible consumers, not
;; to render calls; one worker batch and one completion serve the whole head.
(import (only (foundation edoc) elibrary))
(elibrary (head range)
  (export acquire! init! locate pump! read release! request! seek summary)
  (import (except (chezscheme) read) (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:) (prefix (foundation wire) wire:)
          (prefix (head head) head:) (prefix (state collection) collection:)
          (prefix (state model) model:))
  (define readers (kernel:make-registry car)) ; token query callback model-token demand
  (define cache '()) ; (request reply bytes serial)
  (define exhausted (make-hashtable equal-hash equal?)) ; query -> generation
  (define serial 0)
  (define lock (make-mutex))
  (define completed #f)
  (define dirty? #f)
  (define active? #f)
  (define page-size 64)
  (define (column<? a b) (string<? (symbol->string a) (symbol->string b)))
  (define (field r k) (cdr (assq k r)))
  (define (reader token)
    (or (kernel:registry-find readers (lambda (r) (eq? token (car r)))) (error 'range "released reader")))
  (define (ready? r generation)
    (and r (eq? (field r 'kind) 'collection) (eq? (field (field r 'value) 'status) 'ready) (= generation (field (field r 'value) 'generation))))
  (define (wanted)
    (fold-left
      (lambda (out r)
        (let* ([d (list-ref r 4)] [id (cadr r)] [s (model:snapshot id)])
          (if (or (not d) (not (ready? s (car d))) (equal? (car d) (hashtable-ref exhausted id #f))) out
            (let* ([generation (car d)] [start (cadr d)] [count (caddr d)] [columns (cadddr d)]
                   [keys (list-ref d 4)] [total (field (field s 'value) 'count)]
                   [end (min total (+ start count))]
                   [first (* page-size (div start page-size))]
                   [pages (let loop ([i first] [pages '()])
                            (if (>= i end) pages
                              (loop (+ i page-size) (cons (list 'range id generation i (min page-size (- total i)) columns) pages))))]
                   [ranks (map (lambda (key) (list 'rank id generation key)) keys)]
                   [seeks (map (lambda (intent) (append (list 'seek id generation) intent)) (list-ref d 5))])
              (fold-left (lambda (out key) (if (member key out) out (cons key out))) out (append (reverse pages) ranks seeks))))))
      '() (kernel:registry-items readers)))
  (define (notify!)
    (for-each (lambda (r) (when (kernel:registry-find readers (lambda (current) (eq? r current))) ((caddr r))))
      (kernel:registry-items readers)))
  (define (same-range? a b)
    (and (eq? (car a) 'range) (eq? (car b) 'range)
      (equal? (list-head (cdr a) 2) (list-head (cdr b) 2)) (equal? (list-ref a 5) (list-ref b 5))))
  (define (needed? key requested)
    (exists (lambda (r)
              (or (equal? key r)
                (and (same-range? key r) (< (list-ref key 3) (+ (list-ref r 3) (list-ref r 4)))
                  (> (+ (list-ref key 3) (list-ref key 4)) (list-ref r 3))))) requested))
  (define (cover key ordinal)
    (find (lambda (e)
            (let ([request (car e)] [reply (cadr e)])
              (and (same-range? key request) (<= (list-ref request 3) ordinal)
                (< ordinal (+ (list-ref request 3)
                             (if (eq? (car reply) 'ready) (length (list-ref reply 4)) (list-ref request 4))))))) cache))
  (define (missing-request key)
    (if (not (eq? (car key) 'range)) (and (not (assoc key cache)) key)
      (let* ([start (list-ref key 3)] [end (+ start (list-ref key 4))]
             ;; After a byte-shortened page, fetch only still-visible gaps.
             ;; Repeatedly filling speculative tails wastes wire on big cells.
             [partial? (exists (lambda (e)
                                 (and (same-range? key (car e)) (needed? (car e) (list key))
                                   (eq? (caadr e) 'ready) (< (length (list-ref (cadr e) 4)) (list-ref (car e) 4)))) cache)]
             [spans (if partial?
                      (filter values
                        (map (lambda (r)
                               (let ([d (list-ref r 4)])
                                 (and d (equal? (cadr r) (cadr key)) (= (car d) (caddr key)) (equal? (cadddr d) (list-ref key 5))
                                   (< (cadr d) end) (> (+ (cadr d) (caddr d)) start)
                                   (cons (max start (cadr d)) (min end (+ (cadr d) (caddr d))))))) (kernel:registry-items readers)))
                      (list (cons start end)))]
             [start (if (null? spans) end (apply min (map car spans)))]
             [end (if (null? spans) end (apply max (map cdr spans)))])
        (let loop ([i start])
          (cond [(>= i end) #f] [(cover key i) (loop (+ i 1))]
            [else (list 'range (cadr key) (caddr key) i (- end i) (list-ref key 5))])))))
  (define (prune! requested)
    (vector-for-each (lambda (id)
                       (unless (exists (lambda (r) (and (equal? id (cadr r))
                                                     (ready? (model:snapshot id) (hashtable-ref exhausted id #f)))) (kernel:registry-items readers))
                         (hashtable-delete! exhausted id))) (hashtable-keys exhausted))
    (set! cache
      (filter (lambda (entry)
                (let* ([key (car entry)] [id (cadr key)] [generation (caddr key)])
                  (exists (lambda (r) (and (equal? id (cadr r)) (ready? (model:snapshot id) generation)))
                    (kernel:registry-items readers)))) cache))
    (let loop ()
      (when (or (> (length cache) 64) (> (apply + (map caddr cache)) #x800000))
        (let* ([undemanded (filter (lambda (e) (not (needed? (car e) requested))) cache)]
               [oldest (car (list-sort (lambda (a b) (< (cadddr a) (cadddr b))) (if (null? undemanded) cache undemanded)))])
          (if (pair? undemanded) (set! cache (remq oldest cache))
            ;; All retained payloads are still demanded: eviction alone
            ;; would fetch them forever. Diagnose the oldest query until a
            ;; consumer changes its demand, instead of an RPC retry storm.
            (let ([id (cadar oldest)] [generation (caddar oldest)])
              (hashtable-set! exhausted id generation)
              (set! cache (filter (lambda (e) (not (equal? id (cadar e)))) cache))))
          (loop)))))

  (edoc "Adopt one completed range batch and schedule missing current demand. Call on the head pump, outside painting.")
  (define (pump!)
    (let* ([requested (wanted)]
           [packet (with-mutex lock (let ([p completed]) (set! completed #f) p))]
           [changed? (with-mutex lock (let ([d dirty?]) (set! dirty? #f) d))])
      (when packet
        (set! active? #f)
        (for-each
          (lambda (key reply)
            (when (needed? key requested)
              (set! serial (+ serial 1))
              (set! cache (cons (list key reply (bytevector-length (wire:encode reply)) serial)
                            (filter (lambda (e) (not (equal? key (car e)))) cache))))) (car packet) (cadr packet)))
      (prune! requested)
      (when (or packet changed?) (notify!))
      (unless active?
        (let* ([missing (filter values (map missing-request (wanted)))]
               [batch (list-head missing (min 4 (length missing)))])
          (unless (null? batch)
            (set! active? #t)
            (fork-thread
              (lambda ()
                (let ([replies (guard (ex [else (map (lambda (key) (list 'unavailable (kernel:condition-text ex))) batch)])
                                 (collection:fetch batch))])
                  (with-mutex lock (set! completed (list batch replies)))
                  (head:wake-main!)))))))))
  (define cleanup
    (kernel:registry-observe! readers
      (lambda (removed added)
        (for-each (lambda (r) (model:unsubscribe! (list-ref r 3))) removed)
        (prune! (wanted)))))

  (edoc "Acquire compact collection metadata and shared range demand. Callback runs after pump adoption."
        (id row-source "query") (procedure procedure "zero-argument invalidation") (returns any))
  (define (acquire! id procedure)
    (unless (procedure? procedure) (error 'acquire! "expected callback"))
    (let* ([token (gensym "range")]
           [subscription (model:subscribe! (list id)
                           (lambda (notice) (with-mutex lock (set! dirty? #t)) (head:wake-main!)))])
      (kernel:registry-add! readers (list token (datum:copy id) procedure subscription #f)) token))

  (edoc "Release a viewport's metadata and range interest. Other views keep shared pages."
        (token any "reader"))
  (define (release! token) (kernel:registry-remove! readers (lambda (r) (eq? token (car r)))))

  (edoc "Replace a viewport's bounded logical demand; later pump work fetches uncached pages/ranks. No row identity is inferred from an ordinal."
        (token any "reader") (generation integer "result generation") (start integer "first ordinal")
        (count integer "at most 256 rows") (columns list "requested raw columns") (keys list "stable keys to locate")
        (navigation (list-of list) "optional list of (ordinal direction offset), at most four lookups combined"))
  (define (request! token generation start count columns keys . navigation)
    (unless (and (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0))) (list generation start count))
              (<= count 256) (list? columns) (for-all symbol? columns) (list? keys)
              (<= (length navigation) 1)
              (or (null? navigation) (and (list? (car navigation))
                                       (for-all (lambda (n) (and (list? n) (= (length n) 3) (memq (cadr n) '(forward backward))
                                                              (for-all (lambda (i) (and (integer? i) (exact? i) (>= i 0))) (list (car n) (caddr n))))) (car navigation))))
              (<= (+ (length keys) (if (null? navigation) 0 (length (car navigation)))) 4))
      (error 'request! "invalid bounded demand"))
    (let* ([r (reader token)] [d (list generation start count (list-sort column<? columns) keys (if (null? navigation) '() (car navigation)))])
      (unless (equal? d (list-ref r 4))
        (hashtable-delete! exhausted (cadr r))
        (set-car! (list-tail r 4) (datum:copy d)) (head:wake-main!))))

  (edoc "Read acquired compact metadata locally; no range or wire work is started."
        (id row-source "query") (returns any))
  (define (summary id)
    (unless (exists (lambda (r) (equal? id (cadr r))) (kernel:registry-items readers))
      (error 'summary "acquire before reading" id))
    (model:snapshot id))

  (edoc "Read locally cached rows at a result generation. Return ready, pending or unavailable; this never schedules work."
        (id row-source "query") (generation integer "result generation") (start integer "first ordinal")
        (count integer "row count") (columns list "raw columns") (returns list))
  (define (read id generation start count columns)
    (let* ([s (summary id)] [columns (list-sort column<? columns)])
      (cond [(equal? generation (hashtable-ref exhausted id #f)) '(unavailable cache-budget)]
        [(not (ready? s generation)) '(pending)]
        [else
         (let* ([total (field (field s 'value) 'count)] [end (min total (+ start count))])
           (let loop ([i (min start total)] [rows '()] [basis #f])
             (if (>= i end) (datum:copy (list 'ready generation basis (min start total) (reverse rows) total))
               (let* ([page (* page-size (div i page-size))]
                      [key (list 'range id generation page (min page-size (- total page)) columns)]
                      [entry (cover key i)] [reply (and entry (cadr entry))])
                 (cond [(not reply) '(pending)] [(not (eq? (car reply) 'ready)) reply]
                   [else
                    (let ([row (find (lambda (r) (= i (car r))) (list-ref reply 4))])
                      (if row (loop (+ i 1) (cons row rows) (caddr reply)) '(pending)))])))))])))

  (edoc "Read a demanded stable-key rank locally; false in a ready reply means the key is absent."
        (id row-source "query") (generation integer "result generation") (key datum "stable key") (returns list))
  (define (locate id generation key)
    (if (equal? generation (hashtable-ref exhausted id #f)) '(unavailable cache-budget)
      (if (not (ready? (summary id) generation)) '(pending)
        (let ([entry (assoc (list 'rank id generation key) cache)])
          (if entry (datum:copy (cadr entry))
            (let ([hit (exists (lambda (e)
                                 (and (eq? (caar e) 'range) (equal? (cadar e) id) (= (caddar e) generation)
                                   (eq? (caadr e) 'ready)
                                   (let ([row (find (lambda (r) (equal? (cadr r) key)) (list-ref (cadr e) 4))])
                                     (and row (list 'ready generation (caddr (cadr e)) (car row) (list-ref (cadr e) 5)))))) cache)])
              (or hit '(pending))))))))

  (edoc "Read selectable navigation from cached rows or a demanded indexed lookup, without scheduling work."
        (id row-source "query") (generation integer "result generation") (ordinal integer "display origin")
        (direction (one-of forward backward) "direction") (offset integer "selectable steps") (returns list))
  (define (seek id generation ordinal direction offset)
    (let* ([s (summary id)] [entry (assoc (list 'seek id generation ordinal direction offset) cache)])
      (cond [(equal? generation (hashtable-ref exhausted id #f)) '(unavailable cache-budget)]
        [(not (ready? s generation)) '(pending)] [entry (datum:copy (cadr entry))]
        [else
         (let* ([v (field s 'value)] [total (field v 'count)] [basis (field v 'basis)]
                [step (if (eq? direction 'forward) 1 -1)])
           (define (answer ordinal) (list 'ready generation basis ordinal total))
           (if (zero? total) (answer #f)
             (let loop ([at (min ordinal (- total 1))] [left offset] [last #f] [budget 256])
               (cond [(or (< at 0) (>= at total)) (if last (answer last) '(pending))]
                 [(zero? budget) '(pending)]
                 [else
                  (let ([row (exists (lambda (e)
                                       (and (eq? (caar e) 'range) (equal? (cadar e) id) (= (caddar e) generation)
                                         (eq? (caadr e) 'ready) (find (lambda (r) (= (car r) at)) (list-ref (cadr e) 4)))) cache)])
                    (cond [(not row) '(pending)]
                      [(cond [(assq 'selectable (cadddr row)) => cdr] [else #t])
                       (if (zero? left) (answer at) (loop (+ at step) (- left 1) at (- budget 1)))]
                      [else (loop (+ at step) left last (- budget 1))]))]))))])))

  (edoc "Integrate range adoption and demand with the existing head frame pump." (public))
  (define (init!) (head:add-pre-redraw-hook! pump!)))
