;; surface.e -- versioned rendition for store text. A publication installs
;; row changes, cursor and size together. Readers pin a frame generation;
;; the head must pair it with text at that frame's store revision.
(library (surface)
  (export init! publish! withdraw! snapshot rows subscribe! unsubscribe!)
  (import (rnrs)
          (only (chezscheme) unbox make-mutex with-mutex void)
          (prefix (kernel) kernel:)
          (prefix (store) store:)
          (prefix (datum) datum:))

  ;; Stable record identities let reloaded code use the persistent state.
  ;; Frames and their row tables are private immutable snapshots; only a
  ;; pending notification is replaced when a slow subscriber falls behind.
  (define-record-type frame
    (nongenerative e-surface-frame-v1)
    (fields generation revision count table cursor size))
  (define-record-type subscription
    (nongenerative e-surface-subscription-v1)
    (fields token id procedure pending))
  (define-record-type state
    (nongenerative e-surface-state-v1)
    (fields lock frames subscriptions deliveries
            (mutable serial) (mutable store-token)))
  (define data
    (unbox (kernel:persistent-cell 'surface
             (lambda ()
               (make-state (make-mutex) (make-eqv-hashtable)
                           (kernel:make-registry subscription-token)
                           (kernel:make-delivery-queue) 0 #f)))))

  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (positive-integer? n) (and (natural? n) (> n 0)))
  (define (next-serial!)
    (let ([n (+ (state-serial data) 1)]) (state-serial-set! data n) n))
  (define (drain!) (kernel:drain-deliveries! (state-deliveries data)))
  (define (frame-of id) (hashtable-ref (state-frames data) id #f))
  (define (same-basis? frame basis)
    (equal? (and frame (frame-generation frame)) basis))
  (define (header frame)
    (list (frame-generation frame) (frame-revision frame)
          (frame-cursor frame) (frame-size frame)))

  (define (link? value)
    (or (not value)
        (and (list? value) (= (length value) 2)
             (string? (car value)) (> (string-length (car value)) 0)
             (or (not (cadr value)) (string? (cadr value))))))

  (define (own-changes changes)
    ;; ((row styles hyperlinks attributes) ...); (row . #f) removes
    ;; rendition. Cell styles and links have equal widths, which can differ
    ;; between scrollback rows and need not equal Scheme character counts.
    (unless (list? changes) (error 'publish! "expected row changes" changes))
    (let ([seen (make-eqv-hashtable)])
      (map
        (lambda (entry)
          (unless (and (pair? entry) (natural? (car entry))
                       (not (hashtable-contains? seen (car entry))))
            (error 'publish! "expected unique nonnegative row numbers" entry))
          (hashtable-set! seen (car entry) #t)
          (let ([entry (datum:copy entry)])
            (when (cdr entry)
              (unless (and (list? entry) (= (length entry) 4)
                           (vector? (cadr entry)) (vector? (caddr entry))
                           (= (vector-length (cadr entry)) (vector-length (caddr entry))))
                (error 'publish! "expected styles, links and attributes for a row" entry))
              (vector-for-each
                (lambda (face)
                  (unless (or (not face) (symbol? face) (string? face))
                    (error 'publish! "expected a style symbol, string or #f" face)))
                (cadr entry))
              (vector-for-each
                (lambda (link) (unless (link? link) (error 'publish! "expected (uri id) or #f" link)))
                (caddr entry)))
            entry))
        changes)))

  (define (publish! id basis revision changes cursor size)
    ;; basis is the previous frame generation, or #f for first publication.
    ;; Coordinates describe the RESULT at revision, never a rebased patch:
    ;; omitted rows are explicitly retained, and removed rows must be dropped.
    ;; -> applied generation | stale frame-changed|text-changed.
    (unless (and (positive-integer? id) (or (not basis) (positive-integer? basis)) (natural? revision))
      (error 'publish! "expected buffer id, frame generation or #f, and text revision" id basis revision))
    (unless (and (list? size) (= (length size) 2) (for-all positive-integer? size)
                 (or (not cursor)
                     (and (list? cursor) (= (length cursor) 3)
                          (natural? (car cursor)) (natural? (cadr cursor))
                          (boolean? (caddr cursor)) (< (cadr cursor) (cadr size)))))
      (error 'publish! "expected size (rows cols) and cursor (row col visible?) or #f" cursor size))
    (let ([changes (own-changes changes)] [cursor (datum:copy cursor)] [size (datum:copy size)])
      (let-values ([(status detail)
                    (with-mutex (state-lock data)
                      (let ([old (frame-of id)])
                        (let-values ([(text current) (store:snapshot id)])
                          (cond
                            [(not (same-basis? old basis)) (values 'stale 'frame-changed)]
                            [(not (= revision current)) (values 'stale 'text-changed)]
                            [else
                             (let ([table (if old (hashtable-copy (frame-table old) #t) (make-eqv-hashtable))]
                                   [changed '()] [count (vector-length text)])
                               (for-each
                                 (lambda (entry)
                                   (let ([row (car entry)] [value (cdr entry)])
                                     (unless (equal? value (hashtable-ref table row #f))
                                       (set! changed (cons row changed))
                                       (if value (hashtable-set! table row value) (hashtable-delete! table row)))))
                                 changes)
                               (unless (and (or (not cursor) (< (car cursor) count))
                                            (for-all (lambda (row) (< row count))
                                                     (vector->list (hashtable-keys table))))
                                 (error 'publish! "frame coordinates exceed its text" id revision))
                               (if (and old (= revision (frame-revision old)) (null? changed)
                                        (equal? cursor (frame-cursor old)) (equal? size (frame-size old)))
                                   (values 'applied (frame-generation old))
                                   (let* ([generation (next-serial!)]
                                          [frame (make-frame generation revision count table cursor size)])
                                     (hashtable-set! (state-frames data) id frame)
                                     (enqueue! (list 'surface id generation revision
                                                     (if (and old (= revision (frame-revision old)))
                                                         (list-sort < changed) 'all)
                                                     cursor size))
                                     (values 'applied generation))))]))))])
        (drain!)
        (values status detail))))

  (define (retire! id)
    ;; Caller holds state-lock; repeated withdrawal is inert. A global serial
    ;; prevents a new frame from reusing a withdrawn generation (ABA).
    (and (frame-of id)
         (let ([generation (next-serial!)])
           (hashtable-delete! (state-frames data) id)
           (enqueue! (list 'surface id generation #f 'all #f #f))
           generation)))

  (define (withdraw! id basis)
    (unless (and (positive-integer? id) (or (not basis) (positive-integer? basis)))
      (error 'withdraw! "expected buffer id and frame generation or #f" id basis))
    (let-values ([(status detail)
                  (with-mutex (state-lock data)
                    (let ([old (frame-of id)])
                      (cond [(not old) (values 'applied #f)]
                            [(not (same-basis? old basis)) (values 'stale 'frame-changed)]
                            [else (values 'applied (retire! id))])))])
      (drain!)
      (values status detail)))

  (define (live-frame id)
    ;; A delete may have committed while its cleanup notification is queued.
    (let ([frame (frame-of id)]) (and frame (store:exists? id) frame)))

  (define (snapshot id)
    ;; -> (generation text-revision cursor size), or #f. The revision can
    ;; lag the store: render only with text at EXACTLY this revision.
    (with-mutex (state-lock data)
      (let ([frame (live-frame id)]) (and frame (datum:copy (header frame))))))

  (define (rows id generation from to)
    ;; Owned row data for [from,to), including (row . #f) for plain rows.
    ;; #f means withdrawn or superseded; never combine ranges across frames.
    (unless (and (positive-integer? id) (positive-integer? generation)
                 (natural? from) (natural? to) (<= from to))
      (error 'rows "expected buffer id, generation, and ordered row range" id generation from to))
    (with-mutex (state-lock data)
      (let ([frame (live-frame id)])
        (and frame (= generation (frame-generation frame))
             (begin
               (unless (<= to (frame-count frame)) (error 'rows "range exceeds frame text" from to))
               (let read ([row from])
                 (if (= row to) '()
                     (cons (cons row (datum:copy (hashtable-ref (frame-table frame) row #f)))
                           (read (+ row 1))))))))))

  (define (subscribe! id procedure)
    (unless (and (or (not id) (positive-integer? id)) (procedure? procedure))
      (error 'subscribe! "expected buffer id or #f and a procedure" id procedure))
    (let ([token (with-mutex (state-lock data) (next-serial!))])
      (kernel:registry-add! (state-subscriptions data)
        (make-subscription token id procedure (make-eqv-hashtable)))
      token))

  (define (unsubscribe! token)
    (kernel:registry-remove! (state-subscriptions data)
      (lambda (entry) (eqv? (subscription-token entry) token)))
    (void))

  (define (merge-notice old next)
    (let ([changed
           (if (or (eq? (list-ref old 4) 'all) (eq? (list-ref next 4) 'all)) 'all
               (let ([seen (make-eqv-hashtable)])
                 (for-each (lambda (row) (hashtable-set! seen row #t))
                           (append (list-ref old 4) (list-ref next 4)))
                 (list-sort < (vector->list (hashtable-keys seen)))))])
      (append (list (car next) (cadr next) (caddr next) (cadddr next) changed) (list-tail next 5))))

  (define (deliver! subscriber id)
    (let ([event
           (with-mutex (state-lock data)
             (let* ([pending (subscription-pending subscriber)]
                    [event (hashtable-ref pending id #f)])
               (hashtable-delete! pending id)
               event))])
      (when (and event (kernel:registry-find (state-subscriptions data) (lambda (entry) (eq? entry subscriber))))
        ((subscription-procedure subscriber) (datum:copy event)))))

  (define (enqueue! event)
    ;; Caller holds state-lock. One pending notice per subscriber/buffer;
    ;; merge changed rows and retain the latest complete header. This bounds
    ;; backlog while callbacks run, without introducing a frame timer.
    (let ([id (cadr event)])
      (for-each
        (lambda (subscriber)
          (when (or (not (subscription-id subscriber)) (eqv? (subscription-id subscriber) id))
            (let* ([pending (subscription-pending subscriber)] [old (hashtable-ref pending id #f)])
              (hashtable-set! pending id (if old (merge-notice old event) event))
              (unless old
                (kernel:enqueue-delivery! (state-deliveries data)
                  (lambda () (deliver! subscriber id)))))))
        (kernel:call-with-runtime-registrations
          (lambda () (kernel:registry-items (state-subscriptions data)))))))

  (define (init!)
    ;; The base cleanup subscription outlives an initializer that first
    ;; imports this seam. Refresh its code on reload, preserving live frames
    ;; and subscriptions; sweep deletions missed during listener replacement.
    (kernel:call-with-runtime-registrations
      (lambda ()
        (with-mutex (state-lock data)
          (kernel:call-with-registration-update
            (lambda ()
              (when (state-store-token data) (store:unsubscribe! (state-store-token data)))
              (state-store-token-set! data
                (store:subscribe! #f
                  (lambda (event)
                    (when (eq? (car event) 'delete)
                      (with-mutex (state-lock data) (retire! (cadr event)))
                      (drain!)))))))
          (vector-for-each
            (lambda (id) (unless (store:exists? id) (retire! id)))
            (hashtable-keys (state-frames data))))))
    (drain!)
    (void))

  (define initialized (init!)))
