;; One subscribed buffer inventory; collection providers own derived indexes.
(import (only (foundation edoc) elibrary))
(elibrary (state catalogue)
  (export attach! contribute! create-source!)
  (import (chezscheme) (prefix (core descriptor) descriptor:) (prefix (core kernel) kernel:)
          (prefix (core property) property:) (prefix (core row) row:)
          (prefix (foundation datum) datum:) (prefix (foundation string) string:)
          (prefix (foundation wire) wire:) (prefix (state actor) actor:)
          (prefix (state collection) collection:) (prefix (state model) model:)
          (prefix (state store) store:))

  (define (get row key default) (cond [(assq key row) => cdr] [else default]))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (unique xs) (fold-left (lambda (out x) (if (member x out) out (cons x out))) '() xs))
  (define attachments (kernel:make-registry car)) ; actor token contribution-table
  (define lock (make-mutex))
  (define ready (make-condition))
  (define serial 0)
  (define changed? #t)
  (define running? #f)
  (define take-events #f)
  (define jobs (make-hashtable equal-hash equal?))
  (define job-order '())
  (define watched-models (make-hashtable equal-hash equal?))
  (define inventory (make-eqv-hashtable)) ; worker-owned, metadata only
  (define orders (make-hashtable equal-hash equal?)) ; worker-owned, shared across filters
  (define local-rows (make-hashtable equal-hash equal?)) ; semantic contribution snapshots
  (define (wake!) (with-mutex lock (set! changed? #t) (condition-signal ready)))
  (define (attachment actor)
    (kernel:registry-find attachments (lambda (a) (equal? (car a) actor))))
  (define attachment-changes (kernel:registry-observe! attachments (lambda (removed added) (wake!))))
  (define model-changes
    (model:subscribe! #f
      (lambda (notice)
        ;; Notices carry identities; never copy unrelated model values just
        ;; to find out their kinds (a collection source may be enormous).
        (when (with-mutex lock
                (or (not (cadr notice)) (exists (lambda (id) (hashtable-contains? watched-models id)) (cadr notice))))
          (wake!)))))
  (define (recipe? v)
    (and (list? v) (for-all pair? v) (equal? (map car v) '(owner home epoch))
      (actor:identity? (get v 'owner #f)) (string? (get v 'home #f)) (natural? (get v 'epoch #f))))
  (define kind
    (model:register-kind! 'buffer-catalogue 1 recipe?))
  (define (sources)
    (filter (lambda (id)
              (let ([r (model:snapshot id)])
                (and r (= (get r 'schema 0) 1) (recipe? (get r 'value #f)) (model:available? id))))
      (model:ids 'buffer-catalogue)))

  (edoc "Open this head attachment's temporary local-buffer contribution. The registering connection owns its lifetime; detach removes it."
        (actor actor "head identity") (returns integer "attachment token"))
  (define (attach! actor)
    (unless (and (actor:identity? actor) (eq? (car actor) 'head)) (error 'attach! "expected head identity"))
    (when (attachment actor) (error 'attach! "head already contributes"))
    (let ([token (with-mutex lock (set! serial (+ serial 1)) serial)])
      (kernel:registry-add! attachments (list (datum:copy actor) token (make-hashtable equal-hash equal?))) token))

  (define (local-facts? facts)
    (and (list? facts) (for-all pair? facts) (string? (get facts 'name #f))
      (natural? (get facts 'version #f))
      (let loop ([rest facts] [seen '()])
        (or (null? rest)
          (let ([p (car rest)])
            (and (not (memq (car p) seen))
              (case (car p)
                [(name file mode) (string? (cdr p))] [(version lines modified-at) (natural? (cdr p))]
                [(flags) (and (list? (cdr p)) (for-all (lambda (f) (memq f '(conflicted read-only))) (cdr p)))]
                [else #f]) (loop (cdr rest) (cons (car p) seen))))))))

  (edoc "Apply at most 256 local metadata changes within 64 KiB to an active attachment. Entries are (key facts-or-false): local numeric tokens carry raw facts; (model id) entries carry an empty list and borrow a base view's metadata. No generated rows or text are accepted."
        (actor actor "head identity") (token integer "attachment token") (changes list "upserts and removals") (returns boolean))
  (define (contribute! actor token changes)
    (let ([changes (datum:copy changes)] [a (attachment actor)])
      (unless (and (list? changes) (<= (length changes) 256) (<= (bytevector-length (wire:encode changes)) 65536)
                (for-all (lambda (c)
                           (and (list? c) (= (length c) 2)
                             (if (row:source? (car c))
                               (or (not (cadr c)) (null? (cadr c)))
                               (and (natural? (car c)) (> (car c) 0) (or (not (cadr c)) (local-facts? (cadr c))))))) changes))
        (error 'contribute! "invalid bounded local metadata"))
      (and a (equal? token (cadr a))
        (with-mutex lock
          (and (eq? a (attachment actor))
            (begin
              (for-each (lambda (c)
                          (unless (equal? (cadr c) (hashtable-ref (caddr a) (car c) #f))
                            (set! changed? #t)
                            (if (cadr c) (hashtable-set! (caddr a) (car c) (cadr c)) (hashtable-delete! (caddr a) (car c))))) changes)
              (when changed? (condition-signal ready)) #t))))))

  (define (start!)
    (with-mutex lock
      (unless running?
        ;; Subscribe before the first snapshot. A racing change is in the
        ;; snapshot, the pending IDs, or both; one worker applies both reads.
        (let-values ([(token take) (store:watch! wake!)]) (set! take-events take))
        (set! running? #t) (fork-thread work!))))

  (edoc "Create a buffer catalogue source for a head, with explicit home spelling for filtering. Inventories and local contributions are runtime state; only this small recipe persists."
        (actor actor "owner/head") (home string "absolute home directory")
        (persistence (one-of transient persistent) "restart policy") (returns row-source))
  (define (create-source! actor home persistence)
    (unless (and (string? home) (> (string-length home) 0) (char=? (string-ref home 0) #\/)) (error 'create-source! "expected absolute home directory"))
    (start!)
    (let ([id (model:create! actor 'buffer-catalogue 1 'session persistence '()
                (list (cons 'owner actor) (cons 'home home) '(epoch . 0)))]) (wake!) id))

  (define columns
    '((modified "Modified" integer) (flags "Flags" (list-of buffer-flag)) (name "Buffer" string)
      (lines "Lines" integer) (mode "Mode" string) (file "File" string)
      (version "Version" integer) (archive "Archive" (one-of live backup trash)) (archived-at "Archived" integer)))
  (define (document m)
    (let ([trashed (get m 'trashed #f)] [backup (get m 'backup #f)])
      (list (list 'buffer (get m 'id #f))
        (append (list (cons 'name (get m 'name "")) (cons 'flags (get m 'flags '())) (cons 'version (get m 'version 0))
                  (cons 'lines (get m 'lines 0)) (cons 'archive (if trashed (if backup 'backup 'trash) 'live)))
          (if (and (get m 'modified #f) (get m 'modified-at #f)) (list (cons 'modified (get m 'modified-at #f))) '())
          (if trashed (list (cons 'archived-at (car trashed))) '())
          (filter values (map (lambda (k) (let ([v (if (and (eq? k 'file) backup) (car backup) (get m k #f))])
                                            (and (string? v) (cons k v)))) '(mode file))))
        (cond [trashed '((roles ghost))] [(get m 'modified #f) '((roles italic))] [else '()]))))
  (define (contributions actor)
    (let ([a (attachment actor)])
      (if (not a) '()
        (let ([entries (with-mutex lock (let-values ([(ks vs) (hashtable-entries (caddr a))]) (map cons (vector->list ks) (vector->list vs))))])
          (filter values
            (map (lambda (p)
                   (if (row:source? (car p))
                     (let* ([r (model:snapshot (car p))] [d (and r (get r 'value #f))])
                       (and r (eq? (get r 'kind #f) 'widget-view) (descriptor:valid? d)
                         (or (not (descriptor:owner d)) (equal? (descriptor:owner d) actor))
                         (list (car p) (list (cons 'name (let ([name (get (descriptor:options d) 'name #f)])
                                                           (if (string? name) name (format "<widget ~a>" (cadar p)))))
                                         (cons 'version (descriptor:generation d)) '(flags) '(mode . "widget") '(archive . live)) '())))
                     (list (list 'local actor (cadr a) (car p))
                       (cons '(archive . live) (map (lambda (f) (if (eq? (car f) 'modified-at) (cons 'modified (cdr f)) f)) (cdr p))) '()))) entries))))))
  (define (cell r name) (assq name (cadr r)))
  (define (order-key r) (format "~s" (car r)))
  (define (ordered source sort cancelled?)
    (let* ([v (get source 'value '())] [actor (get v 'owner #f)] [home (get v 'home "")]
           [key (list actor home sort)] [cached (hashtable-ref orders key #f)])
      (or cached
        (let ([rows (append
                      (filter values (map (lambda (m)
                                            (and (not (get m 'internal #f)) (actor:in-audience? actor (get m 'audience 'all)) (document m)))
                                       (vector->list (hashtable-values inventory))))
                      (hashtable-ref local-rows actor '()))])
          (define (fallback a b)
            (let ([a-name (cdr (cell a 'name))] [b-name (cdr (cell b 'name))])
              (or (string-ci<? a-name b-name)
                (and (string-ci=? a-name b-name) (or (string<? a-name b-name)
                                                   (and (string=? a-name b-name) (string<? (order-key a) (order-key b))))))))
          (define (less a b)
            (when (cancelled?) (error 'catalogue "cancelled"))
            (let loop ([keys sort])
              (if (null? keys) (fallback a b)
                (let* ([key (caar keys)] [x (cell a key)] [y (cell b key)]
                       [before? (if (eq? key 'flags) (lambda (x y) (property:flags<? (if x (cdr x) '()) (if y (cdr y) '()))) row:less?)])
                  (cond [(before? x y) (eq? (cadar keys) 'ascending)] [(before? y x) (eq? (cadar keys) 'descending)] [else (loop (cdr keys))])))))
          (define (archived<? a b)
            (when (cancelled?) (error 'catalogue "cancelled"))
            (let ([x (cdr (cell a 'archived-at))] [y (cdr (cell b 'archived-at))])
              (or (> x y) (and (= x y) (> (cadar a) (cadar b))))))
          (let ([parts (map (lambda (kind)
                              (list-sort (if (eq? kind 'live) less archived<?)
                                (filter (lambda (r) (eq? (cdr (cell r 'archive)) kind)) rows))) '(live backup trash))])
            (hashtable-set! orders key parts) parts)))))
  (define (prepare source query cancelled?)
    (let* ([home (get (get source 'value '()) 'home "")]
           [needle (get query 'filter "")] [match (string:searcher needle #t)]
           [parts (ordered source (get query 'sort '()) cancelled?)])
      (define (matches? r)
        (when (cancelled?) (error 'catalogue "cancelled"))
        (or (string=? needle "")
          (exists (lambda (s) (match s 0 (string-length s)))
            (let* ([name (cdr (cell r 'name))] [path (and (cell r 'file) (cdr (cell r 'file)))]
                   [home (if (string:suffix? "/" home) home (string-append home "/"))])
              (append (list name) (if path (list path) '())
                (if (and path (string:prefix? home path)) (list (string-append "~/" (substring path (string-length home) (string-length path)))) '()))))))
      (let* ([parts (map (lambda (rows) (filter matches? rows)) parts)]
             [rows (list->vector (append (car parts)
                                   (apply append (map (lambda (kind label entries)
                                                        (if (null? entries) '()
                                                          (cons (list (list 'section kind) (list (cons 'name label)) '((selectable . #f))) entries)))
                                                   '(backup trash) '("Backups" "Trash") (cdr parts)))))]
             [positions (make-hashtable equal-hash equal?)] [eligible '()])
        (vector-for-each (lambda (i r) (hashtable-set! positions (car r) i)
                           (when (row:selectable? r) (set! eligible (cons i eligible))))
          (list->vector (iota (vector-length rows))) rows)
        (let* ([eligible (list->vector (reverse eligible))] [n (vector-length eligible)])
          (collection:make-result columns (vector-length rows) (lambda (i) (vector-ref rows i))
            (lambda (key) (hashtable-ref positions key #f))
            (lambda (at direction offset)
              (and (> n 0)
                (let search ([lo 0] [hi n])
                  (if (= lo hi)
                    (vector-ref eligible (max 0 (min (- n 1) (+ (if (eq? direction 'forward) lo (- lo 1)) (if (eq? direction 'forward) offset (- offset))))))
                    (let ([mid (div (+ lo hi) 2)])
                      (if (if (eq? direction 'forward) (< (vector-ref eligible mid) at) (<= (vector-ref eligible mid) at))
                        (search (+ mid 1) hi) (search lo mid)))))))
            '((sortable modified flags name lines mode file)))))))
  (define (touch-sources! all? actors)
    (for-each (lambda (id)
                (let ([r (model:snapshot id)])
                  (when (and r (or all? (member (get (get r 'value '()) 'owner #f) actors)))
                    (let ([v (get r 'value '())])
                      (model:commit! '(base catalogue)
                        (list (list id (get r 'revision 0) (get r 'references '())
                                (map (lambda (p) (if (eq? (car p) 'epoch) (cons 'epoch (+ 1 (cdr p))) p)) v))))))))
      (sources)))
  (define (work!)
    (let loop ([initial? #t])
      (let-values ([(dirty? job)
                    (with-mutex lock
                      (let wait () (unless (or changed? (> (hashtable-size jobs) 0)) (condition-wait ready lock) (wait)))
                      (let ([dirty? changed?] [job (and (pair? job-order) (hashtable-ref jobs (car job-order) #f))])
                        (set! changed? #f)
                        (when job (hashtable-delete! jobs (car job-order)) (set! job-order (cdr job-order)))
                        (values dirty? job)))])
        (let* ([source-ids (sources)] [sources? (pair? source-ids)] [events (take-events)] [ids (and events (map car events))]
               [packet (and sources? (or initial? (not events) (pair? ids)) (if (or initial? (not events)) (store:metadata) (store:metadata ids)))]
               [updated? initial?] [actors '()])
          (unless sources? (hashtable-clear! inventory) (hashtable-clear! orders) (hashtable-clear! local-rows))
          (when packet
            (when (or initial? (not events)) (hashtable-clear! inventory) (set! updated? #t))
            (for-each (lambda (p)
                        (let ([m (and (cadr p) (not (get (cadr p) 'internal #f)) (cadr p))])
                          (unless (equal? m (hashtable-ref inventory (car p) #f))
                            (set! updated? #t)
                            (if m (hashtable-set! inventory (car p) m) (hashtable-delete! inventory (car p)))))) (cadr packet)))
          (when dirty?
            ;; Subscribe to a view's identity before reading its metadata.
            ;; Contribution changes racing this capture leave another wake.
            (with-mutex lock
              (hashtable-clear! watched-models)
              (for-each (lambda (id) (hashtable-set! watched-models id #t)) source-ids)
              (for-each (lambda (a)
                          (vector-for-each (lambda (key) (when (row:source? key) (hashtable-set! watched-models key #t)))
                            (hashtable-keys (caddr a)))) (if sources? (kernel:registry-items attachments) '())))
            (let ([owners (if sources? (unique (append (map car (kernel:registry-items attachments)) (vector->list (hashtable-keys local-rows)))) '())])
              (for-each (lambda (actor)
                          (let ([rows (list-sort (lambda (a b) (string<? (order-key a) (order-key b))) (contributions actor))])
                            (unless (equal? rows (hashtable-ref local-rows actor '()))
                              (set! actors (cons actor actors))
                              (if (null? rows) (hashtable-delete! local-rows actor) (hashtable-set! local-rows actor rows))))) owners)))
          (when (or updated? (pair? actors))
            (hashtable-clear! orders) (touch-sources! updated? actors))
          (when job
            (let ([cancelled? (caddr job)] [publish! (cadddr job)])
              (unless (cancelled?)
                (guard (ex [else (unless (cancelled?) (publish! #f (kernel:condition-text ex)))])
                  (publish! (prepare (car job) (cadr job) cancelled?) #f)))))
          (loop (not sources?))))))
  (define provider
    (collection:register! 'buffer-catalogue 1
      (lambda (source query cancelled? publish!)
        (start!)
        (with-mutex lock
          (let ([id (get query 'id #f)])
            (unless (hashtable-contains? jobs id) (set! job-order (append job-order (list id))))
            (hashtable-set! jobs id (list source query cancelled? publish!)))
          (condition-signal ready)))))
)
