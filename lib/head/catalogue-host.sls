;; Default window placement adapter; catalogue identity lives in the base.
(import (only (foundation edoc) elibrary))
(elibrary (head catalogue-host)
  (export reference resolve! retire!)
  (import (chezscheme) (prefix (core row) row:)
          (prefix (head head) head:)
          (prefix (state model) model:) (prefix (state view) view:))

  (edoc "The shared document or widget view represented by an outer window buffer; false for legacy local text."
        (b buffer "head buffer") (returns any))
  (define (reference b)
    (and (memq b (head:buffers))
      (if (head:buffer-store-id b) (list 'buffer (head:buffer-store-id b))
        (head:buffer-fact b 'widget-id #f))))

  (edoc "Resolve a document reference in the default window host, adopting shared text when needed. A widget without a local placement returns false; window:show-widget! can mount it."
        (ref any "buffer or model reference") (returns (or buffer #f)))
  (define (resolve! ref)
    (and (list? ref)
      (cond [(and (= (length ref) 2) (eq? (car ref) 'buffer) (integer? (cadr ref)))
             (head:adopt-store-buffer! (cadr ref))]
        [(row:source? ref) (find (lambda (b) (equal? ref (reference b))) (head:buffers))]
        [else #f])))

  (edoc "Retire a named root view against its shown structural generation, preserving borrowed sources. Remove its default window placement after successful retirement. Foreign mounted views refuse."
        (ref model "widget reference") (version integer "shown generation") (returns boolean))
  (define (retire! ref version)
    (let* ([r (caddar (cadr (model:snapshots (list ref))))]
           [d (and r (cdr (assq 'value r)))] [b (resolve! ref)])
      (and d (= version (view:generation d)) (not (view:parent d))
        (let-values ([(status record) (view:retire! head:ui-actor ref (cdr (assq 'revision r)))])
          (and (eq? status 'applied) (begin (when b (head:forget-buffer! b)) #t)))))))
