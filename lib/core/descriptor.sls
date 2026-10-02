;; Portable view data shared by the base and head implementations of view.
(import (only (foundation edoc) elibrary))
(elibrary (core descriptor)
  (export basis children cleanup commands focus generation head? kind make options owned owner parent references schema sequence source state valid? with)
  (import (except (rnrs) parent) (prefix (core identity) identity:) (prefix (foundation datum) datum:))
  (define keys '(source kind schema parent children options generation owner sequence basis state focus))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (id? x tags) (and (list? x) (= (length x) 2) (memq (car x) tags)
                            (natural? (cadr x)) (> (cadr x) 0)))

  (edoc "Whether an identity denotes a head." (x datum "identity") (returns boolean))
  (define (head? x) (and (list? x) (= (length x) 2) (eq? (car x) 'head) (string? (cadr x))))
  (define (field d key) (cdr (assq key d)))

  (edoc "Read the portable view's source field without a state lookup." (d list "descriptor") (returns any))
  (define (source d) (field d 'source))

  (edoc "Read the portable view's kind field without a state lookup." (d list "descriptor") (returns any))
  (define (kind d) (field d 'kind))

  (edoc "Read the portable view's schema field without a state lookup." (d list "descriptor") (returns any))
  (define (schema d) (field d 'schema))

  (edoc "Read the portable view's parent field without a state lookup." (d list "descriptor") (returns any))
  (define (parent d) (field d 'parent))

  (edoc "Read the portable view's children field without a state lookup." (d list "descriptor") (returns any))
  (define (children d) (field d 'children))

  (edoc "Read the portable view's options field without a state lookup." (d list "descriptor") (returns any))
  (define (options d) (field d 'options))

  (edoc "Read explicit named command targets, each (name view action arguments)." (d list "descriptor") (returns list))
  (define (commands d) (cond [(assq 'commands (options d)) => cdr] [else '()]))

  (edoc "Read explicitly owned per-view model resources. Borrowed sources are separate; owned resources fork and retire with this view."
        (d list "descriptor") (returns list))
  (define (owned d) (cond [(assq 'owned (options d)) => cdr] [else '()]))

  (edoc "Read pending output disposal. These buffer identities belong to already-retired resources; the durable intent remains until deletion finishes."
        (d list "descriptor") (returns list))
  (define (cleanup d) (cond [(assq 'cleanup (options d)) => cdr] [else '()]))
  (define (commands? xs)
    (and (list? xs)
      (for-all (lambda (x) (and (list? x) (= (length x) 4) (symbol? (car x))
                             (id? (cadr x) '(model)) (symbol? (caddr x)) (list? (cadddr x)))) xs)
      (unique? (map car xs))))

  (edoc "Read the portable view's generation field without a state lookup." (d list "descriptor") (returns any))
  (define (generation d) (field d 'generation))

  (edoc "Read the portable view's owner field without a state lookup." (d list "descriptor") (returns any))
  (define (owner d) (field d 'owner))

  (edoc "Read the portable view's sequence field without a state lookup." (d list "descriptor") (returns any))
  (define (sequence d) (field d 'sequence))

  (edoc "Read the portable view's basis field without a state lookup." (d list "descriptor") (returns any))
  (define (basis d) (field d 'basis))

  (edoc "Read the portable view's state field without a state lookup." (d list "descriptor") (returns any))
  (define (state d) (field d 'state))

  (edoc "Read the portable view's focus field without a state lookup." (d list "descriptor") (returns any))
  (define (focus d) (field d 'focus))
  (define (unique? xs) (or (null? xs) (and (not (member (car xs) (cdr xs))) (unique? (cdr xs)))))
  (define (catalogue-options? options)
    (let ([listed (assq 'catalogue options)] [name (assq 'name options)] [audience (assq 'audience options)])
      (and (or (not listed) (boolean? (cdr listed)))
        (or (not listed) (not (cdr listed))
          (and name (string? (cdr name)) (or (not audience) (identity:audience? (cdr audience))))))))

  (edoc "Whether a portable descriptor has the current structural shape." (d datum "candidate") (returns boolean))
  (define (valid? d)
    (and (list? d) (for-all pair? d) (equal? (map car d) keys)
         (or (not (source d)) (id? (source d) '(model buffer)))
         (symbol? (kind d)) (natural? (schema d)) (> (schema d) 0)
         (or (not (parent d)) (id? (parent d) '(model)))
         (list? (children d))
         (for-all (lambda (c) (and (list? c) (= (length c) 3) (symbol? (car c))
                                   (id? (cadr c) '(model))
                                   (or (eq? (caddr c) 'fit)
                                     (and (list? (caddr c)) (= (length (caddr c)) 2)
                                          (eq? (caaddr c) 'grow) (real? (cadr (caddr c)))
                                          (rational? (cadr (caddr c))) (> (cadr (caddr c)) 0))))) (children d))
         (unique? (map car (children d))) (unique? (map cadr (children d)))
         (list? (options d)) (for-all (lambda (p) (and (pair? p) (symbol? (car p)))) (options d))
         (unique? (map car (options d)))
         (catalogue-options? (options d))
         (commands? (commands d))
         (list? (owned d)) (for-all (lambda (id) (id? id '(model))) (owned d)) (unique? (owned d))
         (list? (cleanup d)) (for-all (lambda (id) (id? id '(buffer))) (cleanup d)) (unique? (cleanup d))
         (natural? (generation d)) (or (not (owner d)) (head? (owner d)))
         (natural? (sequence d)) (or (not (basis d)) (natural? (basis d)))
         (or (not (focus d)) (id? (focus d) '(model)))))

  (edoc "Construct an unowned leaf descriptor." (source datum "source reference") (kind symbol "contract") (schema integer "version") (options list "logical preferences") (state datum "interaction") (returns list))
  (define (make source kind schema options state)
    (map cons keys (list source kind schema #f '() options 0 #f 0 #f state #f)))

  (edoc "Copy a descriptor with explicitly replaced fields." (d list "descriptor") (changes list "unique named fields") (returns list))
  (define (with d changes)
    (unless (and (list? changes) (for-all (lambda (p) (and (pair? p) (memq (car p) keys))) changes)
                 (unique? (map car changes))) (assertion-violation 'with "invalid descriptor fields" changes))
    (datum:copy (map (lambda (p) (or (assq (car p) changes) p)) d)))

  (edoc "The source and child resource references of a descriptor." (d list "descriptor") (returns list))
  (define (references d)
    (fold-left (lambda (out id) (if (member id out) out (append out (list id)))) '()
      (append (if (source d) (list (source d)) '()) (map cadr (children d)) (map cadr (commands d)) (owned d) (cleanup d)))))
