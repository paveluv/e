;; Guarded text commands shared by controls; policy and view state stay in callers.
(import (only (foundation edoc) elibrary))
(elibrary (head text-control)
  (export basis-text context current? history! lines mirror revision submit!)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (head head) head:) (prefix (head interaction) interaction:)
          (prefix (head text-source) text-source:) (prefix (head widget) widget:)
          (prefix (state view) view:))
  (define (refuse message)
    (raise (condition (kernel:make-refusal) (make-message-condition message))))

  (edoc "Read an already acquired text mirror from a widget source envelope, without I/O."
        (source list "borrowed source snapshot") (returns any))
  (define (mirror source)
    (let ([id (and source (cdr (assq 'id source)))])
      (unless (and (pair? id) (eq? (car id) 'buffer)) (error 'text-control "expected a text buffer source" id))
      (or (text-source:lookup (cadr id)) (error 'text-control "source is unavailable" id))))

  (edoc "Read a widget text snapshot's revision." (source list "source snapshot") (returns integer))
  (define (revision source) (cdr (assq 'revision source)))

  (edoc "Borrow a widget text snapshot's immutable lines." (source list "source snapshot") (returns vector))
  (define (lines source) (cdr (assq 'value source)))

  (edoc "Read an explicit text control's invocation source and descriptor; require its kind and schema without resolving a window."
        (id model "mounted text control") (kind symbol "required kind")
        (mode (list-of symbol) "current bypasses a retained action basis"))
  (define (context id kind . mode)
    (let-values ([(source d inputs) (apply widget:context id mode)])
      (unless (and d (eq? (view:kind d) kind) (= (view:schema d) 1)) (error 'text-control "unexpected text view" id kind))
      (mirror source)
      (values source d)))

  (edoc "Reconstruct the text on which this view's positions were established; missing history refuses."
        (source list "source snapshot") (d list "view descriptor") (returns vector))
  (define (basis-text source d)
    (text-source:basis-text (mirror source) (lines source) (revision source)
      (or (view:basis d) (revision source))))

  (edoc "Whether an invocation still addresses the same source and ownership generation."
        (id model "control") (source list "source snapshot") (d list "view descriptor") (returns boolean) (effects internal))
  (define (current? id source d)
    (guard (ex [else #f])
      (let-values ([(now current inputs) (widget:context id 'current)])
        (and current (= (view:generation d) (view:generation current))
          (equal? (cdr (assq 'id source)) (cdr (assq 'id now)))))))
  (define (settle! id source d mirror revision positions state)
    ;; Accepted text survives callbacks that close/reclaim the view. Never
    ;; overwrite a newer interaction made while the store was delivering it.
    (and (current? id source d)
      (let-values ([(now current inputs) (widget:context id 'current)])
        (and (= (view:sequence d) (view:sequence current))
          (let ([points (text-source:rebase positions (text-source:changes mirror revision (text-source:revision mirror)))])
            (and (pair? points)
              (begin (interaction:set-state! head:ui-actor id (text-source:revision mirror) (state points)) #t)))))))

  (edoc "Submit stated text intent, adopt its receipt once and settle a still-current view. The state procedure maps accepted logical positions to the control's portable state."
        (id model "control") (source list "source snapshot") (d list "descriptor") (old vector "proposal text")
        (basis integer "proposal revision") (span any "replaced span") (replacement list "replacement lines")
        (context any "journal grouping and witnesses") (positions list "desired result positions") (state procedure "portable state constructor"))
  (define (submit! id source d old basis span replacement context positions state)
    (unless (current? id source d) (refuse "The text view was closed or its source changed"))
    (when (cond [(assq 'read-only (view:options d)) => cdr] [else #f]) (refuse "This text view is read-only"))
    (let ([m (mirror source)])
      (let-values ([(lines rev changes points committed)
                    (text-source:edit! head:ui-actor (list old (text-source:id m) basis) span replacement context positions)])
        (text-source:adopt! m basis lines rev changes)
        (head:note-ui-edit! (text-source:id m) committed)
        (settle! id source d m rev points state))))

  (edoc "Apply source undo/redo and rebase a still-current view's logical positions through the same journal."
        (id model "control") (source list "source snapshot") (d list "descriptor")
        (direction symbol "undo or redo") (scope any "actor scope")
        (positions list "logical anchors") (state procedure "portable state constructor"))
  (define (history! id source d direction scope positions state)
    (unless (current? id source d) (refuse "The text view was closed or its source changed"))
    (when (cond [(assq 'read-only (view:options d)) => cdr] [else #f]) (refuse "This text view is read-only"))
    (let* ([m (mirror source)] [basis (or (view:basis d) (revision source))])
      (let-values ([(status detail) (text-source:history! head:ui-actor (text-source:id m) direction scope)])
        (when (eq? status 'applied)
          (text-source:open! head:ui-actor (text-source:id m) basis)
          (settle! id source d m basis positions state))
        (values status detail)))))
