;; kernel.e -- the v2 kernel, growing in place (docs/DESIGN2.md).
;;
;; For now it owns one mechanism: persistent cells -- module state
;; that survives hot reloads.  Registries, module lifecycle, and the
;; scheduling substrate migrate here from the core as the v2 layers
;; take shape.  The kernel imports nothing above itself and, like the
;; v0.1 core, is never reloaded in place.

(library (kernel)
  (export persistent-cell
          registering-module make-registry registry-add!
          registry-items registry-entries registry-find
          retract-module! registration-snapshot restore-registrations!)
  (import (rnrs)
          (only (chezscheme)
                box unbox set-box! make-hashtable equal-hash
                make-parameter))

  ;;; Registries ------------------------------------------------------------

  ;; Everything a module registers -- key bindings, modes,
  ;; highlighters, whatever a future hook adds -- goes through a
  ;; registry and is tagged with the module whose init! is running.
  ;; Reloading a module retracts its entries wholesale before running
  ;; its init! afresh, so registration is replace-by-module by
  ;; construction: a new hook gets it by using make-registry, with
  ;; nothing to remember.  Entries registered outside any module
  ;; (M-x, say) have owner #f and survive reloads.  Lookups prefer
  ;; newer entries.

  (define registering-module (make-parameter #f))
  (define registries '())

  (define (make-registry)
    (let ([r (box '())])            ; entries (owner . item), newest first
      (set! registries (cons r registries))
      r))

  (define (registry-add! r item)
    (set-box! r (cons (cons (registering-module) item) (unbox r))))

  (define (registry-items r) (map cdr (unbox r)))

  (define (registry-entries r) (unbox r))

  (define (registry-find r match?)
    (let loop ([entries (unbox r)])
      (cond [(null? entries) #f]
            [(match? (cdar entries)) (cdar entries)]
            [else (loop (cdr entries))])))

  (define (retract-module! owner)
    (for-each (lambda (r)
                (set-box! r (remp (lambda (e) (eq? (car e) owner))
                                  (unbox r))))
              registries))

  (define (registration-snapshot)
    ;; Registry lists are persistent: registration and retraction
    ;; replace a box's list rather than mutating it, so retaining each
    ;; old head is a complete, cheap rollback point.
    (map (lambda (r) (cons r (unbox r))) registries))

  (define (restore-registrations! snapshot)
    (for-each (lambda (entry) (set-box! (car entry) (cdr entry)))
              snapshot))

  (define persistent-cells (make-hashtable equal-hash equal?))

  (define (persistent-cell key make-initial)
    ;; A box that survives module reloads: the first request under a
    ;; key creates it; a reloaded module re-initializing gets the same
    ;; box back, its state intact.
    (or (hashtable-ref persistent-cells key #f)
        (let ([cell (box (make-initial))])
          (hashtable-set! persistent-cells key cell)
          cell))))
