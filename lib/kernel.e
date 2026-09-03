;; kernel.e -- the v2 kernel, growing in place (docs/DESIGN2.md).
;;
;; For now it owns one mechanism: persistent cells -- module state
;; that survives hot reloads.  Registries, module lifecycle, and the
;; scheduling substrate migrate here from the core as the v2 layers
;; take shape.  The kernel imports nothing above itself and, like the
;; v0.1 core, is never reloaded in place.

(library (kernel)
  (export persistent-cell)
  (import (rnrs) (only (chezscheme) box make-hashtable equal-hash))

  (define persistent-cells (make-hashtable equal-hash equal?))

  (define (persistent-cell key make-initial)
    ;; A box that survives module reloads: the first request under a
    ;; key creates it; a reloaded module re-initializing gets the same
    ;; box back, its state intact.
    (or (hashtable-ref persistent-cells key #f)
        (let ([cell (box (make-initial))])
          (hashtable-set! persistent-cells key cell)
          cell))))
