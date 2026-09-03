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
          registry-remove!
          retract-module! registration-snapshot restore-registrations!
          module-source loaded-modules seam-modules
          init-module! load-module! load-modules! module-requires?
          reload-module! add-after-reload-hook!
          make-mailbox mailbox-post! mailbox-receive!)
  (import (rnrs)
          (only (chezscheme)
                box unbox set-box! make-hashtable equal-hash
                make-parameter format interaction-environment eval
                library-exports library-requirements
                library-directories directory-list load sort
                parameterize make-mutex with-mutex make-condition
                condition-wait condition-signal))

  ;;; Mailboxes ---------------------------------------------------------------

  ;; The scheduling substrate: a mailbox is a thread-safe FIFO with a
  ;; blocking receive.  Actors -- the main loop first of all -- wait
  ;; on their mailbox; any thread posts.

  (define-record-type (mailbox %make-mailbox mailbox?)
    (fields lock signal (mutable head) (mutable tail)))

  (define (make-mailbox)
    (%make-mailbox (make-mutex) (make-condition) '() '()))

  (define (mailbox-post! mb message)
    (with-mutex (mailbox-lock mb)
      (mailbox-tail-set! mb (cons message (mailbox-tail mb)))
      (condition-signal (mailbox-signal mb))))

  (define (mailbox-receive! mb)
    ;; blocks until a message arrives; strictly FIFO
    (with-mutex (mailbox-lock mb)
      (let wait ()
        (cond
          [(pair? (mailbox-head mb))
           (let ([message (car (mailbox-head mb))])
             (mailbox-head-set! mb (cdr (mailbox-head mb)))
             message)]
          [(pair? (mailbox-tail mb))
           (mailbox-head-set! mb (reverse (mailbox-tail mb)))
           (mailbox-tail-set! mb '())
           (wait)]
          [else
           (condition-wait (mailbox-signal mb) (mailbox-lock mb))
           (wait)]))))

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

  (define (registry-remove! r match?)
    ;; drop entries whose item satisfies match?, whoever owns them --
    ;; for registrations with an explicit revocation handle (a state
    ;; subscription's token, say), alongside ownership retraction
    (set-box! r (remp (lambda (e) (match? (cdr e))) (unbox r))))

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
          cell)))

  ;;; Module lifecycle --------------------------------------------------------

  ;; Extension modules are libraries in the lib directory, loaded
  ;; through here -- by the loader at startup, or later by hand -- so
  ;; the kernel knows which modules exist and owns their
  ;; registrations.  The kernel and the core are the two libraries
  ;; that never reload; everything else does, in place.

  (define modules '())          ; module names, in load order

  (define (loaded-modules) modules)

  (define (module-source name)
    (format "~a/~a.e" (caar (library-directories)) name))

  ;; the v2 seam modules arrive prefixed in the editor's top level,
  ;; exactly as code imports them -- M-x says (state:edit! ...) too
  (define seam-modules
    '(text state actors policy sandbox log styles keymap tty strings
       paint echo head))

  (define (init-module! name)
    ;; Import the module's library into the editor's top level
    ;; (compiling it when stale) and run its init!, if any, owning its
    ;; registrations.
    (let ([lib (list (string->symbol name))])
      (eval (if (memq (car lib) seam-modules)
                `(import (prefix ,lib
                                 ,(string->symbol
                                    (string-append name ":"))))
                `(import ,lib))
            (interaction-environment))
      (when (memq 'init! (library-exports lib))
        (parameterize ([registering-module (string->symbol name)])
          (eval `(let () (import (only ,lib init!)) (init!))
                (interaction-environment))))))

  (define (load-module! name)
    ;; Loading is idempotent.  A failed first initialization also
    ;; rolls back any registrations it made before raising.
    (unless (member name modules)
      (let ([old-registrations (registration-snapshot)])
        (guard (ex [else
                    (restore-registrations! old-registrations)
                    (raise ex)])
          (init-module! name)
          (set! modules (append modules (list name)))))))

  (define (dot-e? file)
    (let ([n (string-length file)])
      (and (> n 2) (string=? (substring file (- n 2) n) ".e"))))

  (define (load-modules!)
    ;; Load every module in the lib directory, in name order --
    ;; everything but the two libraries that never reload.  A broken
    ;; module must not keep the others from loading: failures are
    ;; returned as ((file . condition) ...) for the caller to report.
    (fold-left
      (lambda (failures file)
        (guard (ex [else (cons (cons file ex) failures)])
          (load-module! (substring file 0 (- (string-length file) 2)))
          failures))
      '()
      (sort string<?
            (filter (lambda (file)
                      (and (dot-e? file)
                           (not (member file '("core.e" "kernel.e")))))
                    (directory-list (caar (library-directories)))))))

  (define (module-requires? name target)
    ;; Does library (name) build on (target), directly or through
    ;; others?
    (let ([t (string->symbol target)]
          [seen (make-hashtable equal-hash equal?)])
      (let walk ([lib (list (string->symbol name))])
        (if (hashtable-ref seen lib #f)
            #f
            (begin
              (hashtable-set! seen lib #t)
              (exists (lambda (req) (or (eq? (car req) t) (walk req)))
                      (guard (ex [else '()])
                        (library-requirements lib))))))))

  ;; Layers above hang their after-reload work here (the core reapplies
  ;; config, refreshes buffer modes, repaints); hooks receive the
  ;; reloaded module's name and run inside the reload's rollback guard.
  (define after-reload-hooks (make-registry))

  (define (add-after-reload-hook! proc)
    (registry-add! after-reload-hooks proc))

  (define (reload-module! name*)
    ;; Reload a module in place: redefine its library from the
    ;; (edited) source, likewise every loaded module built on it, then
    ;; retract all module registrations and run every init! afresh --
    ;; the effect is exactly a clean startup, with the running
    ;; session's state untouched.  Closures already captured keep
    ;; running the old code; a module's own state starts over (unless
    ;; it lives in a persistent cell).
    (let* ([name (if (symbol? name*) (symbol->string name*) name*)]
           [source (module-source name)]
           [old-modules modules]
           [old-registrations (registration-snapshot)])
      (guard (ex [else
                  (set! modules old-modules)
                  (restore-registrations! old-registrations)
                  (raise ex)])
        (when (member name '("core" "kernel"))
          (error 'reload-module!
                 (format "the ~a cannot be reloaded in place" name)))
        ;; The core cannot reload, so a module it links against would
        ;; fork on reload: the core keeps the instance it compiled
        ;; against while everything else moves to the new one --
        ;; coherent stores through persistent cells, forked
        ;; registries.  Refuse rather than leave two instances.
        (when (module-requires? "core" name)
          (error 'reload-module!
                 (format "core links against ~a: restart e to pick up changes"
                         name)))
        (unless (file-exists? source)
          (error 'reload-module! "no module source" source))
        (load source)
        (unless (member name modules)
          (set! modules (append modules (list name))))
        (for-each (lambda (m)
                    (when (and (not (string=? m name))
                               (module-requires? m name))
                      (load (module-source m))))
                  modules)
        (for-each (lambda (m) (retract-module! (string->symbol m)))
                  modules)
        (for-each init-module! modules)
        (for-each (lambda (hook) (hook name))
                  (registry-items after-reload-hooks))))))
