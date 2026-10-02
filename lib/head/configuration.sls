;; Optional configuration policy for heads that opt into config.e.
(import (only (foundation edoc) elibrary))
(elibrary (head configuration)
  (export init! load! modules-reload-on-save reload-on-save)
  (import (chezscheme) (prefix (core kernel) kernel:)
    (prefix (head head) head:) (prefix (head mode) mode:) (prefix (head tui) tui:)
    (prefix (service file) file:) (prefix (service log) log:) (prefix (sys sys) sys:))
  ;;; Configuration and reloads -----------------------------------------------------

  (edoc "Load config.e into the editor top level, repainting and re-resolving buffer modes; whether it loaded cleanly."
        (returns boolean))
  (define (load!)
    ;; The kernel loads config.e (kernel:load-config!); the head repaints
    ;; around it -- a recolor must repaint rows cached under the old
    ;; codes -- re-resolves buffer modes, and reports an error.  ->
    ;; whether it loaded cleanly.
    (tui:invalidate-screen-cache!)
    (let ([result (kernel:load-config!)])
      (cond [(eq? result #t)
             (mode:refresh!)
             (tui:invalidate-screen-cache!)
             #t]
            [(eq? result 'absent) #f]
            [else
             (log:add! 'configuration:load! (format "Error in config.e: ~a"
                                              (kernel:condition-text result)))
             #f])))

  ;; Saving a module's source reloads it on the spot (a fresh .sls file
  ;; in an active library root is loaded for the first time), and saving
  ;; config.e applies it, so editing the editor from inside itself
  ;; takes effect on save.  Both on by default; (configuration:modules-reload-on-save
  ;; #f) or (configuration:reload-on-save #f) -- in config.e for an
  ;; installation, at M-x for a session -- turns either off.
  (edoc "Whether saving a module's source reloads it on the spot."
        (value boolean))
  (define modules-reload-on-save (make-parameter #t))

  (edoc "Whether saving config.e applies it on the spot."
        (value boolean))
  (define reload-on-save (make-parameter #t))

  (define (module-name-of-path path)
    ;; The module name a saved path denotes in the selected source roots;
    ;; #f for other paths, including kernel/main, which cannot be reloaded.
    (let* ([full (or (sys:canonical-file-path path) (file:canonical path))]
           [library (kernel:source-library full)])
      (and library
           (not (member library '((core kernel) (run main))))
           (or (find (lambda (name) (equal? (kernel:module-library name) library))
                     (kernel:loaded-modules))
               ;; Saving an independent new module still publishes its API.
               ;; Imported helpers retain their full library identity and
               ;; do not acquire another public prefix merely by being saved.
               (and (not (exists (lambda (name) (kernel:module-requires? name library))
                           (kernel:loaded-modules)))
                    (let ([name (symbol->string (car (reverse library)))])
                      (and (equal? (kernel:module-library name) library) name)))
               library))))

  (define (reload-on-save! path editor)
    ;; The post-save hook.  A reload that fails (a module saved mid-edit,
    ;; say) reports itself without disturbing the save -- or the editor,
    ;; which keeps running the module's old version.  A saved config.e
    ;; applies on the spot the same way.
    (let ([name (and (modules-reload-on-save) (module-name-of-path path))])
      (cond
        [name
         (guard (ex [else (log:add! 'configuration:reload-on-save!
                            (format "Reload of ~a failed: ~a"
                                    name (kernel:condition-text ex)))])
           (kernel:reload-module! name)
           (log:add! 'configuration:reload-on-save! (format "Reloaded ~a" name)))]
        [(and (reload-on-save)
              (string=? (file:canonical path) (file:canonical (kernel:config-file))))
         (when (load!)
           (log:add! 'configuration:reload-on-save! "Applied config.e"))])))

  (edoc "Register config and module reload policy for this composition. Loading this definition alone applies no configuration." (public))
  (define (init!)
    (kernel:add-after-reload-hook!
      (lambda (name)
        (load!)
        (tui:invalidate-screen-cache!)
        (head:report! (format "Reloaded ~a" name))))
    (file:add-post-save-hook! reload-on-save!))
)
