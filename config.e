;; config.e -- the e editor's configuration.
;;
;; Plain Scheme, no library, no shebang: every expression evaluates in
;; the editor's top level -- the same place M-x expressions run, with
;; the whole public API in scope.  Loaded at startup once the modules
;; are up, and loaded again after every module reload so the settings
;; reapply on top of fresh registrations: write it to tolerate being
;; loaded any number of times.  Saving this file inside the editor
;; applies it on the spot; so does M-x (load-config!).  An error
;; reports in the echo area and leaves the editor running.

(modules-reload-on-save #t)   ; saving a module source reloads it in place
(config-reload-on-save #t)    ; saving this file applies it on the spot

;; Some things to try:
;;
;; (indent-on-tab! "scheme" #f)   ; TAB stops auto-indenting Scheme
;; (scheme-format-brackets #f)    ; format-* leaves ( ) and [ ] as written
;; (scheme-tab-width 8)           ; tabs widen to 8 spaces (#f keeps tabs)
;; (min-window-lines 4)           ; squeezed windows keep 4 text lines
;; (bind-key! "M-l" show-log!)    ; pop the *log* view with one chord
