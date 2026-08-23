;; config.template.e -- a template for the e editor's configuration.
;;
;; Copy this file to config.e next to it to configure the editor:
;;
;;     cp config.template.e config.e
;;
;; config.e is yours alone -- git ignores it, so nothing you set ever
;; shows up in a diff.  It is plain Scheme, no library, no shebang:
;; every expression evaluates in the editor's top level -- the same
;; place M-x expressions run, with the whole public API in scope.
;; Loaded at startup once the modules are up, and loaded again after
;; every module reload so the settings reapply on top of fresh
;; registrations: write it to tolerate being loaded any number of
;; times.  Saving it inside the editor applies it on the spot; so
;; does M-x (load-config!).  An error reports in the echo area and
;; leaves the editor running.
;;
;; Everything below is commented out and shows the default: the
;; editor behaves exactly the same with or without it.  Uncomment a
;; line and change its value to disagree with a default.

;; (modules-reload-on-save #t)    ; saving a module source reloads it in place
;; (config-reload-on-save #t)     ; saving config.e applies it on the spot
;; (scheme-format-on-save #t)     ; Scheme buffers format as they are saved
;; (scroll-margin 8)              ; rows kept between the cursor and the edges
;; (wrap-lines #t)                ; #f: long lines truncate ($) instead of wrapping (\)
;; (search-fold-case #t)          ; C-s matches ignore case (#f: exact)
;; (indent-on-tab! "scheme" #t)   ; #f: TAB stops auto-indenting Scheme
;; (scheme-format-brackets #t)    ; #f: format-* leaves ( ) and [ ] as written
;; (scheme-tab-width 2)           ; tabs widen to this many spaces (#f keeps tabs)
;; (min-window-lines 2)           ; squeezed windows keep this many text lines
;; (column-native-scroll #f)      ; #t: C-x 3 columns scroll natively (VT420
;;                                ; margins) -- M-x (probe-terminal!) detects
;;                                ; the support and offers to record this line
;; (bind-key! "M-l" show-log!)    ; pop the *log* view with one chord
