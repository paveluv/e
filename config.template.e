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
;; does M-x (main:load-config!).  An error reports in the echo area and
;; leaves the editor running.
;;
;; Everything below is commented out and shows the default: the
;; editor behaves exactly the same with or without it.  Uncomment a
;; line and change its value to disagree with a default.

;; (main:modules-reload-on-save #t)    ; saving a module source reloads it in place
;; (main:config-reload-on-save #t)     ; saving config.e applies it on the spot
;; (scheme-mode:format-on-save #t)     ; Scheme buffers format as they are saved
;; (scroll-margin 8)              ; rows kept between the cursor and the edges
;; (scrollbar #f)                 ; #t: show position bars in ordinary buffers
;; (scrollbar-position 'right)    ; position bars on the left or right edge
;; (line-numbers #f)              ; #t: show line numbers in every untoggled buffer
;; (wrap-lines #t)                ; #f: long lines truncate ($) instead of wrapping (\)
;; (paren:matching-style 'bold)   ; matched brackets: bold, underline,
;;                                ; box, or colored -- or design your
;;                                ; own marking with the style DSL:
;; (style:set! 'matching-paren '(curly-underline (underline-color 208)))
;; (https:backend 'curl)          ; web requests through curl instead of
;;                                ; the built-in FFI TLS (default 'native)
;; (markdown:browser "firefox")   ; command opening a markdown view's
;;                                ; web links (default "xdg-open")
;; (markdown:view-max-width 120)  ; reading-width cap of markdown views
;;                                ; in wide windows (default 80)
;; (search:fold-case #t)          ; C-s smart case: all-lowercase needles
;;                                ; ignore case, a capital makes them exact,
;;                                ; M-c toggles (#f: always exact)
;; (eval:copy-result #t)          ; copy non-void eval:run!/M-x results for C-y
;; (forward-kill-ring-to-system-clipboard #f)
;;                              ; #t: also request an OSC 52 system-clipboard
;;                              ; update after kills and copies; the host
;;                              ; terminal may ignore or prohibit the request,
;;                              ; so this might not work in every terminal
;; (indent-on-tab! "scheme" #t)   ; #f: TAB stops auto-indenting Scheme
;; (mode:add-extension! "scheme" ".foo") ; highlight *.foo as Scheme
;; (scheme-format:brackets #t)    ; #f: format-* leaves ( ) and [ ] as written
;; (scheme-format:tab-width 2)           ; tabs widen to this many spaces (#f keeps tabs)
;; (scheme-format:intrusive #f)   ; #t: also fold whitespace and reflow lines
;; (scheme-format:width 100)      ; target columns for intrusive formatting
;; (terminal:scrollback 10000)    ; retained shell lines; alternate screens excluded
;; (terminal:shell "/bin/bash")  ; defaults to $SHELL, then /bin/sh
;; (terminal:forward-clipboard-to-kill-ring #t)
;;                              ; import OSC 52 clipboard writes from terminal
;;                              ; children into e's kill ring
;; (min-window-lines 2)           ; squeezed windows keep this many text lines
;; (style:set! 'chrome '((foreground 244) italic))
;;                                ; style DSL: bold, dim, italic, underline,
;;                                ; blink, reverse, hidden, strike; foreground
;;                                ; or background colors may be named, 0..255,
;;                                ; or (rgb 0 0 0). Numbers and raw SGR strings
;;                                ; remain accepted for compatibility.
;;                                ; Full reference: manual/STYLES.md
;;                                ; chrome is the editor's grey furniture
;;                                ; (prompt labels, log prefixes, ghost text)
;; (keymap:bind-key! "M-l" log-view:show!)    ; pop the *log* view with one chord
;; (keymap:bind-key! "C-c s" save!!)     ; arbitrary multi-key chords work
;; (keymap:unbind-key! "C-v")            ; remove a global binding
;; (keymap:bind-key! 'isearch "M-i" 'toggle-case) ; rebind a contextual action
;; (keymap:unbind-key! 'isearch "M-c")
