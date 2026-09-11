;; The terminal command facade talks to the base's PTY owner.
(library (vt)
  (export open! send! close! scrollback shell color-scheme!)
  (import (chezscheme) (prefix (client) client:))
  (define scrollback
    (case-lambda [() (client:request 'vt-option 'scrollback)]
      [(value) (client:request 'vt-option 'scrollback value)]))
  (define shell
    (case-lambda [() (client:request 'vt-option 'shell)]
      [(value) (client:request 'vt-option 'shell value)]))
  (define (own-head actor)
    (unless (equal? actor (client:identity)) (error 'vt "an attached head acts as itself")))
  (define (open! actor command directory rows cols scheme)
    (own-head actor)
    (client:request 'vt-open command directory rows cols scheme))
  (define (send! actor id text size paste? scheme)
    (own-head actor)
    (client:request 'vt-send id text size paste? scheme))
  (define (close! id) (client:request 'vt-close id))
  (define (color-scheme! scheme actor)
    (own-head actor)
    (client:request 'vt-color scheme))
)
