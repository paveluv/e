;; The terminal command facade talks to the base's PTY owner.
(import (only (edoc) elibrary))
(elibrary (vt)
  (export open! send! close! scrollback shell color-scheme!)
  (import (chezscheme) (prefix (client) client:))
  (edoc "How many scrolled-off lines the base's terminals keep, or set it."
        (value integer "the line count")
        (returns integer))
  (define scrollback
    (case-lambda
      [()
       (client:request 'vt-option 'scrollback)]
      [(value)
       (client:request 'vt-option 'scrollback value)]))
  (edoc "The shell the base's terminals run without a command, or set it."
        (value string "the shell")
        (returns string))
  (define shell
    (case-lambda
      [()
       (client:request 'vt-option 'shell)]
      [(value)
       (client:request 'vt-option 'shell value)]))
  (define (own-head actor)
    (unless (equal? actor (client:identity)) (error 'vt "an attached head acts as itself")))
  (edoc "Ask the base to open a terminal for this head; its buffer id."
        (actor actor "the actor identity")
        (command (or string #f) "the command line, or #f for the shell")
        (directory directory "the working directory")
        (rows integer "the rows")
        (cols integer "the columns")
        (scheme any "the color scheme")
        (returns integer))
  (define (open! actor command directory rows cols scheme)
    (own-head actor)
    (client:request 'vt-open command directory rows cols scheme))
  (edoc "Send text to a terminal as this head, typed or pasted."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (text string "the text")
        (size list "(rows cols)")
        (paste? boolean "whether it is a paste")
        (scheme any "the color scheme"))
  (define (send! actor id text size paste? scheme)
    (own-head actor)
    (client:request 'vt-send id text size paste? scheme))
  (edoc "Close a terminal by its buffer id."
        (id integer "the buffer id"))
  (define (close! id)
    (client:request 'vt-close id))
  (edoc "Tell the terminals this head's color scheme."
        (scheme (or (one-of dark light) #f) "the scheme")
        (actor actor "the actor identity"))
  (define (color-scheme! scheme actor)
    (own-head actor)
    (client:request 'vt-color scheme))
)
