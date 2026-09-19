;; The terminal command facade talks to the base's PTY owner.
(library (vt)
  (export open! send! close! scrollback shell color-scheme!)
  (import (only (edoc) edefine edoc) (chezscheme) (prefix (client) client:))
  (edefine scrollback
    (case-lambda
      [()
       (edoc "How many scrolled-off lines the base's terminals keep." (returns integer))
       (client:request 'vt-option 'scrollback)]
      [(value)
       (edoc "Set how many scrolled-off lines the base's terminals keep." (value integer "the line count"))
       (client:request 'vt-option 'scrollback value)]))
  (edefine shell
    (case-lambda
      [()
       (edoc "The shell the base's terminals run without a command." (returns string))
       (client:request 'vt-option 'shell)]
      [(value)
       (edoc "Set the shell the base's terminals run without a command." (value string "the shell"))
       (client:request 'vt-option 'shell value)]))
  (define (own-head actor)
    (unless (equal? actor (client:identity)) (error 'vt "an attached head acts as itself")))
  (edefine (open! actor command directory rows cols scheme)
    (edoc "Ask the base to open a terminal for this head; its buffer id."
          (actor any "the actor identity")
          (command (or string #f) "the command line, or #f for the shell")
          (directory directory "the working directory")
          (rows integer "the rows")
          (cols integer "the columns")
          (scheme any "the color scheme")
          (returns integer))
    (own-head actor)
    (client:request 'vt-open command directory rows cols scheme))
  (edefine (send! actor id text size paste? scheme)
    (edoc "Send text to a terminal as this head, typed or pasted."
          (actor any "the actor identity")
          (id integer "the buffer id")
          (text string "the text")
          (size list "(rows cols)")
          (paste? boolean "whether it is a paste")
          (scheme any "the color scheme"))
    (own-head actor)
    (client:request 'vt-send id text size paste? scheme))
  (edefine (close! id)
    (edoc "Close a terminal by its buffer id."
          (id integer "the buffer id"))
    (client:request 'vt-close id))
  (edefine (color-scheme! scheme actor)
    (edoc "Tell the terminals this head's color scheme."
          (scheme (or (one-of dark light) #f) "the scheme")
          (actor any "the actor identity"))
    (own-head actor)
    (client:request 'vt-color scheme))
)
