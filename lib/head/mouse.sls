;; TUI pointer adaptation and widget binding inspection.
(import (only (foundation edoc) elibrary))
(elibrary (head mouse)
  (export bindings cancel! gesture-text input! position track!)
  (import (chezscheme) (prefix (head head) head:)
    (prefix (head widget) widget:) (prefix (sys tty) tty:))

  (define last-position #f)
  (define click #f)

  (edoc "The last known one-based pointer cell, retained when keyboard input clears hover emphasis; false before input or when tracking is disabled."
    (returns (or pair #f)))
  (define (position) (or (head:mouse-position) last-position))

  (edoc "Break a pending double-click sequence after keyboard input or composition replacement, preserving the physical pointer location.")
  (define (cancel!) (set! click #f))

  (edoc "Turn terminal mouse reporting on or off; disabling restores native selection and forgets the pointer."
    (on boolean "whether to report the mouse") (public))
  (define (track! on)
    (tty:mouse-reporting! on) (cancel!)
    (set! last-position #f) (head:set-mouse-position! #f)
    (head:report! (format "Mouse ~a" (if on "on" "off"))))

  (edoc "Adapt a decoded terminal mouse report to the shown widget tree. Track physical location and double clicks; widget hit-testing, capture and bindings own the interaction. Return ignore for hover motion, otherwise the handled event marker."
    (handle? boolean "whether the current input scope accepts pointer input")
    (phase char "SGR press or release suffix") (bits integer "button and modifier bits")
    (x integer "one-based column") (y integer "one-based row"))
  (define (input! handle? phase bits x y)
    (when handle?
      (set! last-position (cons x y))
      (head:set-mouse-position! last-position)
      (let* ([press? (and (char=? phase #\M) (zero? (bitwise-and bits 96)) (< (bitwise-and bits 3) 3))]
             [now (real-time)] [at (list bits x y)]
             [double? (and press? click (equal? (car click) at) (< (- now (cdr click)) 500))])
        (when press? (set! click (and (not double?) (cons at now))))
        (widget:pointer! (tty:pointer-event phase bits (if double? 2 1)) (- x 1) (- y 1))))
    (if (and (not (zero? (bitwise-and bits 32))) (= (bitwise-and bits 3) 3)) 'ignore "MOUSE-HANDLED"))

  (edoc "Spell a mouse gesture for help: click or drag with a button, or wheel with a direction, followed by its modifier symbols."
        (gesture list "(click-or-drag button modifiers) or (wheel direction modifiers)") (returns string))
  (define (gesture-text gesture)
    (string-append
      (apply string-append (map (lambda (m) (case m [(control) "C-"] [(meta) "M-"] [(shift) "S-"] [else (error 'gesture-text "unknown modifier" m)])) (caddr gesture)))
      (case (car gesture)
        [(wheel) (string-append "Wheel " (symbol->string (cadr gesture)))]
        [(click double-click drag)
         (string-append (case (cadr gesture) [(primary) "Left"] [(middle) "Middle"] [(secondary) "Right"] [else (error 'gesture-text "unknown button" gesture)])
           " " (symbol->string (car gesture)))]
        [else (error 'gesture-text "unknown gesture" gesture)])))

  (edoc "Read semantic mouse bindings at the physical pointer or an explicit one-based cell. This inspects shown widget frames without dispatching input, changing focus or reading the base."
    (locations (list-of pair) "optional (column . row)") (returns list) (effects internal))
  (define (bindings . locations)
    (unless (<= (length locations) 1) (error 'bindings "expected at most one screen cell"))
    (let ([at (if (pair? locations) (car locations) (position))])
      (if at (or (widget:pointer-bindings (- (car at) 1) (- (cdr at) 1)) '()) '()))))
