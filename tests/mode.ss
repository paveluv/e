#!/usr/bin/env scheme-script

;; The mode registry: registration and lookup, detection by extension
;; and interpreter line, hand-chosen modes, the memoized stylers, and
;; re-resolution after a re-registration.  Headless: (head) supplies
;; both store-backed and local buffers.  Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:) (head literal)
             (prefix (core kernel) kernel:)
             (prefix (head mode) mode:)
             (prefix (head head) head:)
             (prefix (state store) store:)
             (only (chezscheme) format box unbox set-box!)
             (prefix (modes scheme-mode) scheme-mode:) (prefix (apps pretty-scheme) pretty-scheme:)
             (prefix (head keymap) keymap:))


     (define check test:check)

     ;; -- registration and lookup -------------------------------------

     (define styler-calls (box 0))
     (define (probe-styler s)
       (set-box! styler-calls (+ (unbox styler-calls) 1))
       (make-vector (string-length s) 'keyword))

     (mode:register! "probe" '(".probe") '("probesh") probe-styler)

     (check 'find (mode:name (mode:find "probe")) "probe")
     (check 'find-missing (mode:find "no-such-mode") #f)
     (check 'extensions (mode:extensions (mode:find "probe")) '(".probe"))
     (check 'no-render (mode:render (mode:find "probe")) #f)

     ;; -- detection -----------------------------------------------------------

     (define by-file (head:new-buffer! "x.probe"))
     (head:buffer-file-set! by-file "/nowhere/x.probe")
     (head:with-buffer-mirror by-file (mode:assign!))
     (check 'detect-by-extension (mode:name-of (head:buffer-store-id by-file)) "probe")
     (check 'detected-is-auto (head:buffer-mode-auto by-file) #t)

     (define by-interpreter (head:new-buffer! "script"))
     (head:buffer-lines-set! by-interpreter (vector "#!/usr/bin/env probesh" "x"))
     (head:with-buffer-mirror by-interpreter (mode:assign!))
     (check 'detect-by-interpreter (mode:name-of (head:buffer-store-id by-interpreter)) "probe")

     (define plain (head:new-buffer! "notes.txt"))
     (head:buffer-file-set! plain "/nowhere/notes.txt")
     (head:with-buffer-mirror plain (mode:assign!))
     (check 'detect-nothing (mode:name-of (head:buffer-store-id plain)) #f)
     (check 'of-nothing (mode:of (head:buffer-store-id plain)) #f)

     ;; -- choosing by hand ----------------------------------------------------

     (head:with-buffer-mirror plain (mode:choose! "probe"))
     (check 'chosen (list (mode:name-of (head:buffer-store-id plain)) (mode:key-context plain)) '("probe" probe))
     (check 'chosen-is-not-auto (head:buffer-mode-auto plain) #f)
     (head:with-buffer-mirror plain (mode:choose! #f))
     (check 'unchosen (mode:of (head:buffer-store-id plain)) #f)

     ;; the optional buffer goes as its literal or by name
     (mode:choose! "probe" (store:find-named "notes.txt"))
     (check 'chosen-by-literal (mode:name-of (store:find-named "notes.txt")) "probe")
     (check 'mode-rejects-name-coercion (test:raises? (lambda () (mode:choose! "probe" "notes.txt"))) #t)
     (check 'chosen-by-name (list (mode:name-of (store:find-named "notes.txt")) (mode:name-of (head:buffer-store-id plain))) '("probe" "probe"))
     (mode:choose! #f (store:find-named "notes.txt"))
     (mode:assign! (store:find-named "notes.txt"))
     (check 'assigned-by-name (list (mode:of (store:find-named "notes.txt")) (head:buffer-mode-auto plain)) '(#f #t))

     (check 'adoption-distinguishes-undetected-and-explicit-no-mode
       (map (lambda (choice)
              (let* ([id (store:create! '(agent mode-test) "mode adoption" '("text")
                           (append '((file . "/nowhere/x.probe") (wrap . default))
                             (if (eq? choice 'missing) '() (list (cons 'mode choice) '(mode-auto . #f)))))]
                     [events '()]
                     [token (store:subscribe! id (lambda (event) (set! events (cons event events))))]
                     [b (head:adopt-store-buffer! id)])
                (store:unsubscribe! token)
                (list (mode:name-of (head:buffer-store-id b)) (head:buffer-mode-auto b) (length events))))
         '(missing #f "probe"))
       '(("probe" #t 2) (#f #f 0) ("probe" #f 0)))

     ;; Observers see the mode and whether it was detected as one choice.
     (define atomic-mode (head:new-buffer! "atomic.probe"))
     (head:buffer-file-set! atomic-mode "/nowhere/atomic.probe")
     (define mode-observations '())
     (define mode-token
       (store:subscribe! (head:buffer-store-id atomic-mode)
         (lambda (event)
           (when (eq? (car event) 'property)
             (set! mode-observations
               (cons (list (mode:name-of (head:buffer-store-id atomic-mode)) (head:buffer-mode-auto atomic-mode))
                     mode-observations))))))
     (head:with-buffer-mirror atomic-mode (mode:choose! "probe"))
     (check 'manual-mode-choice-is-atomic mode-observations '(("probe" #f) ("probe" #f)))
     (set! mode-observations '())
     (head:with-buffer-mirror atomic-mode (mode:assign!))
     (check 'detected-mode-choice-is-atomic mode-observations '(("probe" #t) ("probe" #t)))
     (store:unsubscribe! mode-token)

     ;; A local buffer uses the same mode API, without a store twin.
     (define local (head:new-local-buffer! "*local-mode*"))
     (head:buffer-lines-set! local (vector "#!/usr/bin/env probesh" "local"))
     (head:with-buffer-mirror local (mode:assign!))
     (check 'local-detection (head:with-buffer-mirror local (mode:name-of)) "probe")
     (check 'local-detected-is-auto (head:buffer-mode-auto local) #t)
     (check 'local-has-no-twin (head:buffer-store-id local) #f)
     (head:with-buffer-mirror local (mode:choose! "probe"))
     (check 'local-chosen (head:with-buffer-mirror local (mode:name-of)) "probe")
     (check 'local-chosen-is-not-auto (head:buffer-mode-auto local) #f)
     (check 'local-line-styles
            (vector->list ((mode:line-styles (head:with-buffer-mirror local (mode:of))) "abc"))
            '(keyword keyword keyword))
     (head:with-buffer-mirror local (mode:choose! #f))
     (check 'local-mode-cleared (head:with-buffer-mirror local (mode:of)) #f)

     ;; -- extensions added later ----------------------------------------------

     (mode:add-extension! "probe" ".pr2")
     (define by-addition (head:new-buffer! "y.pr2"))
     (head:buffer-file-set! by-addition "/nowhere/y.pr2")
     (head:with-buffer-mirror by-addition (mode:assign!))
     (check 'detect-by-added-extension (mode:name-of (head:buffer-store-id by-addition)) "probe")
     (check 'bad-extension-refused
            (guard (ex [else 'refused]) (mode:add-extension! "probe" "pr3"))
            'refused)
     (check 'unknown-mode-refused
            (guard (ex [else 'refused]) (mode:add-extension! "no-such-mode" ".x"))
            'refused)

     ;; -- memoized line styles ------------------------------------------------

     (define styles-of (mode:line-styles (mode:of (head:buffer-store-id by-file))))
     (define line (string #\a #\b #\c))
     (set-box! styler-calls 0)
     (check 'line-styles (vector->list (styles-of line)) '(keyword keyword keyword))
     (styles-of line)
     (styles-of line)
     (check 'line-styles-memoized-by-identity (unbox styler-calls) 1)
     (styles-of (string #\a #\b #\c))
     (check 'line-styles-fresh-string (unbox styler-calls) 2)
     (check 'plain-buffer-styles ((mode:line-styles (mode:of (head:buffer-store-id plain))) "abc") #f)

     (mode:register! "raiser" '(".raise") '() (lambda (s) (error 'raiser "boom")))
     (head:with-buffer-mirror plain (mode:choose! "raiser"))
     (check 'raising-styler-paints-plain ((mode:line-styles (mode:of (head:buffer-store-id plain))) "abc") #f)

     ;; Explicit text presentations share analysis without any head buffer.

     (define analyses (box 0))
     (define row-of
       (mode:memoize-analysis
         (lambda (lines)
           (set-box! analyses (+ (unbox analyses) 1))
           (vector-map string-length lines))))
     (define analysis-lines (vector "one" "three"))
     (define presentation (mode:source analysis-lines '((label . "first"))))
     (define other-presentation (mode:source analysis-lines '((label . "second"))))
     (check 'analysis-is-shared-across-explicit-presentations-with-independent-facts
       (list (row-of presentation 1) (row-of other-presentation 0)
         (row-of presentation -1) (row-of presentation 7) (unbox analyses)
         (map (lambda (s) (mode:source-fact s 'label #f)) (list presentation other-presentation)))
       '(5 3 #f #f 1 ("first" "second")))
     (check 'new-text-is-analyzed-without-invalidating-retained-presentations
       (list (row-of (mode:source (vector "changed") '()) 0) (row-of presentation 1) (unbox analyses))
       '(7 5 2))

     ;; -- re-registration and refresh -----------------------------------------

     (define old (mode:find "probe"))
     (define refresh-writes 0)
     (define refresh-token
       (store:subscribe! #f
         (lambda (event)
           (when (and (eq? (car event) 'property) (memq (caddr event) '(mode mode-auto)))
             (set! refresh-writes (+ refresh-writes 1))))))
     (mode:register! "probe" '(".probe") '("probesh") probe-styler)
     (mode:refresh!)
     (store:unsubscribe! refresh-token)
     (check 'refresh-resolves-replaced-modes-without-rewriting-unchanged-facts
       (list (eq? (mode:find "probe") old) (eq? (mode:of (head:buffer-store-id by-file)) (mode:find "probe"))
             (mode:name-of (head:buffer-store-id by-file)) refresh-writes)
       '(#f #t "probe" 0))

     ;; A derived mode follows live behavior, while detection and keys stay
     ;; its own. Reusing the same line exercises parent-reload cache freshness.
     (define (parent! styler indenter)
       (parameterize ([kernel:registering-module 'derived-parent])
         (kernel:retract-module! 'derived-parent)
         (mode:register! "parent" '(".parent") '("parentsh") styler indenter indenter '(parent-fact))
         (mode:register-indenter! "parent" indenter)
         (mode:register-formatter! "parent" indenter)))
     (define (old-indent b from to) '(1))
     (define (new-indent b from to) '(2))
     (parent! probe-styler old-indent)
     (mode:derive! "child" "parent" '(".child"))
     (mode:derive! "grandchild" "child" '(".grandchild"))
     (mode:derive! "render-child" "parent" '() #f old-indent #f '(child-fact))
     (mode:derive! "replaced-child" "parent" '() #f old-indent old-indent '(child-fact))
     (check 'presentation-facts-follow-effective-callbacks-without-unused-parent-inputs
       (map (lambda (name) (mode:required-facts (mode:find name)))
         '("grandchild" "render-child" "replaced-child" "no-such-mode"))
       '((parent-fact) (child-fact parent-fact) (child-fact) ()))
     (head:with-buffer-mirror plain (mode:choose! "grandchild"))
     (define derived-line (string-copy "abc"))
     ((mode:line-styles (mode:of (head:buffer-store-id plain))) derived-line)
     (mode:indent-on-tab! "parent" #f)
     (check 'derivation-retains-own-detection-and-key-context
       (list (mode:name (mode:detect "x.parent" ""))
             (mode:name (mode:detect "x.child" ""))
             (mode:interpreters (mode:find "child")) (mode:key-context plain)
             (eq? (mode:formatter "child") old-indent) (mode:indent-on-tab? "child"))
       '("parent" "child" () grandchild #t #f))
     (parent! (lambda (s) (make-vector (string-length s) 'string)) new-indent)
     (check 'parent-replacement-updates-presentation-operations-and-cached-styles
       (list (vector->list ((mode:line-styles (mode:of (head:buffer-store-id plain))) derived-line))
             (map (lambda (get) (eq? (get (mode:find "child")) new-indent))
               (list mode:render mode:row-styles))
             (eq? (mode:indenter "grandchild") new-indent)
             (eq? (mode:formatter "grandchild") new-indent) (mode:indent-on-tab? "child"))
       '((string string string) (#t #t) #t #t #f))
     (mode:register-indenter! "child" old-indent #t)
     (mode:register-formatter! "child" old-indent)
     (check 'local-operations-override-inheritance
       (list (eq? (mode:indenter "grandchild") old-indent)
             (eq? (mode:formatter "grandchild") old-indent) (mode:indent-on-tab? "grandchild"))
       '(#t #t #t))
     ;; A submode overrides the presentation parts it defines and inherits the
     ;; rest; its key contexts chain to the parent's; a parent may come later.
     (mode:derive! "styled-child" "parent" '() probe-styler)
     (check 'a-submode-overrides-what-it-defines-and-inherits-the-rest
       (list (eq? (mode:styles (mode:find "styled-child")) probe-styler)
             (eq? (mode:render (mode:find "styled-child")) new-indent)
             (eq? (mode:indenter "styled-child") new-indent)
             (mode:key-contexts plain))
       '(#t #t #t (grandchild child parent)))
     (mode:derive! "orphan" "absent" '())
     (check 'a-parent-registered-later-is-followed-by-name
       (list (mode:styles (mode:find "orphan"))
             (begin (mode:register! "absent" '() '() probe-styler)
                    (eq? (mode:styles (mode:find "orphan")) probe-styler)))
       '(#f #t))
     ;; an extension loaded after its files are open: deriving the mode that
     ;; claims their ending assigns it to them at once, as the worksheet does
     (define late (head:new-buffer! "notes.late"))
     (head:buffer-file-set! late "/tmp/notes.late")
     (head:with-buffer-mirror late (mode:assign!))
     (define before-derivation (mode:name-of (head:buffer-store-id late)))
     (mode:derive! "late" "parent" '(".late"))
     (check 'deriving-a-mode-assigns-it-to-open-buffers-with-its-ending
       (list before-derivation (mode:name-of (head:buffer-store-id late))) '(#f "late"))
     ;; a mode's source edited to claim one more ending, then reloaded: its
     ;; re-registration gives the open buffers with that ending the mode
     (define newly (head:new-buffer! "notes.newly"))
     (head:buffer-file-set! newly "/tmp/notes.newly")
     (head:with-buffer-mirror newly (mode:assign!))
     (define before-reregistration (mode:name-of (head:buffer-store-id newly)))
     (parameterize ([kernel:registering-module 'derived-parent])
       (mode:register! "parent" '(".parent" ".newly") '("parentsh") probe-styler))
     (check 'reregistering-a-mode-with-a-new-ending-assigns-it-to-open-buffers
       (list before-reregistration (mode:name-of (head:buffer-store-id newly)) (mode:name-of (head:buffer-store-id late))) '(#f "parent" "late"))
     ;; a detected or chosen mode stays when others register: registration
     ;; is additive, and a mode chosen by hand follows only its own name
     (head:with-buffer-mirror late (mode:choose! "parent"))
     (parameterize ([kernel:registering-module 'derived-parent])
       (mode:register! "thief" '(".newly" ".late") '() probe-styler))
     (define stolen (head:new-buffer! "z.newly"))
     (head:buffer-file-set! stolen "/tmp/z.newly")
     (head:with-buffer-mirror stolen (mode:assign!))
     (check 'registration-takes-only-buffers-without-a-mode
       (list (mode:name-of (head:buffer-store-id newly)) (mode:name-of (head:buffer-store-id late)) (mode:name-of (head:buffer-store-id stolen))) '("parent" "parent" "thief"))
     (parent! probe-styler new-indent)
     (check 'a-chosen-mode-stays-through-its-reregistration
       (list (mode:name-of (head:buffer-store-id late)) (eq? (mode:of (head:buffer-store-id late)) (mode:find "parent"))) '("parent" #t))
     (check 'a-derivation-cycle-is-refused-and-preserves-the-existing-parent
       (list (test:raises? (lambda () (mode:derive! "parent" "grandchild" '())))
             (mode:extensions (mode:find "parent"))) '(#t (".parent")))
     (kernel:retract-module! 'derived-parent)
     (check 'missing-parent-loses-presentation-but-keeps-child-and-local-overrides
       (list (mode:name-of (head:buffer-store-id plain)) ((mode:line-styles (mode:of (head:buffer-store-id plain))) derived-line)
             (mode:render (mode:find "child")) (eq? (mode:formatter "child") old-indent))
       '("grandchild" #f #f #t))


     ;; pretty-scheme's displays are submodes of Scheme: they indent, format
     ;; and take Tab as Scheme does, with a presentation of their own
     (scheme-mode:init!)
     (pretty-scheme:init!)
     (let* ([lines '#("(define (f x)" "  (+ x 1))")]
            [source (mode:source lines '())]
            [rainbow (mode:row-styles (mode:find "pretty-scheme-rainbow"))]
            [clusters (mode:render (mode:find "pretty-scheme-clusters"))])
       (check 'pretty-modes-render-an-explicit-source-without-a-buffer
         (list (string-length (clusters source 0 (vector-ref lines 0)))
           (not (string=? (clusters source 0 (vector-ref lines 0)) (vector-ref lines 0)))
           (vector-length (rainbow source 1 (vector-ref lines 1)))
           (not (eq? (vector-ref (rainbow source 1 (vector-ref lines 1)) 2) 'plain)))
         '(13 #t 10 #t)))
     (check 'pretty-scheme-modes-inherit-scheme-editing-with-their-own-display
       (list (eq? (mode:indenter "pretty-scheme-rainbow") (mode:indenter "scheme"))
             (eq? (mode:formatter "pretty-scheme-clusters") (mode:formatter "scheme"))
             (mode:indent-on-tab? "pretty-scheme-depth")
             (and (mode:row-styles (mode:find "pretty-scheme-rainbow")) #t)
             (eq? (mode:styles (mode:find "pretty-scheme-rainbow")) (mode:styles (mode:find "scheme")))
             (and (mode:render (mode:find "pretty-scheme-clusters")) #t))
       '(#t #t #t #t #t #t))
     ;; the closing brackets are the hiding modes' own keys, not the global map's
     (check 'pretty-schemes-closing-brackets-are-bound-in-its-hiding-modes-only
       (list (keymap:binding ")") (keymap:binding "]")
             (let ([hit (keymap:resolved-binding 'pretty-scheme-clusters '(")"))]) (and hit (eq? (keymap:call-action-procedure (keymap:binding-action (cdr hit))) pretty-scheme:close-round!)))
             (let ([hit (keymap:resolved-binding 'pretty-scheme-depth '("]"))]) (and hit (eq? (keymap:call-action-procedure (keymap:binding-action (cdr hit))) pretty-scheme:close-square!)))
             (keymap:resolved-binding 'pretty-scheme-rainbow '(")")))
       '(#f #f #t #t #f))

     (test:finish! 'mode)))
