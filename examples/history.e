;; (history-example:create!) returns a nested composition, with no window.
;; (window:show-widget! (head:current-window) (history-example:create!)) hosts it.
(import (prefix (core port) port:))
(port:register! '(view history-choice-preview 1)
  '((input selection (or row-selection #f) (options selection))))
(widget:register! 'history-choice-preview 1
  (list (cons 'prepare
          (lambda (id source inputs)
            (let ([p (assq 'selection inputs)])
              (if (and p (eq? (cadr p) 'ready) (caddr p))
                (string-append "Selected: " (caddr (caddr p))) "Choose a filename"))))
    (cons 'render (lambda (text d width height range)
                    (if (zero? (car range)) (list (glyph:fit text width)) '())))))
(history-view:register! 'file-choice 1
  (lambda (query)
    (let* ([who head:ui-actor] [root (view:create! who #f 'row 1 '() '())]
           [table (table:create! who query '(name))]
           [preview (view:create! who #f 'history-choice-preview 1 '((selection . #f)) '())])
      (view:arrange! who
        (list (list root 0 (list (list 'table table '(grow 1)) (list 'preview preview '(grow 1))) '())) '())
      (connection:bind! who root (list (list preview 'selection #f (list table 'selection)))) root)))

(define (history-example:create!)
  (let* ([who head:ui-actor] [history (history:create! who 'persistent)]
         [text (list 'buffer (store:create! who "history note" '("Text, evaluation and a connected table share one host.") '((internal . #t))))]
         [env (environment:create! who
                (list (cons 'directory (current-directory)) '(roots) '(imports (chezscheme))) 'persistent)]
         [job (environment:evaluate! who env 1 "(+ 20 22)")]
         [source (collection:create-source! who '((name "Filename" string))
                   '#(("alpha.sls" ((name . "alpha.sls")) ()) ("beta.ss" ((name . "beta.ss")) ())) 'persistent)]
         [query (collection:create! who source "" '() 'persistent (list source))])
    (history:append! who history 0 (list 'text 1 text) text (list text))
    (history:append! who history 1 (list 'result 1 job) #f (list env job))
    (history:append! who history 2 (list 'file-choice 1 query) #f (list query))
    (history-view:create! history 3)))
