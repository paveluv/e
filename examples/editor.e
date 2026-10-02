;; Load this file, then (editor-example:open! window document).
;; The window is only the outer host; both inner editors use the same document.
(define (editor-example:open! window document)
  (window-control:open-app! window "editors"
    (lambda (owner commands)
      (let* ([who head:ui-actor]
             [root (view:create! who #f 'row 1 '() '() owner)]
             [left (edit:create-view! who document '() root)]
             [right (edit:create-view! who document '((wrap . #f)) root)])
        (view:arrange! who
          (list (list root 0 (list (list 'wrapped left '(grow 1)) (list 'unwrapped right '(grow 1))) '())) '())
        root))))
