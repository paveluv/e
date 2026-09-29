;; Load this file, then (editor-example:open! (head:buffer-store-id (head:current-buffer))).
;; The window is only the outer host; both inner editors use the same document.
(define (editor-example:open! document)
  (let* ([who head:ui-actor]
         [left (edit:create-view! who document '())]
         [right (edit:create-view! who document '((wrap . #f)))]
         [root (view:create! who #f 'row 1 '() '())])
    (view:arrange! who
      (list (list root 0 (list (list 'wrapped left '(grow 1)) (list 'unwrapped right '(grow 1))) '())) '())
    (window:show-widget! (head:current-window) root)
    root))
