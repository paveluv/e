;; merge.sls -- resolving merge conflicts: the library (merge).
;;
;; A merge with the disk leaves <<<<<<< buffer / ======= / >>>>>>> disk
;; markers in the text: next! hops to a conflict, keep-mine! and
;; keep-disk! resolve the one at point, each as one undo step.  The
;; commands work on the current buffer through edit's editing API; the
;; keys M-n, M-m and M-d are bound in init!, owned by the module.

(import (only (foundation edoc) elibrary))
(elibrary (apps merge)
  (export init! keep-disk! keep-mine! next!)
  (import (chezscheme)
          (prefix (foundation string) string:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:))

  (define (conflict-marker? b row prefix)
    (and (>= row 0) (< row (head:buffer-line-count b))
         (string:prefix? prefix (head:buffer-line b row))))

  (define (conflict-at row)
    ;; The (start mid end) marker rows of the conflict containing row,
    ;; or #f.
    (let ([b (head:current-buffer)])
      (let up ([r row])
        (cond
          [(< r 0) #f]
          [(and (< r row) (conflict-marker? b r ">>>>>>>")) #f]
          [(conflict-marker? b r "<<<<<<<")
           (let mid ([m (+ r 1)])
             (cond
               [(>= m (head:buffer-line-count b)) #f]
               [(conflict-marker? b m "=======")
                (let end ([e (+ m 1)])
                  (cond
                    [(>= e (head:buffer-line-count b)) #f]
                    [(conflict-marker? b e ">>>>>>>")
                     (and (>= e row) (list r m e))]
                    [else (end (+ e 1))]))]
               [else (mid (+ m 1))]))]
          [else (up (- r 1))]))))

  (define (delete-rows! r1 r2)
    ;; Remove rows r1..r2 inclusive, joining across their newlines.
    (head:goto! (cons r1 0))
    (let ([n (let loop ([r r1] [n 0])
               (if (> r r2)
                   n
                   (loop (+ r 1)
                         (+ n 1 (string-length
                                  (head:buffer-line (head:current-buffer) r))))))])
      (do ([i 0 (+ i 1)]) ((= i n)) (edit:delete-forward!))))

  (edoc "Move point to the next merge conflict marker, wrapping around at the end of the buffer.")
  (define (next!)
    ;; Point to the next conflict's <<<<<<< line, wrapping around.
    (let* ([b (head:current-buffer)]
           [n (head:buffer-line-count b)]
           [from (car (head:point))]
           [hit (let scan ([r (+ from 1)] [left n])
                  (cond [(zero? left) #f]
                        [(>= r n) (scan 0 left)]
                        [(conflict-marker? b r "<<<<<<<") r]
                        [else (scan (+ r 1) (- left 1))]))])
      (if hit
          (head:goto! (cons hit 0))
          (edit:set-message! "No conflicts"))
      (void)))

  (define (resolve! label keep! kept)
    ;; the conflict at point resolved by keep! over its marker rows, as
    ;; one undo step, or the message that point is outside one
    (let ([c (conflict-at (car (head:point)))])
      (if c
          (begin
            (edit:call-as-one-edit! label (lambda () (keep! c) (head:goto! (cons (car c) 0))))
            (edit:set-message! kept))
          (edit:set-message! "Not in a conflict"))
      (void)))

  (edoc "Resolve the merge conflict at point in the buffer's favor, as one undo step."
        (edits))
  (define (keep-mine!)
    (resolve! "keep mine"
      (lambda (c) (delete-rows! (cadr c) (caddr c)) (delete-rows! (car c) (car c)))
      "Kept the buffer side"))

  (edoc "Resolve the merge conflict at point in the disk's favor, as one undo step."
        (edits))
  (define (keep-disk!)
    (resolve! "keep disk"
      (lambda (c) (delete-rows! (caddr c) (caddr c)) (delete-rows! (car c) (cadr c)))
      "Kept the disk side"))

  (edoc "Install the merge keys: M-n, M-m and M-d.")
  (define (init!)
    (keymap:bind-default! "M-n" next!)
    (keymap:bind-default! "M-m" keep-mine!)
    (keymap:bind-default! "M-d" keep-disk!)))
