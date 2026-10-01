;; The base search contract shares the editor API fixture and process.
(let ()
  (define actor head:ui-actor)
  (define (get r k) (cdr (assq k r)))
  (define document (store:create! actor "Search source" '("ababa" "ALPHA alpha" "tail")))
  (define editor (view:create! actor document 'editor 1 '() '((0 . 0) (0 . 0) (0 . 0) #f)))
  (define other (store:create! actor "Other source" '("other")))
  (define other-editor (view:create! actor other 'editor 1 '() '((0 . 0) (0 . 0) (0 . 0) #f)))
  (define (request target document revision sequence start needle fold? visible)
    (map cons '(target document basis sequence start needle fold? visible direction summary? overlap?)
      (list target document revision sequence start needle fold? visible 'next #f #t)))
  (define initial (request editor document 0 7 '(0 . 3) "aba" #f '(0 0 2 4)))
  (define id (search-request:create! actor initial #t))
  (define demand (model:subscribe! (list id) void))
  (define (ready generation)
    (test:await 'search-result
      (lambda () (let ([v (get (model:snapshot id) 'value)])
                   (and (= (get v 'generation) generation) (not (eq? (get v 'status) 'pending)) v))))
    (get (model:snapshot id) 'value))
  (define (result generation) (get (ready generation) 'result))
  (let ([r (result 0)])
    (check 'base-search-wraps-overlapping-matches-and-carries-basis
      (list (get r 'basis) (get r 'hit) (caddr (get r 'annotations)))
      '(0 (0 . 0) (((0 0 0 3) match) ((0 2 0 5) match) ((0 0 0 3) match-point)))))
  (check 'search-request-generation-is-independent-of-result-publication
    (list (search-request:configure! actor id 0 (request editor document 0 8 '(1 . 0) "alpha" #t '(1 0 1 11)))
      (search-request:configure! actor id 0 initial)) '(1 #f))
  (check 'base-search-folds-case (get (result 1) 'hit) '(1 . 0))
  (search-request:configure! actor id 1 (request editor document 0 9 '(1 . 0) "alpha" #f '(1 0 1 11)))
  (check 'base-search-exact-case (get (result 2) 'hit) '(1 . 6))
  (store:edit! actor document 0 (text:make-span 0 0 0 0) '("intro" "") #f)
  (test:await 'search-rebased-after-edit
    (lambda () (let ([r (result 2)]) (and (= (get r 'basis) 1) (equal? (get r 'hit) '(2 . 6))))))
  (search-request:configure! actor id 2 (request editor document 1 10 '(0 . 0) "missing" #f '(0 0 0 4)))
  (search-request:configure! actor id 3 (request other-editor other 0 11 '(0 . 0) "other" #f '(0 0 0 5)))
  (let ([v (ready 4)])
    (check 'search-retarget-fences-results-and-retains-original-cancellation-receiver
      (list (get v 'origin) (get (get v 'request) 'target) (car (get (get v 'result) 'annotations)))
      (list initial other-editor other)))
  (store:reset! actor other (list (make-string 10000 #\a)))
  (search-request:configure! actor id 4 (request other-editor other 1 12 '(0 . 0) "aa" #f '(0 0 0 10000)))
  (let ([r (result 5)])
    (check 'search-annotations-are-bounded-without-counting-all-matches
      (list (get r 'hit) (length (caddr (get r 'annotations))) (get r 'truncated?)) '((0 . 0) 513 #t)))
  (store:delete! actor other)
  (let* ([preview (search-request:create! actor
                    (map (lambda (p) (case (car p) [(direction) '(direction . previous)] [(summary?) '(summary? . #t)] [else p])) initial) #f)]
         [token (model:subscribe! (list preview) void)])
    (test:await 'search-summary
      (lambda () (let ([r (get (get (model:snapshot preview) 'value) 'result)]) (and r (get r 'count)))))
    (let* ([v (get (model:snapshot preview) 'value)] [r (get v 'result)])
      (check 'backward-search-and-summary-share-overlap-and-rebase-rules-without-an-extra-draft
        (list (get v 'draft) (get r 'hit) (get r 'ordinal) (get r 'count)) '(#f (1 . 2) 2 2)))
    (search-request:configure! actor preview 0
      (map (lambda (p) (if (eq? (car p) 'overlap?) '(overlap? . #f) p))
        (get (get (model:snapshot preview) 'value) 'request)))
    (test:await 'nonoverlapping-preview
      (lambda () (let* ([v (get (model:snapshot preview) 'value)] [r (get v 'result)])
                   (and (= (get v 'generation) 1) r (get r 'count)))))
    (let ([r (get (get (model:snapshot preview) 'value) 'result)])
      (check 'replacement-preview-count-and-navigation-use-nonoverlapping-occurrences
        (list (get r 'hit) (get r 'ordinal) (get r 'count)) '((1 . 0) 1 1)))
    (search-request:close! actor preview) (model:unsubscribe! token))
  (test:await 'search-unavailable (lambda () (eq? (get (ready 5) 'status) 'unavailable)))
  (search-request:close! actor id)
  (model:unsubscribe! demand)
  (check 'closed-search-cannot-be-recreated-by-late-work
    (list (model:snapshot id) (search-request:configure! actor id 5 initial) (store:exists? document)) '(#f #f #t))
  (store:delete! actor document))
