#! /usr/bin/env scheme-script
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(eval
  '(begin
     (import (prefix (service environment) environment:) (prefix (service history) history:) (prefix (state model) model:)
       (prefix (state store) store:) (prefix (test) test:))
     (define saved (read))
     (define env (caddr saved))
     (define job (cadddr saved))
     (define (get r k) (cdr (assq k r)))
     (define (value id) (get (model:snapshot id) 'value))
     (apply store:import! (car saved))
     (apply model:import! (cadr saved))
     (dynamic-wind void
       (lambda ()
         (environment:restore!)
         (let* ([output (cadr (get (value job) 'output))]
                [fresh (environment:evaluate! '(head "restored") env (get (value env) 'generation) "transient-binding")])
           (test:await 'restored-worker (lambda () (eq? (get (value fresh) 'status) 'error)))
           (test:check 'portable-history-and-output-survive-with-fresh-native-bindings
             (list (get (value job) 'result) (get (value job) 'channels)
               (store:line output 0) (store:property output 'internal) (store:exists? (list-ref saved 4))
               (map (lambda (id) (get (value id) 'count)) (model:ids 'history))
               (map (lambda (id) (car (get (value id) 'recipe))) (model:ids 'history-item)))
             '((value ((portable)) "((portable))") ((stdout 0 0)) "retained" #t #t (2) (result unregistered-example)))))
       environment:stop!)
     (test:finish! 'environment-recovery)))
