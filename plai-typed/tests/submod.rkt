#lang plai-typed

(module m racket/base
  (provide f)
  (define (f x) x))
(require (typed-in (submod "." m)
                   [f : (number -> number)]))


(module m2 racket/base
  (provide g)
  (define (g x) x))
(require (typed-in 'm2
                   [g : (number -> number)]))


(module n plai-typed
  (define n : number 8))
(require (submod "." n))

(module n2 plai-typed
  (define n2 : number 7))
(require 'n2)

(module p plai-typed
  (define p1 : number -3))
(require (rename-in (submod "." p)
                    [p1 p]))

(module p2 plai-typed
  (define p : number -4))
(require (rename-in 'p2
                    [p p2]))

(print-only-errors #t)
(test (- (g (f (+ n n2))) (+ p p2)) 22)
