#lang racket/base

(module m1 plai-typed
  (define (f x) x))

(module m2 racket/base
  (require (submod ".." m1))
  (f 1))

;; ----------------------------------------

(module m3 plai-typed
  (define (id x) x))

(module m4 racket
  (require (submod ".." m3))
  (id 1))
