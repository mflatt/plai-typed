#lang racket/base

(module m1 plai-typed
  (define (f x) x))

(module m2 racket/base
  (require (submod ".." m1))
  (f 1))
