#lang plai-typed

;; Check that `module+` creates a submodule that can be
;; imported as a typed module

(module m1 plai-typed
  (define (f x) x)
  (module+ sub
    (define (g x) x)))

(require (submod "." m1 sub))

(print-only-errors #t)
(test (g 1) 1)
(test (g "x") "x")
