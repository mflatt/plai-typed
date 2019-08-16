#lang plai-typed

;; An extra check of polymorphism, and also a check that forcing is ok
;; with a tuple result.

(let ([f (lambda (x) x)])
  (let ([g f])
    (values (g 1) (g #t))))
