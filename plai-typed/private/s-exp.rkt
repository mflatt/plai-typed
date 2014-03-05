#lang racket/base
(provide s-exp?)

(define (s-exp? v)
  (or (symbol? v)
      (number? v)
      (string? v)
      (boolean? v)
      (and (list? v)
           (andmap s-exp? v))))
