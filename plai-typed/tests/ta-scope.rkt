#lang plai-typed

(define-type-alias (F 'a 'b) ('a -> 'b))

(define (a [x : 'a] [g : (F number 'a)])
  (list x (g 10)))

(test (a "hello" (lambda (x) "there"))
      (list "hello" "there"))


