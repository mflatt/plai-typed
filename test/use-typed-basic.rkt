#lang plai-typed
(require "basic.rkt")

(test 9 ((some-v sid) 9))
(test 'ok ((some-v sid) 'ok))

(test 7 (type-case (T number) (ordinal 7)
          [v (f) (f 6)]
          [ordinal (n) n]))

(test 10 ((lambda ([x : IntT]) ((v-f x) 9)) (v add1)))

(define (sgf [sg : SharedGraph$]) sg)

(test (list) (unbox boxed-null))

(test 1 (length (list kons)))

(test 88 (twice 44))
