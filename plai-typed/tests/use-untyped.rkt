#lang racket
(require "untyped.rkt")

(define-syntax-rule (test a b)
  (unless (equal? a b)
    (error 'test "failed: ~.s" 'b)))

(test x '(a 2 "c" '(d)))
(test "ok" ((v-f i) "ok"))
(test add1 (v-f (v add1)))
(test #t (v? i))

(test "a" (f "a"))
(test 0 (f 0))

(test #t (ordinal? (ordinal "x")))

(test '(6 6) (apply-identity (lambda (x) 6) 5))

(test (void) (set-box! (box-number 5) 7))
(test (void) (set-box! (box-number 5) "apple"))

(test (list) (unbox boxed-null))

(test 7 (prm))
(test 18 (parameterize ([prm 18])
           (get-prm)))
(test "5" ((get-prm-getter sprm)))

(test "dog?" (add-char "dog" #\?))

(test 65 (extract-first #"ABC"))
