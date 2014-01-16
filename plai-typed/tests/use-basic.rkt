#lang racket
(require "basic.rkt")

(define-syntax-rule (test a b)
  (unless (equal? a b)
    (error 'test "failed: ~.s" 'b)))

(test x '(a 2 "c" '(d) #f))
(test "ok" ((v-f i) "ok"))
(test add1 (v-f (v add1)))
(test #t (v? i))

(test 'err (with-handlers ([exn:fail:contract? (lambda (exn) 'err)])
             (f "a")))
(test 0 (f 0))

(test 'err (with-handlers ([exn:fail:contract? (lambda (exn) 'err)])
             (ordinal "x")))

(test '(6 6) (apply-identity (lambda (x) 6) 5))

(test (void) (set-box! (box-number 5) 7))
(test 'err
      (with-handlers ([exn:fail:contract? (lambda (exn)
                                            (if (regexp-match? #rx"box-number: contract violation" (exn-message exn))
                                                'err
                                                exn))])
        (set-box! (box-number 5) "apple")))

(test (list) (unbox boxed-null))

(test 7 (prm))
(test 18 (parameterize ([prm 18])
           (get-prm)))
(test "5" ((get-prm-getter sprm)))

(test "dog?" (add-char "dog" #\?))

(test 65 (extract-first #"ABC"))
