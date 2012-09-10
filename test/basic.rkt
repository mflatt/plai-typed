#lang plai-typed

(define x '(a 2 "c" '(d)))

(print-only-errors #t)

(test #t (list? x))
(test #f (string? x))
(test #f (symbol? x))
(test #f (number? x))
(test (symbol->s-exp 'a) (first (s-exp->list x)))
(test #t (symbol? (first (s-exp->list x))))
(test #t (number? (first (rest (s-exp->list x)))))
(test #t (string? (first (rest (rest (s-exp->list x))))))
(test #t (list? (first (rest (rest (rest (s-exp->list x)))))))
(test #t (string? (string->s-exp "a")))
(test #t (number? (number->s-exp 2)))
(test #t (list? (list->s-exp (list))))
(test #t (list? (list->s-exp (list (number->s-exp 2)))))

(test 5 (local [(define x 10)]
          (begin
            (set! x 5)
            x)))

(test 5 (call/cc (lambda (x) 5)))
(test 7 (local [(define y (lambda (q) (+ q 3)))]
          (if (= 0 (call/cc (lambda (k)
                              (begin
                                (set! y k)
                                0))))
              (y 2)
              7)))

(local [(define x (lambda ((x : string)) x))]
  (set! x (lambda (y) (string-append y y))))

(define-type (T 'a) [v (f : ('a -> 'a))])
(define i (v (lambda (x) x)))
(test 10 ((v-f i) 10))
(test "a" ((v-f i) "a"))

(define-type-alias IntT (T number))
(define-type-alias (XT 'x 'y) (T 'x))
(test 7 ((lambda ([i : IntT]) (type-case (T number) i
                                [v (f) (f 6)]))
         (v (lambda (x) (+ 1 x)))))
(test 7 ((lambda ([i : (XT number string)]) (type-case (T number) i
                                              [v (f) (f 6)]))
         (v (lambda (x) (+ 1 x)))))

(test #t (letrec ([even? (lambda (n)
                           (if (= 0 n)
                               #t
                               (odd? (- n 1))))]
                  [odd? (lambda (n)
                          (if (= 0 n)
                              #f
                              (even? (- n 1))))])
           (even? 10)))
(test (list 3 1 2) (let ([x 1]
                         [y 2]
                         [z 3])
                     (let ([x z]
                           [y x]
                           [z y])
                       (list x y z))))
(test 4 (let* ([x 1]
               [y 2]
               [x y]
               [y x])
          (+ y y)))

(test (list 2 4 6) (filter (lambda (x) (not (= x 5)))
                           (list 2 5 5 4 6 5)))
(test 10 (foldl (lambda (x n) (+ x n))
                0
                (list 1 2 3 4)))
(test "1234" (foldr (lambda (x n) (string-append (to-string x) n))
                    ""
                    (list 1 2 3 4)))
