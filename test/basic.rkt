#lang plai-typed

(require (typed-in racket/base
                   [expt : (number number -> number)]
                   [vector-immutable : ('a 'a 'a -> (vectorof 'a))]))

(define x : s-expression '(a 2 "c" '(d)))
(define (f [y : number]) y)

(print-only-errors #t)

(test 1024 (expt 2 10))
(test (vector 3 2 0) (vector-immutable 3 2 0))
(test (make-vector 3 2) (vector-immutable 2 2 2))
(test (make-vector 3 "apple") (vector-immutable "apple" "apple" "apple"))

(define apply-identity : (('a -> 'a) 'a -> (listof 'a))
  (lambda (id x)
    (list (id x) (id x))))

(define box-number
  (lambda (n)
    (if (= n 0)
        (box 0)
        (box n))))

(test 12 (+ (max (add1 7) 0) (min (sub1 5) 9)))

(test #t (zero? (- (ceiling (floor (remainder 5 3)))
                   (identity (modulo 5 3)))))
(test #t (odd? 7))
(test #f (even? 7))

(test 3 (length (list "a" "b" "c")))
(test "b" (list-ref (list "a" "b" "c") 1))
(test "b" (second (list "a" "b" "c")))
(test "c" (third (list "a" "b" "c")))
(test "d" (fourth (list "a" "b" "c" "d")))

(test #t (s-exp-list? x))
(test #f (s-exp-string? x))
(test #f (s-exp-symbol? x))
(test #f (s-exp-number? x))
(test (symbol->s-exp 'a) (first (s-exp->list x)))
(test #t (s-exp-symbol? (first (s-exp->list x))))
(test #t (s-exp-number? (first (rest (s-exp->list x)))))
(test #t (s-exp-string? (first (rest (rest (s-exp->list x))))))
(test #t (s-exp-list? (first (rest (rest (rest (s-exp->list x)))))))
(test #t (s-exp-string? (string->s-exp "a")))
(test #t (s-exp-number? (number->s-exp 2)))
(test #t (s-exp-list? (list->s-exp (list))))
(test #t (s-exp-list? (list->s-exp (list (number->s-exp 2)))))

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

(define il (list (lambda (x) x)))
(test 5 ((first il) 5))
(test "a" ((first il) "a"))

(define-type (T 'a) 
  [v (f : ('a -> 'a))]
  [ordinal (n : number)])
(define i (v (lambda (x) x)))
(test 10 ((v-f i) 10))
(test "a" ((v-f i) "a"))
(test #t (v? i))
(test #f (v? (ordinal 8)))

(define-type-alias IntT (T number))
(define-type-alias (XT 'x 'y) (T 'x))
(test 7 ((lambda ([i : IntT]) (type-case (T number) i
                                [v (f) (f 6)]
                                [ordinal (n) n]))
         (v (lambda (x) (+ 1 x)))))
(test 7 ((lambda ([i : (XT number string)]) (type-case (T number) i
                                              [v (f) (f 6)]
                                              [ordinal (n) n]))
         (v (lambda (x) (+ 1 x)))))

(test 5 (type-case (T number) i
          [ordinal (n) n]
          [else 5]))

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

(test 2 (case 'apple
          [(donut) 1]
          [(apple banana) 2]
          [else 5]))

(test 7 (cond
         [(= 0 1) 6]
         [else 7]))

(test/exn (cond) "no matching")
(test/exn (cond [#f 10]) "no matching")
(test/exn (case 'apple) "no matching")
(test/exn (case 'apple [(banana) 12]) "no matching")

(define vd : void (void))

(define-type SharedGraph$
  [node (s : string)
        (next : (listof SharedGraph$))])

(define g1 : (listof SharedGraph$)
  (shared ([PVD (node "Providence" (list ORH BOS))]
           [ORH (node "Worcester" (list PVD BOS))]
           [BOS (node "Boston" (list PVD ORH))])
    (list PVD ORH BOS)))

(define n (if #f
              (+ (time 10) 1)
              0))

(test 5 (length (build-list 5 (lambda (i) (if (zero? i) "s" "f")))))

(when (zero? 5) 1 "x")
(unless (odd? 5) 1 "x")

(test #t (member 1 (list 3 2 1)))
(test #f (member 6 (list 3 2 1)))

(test 1 (type-case (optionof 'a) (none)
          [none () 1]
          [some (v) 2]))
(test 5 (type-case (optionof 'a) (some 5)
          [none () 1]
          [some (v) v]))

(test #t (some? (some 5)))
(test #t (none? (none)))
(test #f (none? (some 5)))
(test #f (some? (none)))

(define sid (some (lambda (x) x)))
(test 5 ((some-v sid) 5))
(test "5" ((some-v sid) "5"))

(define ht (make-hash (list)))
(test (none) (hash-ref ht "a"))
(test (void) (hash-set! ht "a" 1))
(test (some 1) (hash-ref ht "a"))
(test (list "a") (hash-keys ht))
(test (void) (hash-remove! ht "a"))
(define ht2 (make-hash (list (values 1 'a) (values 3 'b))))
(test (some 'a) (hash-ref ht2 1))
(test (some 'b) (hash-ref ht2 3))
(test (none) (hash-ref ht2 5))
(test 4 (let ([l (hash-keys ht2)])
          (+ (first l) (second l))))

(define-type linked-list
  [llnode (s : string)
          (next : (optionof linked-list))])

(define lls (shared ([x (llnode "a" (some x))]
                     [y (some (llnode "b" (none)))])
              (list x (some-v y))))
(test "a" (llnode-s (some-v (llnode-next (first lls)))))
(test "b" (llnode-s (second lls)))

(test #t (s-exp-symbol? `a))
(test '3 (second (s-exp->list `(a ,(number->s-exp (+ 1 2)) c))))
(test '4 (third (s-exp->list `(a ,@(list '3 '4) c))))
(test (second (s-exp->list `(a `,@(list '3 '4) c))) 
      '(quasiquote (unquote-splicing (list '3 '4))))

(define kons cons)
(define boxed-null (box (list)))

