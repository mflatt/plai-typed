#lang plai-typed

(define (msg o m a)
  ((o m) a))

(define-syntax (object/self-2 x)
  (syntax-case x ()
    [(object [mtd-name (var) val] ...)
     (with-syntax ([self (datum->syntax x 'self)])
       #'(letrec ([self
                   (lambda (msg)
                     (case msg
                       [(mtd-name) (lambda (var) val)]
                       ...))])
           self))]))

(define o-2
  (object/self-2
   [first (x) (msg self 'second (+ x 1))]
   [second (x) (+ x 1)]))


(define-type Expr
  [one (a : number)]
  [two (b : Expr)])

(define (get-number e)
  (type-case Expr e
    [one (n) n]
    [two (e) (get-number e)]))
