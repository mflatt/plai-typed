#lang plai-typed

;; from Shriram

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define (parse s) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]  ;; CAST
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])            ;; CAST
       (case (s-exp->symbol (first sl))     ;; CAST
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-)
          (cond
            [(empty? (rest (rest sl)))
             (uminusS (parse (second sl)))]
            [else
             (bminusS (parse (second sl)) (parse (third sl)))])]))]))

(print-only-errors #t)

(test (parse '3) (numS 3))
(test (parse '(+ 1 (- 2 3))) (plusS (numS 1) (bminusS (numS 2) (numS 3))))
(test (parse '(* (- 1) 3)) (multS (uminusS (numS 1)) (numS 3)))
