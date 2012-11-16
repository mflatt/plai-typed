#lang racket
(require rackunit)

(define (syn-test exp reg)
  (define ns (make-base-namespace))
  (check-regexp-match
   (if (string? reg) (regexp-quote reg) reg)
   (with-handlers ((exn:fail:syntax? exn-message))
     (parameterize ([current-namespace ns])
       (eval exp)
       "NO SYNTAX ERROR"))))

(syn-test
 '(module m plai-typed
    
    (define-type TEnv
      [mt]
      [bind (x : symbol)
            (t : string)
            (rest : TEnv)])
    
    (define (lookup a-tenv id)
      (type-case TEnv a-tenv
        [mt (error 'lookup "ack!")]
        [bind (x t rest)
              (if (equal? x id)
                t
                (lookup rest))])))
 #rx"type-case: .*mt clause.*")

;; Double-check value restrction:
(syn-test
 '(module m plai-typed
    (local [(define f (local [(define b (box (list)))]
                        (lambda (x sel?)
                          (if sel?
                              (first (unbox b))
                              (begin
                                (set-box! b (list x))
                                x)))))]
      (begin
        (f 10 #f)
        (string-append "x" (f "hi" #t)))))
 #rx"typecheck failed: number vs string")
          
;; Check that polymorphism inference in nested scopes
;; doesn't go wrong:
(syn-test
 '(module m plai-typed
    
    (define member : ('a 'b -> 'c)
      (lambda (e l)
        false))
    
    (local [(define (in? n)
              (member n n))]
      (if (string=? "in?" "in?") 
          in?
          (lambda (n) (void)))))
 #rx"typecheck failed: void vs boolean")

(syn-test
 '(module m plai-typed
    (quote #"x"))
 #rx"disallowed content")

(syn-test
 '(module m plai-typed
    (quasiquote #"x"))
 #rx"disallowed content")

(syn-test
 '(module m plai-typed
    (quasiquote unquote))
 #rx"bad syntax")

(syn-test
 '(module m plai-typed
    (quasiquote (unquote 5)))
 #rx"number vs s-expression")

(syn-test
 '(module m plai-typed
    (quasiquote (1 (unquote-splicing 5) 3)))
 #rx"number vs .listof s-expression.")


(syn-test
 '(module m plai-typed
    (define b (let ([y (box (list))])
                (lambda () y)))
    (define c b)
    (set-box! (c) (list 1))
    (string-append (first (unbox (c))) "x"))
 #rx"string vs number")

(syn-test
 '(module m plai-typed
    (define a (box (lambda (x) x)))
    (define (set v)
      (set-box! a v))
    (set (lambda (x) (+ x 1)))
    (set (lambda (x) (string-append x "1"))))
 #rx"number vs string")
