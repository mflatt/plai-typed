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

          
                        