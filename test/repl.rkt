#lang racket

(define ns (make-base-empty-namespace))
(parameterize ([current-namespace ns])
  (namespace-require 'plai-typed))

(define (tl expect expr)
  (define s
    (with-output-to-string
      (lambda ()
        (parameterize ([current-namespace ns])
          ((current-print) (eval `(#%top-interaction . ,expr)))))))
  (unless (equal? s expect)
    (error 'test "failed: ~e vs. ~e" expect s)))

(tl "- number\n1\n" '1)
(tl "" '(define x 5))
(tl "- number\n5\n" 'x)

(tl "" '(define b (box (list))))
(tl "- (boxof (listof '_a))\n'#&()\n" 'b)
(tl "- void\n" '(set-box! b (list 'a)))
(tl "- (boxof (listof symbol))\n'#&(a)\n" 'b)
