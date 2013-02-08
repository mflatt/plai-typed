#lang racket/base
(require (for-syntax racket/base)
         (except-in "main.rkt" 
                    #%module-begin #%top-interaction
                    define-syntax define-syntax-rule
                    module+))

(provide (all-from-out "main.rkt")
         (rename-out [module-begin #%module-begin])
         #%top-interaction
         define-syntax define-syntax-rule
         module+)

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (quasisyntax/loc stx
       (#%module-begin
        form ...
        (provide #,(datum->syntax stx `(,#'all-defined-out)))))]))

(module reader syntax/module-reader
    plai-typed/untyped)
