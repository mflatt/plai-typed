#lang racket/base
(require (for-syntax racket/base
                     racket/require-transform)
         (except-in "main.rkt" 
                    #%module-begin #%top-interaction
                    define-syntax define-syntax-rule
                    require typed-in opaque-type-in
                    module+))

(provide (all-from-out "main.rkt")
         (rename-out [module-begin #%module-begin])
         #%top-interaction
         define-syntax define-syntax-rule
         module+
         require typed-in opaque-type-in)

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (quasisyntax/loc stx
       (#%module-begin
        form ...
        (provide #,(datum->syntax stx `(,#'all-defined-out)))))]))

(define-syntax typed-in 
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx (:)
       [(_ lib [id : type] ...)
        (begin
          (let ([lib #'lib]
                [ids (syntax->list #'(id ...))])
            (unless (module-path? (syntax->datum lib))
              (raise-syntax-error #f "bad module path" stx lib))
            (for ([id (in-list ids)])
              (unless (identifier? id)
                (raise-syntax-error #f "expected an identifier" stx id))))
          (expand-import #'(only-in lib id ...)))]))))

(define-syntax opaque-type-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ lib [id pred-id] ...)
        (begin
          (let ([lib #'lib]
                [ids (syntax->list #'(id ... pred-id ...))])
            (unless (module-path? (syntax->datum lib))
              (raise-syntax-error #f "bad module path" stx lib))
            (for ([id (in-list ids)])
              (unless (identifier? id)
                (raise-syntax-error #f "expected an identifier" stx id))))
          (expand-import #'(only-in lib pred-id ... [pred-id id] ...)))]))))

(module reader syntax/module-reader
    plai-typed/untyped)
