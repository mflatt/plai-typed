#lang racket/base
(require (for-syntax racket/base))

(provide (for-syntax fixup-quote))

(define-for-syntax (fixup-quote stx alt-quote)
  (syntax-case stx (submod)
    [(q id) 
     (and (identifier? #'q)
          (free-identifier=? #'q alt-quote))
     (datum->syntax stx `(,#'quote ,#'id) stx stx)]
    [(submod (q id) . _)
     (and (identifier? #'q)
          (free-identifier=? #'q alt-quote))
     (syntax-case stx ()
       [(sm q . rest)
        (datum->syntax stx `(,#'sm ,(fixup-quote #'q alt-quote) . ,#'rest) stx stx)])]
    [_ stx]))
