#lang racket/base
(require syntax/modcollapse
         racket/list
         racket/string)

(provide collapse-module-path-index/relative)

;; This is a copy of the relative-path conversion in
;; Racket 6.1.1.8, copied here for backward compatibility.

(define (collapse-module-path-index/relative mpi)
  (define relative?
    (let loop ([mpi mpi])
      (define-values (path base) (module-path-index-split mpi))
      (let path-loop ([path path])
        (cond
         [(not path)
          (not base)]
         [(symbol? path)
          #f]
         [(and (pair? path)
               (or (eq? (car path) 'lib)
                   (eq? (car path) 'planet)
                   (eq? (car path) 'quote)))
          #f]
         [(and (pair? path)
               (eq? (car path) 'submod)
               (not (or (equal? (cadr path) ".")
                        (equal? (cadr path) ".."))))
          (path-loop (cadr path))]
         [(and (pair? path)
               (eq? (car path) 'file)
               (complete-path? (cadr path)))
          #f]
         [(and (path? path)
               (complete-path? path))
          #f]
         [else
          (or (not base)
              (and (module-path-index? base)
                   (loop base)))]))))
  
  (if relative?
      (let loop ([mpi mpi])
        (define-values (s base) (if mpi
                                    (module-path-index-split mpi)
                                    (values #f #f)))
        (cond
         [(not s) #f]
         [else
          (define full-prev (loop base))
          (cond
           [(not full-prev)
            s]
           [else
            (define prev (if (and (pair? full-prev)
                                  (eq? 'submod (car full-prev)))
                             (cadr full-prev)
                             full-prev))
            (let s-loop ([s s])
              (cond
               [(string? s)
                ;; Unix-style relative path string
                (cond
                 [(string? prev)
                  (define l (drop-right (explode-relpath-string prev) 1))
                  (if (null? l)
                      s
                      (string-join (append
                                    (for/list ([e (in-list l)])
                                      (case e
                                        [(same) "."]
                                        [(up) ".."]
                                        [else (path-element->string e)]))
                                    (list s))
                                   "/"))]
                 [(path? prev)
                  (define-values (base name dir?) (split-path prev))
                  (apply build-path (if (path? base) base 'same) (explode-relpath-string s))]
                 [else ; `(file ,...)
                  (define-values (base name dir?) (split-path (cadr prev)))
                  (apply build-path (if (path? base) base 'same) (explode-relpath-string s))])]
               [(and (pair? s) (eq? 'file (car s)))
                (build-path
                 (let-values ([(base name dir?)
                               (split-path
                                (cond
                                 [(string? prev) prev]
                                 [(path? prev) prev]
                                 [else ; `(file ,...)
                                  (cadr prev)]))])
                   (if (path? base) base 'same))
                 (cadr s))]
               [(eq? (car s) 'submod) 
                (define (as-submod p sm)
                  (if (and (pair? p) (eq? 'submod (car p)))
                      (append p sm)
                      `(submod ,p ,@sm)))
                (cond
                 [(equal? (cadr s) ".")
                  (as-submod full-prev (cddr s))]
                 [(equal? (cadr s) "..")
                  (as-submod full-prev (cdr s))]
                 [else
                  (as-submod (s-loop (cadr s)) (cddr s))])]))])]))
      (collapse-module-path-index
       mpi
       (lambda ()
         (error 'collapse-module-path-index
                "internal error: should not have needed a base path")))))

(define (explode-relpath-string p)
  (map (lambda (p)
         (cond [(assoc p '((#"." . same) (#".." . up))) => cdr]
               [else (bytes->path p)]))
       (regexp-split #rx#"/+" (string->bytes/utf-8 p))))
