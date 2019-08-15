#lang racket/base
(require racket/list
         racket/pretty
         syntax/srcloc
         (for-template racket/contract/base
                       racket/base
                       "s-exp.rkt"))

(provide gen-tvar make-bool make-num make-sym make-str make-chr make-vd make-sexp
         make-arrow make-listof make-boxof make-tupleof make-vectorof 
         make-datatype make-opaque-datatype make-hashof make-parameterof
         to-contract to-expression
         create-defn as-non-poly
         make-poly poly? poly-instance at-source instantiate-constructor-at
         unify! unify-defn!
         let-based-poly!
         lookup
         type->datum)

(define-struct type ([src #:mutable]))

;; The `non-poly` field controls the layer where a type variable can
;; be generalized by let polyporphism. This is necessary because we
;; delay let-based polymorphism to the end of checking a whole module,
;; which allows inferring types of recursive functions (at the expense
;; of potentially looping in the type checker, although it doesn't
;; happen in practice). The `non-poly` field is a list of gensyms that
;; correspods to the `poly-context` field of a `defn`, or it is #f to
;; mean "extension of any poly context". A type variable can be
;; generalized only for a definition whose context is shorter than
;; `non-poly`. Unification finds the common tail of unified type
;; variables.
;;
;; For example, in
;;
;;   (define (f x)
;;     (let ([y x])
;;       y)))
;;
;; the type for `y` should not instantiate a fresh type variable for
;; `x`, which would break the connection between the type and argument.
;; On the ohter hand, in
;;
;;  (let ([f (lambda (x) x)])
;;    (let ([g f])
;;      (values (g 1) (g #t))))
;;
;; the type for `f` should not be monomorphized for `g`, because it can
;; stay polymorphic. The `non-poly` tracking effectively allows the
;; polymorphism of `f` to propagate to uses of `g` without losing the connection
;; between `x` and `y` in the earlier example.

(define-struct (tvar type) ([rep #:mutable] [non-poly #:mutable]) #:transparent)
(define-struct (arrow-tvar tvar) ()) ; must unify with arrow, which helps improve error messages
(define-struct (bool type) ())
(define-struct (num type) ())
(define-struct (sym type) ())
(define-struct (sexp type) ())
(define-struct (vd type) ())
(define-struct (str type) ())
(define-struct (chr type) ())
(define-struct (arrow type) (args result) #:transparent)
(define-struct (listof type) (element))
(define-struct (boxof type) (element))
(define-struct (vectorof type) (element) #:transparent)
(define-struct (hashof type) (key val))
(define-struct (tupleof type) (args))
(define-struct (parameterof type) (element))
(define-struct (datatype type) (id args))
(define-struct (opaque-datatype datatype) (pred))
(define-struct (poly type) (tvar type) #:transparent)

;; A `defn` is a type for a variable bound by `define`. It's meant to
;; support generalization to a polymorphic type, but that
;; generalization requires some care in a recursive-binding setting.
;; The `poly-context` field is a list of gensyms that reflect the
;; definition's nesting --- one gensym for every enclosing binding
;; context. That way, generalization can recognize deeper and
;; shallower bindings.
(define-struct (defn type) (base rhs poly-context [insts #:mutable] [proto-rhs #:mutable]) #:transparent)

(define (to-contract type enforce-poly?)
  (let loop ([type type]
             [tvar-names #hasheq()]
             [inside-mutable? #f])
    (cond
     [(defn? type) 
      ;; is this the right thing?
      (if (defn-rhs type)
          (loop (defn-rhs type) tvar-names inside-mutable?)
          (loop (car (defn-proto-rhs type)) tvar-names inside-mutable?))]
     [(bool? type) #'boolean?]
     [(num? type) #'number?]
     [(sym? type) #'symbol?]
     [(sexp? type) #'s-exp?]
     [(vd? type) #'void?]
     [(str? type) #'string?]
     [(chr? type) #'char?]
     [(arrow? type)
      #`(-> #,@(map (λ (x) (loop x tvar-names inside-mutable?)) (arrow-args type))
            #,(loop (arrow-result type) tvar-names inside-mutable?))]
     [(listof? type) #`(listof #,(loop (listof-element type) tvar-names inside-mutable?))]
     [(boxof? type) #`(box/c #,(loop (boxof-element type) tvar-names #t))]
     [(vectorof? type) #`(vectorof #,(loop (vectorof-element type) tvar-names #t))]
     [(hashof? type) #`(hash/c #,(loop (hashof-key type) tvar-names #t)
                               #,(loop (hashof-val type) tvar-names #t))]
     [(tupleof? type) #`(vector-immutable/c #,@(map (λ (x) (loop x tvar-names inside-mutable?))
                                                    (tupleof-args type)))]
     [(parameterof? type) #`(parameter/c #,(loop (parameterof-element type) tvar-names #t))]
     [(poly? type) (if enforce-poly?
                       (let* ([name (gensym 'a)]
                              [tvar-names (hash-set tvar-names type name)])
                         #`(let ([#,name (new-∀/c '#,name)])
                             #,(loop (poly-type type) tvar-names inside-mutable?)))
                       (loop (poly-type type) tvar-names inside-mutable?))]
     [(opaque-datatype? type) 
      (opaque-datatype-pred type)]
     [(datatype? type) 
      (datum->syntax 
       (datatype-id type)
       (string->symbol (format "~a?" (syntax-e (datatype-id type)))))]
     [(tvar? type)
      ;; this can be done with new-∀ (in the poly? case), but only new-∃ exists at the moment
      (if (tvar-rep type)
          (loop (tvar-rep type) tvar-names inside-mutable?)
          (or (hash-ref tvar-names type #f)
              #'any/c))]
     [else (raise-syntax-error 'to-contract/expr
                               (format "got confused, trying to generate a contract ~s" type) 
                               (and (type? type) (type-src type)))])))

(define (to-expression type tvar-names)
  (let loop ([type type])
    (cond
     [(defn? type) 
      ;; is this the right thing?
      (if (defn-rhs type)
          (loop (defn-rhs type))
          (loop (car (defn-proto-rhs type))))]
     [(bool? type) #'(make-bool #f)]
     [(num? type) #'(make-num #f)]
     [(sym? type) #'(make-sym #f)]
     [(sexp? type) #'(make-sexp #f)]
     [(vd? type) #'(make-vd #f)]
     [(str? type) #'(make-str #f)]
     [(chr? type) #'(make-chr #f)]
     [(arrow? type) #`(make-arrow #f (list #,@(map loop (arrow-args type)))
                                  #,(loop (arrow-result type)))]
     [(listof? type) #`(make-listof #f #,(loop (listof-element type)))]
     [(boxof? type) #`(make-boxof #f #,(loop (boxof-element type)))]
     [(vectorof? type) #`(make-vectorof #f #,(loop (vectorof-element type)))]
     [(hashof? type) #`(make-hashof #f
                                    #,(loop (hashof-key type))
                                    #,(loop (hashof-val type)))]
     [(tupleof? type) #`(make-tupleof #f (list #,@(map loop (tupleof-args type))))]
     [(parameterof? type) #`(make-parameterof #f #,(loop (parameterof-element type)))]
     [(poly? type) (let ([name (gensym)])
                     #`(let ([#,name (gen-tvar #f)])
                         (make-poly #f #,name #,(to-expression (poly-type type)
                                                               (hash-set tvar-names
                                                                         (poly-tvar type)
                                                                         name)))))]
     [(opaque-datatype? type) #`(make-opaque-datatype #f
                                                      (quote-syntax #,(datatype-id type))
                                                      null
                                                      (quote-syntax #,(opaque-datatype-pred type)))]
     [(datatype? type) #`(make-datatype #f
                                        (quote-syntax #,(datatype-id type))
                                        (list #,@(map loop (datatype-args type))))]
     [(tvar? type) (or (hash-ref tvar-names type #f)
                       (let ([t (tvar-rep type)])
                         (if t
                             (loop t)
                             ;; a non-polymophic but ununified type variable;
                             ;; it has to turn into something that never matches
                             `(make-datatype #f (quote-syntax unknown) null))))]
     [else (raise-syntax-error 'to-contract/expr
                               (format "got confused, trying to trun into an expression ~s" type) 
                               (type-src type))])))

(define (gen-tvar src [arrow? #f])
  ((if arrow? make-arrow-tvar make-tvar) src #f #f))

(define ((type->datum tmap) t)
  (cond
   [(tvar? t)
    (if (tvar-rep t)
        ((type->datum tmap) (tvar-rep t))
        (if (arrow-tvar? t)
            '(... -> ...)
            (let ([a (hash-ref tmap t #f)])
              (if a
                  a
                  (let ([a `',(string->symbol
                               (format "~a~a"
                                       (if (eq? (type-src t) 'poly)
                                           ""
                                           "_")
                                       (let ([n (hash-count tmap)])
                                         (if (n . < . 26)
                                             (integer->char (+ 97 n))
                                             (format "a~a" n)))))])
                    (hash-set! tmap t a)
                    a)))))]
   [(num? t) 'number]
   [(bool? t) 'boolean]
   [(sym? t) 'symbol]
   [(str? t) 'string]
   [(chr? t) 'char]
   [(sexp? t) 's-expression]
   [(vd? t) 'void]
   [(arrow? t) `(,@(map (type->datum tmap) (arrow-args t))
                 ->
                 ,((type->datum tmap) (arrow-result t)))]
   [(listof? t) `(listof ,((type->datum tmap) (listof-element t)))]
   [(boxof? t) `(boxof ,((type->datum tmap) (boxof-element t)))]
   [(vectorof? t) `(vectorof ,((type->datum tmap) (vectorof-element t)))]
   [(hashof? t) `(hashof ,((type->datum tmap) (hashof-key t))
                         ,((type->datum tmap) (hashof-val t)))]
   [(tupleof? t) (let ([a (map (type->datum tmap) (tupleof-args t))])
                   (if (null? a)
                       '()
                       (cons (car a)
                             (let loop ([a (cdr a)])
                               (if (null? a)
                                   '()
                                   (list* '* (car a) (loop (cdr a))))))))]
   [(parameterof? t) `(parameterof ,((type->datum tmap) (parameterof-element t)))]
   [(datatype? t) (let ([name (syntax-e (datatype-id t))])
                    (if (null? (datatype-args t))
                        name
                        `(,name ,@(map (type->datum tmap)
                                       (datatype-args t)))))]
   [(poly? t) ((type->datum tmap) ((instance (poly-tvar t)
                                             (gen-tvar 'poly))
                                   (poly-type t)))]
   [else (format "?~s" t)]))

(define (non-poly! t poly-context)
  (let loop ([t t])
    (cond
     [(tvar? t) 
      (cond
       [(tvar-rep t) (loop (tvar-rep t))]
       [else
        (define pc (tvar-non-poly t))
        (cond
         [(not pc)
          (set-tvar-non-poly! t poly-context)]
         [else
          ;; find common tail
          (define lnew (length poly-context))
          (define lold (length pc))
          (let loop ([lnew lnew] [lold lold]
                     [poly-context poly-context] [pc pc]
                     [synth? #f])
            (cond
             [(= lnew lold)
              (if (equal? poly-context pc)
                  (set-tvar-non-poly! t (if synth?
                                            (cons (gensym) poly-context)
                                            poly-context))
                  (loop (sub1 lnew) (sub1 lold)
                        (cdr poly-context) (cdr pc)
                        #t))]
             [(lnew . > . lold)
              (loop (sub1 lnew) lold (cdr poly-context) pc #f)]
             [else
              (loop lnew (sub1 lold) poly-context (cdr pc) #f)]))])])]
     [(arrow? t)
      (loop (arrow-result t))
      (for-each loop (arrow-args t))]
     [(listof? t) (loop (listof-element t))]
     [(boxof? t) (loop (boxof-element t))]
     [(vectorof? t) (loop (vectorof-element t))]
     [(hashof? t) 
      (loop (hashof-key t))
      (loop (hashof-val t))]
     [(parameterof? t) (loop (parameterof-element t))]
     [(tupleof? t) (for-each loop (tupleof-args t))]
     [(datatype? t) (for-each loop (datatype-args t))]
     [(poly? t) (loop (poly-type t))])))

(define (as-non-poly t poly-context)
  (non-poly! t poly-context)
  t)

(define ((instance old-tvar new-tvar) t)
  (cond
   [(eq? t old-tvar) new-tvar]
   [(arrow? t)
    (make-arrow (type-src t)
                (map (instance old-tvar new-tvar)
                     (arrow-args t))
                ((instance old-tvar new-tvar)
                 (arrow-result t)))]
   [(listof? t) (make-listof (type-src t)
                             ((instance old-tvar new-tvar)
                              (listof-element t)))]
   [(boxof? t) (make-boxof (type-src t)
                           ((instance old-tvar new-tvar)
                            (boxof-element t)))]
   [(vectorof? t) (make-vectorof (type-src t)
                                 ((instance old-tvar new-tvar)
                                  (vectorof-element t)))]
   [(hashof? t) (make-hashof (type-src t)
                             ((instance old-tvar new-tvar)
                              (hashof-key t))
                             ((instance old-tvar new-tvar)
                              (hashof-val t)))]
   [(tupleof? t) (make-tupleof (type-src t)
                               (map (instance old-tvar new-tvar)
                                    (tupleof-args t)))]
   [(parameterof? t) (make-parameterof (type-src t)
                                       ((instance old-tvar new-tvar)
                                        (parameterof-element t)))]
   [(poly? t) (make-poly (type-src t)
                         (poly-tvar t)
                         ((instance old-tvar new-tvar)
                          (poly-type t)))]
   [(datatype? t) (if (null? (datatype-args t))
                      t
                      (make-datatype (type-src t)
                                     (datatype-id t)
                                     (map (instance old-tvar new-tvar)
                                          (datatype-args t))))]
   [else t]))

(define (extract-tvars t poly-context)
  (let ([tvars
         (let loop ([t t])
           (cond
            [(tvar? t) (if (tvar-non-poly t)
                           (let ([c1 (length poly-context)]
                                 [c2 (length (tvar-non-poly t))])
                             (if (and (c1 . < . c2)
                                      (equal? poly-context (list-tail (tvar-non-poly t) 
                                                                      (- c2 c1))))
                                 (list t)
                                 null))
                           (list t))]
            [(arrow? t)
             (append (loop (arrow-result t))
                     (apply append
                            (map loop (arrow-args t))))]
            [(listof? t) (loop (listof-element t))]
            [(boxof? t) (loop (boxof-element t))]
            [(vectorof? t) (loop (vectorof-element t))]
            [(hashof? t) (append (loop (hashof-key t))
                                 (loop (hashof-val t)))]
            [(parameterof? t) (loop (parameterof-element t))]
            [(tupleof? t) (apply append
                                 (map loop (tupleof-args t)))]
            [(datatype? t) (apply append
                                  (map loop (datatype-args t)))]
            [(poly? t) (remq* (list (poly-tvar t))
                              (loop (poly-type t)))]
            [else null]))])
    (if (null? tvars)
        null
        (let ([ht (make-hasheq)])
          (for-each (lambda (t)
                      (hash-set! ht t #t))
                    tvars)
          (hash-map ht (lambda (k v) k))))))

(define (poly-instance t)
  (cond
   [(defn? t)
    (if (defn-rhs t)
        ;; Type is determined:
        (poly-instance (defn-rhs t))
        ;; We only have a skeleton...
        (let ([inst (poly-instance (defn-base t))])
          ;; Remember this instance to check the type later:
          (set-defn-insts! t (cons (cons #f inst) (defn-insts t)))
          inst))]
   [(tvar? t)
    (let ([t (simplify! t)])
      (if (poly? t)
          (poly-instance t)
          t))]
   [(poly? t)
    (poly-instance
     ((instance (poly-tvar t)
                (gen-tvar #f))
      (poly-type t)))]
   [else t]))

(define (instantiate-constructor-at type datatype)
  (let loop ([type type]
             [orig-poly null])
    (cond
     [(poly? type)
      (loop (poly-type type)
            (cons (poly-tvar type) orig-poly))]
     [else
      (unless (= (length orig-poly)
                 (length (datatype-args datatype)))
        (error "constructor abstraction mismatch"))
      (let loop ([type type]
                 [orig (reverse orig-poly)]
                 [new (datatype-args datatype)])
        (if (null? orig)
            type
            (loop ((instance (car orig) (car new)) type)
                  (cdr orig)
                  (cdr new))))])))

(define (create-defn t poly-context)
  (let ([p (poly-ize t poly-context)])
    (make-defn (type-src t)
               p
               (if (poly? p) #f p)
               poly-context
               null
               #f)))

(define (poly-ize t poly-context)
  (let loop ([tvars (extract-tvars t poly-context)] [t t])
    (cond
     [(null? tvars) t]
     [else (loop (cdr tvars)
                 (make-poly (type-src t)
                            (car tvars)
                            t))])))

(define (at-source t expr)
  (let ([t (clone t)])
    (let loop ([t t])
      (add-srcs! t expr)
      (cond
       [(arrow? t) 
        (for-each loop (arrow-args t))
        (loop (arrow-result t))]
       [(listof? t)
        (loop (listof-element t))]
       [(boxof? t)
        (loop (boxof-element t))]
       [(vectorof? t)
        (loop (vectorof-element t))]
       [(hashof? t)
        (loop (hashof-key t))
        (loop (hashof-val t))]
       [(tupleof? t)
        (for-each loop (tupleof-args t))]
       [(parameterof? t)
        (loop (parameterof-element t))]
       [(datatype? t)
        (for-each loop (datatype-args t))]))
    t))

(define (clone t)
  (cond
   [(tvar? t) (if (tvar-rep t)
                  (clone (tvar-rep t))
                  t)]
   [(bool? t) (make-bool (type-src t))]
   [(num? t) (make-num (type-src t))]
   [(sym? t) (make-sym (type-src t))]
   [(sexp? t) (make-sexp (type-src t))]
   [(str? t) (make-str (type-src t))]
   [(chr? t) (make-chr (type-src t))]
   [(vd? t) (make-vd (type-src t))]
   [(arrow? t) (make-arrow
                (type-src t)
                (map (lambda (t) (clone t)) (arrow-args t))
                (clone (arrow-result t)))]
   [(listof? t) (make-listof
                 (type-src t)
                 (clone (listof-element t)))]
   [(boxof? t) (make-boxof
                (type-src t)
                (clone (boxof-element t)))]
   [(vectorof? t) (make-vectorof
                   (type-src t)
                   (clone (vectorof-element t)))]
   [(hashof? t) (make-hashof
                 (type-src t)
                 (clone (hashof-key t))
                 (clone (hashof-val t)))]
   [(tupleof? t) (make-tupleof
                  (type-src t)
                  (map clone (tupleof-args t)))]
   [(parameterof? t) (make-parameterof
                      (type-src t)
                      (clone (parameterof-element t)))]
   [(opaque-datatype? t) (make-opaque-datatype
                          (type-src t)
                          (datatype-id t)
                          null
                          (opaque-datatype-pred t))]
   [(datatype? t) (make-datatype
                   (type-src t)
                   (datatype-id t)
                   (map clone (datatype-args t)))]
   [(poly? t) (error 'clone "shouldn't clone poly")]
   [else (error 'clone "unrecognized: ~e" t)]))

(define (syntax-srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))

(define (extract-srcs! r ht)
  (cond
   [(not r) (void)]
   [(and (syntax? r)
         (syntax-original? (syntax-local-introduce r)))
    (define key (cons (syntax->datum r) (syntax-srcloc r)))
    (hash-update! ht key (lambda (v) (or v r)) r)]
   [(type? r) (extract-srcs! (type-src r) ht)]
   [(list? r) (map (lambda (i)
                     (extract-srcs! i ht))
                   r)]))

(define (raise-typecheck-error main-expr a b
                               [reason #f]
                               #:function-call? [function-call? #f])
  (let ([exprs (let ([ht (make-hash)])
                 (extract-srcs! ht main-expr)
                 (extract-srcs! a ht)
                 (extract-srcs! b ht)
                 (hash-map ht (lambda (k v) v)))])
    (define all-exprs 
      (if main-expr
          (cons main-expr (remq main-expr exprs))
          exprs))
    (raise
     (make-exn:fail:syntax
      (parameterize ([print-as-expression #f])
        (define context (make-hasheq))
        (format "~atypecheck failed~a: ~a vs. ~a~a"
                (if (and (error-print-source-location)
                         main-expr)
                    (let ([s (source-location->string (syntax-srcloc main-expr))])
                      (if (equal? s "")
                          ""
                          (string-append s ": ")))
                    "")
                (cond
                 [(or reason (mismatch-explanation a b function-call?))
                  => (lambda (s) (format ": ~a\n  type mismatch" s))]
                 [else ""])
                (pretty-format ((type->datum context) a))
                (pretty-format ((type->datum context) b))
                (if (and (error-print-source-location)
                         (pair? all-exprs))
                    (apply
                     string-append
                     "\n  sources:"
                     (for/list ([e (in-list all-exprs)])
                       (format "\n   ~s" (syntax->datum e))))
                    "")))
      (current-continuation-marks)
      all-exprs))))

(define (mismatch-explanation a b function-call?)
  (cond
   [(or (and (arrow? a) (not (arrow? b)))
        (and (arrow? b) (not (arrow? a))))
    (if function-call?
        (string-append "call of a non-function\n"
                       "  possible reason: extra parentheses create a function call")
        (string-append "function vs. non-function\n"
                       "  possible reason: extra parentheses create a function call\n"
                       "  another possible reason: missing parentheses for a function call"))]
   [(and function-call?
         (arrow? a)
         (arrow? b)
         (not (= (length (arrow-args a))
                 (length (arrow-args b)))))
    (format (string-append "function call with wrong number of arguments\n"
                           "  argument counts: ~a vs. ~a")
            (length (arrow-args a))
            (length (arrow-args b)))]
   [else #f]))

(define (lookup id env)
  (let loop ([env env] [symbolic? #f])
    (cond
     [(null? env)
      (if symbolic?
          #f
          (raise-syntax-error 
           #f
           "free variable while typechecking"
           id))]
     [(free-identifier=? id (caar env))
      (if (eq? (syntax-e id) (syntax-e (caar env)))
          (cdar env)
          (or (loop (cdr env) #t)
              (and (not symbolic?)
                   (cdar env))))]
     [else (loop (cdr env) symbolic?)])))

(define (add-srcs! r a)
  (let ([srcs (type-src r)])
    (cond
     [(not srcs) (set-type-src! r a)]
     [(pair? srcs) (unless (memq a srcs)
                     (set-type-src! r (cons a srcs)))]
     [else (unless (eq? a srcs)
             (set-type-src! r (list a srcs)))])))

(define (simplify! a)
  (if (tvar? a)
      (let ([r (let loop ([a a])
                 (if (and (tvar? a)
                          (tvar-rep a))
                     (loop (tvar-rep a))
                     a))])
        (let ([r (if (tvar? r)
                     r
                     ;; clone it so we can set the location
                     (clone r))])
          (let loop ([a a])
            (unless (or (eq? r a)
                        (not (tvar? a)))
              (let ([r2 (tvar-rep a)])
                (set-tvar-rep! a r)
                (add-srcs! r a)
                (loop r2)))))
        r)
      a))

(define (simplify!* t)
  (cond
   [(tvar? t) (let ([t2 (simplify! t)])
                (if (tvar? t2)
                    t2
                    (simplify!* t2)))]
   [(arrow? t)
    (make-arrow (type-src t)
                (map simplify!*
                     (arrow-args t))
                (simplify!* (arrow-result t)))]
   [(listof? t) (make-listof (type-src t)
                             (simplify!* (listof-element t)))]
   [(boxof? t) (make-boxof (type-src t)
                           (simplify!* (boxof-element t)))]
   [(vectorof? t) (make-vectorof (type-src t)
                                 (simplify!* (vectorof-element t)))]
   [(hashof? t) (make-hashof (type-src t)
                             (simplify!* (hashof-key t))
                             (simplify!* (hashof-val t)))]
   [(tupleof? t) (make-tupleof (type-src t)
                               (map simplify!* (tupleof-args t)))]
   [(poly? t) (make-poly (type-src t)
                         (poly-tvar t)
                         (simplify!* (poly-type t)))]
   [(parameterof? t) (make-parameterof (type-src t)
                                       (simplify!* (parameterof-element t)))]
   [(datatype? t) (if (null? (datatype-args t))
                      t
                      (make-datatype (type-src t)
                                     (datatype-id t)
                                     (map simplify!*
                                          (datatype-args t))))]
   [else t]))

(define (resolve-defn-types env)
  (map (lambda (p)
         (let ([id (car p)]
               [t (cdr p)])
           (and (defn? t)
                (or (defn-rhs t)
                    (let* ([b (simplify!* (defn-proto-rhs t))]
                           [poly (poly-ize b (defn-poly-context t))])
                      (for-each (lambda (x)
                                  (unify! (car x) (cdr x) (poly-instance poly)))
                                (defn-insts t))
                      poly)))))
       env))

(define (let-based-poly! env)
  (let ([defn-types
          ;; Find fixpoint of defn-type polymorphism:
          (let loop ([defn-types (resolve-defn-types env)])
            (let ([new-defn-types (resolve-defn-types env)])
              (if (andmap (lambda (a b)
                            (let loop ([a a] [b b])
                              (cond
                               [(poly? a)
                                (and (poly? b)
                                     (loop (poly-type a) (poly-type b)))]
                               [(poly? b) #f]
                               [else #t])))
                          defn-types new-defn-types)
                  new-defn-types
                  (loop new-defn-types))))])
    (map (lambda (p defn-type)
           (let ([id (car p)]
                 [t (cdr p)])
             (if (defn? t)
                 (cons id defn-type)
                 p)))
         env defn-types)))

(define (occurs? a b)
  (cond
   [(eq? a b) #t]
   [(and (tvar? b)
         (tvar-rep b))
    (occurs? a (tvar-rep b))]
   [(arrow? b)
    (or (ormap (lambda (arg)
                 (occurs? a arg))
               (arrow-args b))
        (occurs? a (arrow-result b)))]
   [(listof? b)
    (occurs? a (listof-element b))]
   [(boxof? b)
    (occurs? a (boxof-element b))]
   [(vectorof? b)
    (occurs? a (vectorof-element b))]
   [(hashof? b)
    (or (occurs? a (hashof-key b))
        (occurs? a (hashof-val b)))]
   [(tupleof? b)
    (ormap (lambda (arg) (occurs? a arg))
           (tupleof-args b))]
   [(parameterof? b)
    (occurs? a (parameterof-element b))]
   [(datatype? b)
    (ormap (lambda (arg) (occurs? a arg))
           (datatype-args b))]
   [else #f]))

(define (unify-defn! expr a b)
  (if (defn? a)
      (let ([pi (poly-instance (defn-base a))])
        (non-poly! b (cons (gensym) (defn-poly-context a)))
        (unify! expr pi b)
        (unless (defn-rhs a) 
          (set-defn-proto-rhs! a b)))
      (unify! expr a b)))

(define (unify! expr a b
                #:function-call? [function-call? #f])
  (define (sub-unify! a b expr aa ba)
    (add-srcs! aa a)
    (add-srcs! ba b)
    (unify! expr aa ba))
  (let ([a (simplify! a)]
        [b (simplify! b)])        
    (if (and (tvar? b)
             (not (tvar? a)))
        (unify! expr b a)
        (cond
         [(eq? a b) (void)]
         [(tvar? a)
          (when (occurs? a b)
            (raise-typecheck-error expr a b "cycle in type constraints"))
          (if (tvar? b)
              (if (arrow-tvar? b)
                  (begin
                    (when (tvar-non-poly a)
                      (non-poly! b (tvar-non-poly a)))
                    (set-tvar-rep! a b)
                    (add-srcs! b a))
                  (begin
                    (when (tvar-non-poly b)
                      (non-poly! a (tvar-non-poly b)))
                    (set-tvar-rep! b a)
                    (add-srcs! a b)))
              (if (and (arrow-tvar? a)
                       (not (arrow? b)))
                  (raise-typecheck-error expr a b "tracing requires a procedure")
                  (let ([b (clone b)])
                    (when (tvar-non-poly a)
                      (non-poly! b (tvar-non-poly a)))
                    (set-tvar-rep! a b)
                    (add-srcs! b a))))]
         [(and function-call? (arrow? b) (not (arrow? a)))
          (unify! expr b a #:function-call? #t)]
         [(bool? a)
          (unless (bool? b)
            (raise-typecheck-error expr a b))]
         [(num? a)
          (unless (num? b)
            (raise-typecheck-error expr a b))]
         [(sym? a)
          (unless (sym? b)
            (raise-typecheck-error expr a b))]
         [(sexp? a)
          (unless (sexp? b)
            (raise-typecheck-error expr a b))]
         [(vd? a)
          (unless (vd? b)
            (raise-typecheck-error expr a b))]
         [(str? a)
          (unless (str? b)
            (raise-typecheck-error expr a b))]
         [(chr? a)
          (unless (chr? b)
            (raise-typecheck-error expr a b))]
         [(arrow? a)
          (unless (and (arrow? b)
                       (= (length (arrow-args b))
                          (length (arrow-args a))))
            (raise-typecheck-error expr a b #:function-call? function-call?))
          (for-each (lambda (aa ba) (sub-unify! a b expr aa ba)) 
                    (arrow-args a) (arrow-args b))
          (sub-unify! a b expr (arrow-result a) (arrow-result b))]
         [(listof? a)
          (unless (listof? b)
            (raise-typecheck-error expr a b))
          (sub-unify! a b expr (listof-element a) (listof-element b))]
         [(boxof? a)
          (unless (boxof? b)
            (raise-typecheck-error expr a b))
          (sub-unify! a b expr (boxof-element a) (boxof-element b))]
         [(vectorof? a)
          (unless (vectorof? b)
            (raise-typecheck-error expr a b))
          (sub-unify! a b expr (vectorof-element a) (vectorof-element b))]
         [(hashof? a)
          (unless (hashof? b)
            (raise-typecheck-error expr a b))
          (sub-unify! a b expr (hashof-key a) (hashof-key b))
          (sub-unify! a b expr (hashof-val a) (hashof-val b))]
         [(tupleof? a)
          (unless (and (tupleof? b)
                       (= (length (tupleof-args a))
                          (length (tupleof-args b))))
            (raise-typecheck-error expr a b))
          (for-each (lambda (aa ba) (sub-unify! a b expr aa ba)) (tupleof-args a) (tupleof-args b))]
         [(parameterof? a)
          (unless (parameterof? b)
            (raise-typecheck-error expr a b))
          (sub-unify! a b expr (parameterof-element a) (parameterof-element b))]
         [(datatype? a)
          (unless (and (datatype? b)
                       (free-identifier=? (datatype-id a)
                                          (datatype-id b)))
            (raise-typecheck-error expr a b))
          (for-each (lambda (aa ba) (sub-unify! a b expr aa ba)) (datatype-args a) (datatype-args b))]
         [else
          (raise-typecheck-error expr a b (format "unrecognized type ~s" a))]))))
