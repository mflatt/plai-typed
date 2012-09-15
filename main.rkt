#lang racket

(require (only-in plai
                  define-type
                  type-case
                  test
                  test/exn
                  print-only-errors
                  error)
         racket/trace
         (for-syntax racket/list
                     "types.ss"
                     racket/struct-info))

(provide :
         (rename-out [define: define]
                     [define-values: define-values]
                     [lambda: lambda]
                     [begin: begin]
                     [local: local]
                     [letrec: letrec] [let: let] [let*: let*]
                     [shared: shared]
                     [cond: cond]
                     [case: case]
                     [if: if]
                     [when: when]
                     [unless: unless]
                     [or: or]
                     [and: and]
                     [quote: quote]
                     [set!: set!]
                     [time: time]
                     [trace: trace]
                     [require: require])
         #%app #%datum #%top 
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         else typed-in

         (rename-out [define-type: define-type]
                     [type-case: type-case])
         define-type-alias

         test test/exn print-only-errors
         
         cons list empty first rest empty? cons?
         second third fourth list-ref build-list length
         map reverse map2 append
         filter foldl foldr
         (rename-out [member: member])

         + - = > < <= >= / * min max 
         add1 sub1 zero? odd? even?
         modulo remainder floor ceiling
         symbol=? string=? equal? eq? not
         error try call/cc

         string->symbol symbol->string
         string-append to-string
         display

         (rename-out [hash-ref: hash-ref])
         make-hash hash-ref/k hash-ref?
         hash-set! hash-remove! hash-keys

         s-exp-symbol? s-exp->symbol symbol->s-exp
         s-exp-number? s-exp->number number->s-exp
         s-exp-string? s-exp->string string->s-exp
         s-exp-list? s-exp->list list->s-exp
         (rename-out [read: read])
         
         box unbox set-box!
         
         make-vector vector-ref vector-set! vector-length vector
         
         (rename-out [values: values])

         identity

         true false

         number boolean symbol 
         (rename-out [string: string])
         s-expression
         -> 
         (rename-out [listof: listof]
                     [boxof: boxof]
                     [vectorof: vectorof]
                     [hashof: hashof]
                     [void: void]))

(define (symbol=? a b) (eq? a b))

(define not-there (gensym))

(define (hash-ref: ht k)
  (define v (hash-ref ht k not-there))
  (if (eq? v not-there)
      (error 'hash-ref "no value found for key: ~e" k) ; `error' from `plai'
      v))
(define (hash-ref? ht k)
  (not (eq? not-there (hash-ref ht k not-there))))
(define (hash-ref/k ht k success failure)
  (define v (hash-ref ht k not-there))
  (if (eq? v not-there)
      (failure)
      (success v)))

(define (s-exp-symbol? s) (symbol? s))
(define (s-exp->symbol s) (if (symbol? s) s (error 's-exp->symbol "not a symbol: ~e" s)))
(define (symbol->s-exp s) s)
(define (s-exp-number? s) (number? s))
(define (s-exp->number s) (if (number? s) s (error 's-exp->number "not a number: ~e" s)))
(define (number->s-exp s) s)
(define (s-exp-string? s) (string? s))
(define (s-exp->string s) (if (string? s) s (error 's-exp->string "not a string: ~e" s)))
(define (string->s-exp s) s)
(define (s-exp-list? s) (list? s))
(define (s-exp->list s) (if (list? s) s (error 's-exp->list "not a list: ~e" s)))
(define (list->s-exp s) s)

(define (read:)
  (define v (read))
  (unless (let loop ([v v])
            (or (symbol? v)
                (string? v) 
                (number? v)
                (and (list? v)
                     (map loop v))))
    (error 'read "input is not an s-expression: ~e" v))
  v)

(define (map2 f l1 l2) (map f l1 l2))

(define-syntax (: stx)
  (raise-syntax-error
   #f
   "expected an expression instead of a colon"
   stx))

(define-for-syntax type
  (lambda (stx)
    (raise-syntax-error
     #f
     "expected an expression, found a type"
     stx)))

(define-syntax number type)
(define-syntax boolean type)
(define-syntax symbol type)
(define-syntax string: type)
(define-syntax s-expression type)
(define-syntax -> type)
(define-syntax listof: type)
(define-syntax boxof: type)
(define-syntax vectorof: type)
(define-syntax hashof: type)
(define void: void) ; allow as a function

(define (member: a l)
  (and (member a l) #t))

(define (to-string x) (format "~v" x))

(define-for-syntax (is-type-keyword? a)
  (ormap (lambda (i)
           (free-identifier=? a i))
         (syntax->list
          #'(: number boolean symbol s-expression
               string: -> listof: hashof:
               boxof: vectorof:
               void:))))

(define-for-syntax (is-keyword? a)
  (or (is-type-keyword? a)
      (ormap (lambda (i)
               (free-identifier=? a i))
             (syntax->list
              #'(true false
                      else)))))

(define-for-syntax (check-defn-keyword id stx)
  (when (is-keyword? id)
    (raise-syntax-error 
     #f 
     "cannot redefine a keyword"
     stx
     id)))

(define-for-syntax ((parse-arg stx) arg)
  (if (identifier? arg)
      (if (is-keyword? arg)
          (raise-syntax-error 
           #f 
           (format "keyword used as an argument name~a" 
                   (if (is-type-keyword? arg)
                       " (maybe missing square brackets?)"
                       ""))
           stx
           arg)
          arg)
      (syntax-case arg (:)
        [(id : type)
         (identifier? #'id)
         #'id]
        [_ (raise-syntax-error
            #f
            "expected either <id> or `[<id> : <type>]' for function argument"
            stx
            arg)])))

(define-for-syntax (check-top k)
  (lambda (stx)
    (if (eq? 'module-begin (syntax-local-context))
        #`(module-begin #,stx)
        (k stx))))

(define-syntax try
  (check-top
   (syntax-rules (lambda:)
     [(try expr1 (lambda: () expr2))
      (with-handlers* ([exn:fail? (lambda (exn) expr2)])
        expr1)])))

(define-syntax require:
  (check-top
   (lambda (stx)
     (syntax-case stx (typed-in :)
       [(_ (typed-in lib
                     [id : type]
                     ...)
           ...)
        (let ([libs (syntax->list #'(lib ...))]
              [ids (syntax->list #'(id ... ...))])
          (for ([lib (in-list libs)])
            (unless (module-path? (syntax->datum lib))
              (raise-syntax-error #f "bad module path" stx lib)))
          (for ([id (in-list ids)])
            (unless (identifier? id)
              (raise-syntax-error #f "expected an identifier" stx id))))
        #'(require (only-in lib id ...) ...)]))))

(define-syntax typed-in
  (lambda (stx)
    (raise-syntax-error #f "allowed only in `require'" stx)))

(define-syntax define:
  (check-top
   (lambda (stx)
     (syntax-case stx (:)
       [(_ id expr)
        (identifier? #'id)
        (begin
          (check-defn-keyword #'id stx)
          (syntax/loc stx
            (define id expr)))]
       [(_ id : type expr)
        (identifier? #'id)
        (begin
          (check-defn-keyword #'id stx)
          (syntax/loc stx
            (define id expr)))]
       [(_ (id arg ...) : type expr)
        (identifier? #'id)
        (begin
          (check-defn-keyword #'id stx)
          (with-syntax ([(arg ...)
                         (map (parse-arg stx) (syntax->list #'(arg ...)))])
            (syntax/loc stx
              (define (id arg ...) (#%expression expr)))))]
       [(_ (id arg ...) expr)
        (identifier? #'id)
        (begin
          (check-defn-keyword #'id stx)
          (with-syntax ([(arg ...)
                         (map (parse-arg stx) (syntax->list #'(arg ...)))])
            (syntax/loc stx
              (define (id arg ...) (#%expression expr)))))]))))

(define values: vector-immutable)

(define-syntax define-values:
  (check-top
   (lambda (stx)
     (syntax-case stx (:)
       [(_ (id ...) expr)
        (with-syntax ([(id ...)
                       (map (lambda (id)
                              (check-defn-keyword id stx)
                              (if (identifier? id)
                                  id
                                  (syntax-case id (:)
                                    [(id : type)
                                     #'id]
                                    [else
                                     (raise-syntax-error
                                      #f
                                      "expected <id> or `[<id> : <type>]'"
                                      stx
                                      id)])))
                            (syntax->list #'(id ...)))])
          (syntax/loc stx
            (define-values (id ...) (vector->values expr))))]))))

(define-syntax lambda:
  (check-top
   (lambda (stx)
     (syntax-case stx (:)
       [(_ (arg ...) : type expr)
        (with-syntax ([(arg ...)
                       (map (parse-arg stx) (syntax->list #'(arg ...)))])
          (syntax/loc stx
            (lambda (arg ...) (#%expression expr))))]
       [(_ (arg ...) expr)
        (with-syntax ([(arg ...)
                       (map (parse-arg stx) (syntax->list #'(arg ...)))])
          (syntax/loc stx
            (lambda (arg ...) (#%expression expr))))]))))

(begin-for-syntax
 ;; Used to declare a variant name so that `shared' can create instances
 (struct constructor-syntax (id selectors mutators)
   #:property prop:set!-transformer
   (lambda (c stx)
     (with-syntax ([id (syntax-property (constructor-syntax-id c)
                                        'constructor-for
                                        (syntax-case stx (set!)
                                          [(set! id . _) #'id]
                                          [(id . _) #'id]
                                          [_ stx]))])
       (syntax-case stx (set!)
         [(set! _ rhs) (syntax/loc stx (set! id rhs))]
         [(_ arg ...) (syntax/loc stx (id arg ...))]
         [_ #'id])))
   #:property prop:struct-info
   (lambda (c)
     (list #f 
           (constructor-syntax-id c)
           #f
           (reverse (constructor-syntax-selectors c))
           (reverse (constructor-syntax-mutators c))
           #f))))

(define-syntax define-type:
  (check-top
   (lambda (stx)
     (syntax-case stx (:)
       [(_ thing . rest)
        (not (or (identifier? #'thing)
                 (syntax-case #'thing (quote:)
                   [(id (quote: arg) ...)
                    (and (identifier? #'id)
                         (andmap identifier? (syntax->list #'(arg ...))))]
                   [_ #f])))
        (raise-syntax-error
         #f
         "expected an <id> for a type name or (<id> '<id> ...) for polymorphic type"
         stx
         #'thing)]
       [(_ id [variant (field : type) ...] ...)
        (with-syntax ([id (if (identifier? #'id)
                              #'id
                              (syntax-case #'id (quote:)
                                [(id (quote: arg) ...)
                                 #'id]))]
                      [($variant ...) (map (lambda (variant)
                                             (datum->syntax variant
                                                            (string->uninterned-symbol
                                                             (symbol->string (syntax-e variant)))
                                                            variant
                                                            variant))
                                           (syntax->list #'(variant ...)))]
                      [(((variant-field set-variant-field!) ...) ...)
                       (map (lambda (variant fields)
                              (map (lambda (field)
                                     (define (mk fmt)
                                       (datum->syntax variant
                                                      (string->symbol
                                                       (format fmt
                                                               (syntax-e variant)
                                                               (syntax-e field)))
                                                      variant
                                                      variant))
                                     (list (mk "~a-~a") (mk "set-~a-~a!")))
                                   (syntax->list fields)))
                            (syntax->list #'(variant ...))
                            (syntax->list #'((field ...) ...)))])
          (let ([s #'(define-type id
                         [$variant (field (lambda (x) #t)) ...] ...)])
            #`(begin
                #,(datum->syntax stx (syntax-e s) stx stx)
                (define-syntax variant (constructor-syntax
                                        (quote-syntax $variant)
                                        (list (quote-syntax variant-field)
                                              ...)
                                        (list (quote-syntax set-variant-field!)
                                              ...)))
                ...)))]
       [(_ id thing ...)
        (for-each (lambda (thing)
                    (syntax-case thing ()
                      [[variant thing ...]
                       (for-each (lambda (thing)
                                   (syntax-case thing (:)
                                     [(field : type) 'ok]
                                     [_ (raise-syntax-error
                                         #f
                                         "expected `(<id> : <type>)'"
                                         stx
                                         thing)]))
                                 (syntax->list #'(thing ...)))]
                      [_ (raise-syntax-error
                          #f
                          "expected `[<id> (<id> : <type>) ...]'"
                          stx
                          thing)]))
                  (syntax->list #'(thing ...)))]))))

(define-syntax define-type-alias
  (check-top
   (lambda (stx)
     (syntax-case stx (quote:)
       [(_ (id (quote: arg) ...) t)
        (begin
          (map (lambda (id)
                 (unless (identifier? id)
                   (raise-syntax-error #f "expected an identifier" stx id)))
               (syntax->list #'(id arg ...)))
          #'(void))]
       [(_ id t)
        (let ([id #'id])
          (unless (identifier? id)
            (raise-syntax-error #f "expected `<id>' or `(<id> '<id> ...)'" stx id))
          #'(void))]))))

(define-syntax begin:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ e0 e ...)
        (with-syntax ([body (syntax/loc stx
                              (begin e0 e ...))])
          #'(#%expression body))]))))

(define-syntax local:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ ((def . _d) ...) e)
        (andmap (lambda (def)
                  (syntax-case def (define: define-values:)
                    [define: #t]
                    [define-values: #t]
                    [_ #f]))
                (syntax->list #'(def ...)))
        (syntax/loc stx
          (local ((def . _d) ...) (#%expression e)))]
       [(_ (thing ...) e)
        (for-each (lambda (thing)
                    (syntax-case thing (define: define-values:)
                      [(define: . _) 'ok]
                      [(define-values: . _) 'ok]
                      [else (raise-syntax-error
                             #f
                             "expected a function, constant, or tuple defininition"
                             thing)]))
                  (syntax->list #'(thing ...)))]))))

(define-for-syntax (make-let kind)
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ ([id rhs] ...) body)
        (let ([ids (syntax->list #'(id ...))])
          (for ([id (in-list ids)])
            (unless (identifier? id) (raise-syntax-error #f
                                                         "expected an identifier"
                                                         stx
                                                         id)))
          (case kind
            [(letrec) (syntax/loc stx (local: [(define: id rhs) ...] body))]
            [(let) (with-syntax ([(tmp ...) (generate-temporaries ids)])
                     (syntax/loc stx
                       (local: [(define: tmp rhs) ...]
                               (local: [(define: id tmp) ...]
                                       body))))]
            [(let*) (let loop ([ids ids]
                               [rhss (syntax->list #'(rhs ...))])
                      (cond
                       [(empty? ids) #'body]
                       [else (with-syntax ([body (loop (cdr ids) (cdr rhss))]
                                           [id (car ids)]
                                           [rhs (car rhss)]
                                           [tmp (car (generate-temporaries (list (car ids))))])
                               (syntax/loc stx
                                 (local: [(define: tmp rhs)]
                                         (local: [(define: id tmp)]
                                                 body))))]))]))]))))

(define-syntax letrec: (make-let 'letrec))
(define-syntax let: (make-let 'let))
(define-syntax let*: (make-let 'let*))

(define-syntax shared:
  (check-top
   (lambda (stx)
     (if (eq? (syntax-local-context) 'expression)
         (syntax-case stx ()
           [(_ ([id rhs] ...) body)
            (syntax/loc stx
              (shared ([id rhs] ...) body))])
         #`(#%expression #,stx)))))

(define-for-syntax (convert-clauses stx)
  ;; Preserve srcloc of each clause:
  (map (lambda (clause)
         (syntax-case clause (else)
           [[variant (id ...) ans]
            (for-each (lambda (id)
                        (when (is-keyword? id)
                          (raise-syntax-error 
                           #f 
                           "cannot bind a keyword"
                           stx
                           id)))
                      (syntax->list #'(id ...)))
            (with-syntax ([$variant (let ([c (syntax-local-value #'variant (lambda () #f))])
                                      (if (constructor-syntax? c)
                                          (constructor-syntax-id c)
                                          #'variants))])
              (syntax/loc clause
                [$variant (id ...) (#%expression ans)]))]
           [[else ans]
            (syntax/loc clause
              [else (#%expression ans)])]))
       (syntax-case stx ()
         [(_ type expr clause ...) 
          (syntax->list #'(clause ...))])))

(define-for-syntax (signal-typecase-syntax-error stx)
  (syntax-case stx ()
    [(_ type expr clause ...)
     (for-each (lambda (clause)
                 (syntax-case clause (else)
                   [[variant (id ...) ans]
                    (identifier? #'variant)
                    'ok]
                   [[(variant args ...) (id ...) ans]
                    (identifier? #'variant)
                    'ok]
                   [[else ans] 'ok]
                   [[var . rest]
                    (not (identifier? #'var))
                    (raise-syntax-error
                     #f
                     "expected an identifier from a define-type"
                     stx
                     #'var)]
                   [[var ids . rest]
                    (syntax-case #'ids ()
                      [(x ...) (andmap identifier? (syntax->list #'(x ...))) #f]
                      [else #t])
                    (raise-syntax-error
                     #f
                     (format "second piece of the ~a clause must be a sequence of identifiers"
                             (syntax-e #'var))
                     stx
                     clause)]
                   [[var ids ans1 ans2 . ans]
                    (raise-syntax-error
                     #f
                     "clause does not contain a single result expression"
                     stx
                     clause)]
                   [[variant (id ...) ans ...]
                    (andmap identifier? (syntax->list #'(id ...)))
                    (raise-syntax-error
                     #f
                     "clause does not contain a single result expression"
                     stx
                     clause)]
                   [else (raise-syntax-error
                          #f
                          "ill-formed clause"
                          stx
                          clause)]))
               (syntax->list #'(clause ...)))]
    [else
     (raise-syntax-error #f "ill-formed type-case" stx)]))

(define-syntax type-case:
  (check-top
   (lambda (stx)
     (syntax-case stx (else)
       [(_ thing . rest)
        (not (or (identifier? #'thing)
                 (syntax-case #'thing ()
                   [(id arg ...)
                    (identifier? #'id)]
                   [_ #f])))
        (raise-syntax-error
         #f
         "expected an <id> for a type name or `(<id> <type> ...)' for polymorphic type"
         stx
         #'thing)]
       [(_ type expr [variant (id ...) ans] ...)
        (with-syntax ([type (if (identifier? #'type)
                                #'type
                                (syntax-case #'type ()
                                  [(id arg ...) #'id]))]
                      [(clause ...) (convert-clauses stx)])
          (syntax/loc stx
            (type-case type expr clause ...)))]
       [(_ type expr [variant (id ...) ans] ... [else else-ans])
        (with-syntax ([type (if (identifier? #'type)
                                #'type
                                (syntax-case #'type ()
                                  [(id arg ...) #'id]))]
                      [(clause ...) (convert-clauses stx)])
          (syntax/loc stx
            (type-case type expr clause ...)))]
       [_
        (signal-typecase-syntax-error stx)]))))

(define-syntax cond:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ [ques ans] ...)
        (with-syntax ([(catch ...)
                       (let ([ques (syntax->list #'(ques ...))])
                         (if (and (pair? ques)
                                  (identifier? (last ques))
                                  (free-identifier=? (last ques) #'else))
                             null
                             #'([else (cond-error)])))])
          (syntax/loc stx
            (cond [ques (#%expression ans)] ... catch ...)))]
       [(_ thing ...)
        (for-each (lambda (thing)
                    (syntax-case thing ()
                      [[ques ans] 'ok]
                      [_else (raise-syntax-error
                              #f
                              "expected [<test-expr> <result-expr>]"
                              stx
                              thing)]))
                  (syntax->list #'(thing ...)))]))))

(define (cond-error)
  (error 'cond "no matching clause"))

(define-syntax case:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ expr [alts ans] ...)
        (with-syntax ([(catch ...)
                       (let loop ([altss (syntax->list #'(alts ...))])
                         (if (null? altss)
                             #'([else (case-error)])
                             (syntax-case (car altss) (else)
                               [else
                                (if (null? (cdr altss))
                                    '()
                                    (raise-syntax-error #f
                                                        "an `else' case must be last"
                                                        stx
                                                        (car altss)))]
                               [(id ...)
                                (begin
                                  (for-each (lambda (id)
                                              (unless (identifier? id)
                                                (raise-syntax-error #f
                                                                    "alternative must be an identitifer"
                                                                    stx
                                                                    id)))
                                            (syntax->list #'(id ...)))
                                  (loop (cdr altss)))]
                               [_ (raise-syntax-error #f
                                                      "expected (<id> ...)"
                                                      stx
                                                      (car altss))])))])
          (syntax/loc stx
            (case expr [alts (#%expression ans)] ... catch ...)))]
       [(_ expr thing ...)
        (for-each (lambda (thing)
                    (syntax-case thing ()
                      [[alts ans] 'ok]
                      [_else (raise-syntax-error
                              #f
                              "expected [(<id> ...) <result-expr>] or [else <result-expr>]"
                              stx
                              thing)]))
                  (syntax->list #'(thing ...)))]))))

(define (case-error)
  (error 'case "no matching clause"))

(define-syntax if:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ test then else)
        (syntax/loc stx
          (if test then else))]
       [(_ test then)
        (raise-syntax-error #f
                            "missing else-expression"
                            stx)]))))

(define-syntax when:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ tst expr ...)
        (syntax/loc stx (when tst expr ...))]))))

(define-syntax unless:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ tst expr ...)
        (syntax/loc stx (unless tst expr ...))]))))

(define-syntax quote:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ s)
        (if (let loop ([s #'s])
              (or (let ([v (syntax-e s)])
                    (or (symbol? v)
                        (number? v)
                        (string? v)))
                  (let ([l (syntax->list s)])
                    (and l
                         (andmap loop l)))))
            #'(quote s)
            (raise-syntax-error #f
                                "quote allowed only for s-expressions containing symbols, numbers, strings, and lists"
                                stx))]))))

(define-syntax and:
  (check-top
   (syntax-rules ()
     [(_ arg ...) (and arg ...)])))

(define-syntax or:
  (check-top
   (syntax-rules ()
     [(_ arg ...) (or arg ...)])))

(define-syntax set!:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ id expr)
        (if (identifier? #'id)
            #'(set! id expr)
            (raise-syntax-error #f
                                "expected an identifier"
                                stx
                                #'id))]))))

(define-syntax time:
  (check-top
   (syntax-rules ()
     [(_ expr) (time expr)])))

(define-syntax trace:
  (check-top
   (lambda (stx)
     (unless (eq? (syntax-local-context) 'module)
       (raise-syntax-error #f "allowed only module top level" stx))
     (syntax-case stx ()
       [(_ id ...)
        (let ([ids (syntax->list #'(id ...))])
          (for-each (lambda (id)
                      (unless (identifier? id)
                        (raise-syntax-error 'trace
                                            "expected an identifier"
                                            id))
                      (let ([b (identifier-binding id)])
                        (when b
                          (let-values ([(base name) (module-path-index-split (car b))])
                            (when (or base name)
                              (printf "~s\n" (list base name))
                              (raise-syntax-error 'trace
                                                  "not a defined name in this module"
                                                  id))))))
                    ids)
          #'(trace id ...))]))))

;; ----------------------------------------

(define-for-syntax (mk stx . l)
  (datum->syntax
   stx
   (string->symbol
    (apply string-append
           (map (lambda (e)
                  (if (string? e)
                      e
                      (symbol->string (syntax-e e))))
                l)))))

(define-for-syntax (typecheck-defns tl datatypes aliases init-env init-variants just-id? orig-let-polys)
  (let* ([types (filter
                 values
                 (map
                  (lambda (stx)
                    (syntax-case stx (define-type)
                      [(define-type id . _)
                       #'id]
                      [else #f]))
                  tl))]
         [datatypes (append (filter
                             values
                             (map
                              (lambda (stx)
                                (syntax-case stx (define-type:)
                                  [(define-type: (name arg ...) . _) 
                                   (cons #'name (length (syntax->list #'(arg ...))))]
                                  [(define-type: name . _) (cons #'name 0)]
                                  [else #f]))
                              tl))
                            datatypes)]
         [aliases (append (for/fold ([aliases null]) ([stx (in-list tl)])
                            (syntax-case stx (define-type-alias quote:)
                              [(define-type-alias (name (quote: arg) ...) ty) 
                               (cons (list #'name (syntax->list #'(arg ...)) #'ty)
                                     aliases)]
                              [(define-type-alias name ty) 
                               (cons (list #'name null #'ty) aliases)]
                              [else aliases]))
                          aliases)]
         [make-polymorphic-wrt
          (lambda (t ty tvars)
            (let loop ([tvars tvars][ty ty])
              (if (null? tvars)
                  ty
                  (make-poly t
                             (car tvars)
                             (loop (cdr tvars) ty)))))]
         [parse-type/tenv 
          (lambda (t tenv)
            (let ([tvars null])
              (let ([ty
                     (letrec ([parse-one
                               (lambda (seen tenv t)
                                 (let loop ([t t])
                                   (syntax-case t (number boolean symbol string: s-expression
                                                          gensym listof: boxof: hashof: void: -> 
                                                          vectorof: quote: *)
                                     [(quote: id)
                                      (identifier? #'id)
                                      (let ([a (ormap (lambda (p)
                                                        (and (free-identifier=? (car p) #'id)
                                                             p))
                                                      (append tvars
                                                              tenv))])
                                        (if a
                                            (cdr a)
                                            (let ([t (gen-tvar #'id)])
                                              (set! tvars (cons (cons #'id t) tvars))
                                              t)))]
                                     [number (make-num t)]
                                     [boolean (make-bool t)]
                                     [symbol (make-sym t)]
                                     [s-expression (make-sexp t)]
                                     [string: (make-str t)]
                                     [void: (make-vd t)]
                                     [(gensym who) (gen-tvar #'who)]
                                     [(arg-type ... -> result-type)
                                      (make-arrow t 
                                                  (map loop (syntax->list #'(arg-type ...)))
                                                  (loop #'result-type))]
                                     [(listof: elem)
                                      (make-listof t (loop #'elem))]
                                     [(boxof: elem)
                                      (make-boxof t (loop #'elem))]
                                     [(vectorof: elem)
                                      (make-vectorof t (loop #'elem))]
                                     [(hashof: key val)
                                      (make-hashof t (loop #'key) (loop #'val))]
                                     [(a * more ...)
                                      (let ([m (syntax->list #'(more ...))])
                                        (let loop ([m m])
                                          (cond
                                           [(null? m) #f]
                                           [(null? (cdr m)) #t]
                                           [(free-identifier=? #'* (cadr m))
                                            (loop (cddr m))])))
                                      (make-tupleof t
                                                    (let ploop ([m (syntax->list #'(a * more ...))])
                                                      (cond
                                                       [(null? (cdr m))
                                                        (list (loop (car m)))]
                                                       [else
                                                        (cons (loop (car m))
                                                              (ploop (cddr m)))])))]
                                     [() (make-tupleof t null)]
                                     [(id type0 type ...)
                                      (let ([types (syntax->list #'(type0 type ...))])
                                        (or (and (identifier? #'id)
                                                 (ormap (lambda (d)
                                                          (and (free-identifier=? (car d) #'id)
                                                               (if (= (cdr d) (length types))
                                                                   #t
                                                                   (raise-syntax-error
                                                                    #f
                                                                    (if (zero? (cdr d))
                                                                        "bad type (incorrect use of a non-polymorphic type name)"
                                                                        "type constructor applied to the wrong number of types")
                                                                    t))))
                                                        datatypes)
                                                 (make-datatype t (car (syntax-e t)) (map loop types)))
                                            (ormap (lambda (d)
                                                     (and (free-identifier=? (car d) #'id)
                                                          (begin
                                                            (unless (= (length (cadr d)) (length types))
                                                              
                                                              (raise-syntax-error
                                                               #f
                                                               (if (zero? (cdr d))
                                                                   "bad type (incorrect use of a non-polymorphic type alias name)"
                                                                   "type alias constructor applied to the wrong number of types")
                                                               t))
                                                            (when (ormap (lambda (s)
                                                                           (free-identifier=? s t))
                                                                         seen)
                                                              (raise-syntax-error
                                                               #f
                                                               "recursively defined type alias"
                                                               t))
                                                            (parse-one 
                                                             (cons (car d) seen)
                                                             (append (map (lambda (formal arg) 
                                                                            (cons formal 
                                                                                  (loop arg)))
                                                                          (cadr d)
                                                                          types)
                                                                     tenv)
                                                             (caddr d)))))
                                                   aliases)
                                            (raise-syntax-error
                                             #f
                                             "bad type"
                                             t)))]
                                     [else
                                      (or (and (identifier? t)
                                               (ormap (lambda (d)
                                                        (and (free-identifier=? (car d) t)
                                                             (if (zero? (cdr d))
                                                                 #t
                                                                 (raise-syntax-error
                                                                  #f
                                                                  "type constructor must be applied to types"
                                                                  t))))
                                                      datatypes)
                                               (make-datatype t t null))
                                          (and (identifier? t)
                                               (ormap (lambda (d)
                                                        (and (free-identifier=? (car d) t)
                                                             (begin
                                                               (unless (cadr d)
                                                                 (raise-syntax-error
                                                                  #f
                                                                  "type alias constructor must be applied to types"
                                                                  t))
                                                               (when (ormap (lambda (s)
                                                                              (free-identifier=? s t))
                                                                            seen)
                                                                 (raise-syntax-error
                                                                  #f
                                                                  "recursively defined type alias"
                                                                  t))
                                                               (parse-one (cons (car d) seen) tenv (caddr d)))))
                                                      aliases))
                                          (raise-syntax-error
                                           #f
                                           "bad type"
                                           t))])))])
                       (parse-one null tenv t))])
                (make-polymorphic-wrt t ty (map cdr tvars)))))]
         [parse-type (lambda (type)
                       (parse-type/tenv type null))]
         [parse-mono-type (lambda (type)
                            (poly-instance (parse-type type)))]
         [parse-param-type (lambda (tenv)
                             (lambda (type)
                               (poly-instance (parse-type/tenv type tenv))))]
         [extract-arg-type (lambda (arg)
                             (syntax-case arg (:)
                               [(id : type) (parse-mono-type #'type)]
                               [_ (gen-tvar #'arg)]))]
         [variants (apply append
                          init-variants
                          (map 
                           (lambda (stx)
                             (syntax-case stx (define-type: :)
                               [(define-type: name
                                  [variant (field-id : type) ...] ...)
                                (let-values ([(name args)
                                              (syntax-case #'name (quote:)
                                                [(name (quote arg) ...)
                                                 (values #'name (syntax->list #'(arg ...)))]
                                                [else (values #'name null)])])
                                  (let ([arg-types (map gen-tvar args)])
                                    (map (lambda (variant types)
                                           (cons variant
                                                 (map (lambda (te)
                                                        (make-polymorphic-wrt
                                                         variant
                                                         ((parse-param-type (map cons args arg-types)) te)
                                                         arg-types))
                                                      (syntax->list types))))
                                         (syntax->list #'(variant ...))
                                         (syntax->list #'((type ...) ...)))))]
                               [else null]))
                           tl))]
         [is-value? (lambda (expr)
                      (let loop ([expr expr])
                        (syntax-case expr (lambda: list values: cons empty quote:)
                          [(lambda: . _) #t]
                          [(values: a ...)
                           (andmap loop (syntax->list #'(a ...)))]
                          [(list a ...)
                           (andmap loop (syntax->list #'(a ...)))]
                          [empty #t]
                          [(cons a b)
                           (and (loop #'a) (loop #'b))]
                          [(id a ...)
                           (and (identifier? #'id)
                                (ormap (lambda (v)
                                         (free-identifier=? #'id (car v)))
                                       variants))
                           (andmap loop (syntax->list #'(a ...)))]
                          [(quote: a) #t]
                          [_ (or (string? (syntax-e expr))
                                 (number? (syntax-e expr))
                                 (boolean? (syntax-e expr)))])))]
         [req-env (apply
                   append
                   (map
                    (lambda (stx)
                      (syntax-case stx (require: typed-in :)
                        [(require: (typed-in lib (id : type) ...) ...)
                         (map (lambda (id type)
                                (cons id (parse-type type)))
                              (syntax->list #'(id ... ...))
                              (syntax->list #'(type ... ...)))]
                        [else
                         null]))
                    tl))]
         [def-env (apply
                   append
                   (map
                    (lambda (stx)
                      (syntax-case stx (require: define: define-values: define-type: lambda: :)
                        [(define-values: (id ...) rhs)
                         (let ([val? (is-value? #'rhs)])
                           (map (lambda (id)
                                  (if (identifier? id)
                                      (cons id (if val?
                                                   (create-defn
                                                    (gen-tvar id))
                                                   (gen-tvar id)))
                                      (syntax-case id (:)
                                        [(id : type)
                                         (cons #'id 
                                               (if val?
                                                   (create-defn
                                                    (parse-type #'type))
                                                   (parse-mono-type #'type)))])))
                                (syntax->list #'(id ...))))]
                        [(define: (id . args) : result-type . _body)
                         (list (cons #'id
                                     (create-defn
                                      (make-arrow 
                                       #'id
                                       (map extract-arg-type
                                            (syntax->list #'args))
                                       (parse-mono-type #'result-type)))))]
                        [(define: (id . args) . _body)
                         (list (cons #'id (create-defn (make-arrow 
                                                        #'id
                                                        (map extract-arg-type (syntax->list #'args))
                                                        (gen-tvar #'id)))))]
                        [(define: id : type (lambda: . _))
                         (list (cons #'id
                                     (create-defn (parse-type #'type))))]
                        [(define: id (lambda: args : result-type expr))
                         (list (cons #'id
                                     (create-defn
                                      (make-arrow
                                       #'id
                                       (map extract-arg-type (syntax->list #'args))
                                       (parse-type #'result-type)))))]
                        [(define: id (lambda: args expr))
                         (list (cons #'id
                                     (create-defn
                                      (make-arrow
                                       #'id
                                       (map extract-arg-type (syntax->list #'args))
                                       (gen-tvar #'id)))))]
                        [(define: id : type expr)
                         (list (cons #'id 
                                     (if (is-value? #'expr)
                                         (create-defn
                                          (parse-type #'type))
                                         (parse-mono-type #'type))))]
                        [(define: id expr)
                         (list (cons #'id 
                                     (if (is-value? #'expr)
                                         (create-defn
                                          (gen-tvar #'id))
                                         (gen-tvar #'id))))]
                        [(define-type: name
                           [variant (field-id : type) ...]
                           ...)
                         (let-values ([(name args)
                                       (syntax-case #'name (quote:)
                                         [(name (quote arg) ...)
                                          (values #'name (syntax->list #'(arg ...)))]
                                         [else (values #'name null)])])
                           (let ([arg-tvars (map gen-tvar args)])
                             (apply append
                                    (map (lambda (var fields types)
                                           (let ([types (map (parse-param-type 
                                                              (map cons args arg-tvars))
                                                             (syntax->list types))]
                                                 [dt (make-datatype name 
                                                                    name
                                                                    arg-tvars)])
                                             (list* (cons var
                                                          (make-polymorphic-wrt
                                                           var
                                                           (make-arrow
                                                            var
                                                            types
                                                            dt)
                                                           arg-tvars))
                                                    (cons (mk var var "?")
                                                          (make-polymorphic-wrt
                                                           var
                                                           (make-arrow
                                                            var
                                                            (list dt)
                                                            (make-bool var))
                                                           arg-tvars))
                                                    (map (lambda (field type)
                                                           (cons (mk var var "-" field)
                                                                 (make-polymorphic-wrt
                                                                  field
                                                                  (make-arrow
                                                                   field
                                                                   (list dt)
                                                                   type)
                                                                  arg-tvars)))
                                                         (syntax->list fields)
                                                         types))))
                                         (syntax->list #'(variant ...))
                                         (syntax->list #'((field-id ...) ...))
                                         (syntax->list #'((type ...) ...))))))]
                        [else null]))
                    tl))]
         [env (append def-env
                      req-env
                      init-env)]
         [let-polys (or orig-let-polys (box null))]
         ;; typecheck the sequence:
         [types
          (map
           (lambda (tl)
             (let typecheck ([expr tl] [env env])
               (syntax-case expr (: require: define-type: define: define-values: 
                                    define-type-alias
                                    lambda: begin: local: letrec: let: let*: shared:
                                    begin: cond: case: if: when: unless:
                                    or: and: set!: trace:
                                    type-case: quote: time:
                                    list vector values: try)
                 [(require: . _)
                  ;; handled in require env
                  (void)]
                 [(define-type: id [variant (field-id : field-type) ...] ...)
                  ;; handled in initial env
                  (void)]
                 [(define-type-alias (id (quote: arg) ...) t)
                  ;; check that `t' makes sense
                  ((parse-param-type (map (lambda (arg) (cons arg (gen-tvar arg)))
                                          (syntax->list #'(arg ...))))
                   #'t)]
                 [(define-type-alias id t)
                  ;; check that `t' makes sense
                  (parse-type #'t)]
                 [(define: (id arg ...) . rest)
                  (typecheck #'(define: id (lambda: (arg ...) . rest))
                             env)]
                 [(define: id : type expr)
                  (unify-defn! #'expr (lookup #'id env)
                               (typecheck #'expr env))]
                 [(define: id expr)
                  (typecheck #'(define: id : (gensym id) expr)
                             env)]
                 [(define-values: (id ...) rhs)
                  (let ([tvars (map (lambda (id)
                                      (gen-tvar id))
                                    (syntax->list #'(id ...)))])
                    (unify! expr 
                            (make-tupleof expr tvars)
                            (typecheck #'rhs env))
                    (for-each (lambda (id tvar)
                                (unify-defn! expr
                                             (lookup id env)
                                             tvar))
                              (syntax->list #'(id ...))
                              tvars))]
                 [(lambda: (arg ...) : type body)
                  (let ([arg-ids (map (lambda (arg)
                                        (if (identifier? arg)
                                            arg
                                            (car (syntax-e arg))))
                                      (syntax->list #'(arg ...)))]
                        [arg-types (map (lambda (arg)
                                          (syntax-case arg (:)
                                            [(id : type)
                                             (poly-instance (parse-type #'type))]
                                            [else (gen-tvar arg)]))
                                        (syntax->list #'(arg ...)))]
                        [result-type (poly-instance (parse-type #'type))])
                    (unify! #'body
                            (typecheck #'body (append (map cons 
                                                           arg-ids
                                                           arg-types)
                                                      env))
                            result-type)
                    (make-arrow expr arg-types result-type))]
                 [(lambda: (arg ...) body)
                  (with-syntax ([expr expr])
                    (typecheck (syntax/loc #'expr
                                 (lambda: (arg ...) : (gensym expr) body))
                               env))]
                 [(begin: e ... last-e)
                  (begin
                    (map (lambda (e)
                           (typecheck e env))
                         (syntax->list #'(e ...)))
                    (typecheck #'last-e env))]
                 [(local: [defn ...] expr)
                  (let-values ([(ty env datatypes aliases vars tl-tys)
                                (typecheck-defns (syntax->list #'(defn ...))
                                                 datatypes
                                                 aliases
                                                 env
                                                 variants
                                                 #f
                                                 let-polys)])
                    (typecheck #'expr env))]
                 [(letrec: . _)
                  (typecheck ((make-let 'letrec) expr) env)]
                 [(let: . _)
                  (typecheck ((make-let 'let) expr) env)]
                 [(let*: . _)
                  (typecheck ((make-let 'let*) expr) env)]
                 [(shared: ([id rhs] ...) expr)
                  (let-values ([(ty env datatypes aliases vars tl-tys)
                                (typecheck-defns (syntax->list #'((define: id rhs) ...))
                                                 datatypes
                                                 aliases
                                                 env
                                                 variants
                                                 #f
                                                 let-polys)])
                    (typecheck #'expr env))]
                 [(cond: [ques ans] ...)
                  (let ([res-type (gen-tvar expr)])
                    (for-each
                     (lambda (ques ans)
                       (unless (syntax-case ques (else)
                                 [else #t]
                                 [_ #f])
                         (unify! #'ques
                                 (make-bool ques)
                                 (typecheck ques env)))
                       (unify! #'ans
                               res-type
                               (typecheck ans env)))
                     (syntax->list #'(ques ...))
                     (syntax->list #'(ans ...)))
                    res-type)]
                 [(case: expr [alts ans] ...)
                  (let ([res-type (gen-tvar #'expr)])
                    (unify! #'expr 
                            (make-sym #'expr) 
                            (typecheck #'expr env))
                    (for-each
                     (lambda (ans)
                       (unify! #'ans
                               res-type
                               (typecheck ans env)))
                     (syntax->list #'(ans ...)))
                    res-type)]
                 [(if: test then else)
                  (begin
                    (unify! #'test
                            (make-bool #'test)
                            (typecheck #'test env))
                    (let ([then-type (typecheck #'then env)])
                      (unify! #'then then-type (typecheck #'else env))
                      then-type))]
                 [(when: test e ...)
                  (begin
                    (unify! #'test
                            (make-bool #'test)
                            (typecheck #'test env))
                    (typecheck #'(begin: e ...) env)
                    (make-vd expr))]
                 [(unless: test e ...)
                  (begin
                    (unify! #'test
                            (make-bool #'test)
                            (typecheck #'test env))
                    (typecheck #'(begin: e ...) env)
                    (make-vd expr))]
                 [(and: e ...)
                  (let ([b (make-bool expr)])
                    (for-each (lambda (e)
                                (unify! e b (typecheck e env)))
                              (syntax->list #'(e ...)))
                    b)]
                 [(or: e ...)
                  (let ([b (make-bool expr)])
                    (for-each (lambda (e)
                                (unify! e b (typecheck e env)))
                              (syntax->list #'(e ...)))
                    b)]
                 [(set!: id e)
                  (let ([t (lookup #'id env)])
                    (if (poly? t)
                        (raise-syntax-error #f
                                            "cannot mutate identifier with a polymorphic type"
                                            expr
                                            #'id)
                        (unify-defn! #'id t (typecheck #'e env))))
                  (make-vd expr)]
                 [(trace: id ...)
                  (let ([ids (syntax->list #'(id ...))])
                    (for-each (lambda (id)
                                (unify! id (gen-tvar id #t) (typecheck id env)))
                              ids)
                    (make-tupleof expr null))]
                 [(type-case: type val [variant (id ...) ans] ...)
                  (let ([type (parse-mono-type #'type)]
                        [res-type (gen-tvar expr)])
                    (unify! #'val type (typecheck #'val env))
                    (for-each (lambda (var ids ans)
                                (let ([id-lst (syntax->list ids)]
                                      [variant-params (lookup var variants)])
                                  (unless (= (length id-lst)
                                             (length variant-params))
                                    (raise-syntax-error 'type-case
                                                        (format "variant ~a has ~a fields in the definition but ~a fields here at a use"
                                                                (syntax-e var)
                                                                (length variant-params)
                                                                (length id-lst))
                                                        var))
                                  (unify!
                                   expr
                                   res-type
                                   (typecheck ans
                                              (append (map (lambda (id ftype)
                                                             (cons id
                                                                   (instantiate-constructor-at
                                                                    ftype
                                                                    type)))
                                                           id-lst
                                                           variant-params)
                                                      env)))))
                              (syntax->list #'(variant ...))
                              (syntax->list #'((id ...) ...))
                              (syntax->list #'(ans ...)))
                    res-type)]
                 [(type-case: type val [variant (id ...) ans] ... [else else-ans])
                  (let ([t (typecheck (syntax/loc expr
                                        (type-case: type val [variant (id ...) ans] ...))
                                      env)])
                    (unify! #'else-ans t (typecheck #'else-ans env))
                    t)]
                 [(type-case: . rest)
                  (signal-typecase-syntax-error expr)]
                 [(quote: sym)
                  (if (identifier? #'sym)
                      (make-sym expr)
                      (make-sexp expr))]
                 [(time: expr)
                  (typecheck #'expr env)]
                 [(try expr1 (lambda: () expr2))
                  (let ([t (typecheck #'expr1 env)])
                    (unify! #'expr2 t (typecheck #'expr2 env))
                    t)]
                 [(list arg ...)
                  (let ([t (gen-tvar expr)])
                    (for-each (lambda (arg)
                                (unify! arg t (typecheck arg env)))
                              (syntax->list #'(arg ...)))
                    (make-listof expr t))]
                 [list
                  (raise-syntax-error #f
                                      "list constructor must be applied directly to arguments"
                                      expr)]
                 [(vector arg ...)
                  (let ([t (gen-tvar expr)])
                    (for-each (lambda (arg)
                                (unify! arg t (typecheck arg env)))
                              (syntax->list #'(arg ...)))
                    (make-vectorof expr t))]
                 [vector
                  (raise-syntax-error #f
                                      "vector constructor must be applied directly to arguments"
                                      expr)]
                 [(values: arg ...)
                  (make-tupleof expr
                                (map (lambda (arg)
                                       (typecheck arg env))
                                     (syntax->list #'(arg ...))))]
                 [values:
                  (raise-syntax-error #f
                                      "tuple constructor must be applied directly to arguments"
                                      expr)]
                 [(f arg ...)
                  (let ([res-type (gen-tvar expr)])
                    (unify! #'f
                            (typecheck #'f env)
                            (make-arrow #'f
                                        (map (lambda (arg)
                                               (typecheck arg env))
                                             (syntax->list #'(arg ...)))
                                        res-type))
                    res-type)]
                 [_else
                  (cond
                   [(identifier? expr)
                    (let ([t (lookup expr env)])
                      (if just-id?
                          t
                          (at-source (poly-instance t) expr)))]
                   [(boolean? (syntax-e expr))
                    (make-bool expr)]
                   [(number? (syntax-e expr))
                    (make-num expr)]
                   [(string? (syntax-e expr))
                    (make-str expr)]
                   [else
                    (raise-syntax-error #f
                                        "don't know how to typecheck"
                                        expr)])])))
           tl)])
    (set-box! let-polys (cons def-env (unbox let-polys)))
    (define poly-env
      (if orig-let-polys
          def-env
          (let-based-poly! (apply append (unbox let-polys)))))
    (define poly-def-env
      (if (eq? poly-env def-env)
          def-env
          (take poly-env (length def-env))))
    (values
     types
     (if (eq? def-env poly-def-env)
         env
         (append poly-def-env
                 req-env
                 init-env))
     datatypes
     aliases
     variants
     poly-def-env)))

(define-for-syntax tl-env #f)
(define-for-syntax tl-datatypes #f)
(define-for-syntax tl-aliases #f)
(define-for-syntax tl-variants #f)

(define-for-syntax (do-original-typecheck tl)
  (let ([datatypes null]
        [aliases null]
        [init-env (let ([NN->N (make-arrow #f 
                                           (list (make-num #f)
                                                 (make-num #f))
                                           (make-num #f))]
                        [N->N (make-arrow #f 
                                          (list (make-num #f))
                                          (make-num #f))]
                        [NN->B (make-arrow #f 
                                           (list (make-num #f)
                                                 (make-num #f))
                                           (make-bool #f))]
                        [N->B (make-arrow #f 
                                          (list (make-num #f))
                                          (make-bool #f))]
                        [N (make-num #f)]
                        [B (make-bool #f)]
                        [STR (make-str #f)]
                        [SYM (make-sym #f)])
                    (define-syntax-rule (POLY a e)
                      (let ([a (gen-tvar #f)]) (make-poly #f a e)))
                    (list
                     (cons #'error
                           (POLY a
                                 (make-arrow #f
                                             (list SYM
                                                   STR)
                                             a)))
                     (cons #'void:
                           (make-arrow #f null (make-vd #f)))
                     (cons #'not
                           (make-arrow #f
                                       (list B)
                                       B))
                     (cons #'+ NN->N)
                     (cons #'- NN->N)
                     (cons #'/ NN->N)
                     (cons #'* NN->N)
                     (cons #'= NN->B)
                     (cons #'< NN->B)
                     (cons #'> NN->B)
                     (cons #'<= NN->B)
                     (cons #'>= NN->B)
                     (cons #'min NN->N)
                     (cons #'max NN->N)
                     (cons #'modulo NN->N)
                     (cons #'remainder NN->N)
                     (cons #'floor N->N)
                     (cons #'ceiling N->N)
                     (cons #'add1 N->N)
                     (cons #'sub1 N->N)
                     (cons #'zero? N->B)
                     (cons #'odd? N->B)
                     (cons #'even? N->B)
                     (cons #'symbol=? (make-arrow #f 
                                                  (list SYM
                                                        SYM)
                                                  B))
                     (cons #'string=? (make-arrow #f 
                                                  (list STR
                                                        STR)
                                                  B))
                     (cons #'make-hash (POLY a (POLY b (make-arrow #f
                                                                   (list)
                                                                   (make-hashof #f a b)))))
                     (cons #'hash-ref: (POLY a (POLY b (make-arrow #f
                                                                   (list (make-hashof #f a b)
                                                                         a)
                                                                   b))))
                     (cons #'hash-ref/k (POLY a 
                                              (POLY b 
                                                    (POLY c
                                                          (make-arrow #f
                                                                      (list (make-hashof #f a b)
                                                                            a
                                                                            (make-arrow #f
                                                                                        (list b)
                                                                                        c)
                                                                            (make-arrow #f
                                                                                        (list)
                                                                                        c))
                                                                      c)))))
                     (cons #'hash-ref? (POLY a (POLY b (make-arrow #f
                                                                   (list (make-hashof #f a b)
                                                                         a)
                                                                   B))))
                     (cons #'hash-set! (POLY a (POLY b (make-arrow #f
                                                                   (list (make-hashof #f a b)
                                                                         a
                                                                         b)
                                                                   (make-vd #f)))))
                     (cons #'hash-remove! (POLY a (POLY b (make-arrow #f
                                                                      (list (make-hashof #f a b)
                                                                            a)
                                                                      (make-vd #f)))))
                     (cons #'hash-keys (POLY a (POLY b (make-arrow #f
                                                                   (list (make-hashof #f a b))
                                                                   (make-listof #f a)))))
                     (cons #'s-exp-symbol? (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       B))
                     (cons #'s-exp->symbol (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       SYM))
                     (cons #'symbol->s-exp (make-arrow #f 
                                                       (list SYM)
                                                       (make-sexp #f)))
                     (cons #'s-exp-number? (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       B))
                     (cons #'s-exp->number (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       N))
                     (cons #'number->s-exp (make-arrow #f 
                                                       (list N)
                                                       (make-sexp #f)))
                     (cons #'s-exp-string? (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       B))
                     (cons #'s-exp->string (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       STR))
                     (cons #'string->s-exp (make-arrow #f 
                                                       (list STR)
                                                       (make-sexp #f)))
                     (cons #'s-exp-list? (make-arrow #f 
                                                     (list (make-sexp #f))
                                                     B))
                     (cons #'s-exp->list (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       (make-listof #f (make-sexp #f))))
                     (cons #'list->s-exp (make-arrow #f 
                                                     (list (make-listof #f (make-sexp #f)))
                                                     (make-sexp #f)))
                     (cons #'read: (make-arrow #f null (make-sexp #f)))
                     (cons #'equal? (POLY a (make-arrow #f 
                                                        (list a a)
                                                        B)))
                     (cons #'eq? (POLY a (make-arrow #f 
                                                     (list a a)
                                                     B)))
                     (cons #'test (POLY a (make-arrow #f 
                                                      (list a a)
                                                      (make-vd #f))))
                     (cons #'test/exn (POLY a (make-arrow #f 
                                                          (list a
                                                                STR)
                                                          (make-vd #f))))
                     (cons #'print-only-errors (make-arrow #f 
                                                           (list B)
                                                           (make-vd #f)))
                     (cons #'call/cc (POLY a
                                           (POLY b
                                                 (make-arrow #f
                                                             (list (make-arrow
                                                                    #f
                                                                    (list (make-arrow
                                                                           #f
                                                                           (list a)
                                                                           b))
                                                                    a))
                                                             a))))
                     (cons #'true B)
                     (cons #'false B)                       
                     (cons #'empty (POLY a (make-listof #f a)))
                     (cons #'cons (POLY a (make-arrow #f
                                                      (list a (make-listof #f a))
                                                      (make-listof #f a))))
                     (cons #'cons? (POLY a (make-arrow #f
                                                       (list (make-listof #f a))
                                                       B)))
                     (cons #'empty? (POLY a (make-arrow #f
                                                        (list (make-listof #f a))
                                                        B)))
                     (cons #'first (POLY a (make-arrow #f
                                                       (list (make-listof #f a))
                                                       a)))
                     (cons #'rest (POLY a (make-arrow #f
                                                      (list (make-listof #f a))
                                                      (make-listof #f a))))
                     (cons #'second (POLY a
                                       (make-arrow #f
                                                   (list (make-listof #f a))
                                                   a)))
                     (cons #'third (POLY a (make-arrow #f
                                                       (list (make-listof #f a))
                                                       a)))
                     (cons #'fourth (POLY a (make-arrow #f
                                                        (list (make-listof #f a))
                                                        a)))
                     (cons #'list-ref (POLY a (make-arrow #f
                                                          (list (make-listof #f a)
                                                                N)
                                                          a)))
                     (cons #'build-list (POLY a (make-arrow #f
                                                            (list N
                                                                  (make-arrow #f
                                                                              (list N)
                                                                              a))
                                                            (make-listof #f a))))
                     (cons #'length (POLY a (make-arrow #f
                                                        (list (make-listof #f a))
                                                        N)))
                     (cons #'map (POLY a
                                       (POLY b
                                             (make-arrow #f
                                                         (list (make-arrow #f (list a) b)
                                                               (make-listof #f a))
                                                         (make-listof #f b)))))
                     (cons #'map2 (POLY a
                                        (POLY b
                                              (POLY c
                                                    (make-arrow #f
                                                                (list (make-arrow #f (list a b) c)
                                                                      (make-listof #f a)
                                                                      (make-listof #f b))
                                                                (make-listof #f c))))))
                     (cons #'member: (POLY a (make-arrow #f
                                                         (list a
                                                               (make-listof #f a))
                                                         B)))
                     (cons #'filter (POLY a (make-arrow #f
                                                        (list (make-arrow #f 
                                                                          (list a) 
                                                                          B)
                                                              (make-listof #f a))
                                                        (make-listof #f a))))
                     (cons #'foldl (POLY a
                                         (POLY b
                                               (make-arrow #f
                                                           (list (make-arrow #f (list a b) b)
                                                                 b
                                                                 (make-listof #f a))
                                                           b))))
                     (cons #'foldr (POLY a
                                         (POLY b
                                               (make-arrow #f
                                                           (list (make-arrow #f (list a b) b)
                                                                 b
                                                                 (make-listof #f a))
                                                           b))))
                     (cons #'reverse (POLY a (make-arrow #f
                                                         (list (make-listof #f a))
                                                         (make-listof #f a))))
                     (cons #'append (POLY a (make-arrow #f
                                                        (list (make-listof #f a)
                                                              (make-listof #f a))
                                                        (make-listof #f a))))
                     (cons #'box (POLY a (make-arrow #f
                                                     (list a)
                                                     (make-boxof #f a))))
                     (cons #'unbox (POLY a (make-arrow #f
                                                       (list (make-boxof #f a))
                                                       a)))
                     (cons #'set-box! (POLY a (make-arrow #f
                                                          (list (make-boxof #f a) a)
                                                          (make-vd #f))))
                     (cons #'make-vector (POLY a (make-arrow #f
                                                             (list N a)
                                                             (make-vectorof #f a))))
                     (cons #'vector-ref (POLY a (make-arrow #f
                                                            (list (make-vectorof #f a)
                                                                  N)
                                                            a)))
                     (cons #'vector-set! (POLY a (make-arrow #f
                                                             (list (make-vectorof #f a)
                                                                   N
                                                                   a)
                                                             (make-vd #f))))
                     (cons #'vector-length (POLY a (make-arrow #f
                                                               (list (make-vectorof #f a))
                                                               N)))

                     (cons #'string-append (make-arrow #f 
                                                       (list STR STR)
                                                       STR))
                     (cons #'string->symbol (make-arrow #f 
                                                        (list STR)
                                                        SYM))
                     (cons #'symbol->string (make-arrow #f 
                                                        (list SYM)
                                                        STR))
                     (cons #'identity (POLY a
                                         (make-arrow #f
                                                     (list a)
                                                     a)))
                     (cons #'to-string (POLY a
                                             (make-arrow #f
                                                         (list a)
                                                         STR)))
                     (cons #'display (POLY a
                                           (make-arrow #f
                                                       (list a)
                                                       (make-vd #f))))
                     ))])
    (typecheck-defns tl datatypes aliases init-env null #f #f)))

(define-syntax (typecheck-and-provide stx)
  (let-values ([(tys e2 d2 a2 vars tl-types) 
                (with-handlers ([exn:fail? (lambda (exn)
                                             (values exn #f #f #f #f null))])
                  (do-original-typecheck (cdr (syntax->list stx))))])
    (if (exn? tys)
        ;; There was an exception while type checking. To order
        ;; type-checking errors after expansion, push the error into
        ;; a sub-expression:
        #`(#%expression (let-syntax ([x (raise #,tys)])
                          x))
        #`(provide
           (contract-out
            #,@(map (λ (tl-thing)
                       #`[#,(car tl-thing)
                          #,(to-contract (cdr tl-thing) #f)])
                    tl-types))))))

(define-for-syntax orig-body #f)
(define-for-syntax (set-orig-body! v)
  (set! orig-body v))

(define-syntax (typecheck stx)
  (syntax-case stx ()
    [(_ . body)
     #'(begin
         (begin-for-syntax (set-orig-body! (quote-syntax body)))
         ;; Typechecking happens at the `provide' expansion phase,
         ;; which is after everything else is expanded:
         (typecheck-and-provide . body))]))

;; ----------------------------------------

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . body)
     (let ([expanded-body (local-expand #'body 'top-level null)])
       (unless tl-env
         (let-values ([(ts e d a vars tl-types) (do-original-typecheck (syntax->list (or orig-body #'())))])
           (set! tl-datatypes d)
           (set! tl-aliases a)
           (set! tl-env e)
           (set! tl-variants vars)))
       (let-values ([(tys e2 d2 a2 vars tl-types) 
                     (typecheck-defns (list #'body) tl-datatypes tl-aliases tl-env tl-variants (identifier? #'body) #f)])
         (set! tl-datatypes d2)
         (set! tl-aliases a2)
         (set! tl-env e2)
         (with-syntax ([ty ((type->datum (make-hasheq)) (car tys))]
                       [body expanded-body])
           (if (void? (car tys))
               #'body
               #'(begin
                   (print-type 'ty)
                   body)))))]))

(define (print-type t)
  (parameterize ([pretty-print-print-line
                  (lambda (line p len maxcol) 
                    (if (equal? line 0)
                        (display "- " p)
                        (if line
                            (display "\n  " p)
                            (display "\n" p)))
                    0)])
    (pretty-write t)))

;; ----------------------------------------

;; printing top-level expressions --- stolen from v4 "modbeg.ss",
;;  which is why it is written in such primitive terms

(define-values (print-value)
  (lambda (v)
    (if (void? v)
        (void)
        (begin
          (print v)
          (newline)))))

(define-syntaxes (module-begin)
  (lambda (stx)
    (if (eq? 'module-begin (syntax-local-context))
        (void)
        (raise-syntax-error
         #f
         "allowed only around a module body"
         stx))
    (if (symbol? (syntax-e stx))
        (raise-syntax-error
         #f
         "bad syntax" 
         stx)
        (void))
    (datum->syntax
     stx
     (list (quote-syntax #%module-begin)
           (cons (quote-syntax begin)
                 (cdr (syntax-e stx)))
           (cons (quote-syntax typecheck)
                 (cdr (syntax-e stx))))
     stx
     stx)))
