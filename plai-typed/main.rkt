#lang racket/base

(require (only-in plai
                  define-type
                  type-case
                  test
                  test/exn
                  print-only-errors
                  error)
         racket/pretty
         racket/list
         racket/bool
         racket/local
         racket/shared
         racket/include
         (only-in racket/contract/base contract-out)
         racket/trace
         "private/fixup-quote.ss"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     "private/types.ss"
                     racket/struct-info
                     "private/collapse.ss"))

(provide :
         (rename-out [define: define]
                     [define-values: define-values]
                     [lambda: lambda]
                     [lambda: λ]
                     [begin: begin]
                     [local: local]
                     [letrec: letrec] [let: let] [let*: let*]
                     [shared: shared]
                     [parameterize: parameterize]
                     [cond: cond]
                     [case: case]
                     [if: if]
                     [when: when]
                     [unless: unless]
                     [or: or]
                     [and: and]
                     [quote: quote]
                     [quasiquote: quasiquote]
                     [set!: set!]
                     [time: time]
                     [trace: trace]
                     [require: require]
                     [module+: module+]
                     [include: include]
                     [splice: splice]
                     [define-syntax: define-syntax]
                     [define-syntax-rule: define-syntax-rule])
         #%app #%datum #%top unquote unquote-splicing
         module submod
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         else typed-in rename-in opaque-type-in
         has-type

         (for-syntax (all-from-out racket/base))

         (rename-out [define-type: define-type]
                     [type-case: type-case])
         define-type-alias

         (rename-out [test: test]
                     [test/exn: test/exn])
         print-only-errors
         
         cons list empty first rest empty? cons?
         second third fourth list-ref build-list length
         map reverse map2 append
         filter foldl foldr
         (rename-out [member: member])

         + - = > < <= >= / * min max 
         add1 sub1 zero? odd? even?
         modulo remainder floor ceiling
         symbol=? string=? equal? eq? not
         error try call/cc let/cc

         string->symbol symbol->string
         string-append to-string
         display

         char=? string-ref substring string-length string->list list->string

         (rename-out [make-hash: make-hash]
                     [hash: hash]
                     [hash-ref: hash-ref])
         hash-set! hash-remove! hash-keys
         hash-set hash-remove

         s-exp-symbol? s-exp->symbol symbol->s-exp
         s-exp-number? s-exp->number number->s-exp
         s-exp-string? s-exp->string string->s-exp
         s-exp-boolean? s-exp->boolean boolean->s-exp
         s-exp-list? s-exp->list list->s-exp
         (rename-out [read: read])
         
         box unbox set-box!

         make-vector vector-ref vector-set! vector-length vector
         
         (rename-out [values: values])
         pair fst snd

         identity

         true false

         make-parameter parameter-ref parameter-set!
         
         number boolean symbol 
         (rename-out [string: string])
         char
         s-expression
         -> 
         (rename-out [listof: listof]
                     [boxof: boxof]
                     [parameterof: parameterof]
                     [vectorof: vectorof]
                     [hashof: hashof]
                     [void: void])

         optionof none some some-v none? some?)

(define-type optionof
  [none]
  [some (v (lambda (x) #t))])

(define not-there (gensym))

(define (hash: l)
  (apply hash
         (apply
          append
          (for/list ([v (in-list l)])
            (list (vector-ref v 0)
                  (vector-ref v 1))))))

(define (make-hash: l)
  (make-hash (for/list ([v (in-list l)])
               (cons (vector-ref v 0)
                     (vector-ref v 1)))))

(define (hash-ref: ht k)
  (define v (hash-ref ht k not-there))
  (if (eq? v not-there)
      (none)
      (some v)))

(define (s-exp-symbol? s) (symbol? s))
(define (s-exp->symbol s) (if (symbol? s) s (error 's-exp->symbol "not a symbol: ~e" s)))
(define (symbol->s-exp s) s)
(define (s-exp-number? s) (number? s))
(define (s-exp->number s) (if (number? s) s (error 's-exp->number "not a number: ~e" s)))
(define (number->s-exp s) s)
(define (s-exp-string? s) (string? s))
(define (s-exp->string s) (if (string? s) s (error 's-exp->string "not a string: ~e" s)))
(define (string->s-exp s) s)
(define (s-exp-boolean? s) (boolean? s))
(define (s-exp->boolean s) (if (boolean? s) s (error 's-exp->boolean "not a boolean: ~e" s)))
(define (boolean->s-exp s) s)
(define (s-exp-list? s) (list? s))
(define (s-exp->list s) (if (list? s) s (error 's-exp->list "not a list: ~e" s)))
(define (list->s-exp s) s)

(define (identity x) x)

(define (read:)
  (define v (read))
  (unless (let loop ([v v])
            (or (symbol? v)
                (string? v) 
                (number? v)
                (boolean? v)
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
(define-syntax char type)
(define-syntax s-expression type)
(define-syntax -> type)
(define-syntax listof: type)
(define-syntax boxof: type)
(define-syntax vectorof: type)
(define-syntax hashof: type)
(define-syntax parameterof: type)
(define void: void) ; allow as a function

(define (member: a l)
  (and (member a l) #t))

(define (to-string x) (format "~v" x))

(define (parameter-ref p)
  (p))
(define (parameter-set! p v)
  (p v))

(define-for-syntax (is-type-keyword? a)
  (ormap (lambda (i)
           (free-identifier=? a i))
         (syntax->list
          #'(: number boolean symbol char s-expression
               string: -> * listof: hashof:
               boxof: vectorof: parameterof:
               void: optionof))))

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

(define-for-syntax (absolute-module-path s)
  (if (and (pair? s)
           (or (and (eq? (car s) 'submod)
                    (or (equal? (cadr s) ".")
                        (equal? (cadr s) "..")))
               (and (eq? (car s) 'quote))))
      (module-path-index-join
       s
       (eval #'(variable-reference->module-path-index
                (#%variable-reference))))
      s))

(define-syntax require:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ clause ...)
        (with-syntax ([(new-clause ...)
                       (map (lambda (clause)
                              (let loop ([clause clause])
                                (syntax-case clause (typed-in rename-in opaque-type-in :)
                                  [(typed-in lib
                                             [id : type]
                                             ...)
                                   (begin
                                     (let ([lib #'lib]
                                           [ids (syntax->list #'(id ...))])
                                       (unless (module-path? (syntax->datum lib))
                                         (raise-syntax-error #f "bad module path" stx lib))
                                       (for ([id (in-list ids)])
                                         (unless (identifier? id)
                                           (raise-syntax-error #f "expected an identifier" stx id))))
                                     (with-syntax ([lib (fixup-quote #'lib #'quote:)])
                                       (syntax/loc clause (only-in lib id ...))))]
                                  [(typed-in lib spec ...)
                                   (for ([spec (in-list (syntax->list #'(spec ...)))])
                                     (syntax-case spec (:)
                                       [(id : type) (void)]
                                       [(id something . _) 
                                        (and (identifier? #'id)
                                             (or (not (identifier? #'something))
                                                 (not (free-identifier=? #'something #':))))
                                        (raise-syntax-error
                                           #f
                                           (format "expected a colon after the identifier `~s'"
                                                   (syntax-e #'id))
                                           clause
                                           #'something)]
                                       [_ (raise-syntax-error
                                           #f
                                           "expected a specification of the form [<id> : <type>]"
                                           clause
                                           spec)]))]
                                  [(rename-in sub-clause [old-id new-id] ...)
                                   (let ([sub (loop #'sub-clause)])
                                     (define (check id)
                                       (unless (identifier? id) 
                                         (raise-syntax-error #f "expected an identifier" clause id)))
                                     (for ([old-id (in-list (syntax->list #'(old-id ...)))]
                                           [new-id (in-list (syntax->list #'(new-id ...)))])
                                       (check old-id)
                                       (check new-id))
                                     (with-syntax ([sub sub])
                                       (syntax/loc clause (rename-in sub [old-id new-id] ...))))]
                                  [(opaque-type-in lib [id predicate-id] ...)                                   
                                   (let ()
                                     (define (check id)
                                       (unless (identifier? id) 
                                         (raise-syntax-error #f "expected an identifier" clause id)))
                                     (for ([id (in-list (syntax->list #'(id ...)))]
                                           [predicate-id (in-list (syntax->list #'(predicate-id ...)))])
                                       (check id)
                                       (check predicate-id))
                                     (syntax/loc clause (only-in lib predicate-id ...
                                                                 ;; Also import predicate as `id':
                                                                 [predicate-id id] ...)))]
                                  [mp
                                   (module-path? (syntax->datum #'mp))
                                   (let ([s (syntax->datum #'mp)])
                                     (define xs (if (and (pair? s) 
                                                         (eq? (car s) 'quote)
                                                         ;; The following check is intended to
                                                         ;; allow access to modules at the top
                                                         ;; level that have only symbolic names,
                                                         ;; but `syntax-local-submodules' is broken
                                                         ;; in expand mode as of v5.3.3, so we skip it
                                                         ;; for now.
                                                         #;
                                                         (memq (cadr s) (syntax-local-submodules)))
                                                    ;; convert to `submod' form:
                                                    (list 'submod "." (cadr s))
                                                    ;; ok as-is:
                                                    s))
                                     (define typed? 
                                       (module-declared? (absolute-module-path
                                                          (if (and (pair? xs) (eq? (car xs) 'submod))
                                                              `(,@xs plai-typed)
                                                              `(submod ,xs plai-typed)))
                                                         #t))
                                     (unless typed?
                                       (when (module-declared? (absolute-module-path xs) #t)
                                         (raise-syntax-error #f
                                                             "not a `plai-typed' module"
                                                             stx
                                                             #'mp)))
                                     (fixup-quote
                                      (if typed?
                                          (let ([new-clause
                                                 (if (and (pair? s) (eq? (car s) 'submod))
                                                     (quasisyntax/loc clause (#,@#'mp plai-typed))
                                                     (quasisyntax/loc clause (submod mp plai-typed)))])
                                            (datum->syntax clause
                                                           (syntax-e new-clause)
                                                           clause
                                                           clause))
                                          clause)
                                      #'quote:))]
                                  [_
                                   (raise-syntax-error #f
                                                       "not a valid require specification"
                                                       stx
                                                       clause)])))
                            (syntax->list #'(clause ...)))])
          #'(require new-clause ...))]))))

(define-syntax typed-in
  (lambda (stx)
    (raise-syntax-error #f "allowed only in `require'" stx)))
(define-syntax opaque-type-in
  (lambda (stx)
    (raise-syntax-error #f "allowed only in `require'" stx)))

(define-syntax test:
  (check-top
   (syntax-rules ()
     [(_ e ...) (test e ...)])))

(define-syntax test/exn:
  (check-top
   (syntax-rules ()
     [(_ e ...) (test/exn e ...)])))

(define-syntax-rule (module+: name e ...)
  (module+ name
    ;; to register implicitly imported types:
    (require (only-in (submod ".." plai-typed)))
    e
    ...))

(define-syntax include: 
  (check-top
   (lambda (stx)
     (unless (memq (syntax-local-context) '(module top-level))
       (raise-syntax-error #f "allowed only as a top-level form" stx))
     (syntax-case stx ()
       [(_ spec) (with-syntax ([orig-stx stx])
                   (syntax/loc stx 
                     (include-at/relative-to orig-stx orig-stx spec)))]))))

(begin-for-syntax 
 (struct typed-macro (proc)
   #:property prop:procedure 0)
 (define macro-inspector (current-code-inspector))

 (define (add-begin proc)
   (lambda (stx)
     (define result (proc stx))
     (if (syntax? result)
         ;; Insert a `begin' wrapper so we can `local-expand' just once:
         #`(begin #,result)
         ;; Otherwise, let expander report the error:
         result))))

(define-syntax define-syntax:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ id rhs)
        (identifier? #'id)
        (syntax-case* #'rhs (syntax-rules lambda) free-transformer-identifier=?
          [(syntax-rules . _)
           (syntax/loc stx
             (define-syntax id (typed-macro (add-begin rhs))))]
          [(lambda (arg) . rest)
           (syntax/loc stx
             (define-syntax id (typed-macro (add-begin rhs))))]
          [_
           (raise-syntax-error #f "expected a `syntax-rules' or single-argument `lambda' form after identifier" stx)])]
       [(_ (id arg-id) . rest)
        (and (identifier? #'id)
             (identifier? #'arg-id))
        (if (and (pair? (syntax-e #'rest))
                 (syntax->list #'rest))
            (syntax/loc stx
              (define-syntax id (typed-macro (add-begin (lambda (arg-id) . rest)))))
            (raise-syntax-error #f "ill-formed macro body" stx))]
       [(_ id . _)
        (raise-syntax-error #f "expected an identifier or `(<identifier> <identifier>)' header" stx #'id)]))))

(define-syntax define-syntax-rule:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ (id . rest) tmpl)
        (identifier? #'id)
        #`(define-syntax: id (syntax-rules () [(id . rest) tmpl]))]))))

(define-for-syntax (disarm stx)
  (let loop ([e stx])
    (cond
     [(syntax? e) 
      (define stx (syntax-disarm e macro-inspector))
      (datum->syntax stx
                     (loop (syntax-e stx))
                     stx
                     stx)]
     [(pair? e) (cons (loop (car e)) (loop (cdr e)))]
     [(vector? e) (list->vector (map loop (vector->list e)))]
     [else e])))

(define-for-syntax (local-expand-typed expr)
  (define stx (local-expand expr 'expression #f))
  (syntax-case stx (begin)
    [(begin e) (disarm #'e)]
    [_ (error 'local-expand-typed "something went wrong: ~e => ~e" expr stx)]))

(define-for-syntax (expand-includes l)
  (let loop ([l l])
    (cond
     [(null? l) null]
     [else
      (syntax-case (car l) (include: splice:)
        [(include: spec)
         (append
          (cdr (syntax->list (local-expand (car l) (syntax-local-context) #f)))
          (loop (cdr l)))]
        [(splice: e ...)
         (loop (append (syntax->list #'(e ...)) (cdr l)))]
        [(id . _)
         (and (identifier? #'id)
              (typed-macro? (syntax-local-value #'id (lambda () #f))))
         (loop (cons (local-expand-typed (car l))
                     (cdr l)))]
        [_ (cons (car l) (loop (cdr l)))])])))

(define-syntax splice: 
  (check-top
   (lambda (stx)
     (unless (memq (syntax-local-context) '(module top-level))
       (raise-syntax-error #f "allowed only as a top-level form" stx))
     (syntax-case stx ()
       [(_ e ...) #'(begin e ...)]))))

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

(define (pair a b) (vector-immutable a b))
(define (fst v) (vector-ref v 0))
(define (snd v) (vector-ref v 1))

(define-syntax define-values:
  (check-top
   (lambda (stx)
     (syntax-case stx (:)
       [(_ (id ...) expr)
        (with-syntax ([(id ...)
                       (map (lambda (id)
                              (if (identifier? id)
                                  (begin (check-defn-keyword id stx)
                                         id)
                                  (syntax-case id (:)
                                    [(id : type)
                                     (begin (check-defn-keyword #'id stx) #'id)]
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

(define-for-syntax expand-define-type
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
                     [((variant? (variant-field set-variant-field!) ...) ...)
                      (map (lambda (variant fields)
                             (cons 
                              (datum->syntax variant
                                             (string->symbol
                                              (format "~a?" (syntax-e variant)))
                                             variant
                                             variant)
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
                                   (syntax->list fields))))
                           (syntax->list #'(variant ...))
                           (syntax->list #'((field ...) ...)))])
         (let ([dup (check-duplicate-identifier
                     (syntax->list #'(id
                                      variant ... 
                                      variant? ...
                                      variant-field ... ... 
                                      set-variant-field! ... ...)))])
           (when dup
             (raise-syntax-error #f 
                                 "duplicate definition for identifier"
                                 stx
                                 dup)))
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
                 (syntax->list #'(thing ...)))])))

(define-syntax define-type:
  (check-top expand-define-type))

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
          (check-defn-keyword #'id stx)
          #'(void))]
       [(_ id t)
        (let ([id #'id])
          (unless (identifier? id)
            (raise-syntax-error #f "expected `<id>' or `(<id> '<id> ...)'" stx id))
          (check-defn-keyword #'id stx)
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
                             "expected a function, constant, or tuple definition"
                             thing)]))
                  (syntax->list #'(thing ...)))]))))

(define-syntax parameterize:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ ([param-expr rhs-expr] ...) e)
        (syntax/loc stx
          (parameterize ([param-expr rhs-expr] ...) e))]))))

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
            (let ([ids (syntax->list #'(id ...))])
              (for ([id (in-list ids)])
                (check-defn-keyword id stx))
              (syntax/loc stx
                (shared ([id rhs] ...) body)))])
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
                                          (let ([id (constructor-syntax-id c)])
                                            (datum->syntax id (syntax-e id) #'variant))
                                          #'variant))])
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
                       (let loop ([altss (syntax->list #'(alts ...))] [kind #f])
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
                                (let id-loop ([ids (syntax->list #'(id ...))]
                                              [kind kind])
                                  (if (null? ids)
                                      (loop (cdr altss) kind)
                                      (let ([id (car ids)])
                                        (unless (or (identifier? id)
                                                    (number? (syntax-e id)))
                                          (raise-syntax-error #f
                                                              "alternative must be a symbol or a number"
                                                              stx
                                                              id))
                                        (let ([new-kind (if (identifier? id)
                                                            'symbol
                                                            'number)])
                                          (when (and kind (not (eq? new-kind kind)))
                                            (raise-syntax-error #f
                                                                (format "~a disallowed after preceding ~a"
                                                                        new-kind
                                                                        kind)
                                                                stx
                                                                id))
                                          (id-loop (cdr ids) new-kind)))))]
                               [_ (raise-syntax-error #f
                                                      "expected (<id/num> ...)"
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
                              "expected [(<id/num> ...) <result-expr>] or [else <result-expr>]"
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

(define-for-syntax (check-quoted stx on-escaped)
  (let loop ([s stx] [qq 0])
    (define (fst s) (car (syntax-e s)))
    (define (d->s s e) (datum->syntax s e s s))
    (define (default s)
      (or (and (let ([v (syntax-e s)])
                 (or (symbol? v)
                     (number? v)
                     (string? v)
                     (boolean? v)))
               s)
          (let ([l (syntax->list s)])
            (and l
                 (d->s s (map (lambda (v) (loop v qq)) l))))
          (raise-syntax-error #f
                              "disallowed content; not a symbol, number, string, or list"
                              stx
                              s)))
    (if on-escaped
        (syntax-case s (unquote unquote-splicing quasiquote:)
          [(unquote e) (if (zero? qq)
                           (on-escaped s)
                           (d->s s `(,(fst s) ,(loop #'e (sub1 qq)))))]
          [(unquote-splicing e) (if (zero? qq)
                                    (on-escaped s)
                                    (d->s s `(,(fst s) ,(loop #'e (sub1 qq)))))]
          [(quasiquote: e) (d->s s `(,(syntax/loc (fst s) quasiquote)
                                     ,(loop #'e (add1 qq))))]
          [unquote (raise-syntax-error #f "bad syntax" s)]
          [unquote-splicing (raise-syntax-error #f "bad syntax" s)]
          [quasiquote: (raise-syntax-error #f "bad syntax" s)]
          [_ (default s)])
        (default s))))

(define-syntax quote:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ s)
        (begin
          (check-quoted stx #f)
          #'(quote s))]))))

(define-syntax quasiquote:
  (check-top
   (lambda (stx)
     (syntax-case stx ()
       [(_ s)
        (check-quoted stx (lambda (s) s))]))))

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

(define-syntax has-type
  (check-top
   (syntax-rules (:)
     [(_ expr : type) expr])))

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

(define-syntax: let/cc
  (lambda (stx)
    (syntax-case stx ()
      [(_ id expr)
       (begin
         (unless (identifier? #'id)
           (raise-syntax-error #f "expected an identifier" stx #'id))
         (syntax/loc stx (call/cc (lambda: (id) expr))))])))

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

(define-for-syntax (rename-ids ids expr)
  (cond
   [(null? ids) expr]
   [else
    (define d (syntax-local-make-definition-context))
    (syntax-local-bind-syntaxes	ids #f d)
    (internal-definition-context-seal d)
    (internal-definition-context-apply d expr)]))

(define-for-syntax (extract-definition-ids defn)
  (syntax-case defn (: define-type: define: define-values: 
                       define-type-alias)
    [(define-type: name [variant (field-id : field-type) ...] ...)
     (let-values ([(name args)
                   (syntax-case #'name (quote:)
                     [(name (quote arg) ...)
                      (values #'name (syntax->list #'(arg ...)))]
                     [else (values #'name null)])])
       (apply append
              (list #'id)
              (map (lambda (var fields)
                     (list* var
                            (mk var var "?")
                            (map (lambda (field)
                                   (mk var var "-" field))
                                 (syntax->list fields))))
                   (syntax->list #'(variant ...))
                   (syntax->list #'((field-id ...) ...)))))]
    [(define-type-alias (id (quote: arg) ...) t)
     (list #'id)]
    [(define-type-alias id t)
     (list #'id)]
    [(define: (id arg ...) . rest)
     (list #'id)]
    [(define: id : type expr)
     (list #'id)]
    [(define: id expr)
     (list #'id)]
    [(define-values: (id ...) rhs)
     (syntax->list #'(id ...))]
    [_ null]))

;; Since we manage macro expansion during type checking, we're also
;; responsible for renaming at local-binding forms:
(define-for-syntax (rename expr)
  (syntax-case expr (: lambda: local: letrec: let: let*: shared:
                       type-case:)
    [(lambda: (arg ...) . _)
     (rename-ids (map (lambda (arg)
                        (syntax-case arg (:)
                          [(id : type) #'id]
                          [else arg]))
                      (syntax->list #'(arg ...))) 
                 expr)]
    [(local: [defn ...] body)
     (rename-ids (apply append
                        (map extract-definition-ids
                             (syntax->list #'(defn ...))))
                 expr)]
    [(letrec: ([id rhs] ...) body)
     (rename-ids (syntax->list #'(id ...)) expr)]
    [(let: ([id rhs] ...) body)
     (rename-ids (syntax->list #'(id ...)) expr)]
    [(let*: ([id rhs] ...) body)
     (rename-ids (syntax->list #'(id ...)) expr)]
    [(shared: ([id rhs] ...) body)
     (rename-ids (syntax->list #'(id ...)) expr)]
    [(type-case: type val clause ...)
     (quasisyntax/loc expr
       (#,(car (syntax-e expr)) type val
        #,@(map (lambda (clause)
                  (syntax-case clause ()
                    [[variant (id ...) ans]
                     (rename-ids (syntax->list #'(id ...)) clause)]
                    [_ clause]))
                (syntax->list #'(clause ...)))))]
    [_ expr]))

(define-for-syntax (typecheck-defns tl datatypes opaques aliases init-env init-variants just-id? 
                                    poly-context orig-let-polys submods)
  (let* ([poly-context (cons (gensym) poly-context)]
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
         [apply-renames (lambda (spec l cdrs-too?)
                          (syntax-case spec (rename-in)
                            [(rename-in spec [old-id new-id] ...)
                             (let ()
                               (define old-ids (syntax->list #'(old-id ...)))
                               (define new-ids (syntax->list #'(new-id ...)))
                               (map (lambda (p)
                                      (let loop ([old-ids old-ids]
                                                 [new-ids new-ids])
                                        (cond
                                         [(null? old-ids) p]
                                         [(free-identifier=? (car old-ids) (car p))
                                          (cons (car new-ids) (cdr p))]
                                         [(and cdrs-too? 
                                               (free-identifier=? (car old-ids) (cdr p)))
                                          (cons (car p) (car new-ids))]
                                         [else (loop (cdr old-ids) (cdr new-ids))])))
                                    l))]))]
         [opaques (append (apply
                           append
                           (map
                            (lambda (stx)
                              (syntax-case stx (require:)
                                [(require: spec ...)
                                 (let loop ([specs (syntax->list #'(spec ...))])
                                   (apply
                                    append
                                    (map (lambda (spec)
                                           (syntax-case spec (opaque-type-in rename-in)
                                             [(opaque-type-in lib [id pred] ...)
                                              (map cons 
                                                   (syntax->list #'(id ...))
                                                   (syntax->list #'(pred ...)))]
                                             [(rename-in sub-spec . _)
                                              (apply-renames spec (loop (list #'sub-spec)) #t)]
                                             [_ null]))
                                         specs)))]
                                [else
                                 null]))
                            tl))
                          opaques)]
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
                                   (syntax-case t (number boolean symbol string: char s-expression
                                                          gensym listof: boxof: hashof: parameterof: void: -> 
                                                          vectorof: quote: * optionof)
                                     [(quote: id)
                                      (identifier? #'id)
                                      (let ([a (ormap (lambda (p)
                                                        (and (free-identifier=? (car p) #'id)
                                                             p))
                                                      (append tenv
                                                              tvars))])
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
                                     [char (make-chr t)]
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
                                     [(parameterof: elem)
                                      (make-parameterof t (loop #'elem))]
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
                                     [(optionof type)
                                      (make-datatype t #'optionof (list (loop #'type)))]
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
                                            (and (identifier? #'id)
                                                 (ormap (lambda (d)
                                                          (and (free-identifier=? (car d) #'id)
                                                               (if (null? types)
                                                                   (make-opaque-datatype
                                                                    t
                                                                    (car (syntax-e t))
                                                                    null
                                                                    (cdr d))
                                                                   (raise-syntax-error
                                                                    #f
                                                                    "bad type (incorrect use of a non-polymorphic type name)"
                                                                    t))))
                                                        opaques))
                                            (ormap (lambda (d)
                                                     (and (and (identifier? #'id)
                                                               (free-identifier=? (car d) #'id))
                                                          (begin
                                                            (unless (= (length (cadr d)) (length types))
                                                              
                                                              (raise-syntax-error
                                                               #f
                                                               (if (zero? (cdr d))
                                                                   "bad type (incorrect use of a non-polymorphic type alias name)"
                                                                   "type alias constructor applied to the wrong number of types")
                                                               t))
                                                            (when (ormap (lambda (s)
                                                                           (free-identifier=? s #'id))
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
                                                             (make-opaque-datatype
                                                              t
                                                              t
                                                              null
                                                              (cdr d))))
                                                      opaques))
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
         [macros (apply append
                        (map 
                         (lambda (stx)
                           (syntax-case stx (define-syntax:)
                             [(define-syntax: (id . _) . _)
                              (list #'id)]
                             [(define-syntax: id . _)
                              (list #'id)]
                             [_ null]))
                         tl))]
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
                        (syntax-case expr (lambda: list values: cons empty hash: quote: none some)
                          [(lambda: . _) #t]
                          [(values: a ...)
                           (andmap loop (syntax->list #'(a ...)))]
                          [(list a ...)
                           (andmap loop (syntax->list #'(a ...)))]
                          [(hash: a ...)
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
                          [(none) #t]
                          [(some e) (loop #'e)]
                          [(quote: a) #t]
                          [_ (or (identifier? expr)
                                 (string? (syntax-e expr))
                                 (char? (syntax-e expr))
                                 (number? (syntax-e expr))
                                 (boolean? (syntax-e expr)))])))]
         [req-env (apply
                   append
                   (map
                    (lambda (stx)
                      (syntax-case stx (require:)
                        [(require: spec ...)
                         (let loop ([specs (syntax->list #'(spec ...))])
                           (apply
                            append
                            (map (lambda (spec)
                                   (syntax-case spec (typed-in rename-in :)
                                     [(typed-in lib (id : type) ...)
                                      (map (lambda (id type)
                                             (cons id (parse-type type)))
                                           (syntax->list #'(id ...))
                                           (syntax->list #'(type ...)))]
                                     [(rename-in sub-spec . _)
                                      (apply-renames spec (loop (list #'sub-spec)) #f)]
                                     [_ null]))
                                 specs)))]
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
                                                    (gen-tvar id)
                                                    poly-context)
                                                   (as-non-poly 
                                                    (gen-tvar id)
                                                    poly-context)))
                                      (syntax-case id (:)
                                        [(id : type)
                                         (cons #'id 
                                               (if val?
                                                   (create-defn
                                                    (parse-type #'type)
                                                    poly-context)
                                                   (as-non-poly
                                                    (parse-mono-type #'type)
                                                    poly-context)))])))
                                (syntax->list #'(id ...))))]
                        [(define: (id . args) : result-type . _body)
                         (list (cons #'id
                                     (create-defn
                                      (make-arrow 
                                       #'id
                                       (map extract-arg-type
                                            (syntax->list #'args))
                                       (parse-mono-type #'result-type))
                                      poly-context)))]
                        [(define: (id . args) . _body)
                         (list (cons #'id (create-defn (make-arrow 
                                                        #'id
                                                        (map extract-arg-type (syntax->list #'args))
                                                        (gen-tvar #'id))
                                                       poly-context)))]
                        [(define: id : type (lambda: . _))
                         (list (cons #'id
                                     (create-defn (parse-type #'type)
                                                  poly-context)))]
                        [(define: id (lambda: args : result-type expr))
                         (list (cons #'id
                                     (create-defn
                                      (make-arrow
                                       #'id
                                       (map extract-arg-type (syntax->list #'args))
                                       (parse-mono-type #'result-type))
                                      poly-context)))]
                        [(define: id (lambda: args expr))
                         (list (cons #'id
                                     (create-defn
                                      (make-arrow
                                       #'id
                                       (map extract-arg-type (syntax->list #'args))
                                       (gen-tvar #'id))
                                      poly-context)))]
                        [(define: id : type expr)
                         (list (cons #'id 
                                     (if (is-value? #'expr)
                                         (create-defn
                                          (parse-type #'type)
                                          poly-context)
                                         (as-non-poly
                                          (parse-mono-type #'type)
                                          poly-context))))]
                        [(define: id expr)
                         (list (cons #'id 
                                     (if (is-value? #'expr)
                                         (create-defn
                                          (gen-tvar #'id)
                                          poly-context)
                                         (as-non-poly
                                          (gen-tvar #'id)
                                          poly-context))))]
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
               (syntax-case (rename expr) (: require: define-type: define: define-values: 
                                             define-type-alias define-syntax: define-syntax-rule:
                                             lambda: begin: local: letrec: let: let*: 
                                             shared: parameterize:
                                             begin: cond: case: if: when: unless:
                                             or: and: set!: trace:
                                             type-case: quote: quasiquote: time:
                                             has-type
                                             list vector values: try
                                             module+: module)
                 [(module+: name e ...)
                  (let*-values ([(datatypes dt-len opaques o-len aliases a-len
                                            variants v-len env e-len
                                            prev-macros prev-tys prev-tl-tys prev-submods)
                                 (vector->values (hash-ref submods (syntax-e #'name)
                                                           (vector datatypes
                                                                   (length datatypes)
                                                                   opaques
                                                                   (length opaques)
                                                                   aliases
                                                                   (length aliases)
                                                                   variants
                                                                   (length variants)
                                                                   env
                                                                   (length env)
                                                                   null ; macros
                                                                   null ; tys
                                                                   null ; tl-tys
                                                                   (hasheq))))]
                                [(tys env datatypes opaques aliases variants macros tl-tys next-submods)
                                 (typecheck-defns (syntax->list #'(e ...))
                                                  datatypes
                                                  opaques
                                                  aliases
                                                  env
                                                  variants
                                                  #f
                                                  poly-context
                                                  let-polys
                                                  prev-submods)])
                    (set! submods (hash-set submods (syntax-e #'name)
                                            (vector datatypes dt-len
                                                    opaques o-len
                                                    aliases a-len
                                                    variants v-len
                                                    env e-len
                                                    (append macros prev-macros)
                                                    (append tys prev-tys)
                                                    (append tl-tys prev-tl-tys)
                                                    next-submods))))]
                 [(module . _)
                  ;; can ignore
                  (void)]
                 [(define-syntax: . _)
                  ;; can ignore
                  (void)]
                 [(define-syntax-rule: . _)
                  ;; can ignore
                  (void)]
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
                  (let ([id-ids (map (lambda (id)
                                       (if (identifier? id)
                                           id
                                           (car (syntax-e id))))
                                     (syntax->list #'(id ...)))]
                        [id-types (map (lambda (id)
                                         (syntax-case id (:)
                                           [(id : type)
                                            (poly-instance (parse-type #'type))]
                                           [else (gen-tvar id)]))
                                       (syntax->list #'(id ...)))])
                    (unify! expr
                            (make-tupleof expr id-types)
                            (typecheck #'rhs env))
                    (for-each (lambda (id tvar)
                                (unify-defn! expr
                                             (lookup id env)
                                             tvar))
                              id-ids
                              id-types))]
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
                  (let-values ([(ty env datatypes opaques aliases vars macros tl-tys subs)
                                (typecheck-defns (syntax->list #'(defn ...))
                                                 datatypes
                                                 opaques
                                                 aliases
                                                 env
                                                 variants
                                                 #f
                                                 poly-context
                                                 let-polys
                                                 submods)])
                    (typecheck #'expr env))]
                 [(letrec: . _)
                  (typecheck ((make-let 'letrec) expr) env)]
                 [(let: . _)
                  (typecheck ((make-let 'let) expr) env)]
                 [(let*: . _)
                  (typecheck ((make-let 'let*) expr) env)]
                 [(shared: ([id rhs] ...) expr)
                  (let-values ([(ty env datatypes opaques aliases vars macros tl-tys subs)
                                (typecheck-defns (syntax->list #'((define: id rhs) ...))
                                                 datatypes
                                                 opaques
                                                 aliases
                                                 env
                                                 variants
                                                 #f
                                                 poly-context
                                                 let-polys submods)])
                    (typecheck #'expr env))]
                 [(parameterize: ([param rhs] ...) expr)
                  (begin
                    (for ([param (in-list (syntax->list #'(param ...)))]
                          [rhs (in-list (syntax->list #'(rhs ...)))])
                      (unify! #'param 
                              (typecheck param env)
                              (make-parameterof rhs (typecheck rhs env))))
                    (typecheck #'expr env))]
                 [(cond: [ques ans] ...)
                  (let ([res-type (gen-tvar expr)])
                    (for-each
                     (lambda (ques ans)
                       (unless (syntax-case ques (else)
                                 [else #t]
                                 [_ #f])
                         (unify! ques
                                 (make-bool ques)
                                 (typecheck ques env)))
                       (unify! ans
                               res-type
                               (typecheck ans env)))
                     (syntax->list #'(ques ...))
                     (syntax->list #'(ans ...)))
                    res-type)]
                 [(case: expr [alts ans] ...)
                  (let ([res-type (gen-tvar #'expr)])
                    (unify! #'expr
                            (let loop ([alts (syntax->list #'(alts ...))])
                              (if (null? alts)
                                  (make-sym #'expr)
                                  (syntax-case (car alts) ()
                                    [() (loop (cdr alts))]
                                    [(v . _)
                                     (number? (syntax-e #'v))
                                     (make-num #'expr)]
                                    [_ (make-sym #'expr)])))
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
                 [(quasiquote: e)
                  (check-quoted #'e (lambda (stx)
                                      (syntax-case stx (unquote unquote-splicing)
                                        [(unquote e) (unify! #'e 
                                                             (typecheck #'e env)
                                                             (make-sexp #f))]
                                        [(unquote-splicing e) (unify! #'e 
                                                                      (typecheck #'e env) 
                                                                      (make-listof #f (make-sexp #f)))])))
                  (make-sexp expr)]
                 [(time: expr)
                  (typecheck #'expr env)]
                 [(has-type expr : type)
                  (let ([t (typecheck #'expr env)]
                        [ty (parse-mono-type #'type)])
                    (unify! #'expr t ty)
                    ty)]
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
                 
                 [(id . _)
                  (and (identifier? #'id)
                       (typed-macro? (syntax-local-value #'id (lambda () #f))))
                  (typecheck (local-expand-typed expr) env)]
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
                   [(char? (syntax-e expr))
                    (make-chr expr)]
                   [(eq? (void) (syntax-e expr))
                    (void)]
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
     opaques
     aliases
     variants
     macros
     poly-def-env
     submods)))

(define-for-syntax tl-env #f)
(define-for-syntax tl-datatypes #f)
(define-for-syntax tl-opaques #f)
(define-for-syntax tl-aliases #f)
(define-for-syntax tl-variants #f)
(define-for-syntax tl-submods #f)

(define-for-syntax (do-original-typecheck tl)
  (let ([datatypes null]
        [opaques null]
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
                        [CHAR (make-chr #f)]
                        [SYM (make-sym #f)]
                        [BOOL (make-bool #f)])
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
                     (cons #'char=? (make-arrow #f 
                                                (list CHAR
                                                      CHAR)
                                                B))
                     (cons #'make-hash: (POLY a (POLY b (make-arrow #f
                                                                    (list (make-listof 
                                                                           #f
                                                                           (make-tupleof
                                                                            #f
                                                                            (list a b))))
                                                                    (make-hashof #f a b)))))
                     (cons #'hash: (POLY a (POLY b (make-arrow #f
                                                               (list (make-listof 
                                                                      #f
                                                                      (make-tupleof
                                                                       #f
                                                                       (list a b))))
                                                               (make-hashof #f a b)))))
                     (cons #'hash-ref: (POLY a (POLY b (make-arrow #f
                                                                   (list (make-hashof #f a b)
                                                                         a)
                                                                   (make-datatype #f #'optionof (list b))))))
                     (cons #'hash-set! (POLY a (POLY b (make-arrow #f
                                                                   (list (make-hashof #f a b)
                                                                         a
                                                                         b)
                                                                   (make-vd #f)))))
                     (cons #'hash-remove! (POLY a (POLY b (make-arrow #f
                                                                      (list (make-hashof #f a b)
                                                                            a)
                                                                      (make-vd #f)))))
                     (cons #'hash-set (POLY a (POLY b (make-arrow #f
                                                                  (list (make-hashof #f a b)
                                                                        a
                                                                        b)
                                                                  (make-hashof #f a b)))))
                     (cons #'hash-remove (POLY a (POLY b (make-arrow #f
                                                                     (list (make-hashof #f a b)
                                                                           a)
                                                                     (make-hashof #f a b)))))
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
                     (cons #'s-exp-boolean? (make-arrow #f 
                                                       (list (make-sexp #f))
                                                       B))
                     (cons #'s-exp->boolean (make-arrow #f 
                                                        (list (make-sexp #f))
                                                        BOOL))
                     (cons #'boolean->s-exp (make-arrow #f 
                                                        (list BOOL)
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
                     (cons #'test: (POLY a (make-arrow #f 
                                                       (list a a)
                                                       (make-vd #f))))
                     (cons #'test/exn: (POLY a (make-arrow #f 
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

                     (cons #'make-parameter (POLY a (make-arrow #f
                                                                (list a)
                                                                (make-parameterof #f a))))
                     (cons #'parameter-ref (POLY a (make-arrow #f
                                                               (list (make-parameterof #f a))
                                                               a)))
                     (cons #'parameter-set! (POLY a (make-arrow #f
                                                                (list (make-parameterof #f a)
                                                                      a)
                                                                (make-vd #f))))
                     

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
                     (cons #'string-ref (make-arrow #f
                                                    (list STR N)
                                                    CHAR))
                     (cons #'string-length (make-arrow #f
                                                       (list STR)
                                                       N))
                     (cons #'substring (make-arrow #f
                                                   (list STR N N)
                                                   STR))
                     (cons #'string->list (make-arrow #f
                                                      (list STR)
                                                      (make-listof #f CHAR)))
                     (cons #'list->string (make-arrow #f
                                                      (list (make-listof #f CHAR))
                                                      STR))
                     (cons #'none (POLY a (make-arrow #f 
                                                      (list) 
                                                      (make-datatype #f #'optionof (list a)))))
                     (cons #'some (POLY a (make-arrow #f 
                                                      (list a) 
                                                      (make-datatype #f #'optionof (list a)))))
                     (cons #'none? (POLY a (make-arrow #f 
                                                      (list
                                                       (make-datatype #f #'optionof (list a)))
                                                      B)))
                     (cons #'some? (POLY a (make-arrow #f 
                                                       (list
                                                        (make-datatype #f #'optionof (list a)))
                                                       B)))
                     (cons #'some-v (POLY a (make-arrow #f 
                                                        (list
                                                         (make-datatype #f #'optionof (list a)))
                                                        a)))
                     (cons #'pair (POLY a 
                                        (POLY b
                                              (make-arrow #f
                                                          (list a b)
                                                          (make-tupleof #f (list a b))))))
                     (cons #'fst (POLY a 
                                       (POLY b
                                             (make-arrow #f
                                                         (list (make-tupleof #f (list a b)))
                                                         a))))
                     (cons #'snd (POLY a 
                                       (POLY b
                                             (make-arrow #f
                                                         (list (make-tupleof #f (list a b)))
                                                         b))))
                     ))]
        [init-variants (list
                        (cons #'none (list))
                        (cons #'some (list (let ([a (gen-tvar #f)])
                                             (make-poly #f a a)))))])
    (typecheck-defns (expand-includes tl)
                     (append import-datatypes datatypes)
                     (append import-opaques opaques)
                     (append import-aliases aliases)
                     (append import-env init-env) 
                     (append import-variants init-variants)
                     #f
                     null
                     #f
                     (hasheq))))

(define-for-syntax import-datatypes null)
(define-for-syntax import-opaques null)
(define-for-syntax import-aliases null)
(define-for-syntax import-variants null)
(define-for-syntax import-env null)
(define-for-syntax (add-types! dts opqs als vars env)
  (set! import-datatypes (append dts import-datatypes))
  (set! import-opaques (append opqs import-opaques))
  (set! import-aliases (append als import-aliases))
  (set! import-variants (append vars import-variants))
  (set! import-env (append env import-env)))

(define-syntax (typecheck-and-provide stx)
  (let-values ([(tys e2 dts opqs als vars macros tl-types subs)
                (with-handlers ([exn:fail? (lambda (exn)
                                             (values exn #f #f #f #f #f #f null (hasheq)))])
                  (do-original-typecheck (cdr (syntax->list stx))))])
    (if (exn? tys)
        ;; There was an exception while type checking. To order
        ;; type-checking errors after expansion, push the error into
        ;; a sub-expression:
        #`(#%expression (let-syntax ([x (raise #,tys)])
                          x))
        (generate-provides tys e2 dts opqs als vars macros tl-types subs))))

(define-for-syntax (generate-provides tys e2 dts opqs als vars macros tl-types subs)
  #`(begin
      ;; Put all contracts implementations in a submodule,
      ;; so they're not loaded in a typed context:
      (module* with-contracts #f
        (begin) ; work around a bug in v6.1.1 and earlier
        (provide
         (contract-out
          #,@(map (λ (tl-thing)
                    #`[#,(car tl-thing)
                       #,(to-contract (cdr tl-thing) #f)])
                  tl-types))))
      ;; Export identifiers for untyped use as redirections to the
      ;; submodule:
      (module with-contracts-reference racket/base
        (require racket/runtime-path
                 (for-syntax racket/base))
        (define-runtime-module-path-index contracts-submod
          '(submod ".." with-contracts))
        (provide contracts-submod))
      (require (for-syntax (submod "." with-contracts-reference)))
      #,(let ([names (map (lambda (_) (gensym)) tl-types)]
              [tl-names (map car tl-types)])
          #`(begin
              (define-syntaxes #,names
                ((make-make-redirects-to-contracts contracts-submod)
                 (syntax->list (quote-syntax #,tl-names))))
              (provide #,@(for/list ([name (in-list names)]
                                     [tl-name (in-list tl-names)])
                            #`(rename-out [#,name #,tl-name])))))
      ;; Providing each binding renamed to a generated symbol doesn't
      ;; make the binding directly inaccessible, but it makes the binding
      ;; marked as "exported" for the purposes of inspector-guarded
      ;; access. (In other words, we're not trying to be as secure
      ;; as Typed Racket, snce we can rely on Racket's safety.)
      (provide
       (rename-out
        #,@(map (λ (tl-thing)
                  #`[#,(car tl-thing)
                     #,(gensym)])
                tl-types)))
      (module* plai-typed #f
        (begin-for-syntax
          (add-types!
           ;; datatypes:
           (list #,@(map (lambda (dt)
                           #`(cons (quote-syntax #,(car dt))
                                   (quote #,(cdr dt))))
                         dts))
           ;; opaques:
           (list #,@(map (lambda (dt)
                           #`(cons (quote-syntax #,(car dt))
                                   (quote-syntax #,(cdr dt))))
                         opqs))
           ;; aliases:
           (list #,@(map (lambda (a)
                           #`(list (quote-syntax #,(car a))
                                   (list #,@(map (lambda (a)
                                                   #`(quote-syntax #,a))
                                                 (cadr a)))
                                   (quote-syntax #,(caddr a))))
                         als))
           ;; variants:
           (list #,@(map (lambda (var)
                           #`(list (quote-syntax #,(car var))
                                   #,@(map (lambda (t)
                                             (to-expression t #hasheq()))
                                           (cdr var))))
                         vars))
           ;; types
           (list #,@(map (λ (tl-thing)
                           #`(cons (quote-syntax #,(car tl-thing))
                                   #,(to-expression (cdr tl-thing) #hasheq())))
                         tl-types))))
        (provide #,@(map (λ (tl-thing)
                           (car tl-thing))
                         tl-types)
                 #,@(map (λ (dt)
                           (car dt))
                         dts)
                 #,@(map (λ (opq)
                           (car opq))
                         opqs)
                 #,@macros
                 ;; datatype predicates for contracts:
                 #,@(map (lambda (dt)
                           (datum->syntax (car dt)
                                          (string->symbol (format "~a?" (syntax-e (car dt))))))
                         dts)))
      ;; Add provides to submodules, too:
      #,@(for/list ([(name vec) (in-hash subs)])
           (let-values ([(datatypes dt-len opaques o-len aliases a-len
                                    variants v-len env e-len
                                    macros tys tl-types submods)
                         (vector->values vec)])
             (define (drop l n) (reverse (list-tail (reverse l) n)))
             #`(module+ #,name
                 #,(generate-provides tys
                                      (drop env e-len)
                                      (drop datatypes dt-len)
                                      (drop opaques o-len)
                                      (drop aliases a-len)
                                      (drop variants v-len)
                                      macros
                                      (let-based-poly! tl-types)
                                      submods))))))

(define-for-syntax ((make-make-redirects-to-contracts submod-modidx) ids)
  (define redirects
    (for/list ([id (in-list ids)])
      (define (redirect stx)
        (cond
         [(identifier? stx)
          (with-syntax ([mp (collapse-module-path-index/relative
                             submod-modidx)]
                        [id (datum->syntax id (syntax-e id) stx stx)])
            #`(let ()
                (local-require (only-in mp [#,(datum->syntax #'mp (syntax-e #'id)) id]))
                id))]
         [else
          (datum->syntax stx
                         (cons (redirect (car (syntax-e stx)))
                               (cdr (syntax-e stx)))
                         stx
                         stx)]))
      redirect))
  (apply values redirects))

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
     (let ([expanded-body (syntax-case #'body (define-type:)
                            [(define-type: . _)
                             ;; Can't `local-expand' without also evaluating
                             ;; due to introduced identifiers interleaved
                             ;; in definitions;the only point of local expansion 
                             ;; is to check syntax, so just call the transformer
                             ;; directly:
                             (begin
                               (expand-define-type #'body)
                               #'body)]
                            [_
                             (local-expand #'body 'top-level null)])])
       (unless tl-env
         (let-values ([(ts e d o a vars macros tl-types subs) 
                       (do-original-typecheck (syntax->list (if orig-body
                                                                (syntax-local-introduce orig-body)
                                                                #'())))])
           (set! tl-datatypes d)
           (set! tl-opaques o)
           (set! tl-aliases a)
           (set! tl-env e)
           (set! tl-variants vars)
           (set! tl-submods subs)))
       (let-values ([(tys e2 d2 o2 a2 vars macros tl-types subs) 
                     (typecheck-defns (expand-includes (list #'body))
                                      tl-datatypes tl-opaques tl-aliases tl-env tl-variants (identifier? #'body) 
                                      null #f tl-submods)])
         (set! tl-datatypes d2)
         (set! tl-opaques o2)
         (set! tl-aliases a2)
         (set! tl-env e2)
         (set! tl-submods subs)
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

;; ----------------------------------------

(module reader syntax/module-reader
  plai-typed)

