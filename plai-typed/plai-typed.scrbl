#lang scribble/manual
@(require (for-label (only-meta-in 0 plai-typed))
          (for-syntax racket/base)
          scribble/racket)

@(define-syntax-rule (define-r r:lambda r:syntax-rules)
  (begin
   (require (for-label racket/base))
   (define-syntax r:lambda
     (make-element-id-transformer
      (lambda (stx) #'@racket[lambda])))
   (define-syntax r:syntax-rules
     (make-element-id-transformer
      (lambda (stx) #'@racket[syntax-rules])))))
@(define-r r:lambda r:syntax-rules)

@title{PLAI Typed Language}

@defmodulelang[plai-typed]

The @racketmodname[plai-typed] language is a statically typed language
that resembles the @racketmodname[plai] language, though with a
smaller set of syntactic forms and built-in functions.

The body of a @schememodname[plai-typed] module is a sequence of
definitions and expressions. Like the @racketmodname[plai] languages
the module exports all top-level definitions. When a
@racketmodname[plai-typed] module is imported into a module that does
not use @racketmodname[plai-typed], the imports have contracts
(matching reflecting the exported bindings' types).

@; --------------------------------------------------

@section{Definitions}

In a @scheme[define-type] declaration, the contract/predicate position
for variant fields changes to a colon followed by a type. In addition,
@scheme[define] and @scheme[lambda] forms support type annotations on
arguments and for results. The syntax is otherwise merely restricted
from the normal PLAI language.

@defform*/subs[#:literals (:)
               [(define id expr)
                (define id : type expr)
                (define (id id/type ...) expr)
                (define (id id/type ...) : type expr)]
               ([id/type id
                         [id : type]])]{

The definition form with optional type annotations. A type written
after @scheme[(id id/type ...)] declares the result type of a
function.}

@defform/subs[#:literals (:)
              (define-values (id/type ...) expr)
              ([id/type id
                        [id : type]])]{

Matches multiple results produced (via @scheme[values]) by
@scheme[expr].}


@defform/subs[#:literals (: quote)
              (define-type tyid/abs
                [variant-id (field-id : type)]
                ...)
              ([tyid/abs id
                         (id '@#,racket[_arg-id] ...)])]{

Defines a type (when @scheme[tyid/abs] is @scheme[id]) or type
constructor (when @scheme[tyid/abs] has the form @scheme[(id 'id
...)]) with its variants.}

@defform/subs[#:literals (quote)
              (define-type-alias tyid/abs type)
              ([tyid/abs id
                         (id '@#,racket[_arg-id] ...)])]{
Defines a type alias @racket[id]. Then @racket[tyid/abs] is
@racket[id], then using @racket[id] is the same as using
@racket[type]. When @racket[tyid/abs] is @racket[(id '@#,racket[_arg-id] ...)], then
using @racket[(id _arg-type ...)] is the same as using @racket[type]
with each @racket['@#,racket[_arg-id]] replaced by the corresponding @racket[_arg-type].}


@defform/subs[#:literals (typed-in opaque-type-in rename-in :)
              (require spec ...)
              ([spec module-path
                     (typed-in module-path [id : type] ...)
                     (opaque-type-in module-path [type-id predicate-id] ...)
                     (rename-in spec [orig-id new-id] ...)])]{
Imports from each @racket[module-path].

When a @racket[module-path] is not wrapped with @racket[typed-in] or @racket[opaque-type-in], then
@racket[module-path] must refer to a module that is implemented with
@racketmodname[plai-typed].

When @racket[module-path] is wrapped with @racket[typed-in], then only the
specified @racket[id]s are imported from @racket[module-path], and the
type system assumes (without static or additional dynamic checks) the
given @racket[type] for each @racket[id].

When @racket[module-path] is wrapped with @racket[opaque-type-in],
then the corresponding @racket[type-id]s are bound as opaque
datatypes, where @racket[predicate-id] from @racket[module-path] is a
run-time predicate (used for contracts as needed for cooperation with
untyped code) for instances of the datatype.}


@defform[(trace id ...)]{

Traces subsequent calls---showing arguments and results---for
functions bound to the @racket[id]s.  This form can be used only in a
module top level, and only for tracing functions defined within the
module.}


@defform[(module+ id form ...)]{

Declares/extends a submodule named @racket[id], which is particularly
useful for defining a @racketidfont{test} submodule to hold tests that
precede relevant definitions (since the submodule implicitly imports
the bindings of its enclosing module, and DrRacket or @exec{raco test}
runs the @racketidfont{test} submodule):

@racketblock[
 (module+ test
   (test 11 (add-one 10)))

 (define (add-one n)
   (+ 1 n))
]}

@defform[(include path-spec)]{

Copy the content of @racket[path-spec] in place of the @racket[include]
form, which can only be used in a top-level position.}

@deftogether[(
@defform[(define-syntax-rule (id pattern ...) template)]
@defform*/subs[#:literals (r:syntax-rules r:lambda)
               [(define-syntax id macro-expr)
                (define-syntax (id arg-id) macro-body ...)]
               ([macro (r:syntax-rules ....)
                       (r:lambda ....)])]
)]{
Defines a macro. In a @racket[macro-expr] or @racket[macro-body], the bindings of
@racketmodname[racket/base] are available.

A macro of the form

@racketblock[
(define-syntax-rule (id pattern ...) template)
]

is equivalent to

@racketblock[
(define-syntax id
  (r:syntax-rules ()
   [(id pattern ...) template]))
]}


@defform[(splice form ...)]{

Equivalent to the @racket[form]s sequence in a module or top-level context,
which is useful for producing multiple definitions from a macro.}

@; ----------------------------------------

@section{Expressions}

An expression can be a literal constant that is a number (type
@scheme[number]), a string (type @scheme[string]), a symbol (type
@scheme[symbol]) written with @scheme[quote] or @litchar{'},
an S-expression (type
@scheme[s-expression]) also written with @scheme[quote] or @litchar{'},
@scheme[#t] (type @scheme[boolean]), @scheme[#f] (type
@scheme[boolean]), or a character (type @scheme[char]). 
An expression also can be a bound identifier (in
which case its type comes from its binding).

@defthing[true boolean]
@defthing[false boolean]

@defform/subs[(quote s-exp)
              ([s-exp id
                      number
                      string
                      (s-exp ...)])]{
A symbol (when the @racket[s-exp] is an identifier) or a literal S-expression.}

@deftogether[(
@defform/subs[#:literals (unquote unquote-splicing quasiquote)
              (quasiquote qq-form)
              ([qq-form id
                        number
                        string
                        (qq-form ...)
                        (#,(racket unquote) expr)
                        (#,(racket unquote-splicing) expr)
                        (#,(racket quasiquote) expr)])]
@defidform[unquote]
@defidform[unquote-splicing]
)]{
An S-expression with escapes. An @racket[id] (to generate a symbol
S-expression) in a @racket[qq-form] must not be @racket[unquote],
@racket[unquote-splicing], or @racket[quasiquote].}

@defform[(#%app expr expr ...)]{

A function call, which is normally written without the @scheme[#%app]
keyword.}

@defform*/subs[#:literals (:)
               [(lambda (id/ty ...) expr)
                (lambda (id/ty ...) : type expr)]
               ([id/ty id
                       [id : type]])]{

A procedure. When a type is written after @scheme[(id/ty ...)], it
declares he result type of the function.}

@defidform[λ]{An alias for @racket[lambda].}

@deftogether[(
@defform[(if test-expr expr expr)]
@defform*[#:literals (else)
          [(cond [test-expr expr] ...)
           (cond [test-expr expr] ... [else expr])]]
)]{

Conditionals. Each @scheme[test-expr] must have type @scheme[boolean].}

@defform*[#:literals (else)
          [(case expr [(id ...) expr] ...)
           (case expr [(id ...) expr] ... [else expr])]]{

Case dispatch on symbols.}

@defform[(begin expr ...+)]{Sequence.}

@deftogether[(
@defform[(when test-expr expr ...+)]
@defform[(unless test-expr expr ...+)]
)]{Conditional sequence.}

@deftogether[(
@defform[(local [definition ...] expr)]
@defform[(letrec ([id expr] ...) expr)]
@defform[(let ([id expr] ...) expr)]
@defform[(let* ([id expr] ...) expr)]
)]{

Local binding.}

@defform[(shared ([id expr] ...) expr)]{
Cyclic- and shared-structure binding.}


@defform[(parameterize ([param-expr val-expr] ...) expr)]{
Dynamic binding. Each @racket[param-expr] must have type @racket[(parameterof _type)]
where the corresponding @racket[val-expr] has type @racket[_type].}


@defform[(set! id expr)]{

Assignment.}

@deftogether[(
@defform[(and expr ...)]
@defform[(or expr ...)]
)]{

Boolean combination. The @scheme[expr]s must have type @scheme[boolean].}

@defform[(list elem ...)]{

Builds a list. All @scheme[elem]s must have the same type.}

@defform[(vector elem ...)]{

Builds a vector. All @scheme[elem]s must have the same type.}


@defform[(values elem ...)]{

Combines multiple values into one; the type of each @scheme[elem] is
independent. Match a @scheme[values] result using
@scheme[define-values].}

@defform*/subs[#:literals (quote else)
               [(type-case tyid/abs val-expr
                  [variant-id (field-id ...) expr] ...)
                (type-case tyid/abs val-expr
                  [variant-id (field-id ...) expr] ...
                  [else expr])]
               ([tyid/abs id
                          (id type ...)])]{

Variant dispatch, where @scheme[val-expr] must have type
@scheme[tyid/abs].}

@defform[#:literals (lambda)
         (try expr (lambda () handle-expr))]{

Either returns @scheme[expr]'s result or catches an exception raised
by @scheme[expr] and calls @scheme[handle-expr].}


@deftogether[(
@defthing[empty (listof 'a)]
@defthing[cons ('a (listof 'a) -> (listof 'a))]
@defthing[first ((listof 'a) -> 'a)]
@defthing[rest ((listof 'a) -> (listof 'a))]
@defthing[empty? ((listof 'a) -> boolean)]
@defthing[cons? ((listof 'a) -> boolean)]
@defthing[second ((listof 'a) -> 'a)]
@defthing[third ((listof 'a) -> 'a)]
@defthing[fourth ((listof 'a) -> 'a)]
@defthing[list-ref ((listof 'a) number -> 'a)]
@defthing[length ((listof 'a) -> number)]
@defthing[reverse ((listof 'a) -> (listof 'a))]
@defthing[member ('a (listof 'a) -> boolean)]
@defthing[map (('a -> 'b) (listof 'a) -> (listof 'b))]
@defthing[map2 (('a 'b -> 'c) (listof 'a) (listof 'b) -> (listof 'c))]
@defthing[filter (('a -> boolean) (listof 'a) -> (listof 'a))]
@defthing[foldl (('a 'b -> 'b) 'b (listof 'a) -> 'b)]
@defthing[foldr (('a 'b -> 'b) 'b (listof 'a) -> 'b)]
@defthing[build-list (number (number -> 'a) -> (listof 'a))]
)]{List primitives.}
 

@defthing[not (boolean -> boolean)]{Boolean primitive.}

@deftogether[(
@defthing[+ (number number -> number)]
@defthing[- (number number -> number)]
@defthing[* (number number -> number)]
@defthing[/ (number number -> number)]
@defthing[quotient (number number -> number)]
@defthing[remainder (number number -> number)]
@defthing[= (number number -> boolean)]
@defthing[> (number number -> boolean)]
@defthing[< (number number -> boolean)]
@defthing[>= (number number -> boolean)]
@defthing[<= (number number -> boolean)]
@defthing[min (number number -> number)]
@defthing[max (number number -> number)]
@defthing[floor (number -> number)]
@defthing[ceiling (number -> number)]
@defthing[add1 (number -> number)]
@defthing[sub1 (number -> number)]
@defthing[zero? (number -> boolean)]
@defthing[odd? (number -> boolean)]
@defthing[even? (number -> boolean)]
)]{Numeric primitives.}

@defthing[symbol=? (symbol symbol -> boolean)]{
Symbol primitive.}

@deftogether[(
@defthing[string=? (string string -> boolean)]
@defthing[string-append (string string -> string)]
@defthing[to-string ('a -> string)]
@defthing[string->symbol (string -> symbol)]
@defthing[symbol->string (symbol -> string)]
@defthing[string-ref (string number -> char)]
)]{String primitives.}

@deftogether[(
@defthing[char=? (char char -> boolean)]
@defthing[string->list (string -> (listof char))]
@defthing[list->string ((listof char) -> string)]
)]{Character primitives.}

@deftogether[(
@defthing[s-exp-symbol? (s-expression -> boolean)]
@defthing[s-exp->symbol (s-expression -> symbol)]
@defthing[symbol->s-exp (symbol -> s-expression)]
@defthing[s-exp-number? (s-expression -> boolean)]
@defthing[s-exp->number (s-expression -> number)]
@defthing[number->s-exp (number -> s-expression)]
@defthing[s-exp-string? (s-expression -> boolean)]
@defthing[s-exp->string (s-expression -> string)]
@defthing[string->s-exp (string -> s-expression)]
@defthing[s-exp-list? (s-expression -> boolean)]
@defthing[s-exp->list (s-expression -> (listof s-expression))]
@defthing[list->s-exp ((listof s-expression) -> s-expression)]
)]{
Coercion primitives to and from S-expressions.

The @racket[s-exp-symbol?] function determines whether an S-expression
is a symbol; in that case, @racket[s-exp->symbol] acts the identity
function to produce the symbol, otherwise an exception is raised. The
@racket[symbol->s-exp] function similarly acts as the identity
function to view a symbol as an S-expression.

The other functions work similarly for numbers, strings, and lists of
S-expressions.}

@defthing[identity ('a -> 'a)]{Identity primitive.}

@deftogether[(
@defthing[equal? ('a 'a -> boolean)]
@defthing[eq? ('a 'a -> boolean)]
)]{Comparison primitives.}

@defthing[error (symbol string -> 'a)]{Error primitive.}

@defthing[display ('a -> void)]{Output primitive.}

@defthing[read (-> s-expression)]{Input primitive.}

@deftogether[(
@defthing[box ('a -> (boxof 'a))]
@defthing[unbox ((boxof 'a) -> 'a)]
@defthing[set-box! ((boxof 'a) 'a -> void)]
)]{Box primitives.}

@deftogether[(
@defthing[make-vector (number 'a -> (vectorof 'a))]
@defthing[vector-ref ((vectorof 'a) number -> 'a)]
@defthing[vector-set! ((vectorof 'a) number 'a -> void)]
@defthing[vector-length ((vectorof 'a) -> number)]
)]{Vector primitives.}

@deftogether[(
@defthing[make-parameter ('a -> (parameterof 'a))]
@defthing[parameter-ref ((parameterof 'a) -> 'a)]
@defthing[parameter-set! ((parameterof 'a) 'a -> void)]
)]{Parameter primitives.}

@deftogether[(
@defthing[make-hash ((listof ('a * 'b)) -> (hashof 'a 'b))]
@defthing[hash ((listof ('a * 'b)) -> (hashof 'a 'b))]
@defthing[hash-ref ((hashof 'a 'b) 'a -> (optionof 'b))]
@defthing[hash-set! ((hashof 'a 'b) 'a 'b -> void)]
@defthing[hash-remove! ((hashof 'a 'b) 'a -> void)]
@defthing[hash-set ((hashof 'a 'b) 'a 'b -> (hashof 'a 'b))]
@defthing[hash-remove ((hashof 'a 'b) 'a -> (hashof 'a 'b))]
@defthing[hash-keys ((hashof 'a 'b) -> (listof 'a))]
)]{Hash table primitives. The @racket[make-hash] function
creates a mutable hash table for use with @racket[hash-set!]
and @racket[hash-remove!], while the @racket[hash] function
creates an immutable hash table for use with @racket[hash-set]
and @racket[hash-remove]. The @racket[hash-ref] and @racket[hash-keys]
functions work on both mutable and immutable hash tables.}

@deftogether[(
@defthing[none (-> (optionof 'a))]
@defthing[some ('a -> (optionof 'a))]
@defthing[some-v ((optionof 'a) -> 'a)]
@defthing[none? ((optionof 'a) -> bool)]
@defthing[some? ((optionof 'a) -> bool)]
)]{
Option constructors, selector, and predicates. See @racket[optionof].}

@defthing[call/cc ((('a -> 'b) -> 'a) -> 'a)]{
Continuation primitive.}

@deftogether[(
@defform[(test expr expr)]
@defform[(test/exn expr string-expr)]
@defthing[print-only-errors (boolean -> void)]
)]{
Test primitive forms.  The @racket[test] and @racket[test/exn] forms
have type @racket[void], although they do not actually produce a void
value; instead, they produce results suitable for automatic display
through a top-level expression, and the @scheme[void] type merely
prevents your program from using the result.

See also @racket[module+].}

@defform[(time expr)]{
Shows the time taken to evaluate @racket[expr].}

@; ----------------------------------------

@section{Types}

@deftogether[(
@defidform[number]
@defidform[boolean]
@defidform[symbol]
@defidform[string]
@defidform[char]
@defidform[s-expression]
@defidform[void]
)]{Primitive types.

The @racket[void] identifier also works as an expression of type
@racket[(-> void)].}

@defform[#:id -> (type ... -> type)]{

Types for functions.}

@defform/none[#:literals (*) (type * ...+)]{

Types for tuples.}

@defform/none[()]{

Type for the empty tuple.}


@defform[(listof type)]{Types for lists of elements.}
@defform[(boxof type)]{Types for mutable boxes.}
@defform[(vectorof type)]{Types for vectors of elements.}
@defform[(parameterof type)]{Types for parameters.}
@defform[(hashof type type)]{Types for hash tables.}

@defform[(optionof type)]{Defined as
@racketblock[
(define-type (optionof 'a)
  [none]
  [some (v : 'a)])
]
and used, for example, for the result of @racket[hash-ref].}

@; ----------------------------------------

@section{Syntactic Literals}

@deftogether[(
@defidform[typed-in]
@defidform[opaque-type-in]
@defidform[:]
)]{

Syntactic literals for use in declarations such as @racket[define] and
@racket[require]; see @racket[define] and @racket[require] for more
information.}

@; ----------------------------------------

@section{Type Checking and Inference}

Type checking and inference is just as in ML (Hindley-Milner), with
a few small exceptions:

@itemize[

 @item{Functions can take multiple arguments, instead of requring a tuple
   of arguments. Thus, @scheme[(number number -> number)] is a different type
   than either @scheme[((number * number) -> number)], which is the tuple
   variant, or @scheme[(number -> (number -> number))], which is the curried
   variant.}

 @item{Since all top-level definitions are in the same
   mutually-recursive scope, the type of a definition's right-hand
   side is not directly unified with references to the defined
   identifier on the right-hand side. Instead, every reference to an
   identifier---even a reference in the identifier's definition---is
   unified with a instantiation of a polymorphic type inferred for the
   definition. Of course, the usual value restriction applies for
   inferring polymorphic types.

   Compare OCaml:

@verbatim[#:indent 2]{
       # let rec f = fun x -> x
             and h = fun y -> f 0 
             and g = fun z -> f "x";;
       This expression has type string but is here used with type int
}

    with

@verbatim[#:indent 2]{
       (define (f x) x)
       (define (h y) (f 0))
       (define (g y) (f "x"))
       ; f : ('a -> 'a)
       ; h : ('a -> number)
       ; g : ('a -> string)
}

   A minor consequence is that polymorphic recursion (i.e., a self
   call with an argument whose type is different than that for the
   current call) is allowed. Recursive types, however, are prohibited.}

 @item{Variables are mutable when @racket[set!] is used, but
  assignment via @racket[set!] is disallowed to a variable that has a
  polymorphic type.}

 @item{Since all definitions are recursively bound, and since the
   right-hand side of a definition does not have to be a function, its
   possible to refer to a variable before it is defined. The type
   system does not prevent ``reference to identifier before
   definition'' errors.}

 @item{Type variables are always scoped locally within a type expression.

   Compare OCaml:

@verbatim[#:indent 2]{
        # function (x : 'a), (y : 'a) -> x;;
        - : 'a * 'a -> 'a = <fun>
}

   with

@verbatim[#:indent 2]{
        > (lambda ((x : 'a) (y : 'a)) x)
        - ('a 'b -> 'a)
       
        > (define f : ('a 'a -> 'a) (lambda (x y) x))
        > f
        - ('a 'a -> 'a)
}}

]

When typechecking fails, the error messages reports and highlights (in
pink) all of the expressions whose type contributed to the
failure. That's often too much information. As usual, explicit type
annotations can help focus the error message.
