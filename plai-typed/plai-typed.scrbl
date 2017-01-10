#lang scribble/manual
@(require (for-label (only-meta-in 0 plai-typed))
          (for-syntax racket/base)
          scribble/racket
          scribble/example)

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

@(define demo (make-base-eval #:lang 'plai-typed))

@(begin
  (define-syntax-rule (define-racket-shared racket-shared)
    (begin
     (require (for-label (only-in racket/shared shared)))
     (define racket-shared @racket[shared])))
  (define-racket-shared racket-shared))

@title{PLAI Typed Language}

@defmodulelang[plai-typed]

The @racketmodname[plai-typed] language syntactically resembles the
@racketmodname[plai] language, which is based on
@racketmodname[scheme], but the type system is close to that of
@hyperlink["http://smlnj.org/"]{ML}.

@table-of-contents[]

@; --------------------------------------------------

@section{Definitions}

The body of a @schememodname[plai-typed] module is a sequence of
definitions and expressions, and the module implicitly exports all
top-level definitions. When a @racketmodname[plai-typed] module is
imported into a module that does not use @racketmodname[plai-typed],
the imports have contracts (matching reflecting the exported bindings'
types).

@defform*/subs[#:literals (:)
               [(define id expr)
                (define id : type expr)
                (define (id id/type ...) expr)
                (define (id id/type ...) : type expr)]
               ([id/type id
                         [id : type]])]{

Defines @racket[id].

The @racket[expr] in each of the first two forms is evaluated to get
the value of @racket[id]. In the first form, the type of @racket[id]
is inferred at the type of @racket[expr], while the second form
declares a specific type for @racket[id].

The third and fourth forms define @racket[id] as a function, where
each @racket[id/type] is a function argument (with an optional declare
type) and @racket[expr] is the body of the function, which is
evaluated when the function is called. In the fourth form, a
@racket[type] before the body @racket[expr] declares the function's
result type (which must match the type of @racket[expr]).

Note that the first and second forms of @racket[define] also define
functions in the case that @racket[expr] produces a function, such as
when @racket[expr] is a @racket[lambda] form. The third and fourth
forms are simplify shorthands for defining a function.

Evaluating a reference to @racket[id] before its definition is
evaluated triggers an ``undefined identifier'' error.

@examples[#:eval demo
(define a 1)
a
(define b : number (+ 1 2))
b
(define (c x)
  (+ x b))
(c 3)
(define (d [y : number]) : number
  (c y))
(d 4)
]}


@defform/subs[#:literals (:)
              (define-values (id/type ...) expr)
              ([id/type id
                        [id : type]])]{

Defines each @racket[id/type] (with an optional type declaration) to
be the values within the @tech{tuple} produced by @racket[expr], which
must have as many values as declared @racket[id/type]s.

@examples[#:eval demo
(define t (values 1 'one "One"))
(define-values (a b c) t)
a
(define-values ([x : number] [b : symbol] [c : string]) t)
c]}


@defform/subs[#:literals (: quote)
              (define-type tyid/abs
                [variant-id (field-id : type)]
                ...)
              ([tyid/abs id
                         (id '@#,racket[_arg-id] ...)])]{

Defines a type (when @racket[tyid/abs] is @racket[id]) or type
constructor (when @racket[tyid/abs] has the form @racket[(id 'id
...)]).

A constructor @racket[variant-id] is defined for each variant. Each
constructor takes an argument for each field of its variant, where the
type of each field is declared by the @racket[type] after each
@racket[field-id]. The result type of each constructor is
@racket[id].

Instances of a type declared with @racket[define-type] are normally
used through @racket[type-case].

In addition to the type and constructors, a @racket[define-type]
expression also defines:

@itemize[

  @item{for each variant, a predicate
    @racketidfont{@racket[variant-id]?} that returns @racket[#t] when
    applied to an instance of @racket[variant-id] and @racket[#f] for
    any other value; and}

  @item{for each field of each variant, an accessor
    @racketidfont{@racket[variant-id]-@racket[field-id]} that takes a
    instance of @racket[variant-id] and returns the value of the field
    corresponding to @racket[field-id].}
]

@examples[#:eval demo
(define-type Shape
  [circle (radius : number)]
  [rectangle (width : number)
             (height : number)])
(define c (circle 10))
c
(circle? c)
(circle-radius c)
(define r (rectangle 2 3))
(+ (rectangle-width r) (rectangle-height r))
]}

@defform/subs[#:literals (quote)
              (define-type-alias tyid/abs type)
              ([tyid/abs id
                         (id '@#,racket[_arg-id] ...)])]{

Defines a type alias @racket[id]. When @racket[tyid/abs] is
@racket[id], then using @racket[id] is the same as using
@racket[type]. When @racket[tyid/abs] is @racket[(id '@#,racket[_arg-id] ...)], then
using @racket[(id _arg-type ...)] is the same as using @racket[type]
with each @racket['@#,racket[_arg-id]] replaced by the corresponding @racket[_arg-type].

@examples[#:eval demo
(define-type-alias size number)
(define (square-area [side : size])
  (* side side))
(square-area 10)
]}


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
untyped code) for instances of the datatype.

@examples[#:eval demo
(require (typed-in racket/base [gensym : (-> symbol)]))
(gensym)]}


@defform[(trace id ...)]{

Traces subsequent calls---showing arguments and results---for
functions bound to the @racket[id]s.  This form can be used only in a
module top level, and only for tracing functions defined within the
module.}


@defform[(module id module-path form ...)]{

Declares a submodule named @racket[id], which can be required in the
enclosing module using @racket['@#,racket[id]] or @racket[(submod "."
id)]:

@racketblock[
 (module sub plai-typed
   (define n 8))
 (require 'sub)
 (+ n 1)
]}


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
@racket[number]), a string (type @racket[string]), a symbol (type
@racket[symbol]) written with @racket[quote] or @litchar{'},
an @tech{S-expression} (type
@racket[s-expression]) also written with @racket[quote] or @litchar{'},
@racket[#t] (type @racket[boolean]), @racket[#f] (type
@racket[boolean]), or a character (type @racket[char]). 
An expression also can be a bound identifier (in
which case its type comes from its binding).

@defform[#:literals (:)
         (has-type expr : type)]{

Equivalent to @racket[expr], but declares/asserts that @racket[expr]
has type @racket[type].

@examples[#:eval demo
(has-type 1 : number)
(eval:error (has-type "a" : number))]}

@defform/subs[(quote s-exp)
              ([s-exp id
                      number
                      string
                      boolean
                      (s-exp ...)])]{

The @racket[quote] form produces a symbol when @racket[s-exp] is an
identifier, otherwise it produces a literal @tech{S-expression}.

The @racket[quote] form is usually written as just a @litchar{'}
before @racket[s-exp]; that is, @racket['@#,racket[s-exp]] and
@racket[(@#,racket[quote] s-exp)] are equivalent.

Note that @racket[(quasiquote @#,racket[_id])] or
@racket[(@#,racket[quasiquote] _id)] produces a literal @tech{S-expression}
with symbol content, as opposed to producing a value of type
@racket[symbol].

@examples[#:eval demo
'a
'(a b c)
`a]}


@deftogether[(
@defform/subs[#:literals (unquote unquote-splicing quasiquote)
              (quasiquote qq-form)
              ([qq-form id
                        number
                        string
                        boolean
                        (qq-form ...)
                        (#,(racket unquote) expr)
                        (#,(racket unquote-splicing) expr)
                        (#,(racket quasiquote) expr)])]
@defidform[unquote]
@defidform[unquote-splicing]
)]{

An @tech{S-expression} that supports escapes via @racket[unquote] and
@racket[unquote-splicing]. A @racket[(@#,racket[unquote] expr)] form
is replaced with the value of @racket[expr], while a
@racket[(@#,racket[unquote-splicing] expr)] form requires that
@racket[expr] produces a list and is replaced by the list content as
an inlined sequence of S-expressions.

The @racket[unquote] form is usually written as just a @litchar{,}
before @racket[expr]; that is, @racket[, @#,racket[expr]] and
@racket[(@#,racket[unquote] expr)] are equivalent.

The @racket[unquote-splicing] form is usually written as just a
@litchar[",@"] before @racket[expr]; that is, @racket[,@@#,racket[expr]]
and @racket[(@#,racket[unquote-splicing] expr)] are equivalent.

With a nested @racket[quasiquote] form, escapes are preserved while
escaping to the enclosing level of quotation. For example,
@racket[``(,(+ 1 2))] is equivalent to @racket['(@#,racket[quasiquote] (@#,racket[unquote] 1))]
where the @racket[quasiquote] and @racket[unquote] symbols are preserved
in the result S-expression.

An @racket[id] (to generate a symbol
S-expression) in a @racket[qq-form] must not be @racket[unquote],
@racket[unquote-splicing], or @racket[quasiquote].

@examples[#:eval demo
`(+ ,(number->s-exp (+ 1 2)) 3)
`(+ ,@(list '1 '2) 3)
]}


@defform[(#%app expr expr ...)]{

A function call, which is normally written without the @racket[#%app]
keyword.

@examples[#:eval demo
(add1 1)]}


@defform*/subs[#:literals (:)
               [(lambda (id/ty ...) expr)
                (lambda (id/ty ...) : type expr)]
               ([id/ty id
                       [id : type]])]{

An anonymous function which takes as many argument as specified
@racket[id/ty]s and produces the result of @racket[expr]. Each
argument has an optional type specification, and when a type is
written after @racket[(id/ty ...)], it declares the result type of the
function.

@examples[#:eval demo
(lambda (x) (+ x 1))
(lambda ([x : number]) (+ x 1))
((lambda (x) (+ x 1)) 3)
(map (lambda (x) (+ x 1)) (list 1 2 3))]}


@defidform[λ]{An alias for @racket[lambda].}

@deftogether[(
@defform[(if test-expr expr expr)]
@defform*[#:literals (else)
          [(cond [test-expr expr] ...)
           (cond [test-expr expr] ... [else expr])]]
)]{

An @racket[if] form produces the value of the first @racket[expr] if
@racket[test-expr] produces true or the value of the second
@racket[expr] otherwise. Only one of the two @racket[expr]s is
evaluated.

A @racket[cond] form produces the value of the first @racket[expr]
whose @racket[test-expr] produces true. The @racket[test-expr]s are
tried in order until a true result is found, and at most one of the
@racket[expr]s is evaluated. If no @racket[test-expr] produces a true
result, a ``no matching clause`` exception is raised. An @racket[else]
in place of the last @racket[test-expr] is equivalent to
@racket[true].

Each @racket[test-expr] must have type @racket[boolean].

@examples[#:eval demo
(if (< 1 2)
    'less
    'greater-or-equal)
(cond
 [(< 2 1) 'bad]
 [(< 2 2) (begin (/ 1 0) 'oops)]
 [(< 2 3) 'ok]
 [(< 2 (/ 1 0)) 'oops]
 [else (begin (/ 1 0) 'oops)])]}


@defform*[#:literals (else)
          [(case val-expr [(id-or-number ...) expr] ...)
           (case val-expr [(id-or-number ...) expr] ... [else expr])]]{

Performs a case dispatch on a symbol or number. The value of the
@racket[case] form is the value of an @racket[expr] whose
@racket[(id-or-number ...)] sequence includes the result of
@racket[val-expr], when symbols are matched to identifiers. If no
@racket[id-or-number] matches, a ``no matching clause`` exception is
raised.

The dispatching mode, symbol or number, is inferred from the
@racket[id-or-number]s, which must all be symbols or numbers for a
given use of @racket[case]. If no clause provides a number or symbol,
then symbol dispatch is inferred.

@examples[#:eval demo
(case (+ 1 2)
  [(0 1 2) 'too-small]
  [(3) 'ok]
  [else 'other])
(case 'goodbye
  [(hello) 'hi]
  [(goodbye) 'bye])
]}
 

@defform[(begin expr ...+)]{

Evaluates the @racket[expr]s in sequence, producing the result of the last @racket[expr].

@examples[#:eval demo
(+ (begin
    (display "hi\n")
    1)
   (begin
    (display "bye\n")
    2))
]}

@deftogether[(
@defform[(when test-expr expr ...+)]
@defform[(unless test-expr expr ...+)]
)]{

Conditionally evaluates @racket[expr]s for their side effects, always
returning @racket[(void)]. A @racket[when] form evaluates its
@racket[expr]s only @racket[test-expr] produces true, while an
@racket[unless] form evaluates its @racket[expr]s only
@racket[test-expr] produces false.

@examples[#:eval demo
(when (< 1 2) (display "yes"))
(unless (< 1 2) (display "no"))
]}


@deftogether[(
@defform[(local [definition ...] expr)]
@defform[(letrec ([id rhs-expr] ...) expr)]
@defform[(let ([id rhs-expr] ...) expr)]
@defform[(let* ([id rhs-expr] ...) expr)]
)]{

Local binding forms. The @racket[local] form accommodates multiple
definitions that visible only among the definitions and the body
@racket[expr]. The @racket[letrec], @racket[let], and @racket[let*]
forms bind each @racket[id] to the value of the corresponding
@racket[rhs-expr] (where the @racket[rhs-expr]s are evaluated in
order). In the case of @racket[letrec], each @racket[id] is visible to
every @racket[rhs-expr] as well as in the body @racket[expr]. In the
case of @racket[let], each @racket[id] is visible only in the body
@racket[expr]. In the case of @racket[letrec], each @racket[id] is
visible only to later @racket[rhs-expr]s as well as in the body
@racket[expr].

@examples[#:eval demo
(local [(define (add-x y) (+ x y))
        (define x 2)]
  (add-x 3))
(eval:error add-x)
(letrec ([add-x (lambda (y) (+ x y))]
         [x 2])
  (add-x 3))
(let ([x 1])
  (let ([add-x (lambda (y) (+ x y))]
        [x 2])
   (add-x 3)))
(let ([x 1])
  (let* ([add-x (lambda (y) (+ x y))]
         [x 2])
   (add-x 3)))   
(let ([x 1])
  (let* ([x 2]
         [add-x (lambda (y) (+ x y))])
   (add-x 3)))]}


@defform[(shared ([id expr] ...) expr)]{

Creates cyclic data for a restricted set of restricted @racket[expr]
patterns. See @racket-shared from @racketmodname[racket/shared] for a
description of allowed patterns, besides the additional restriction
that the form must be typed.

@examples[#:eval demo
(shared ([infinite-ones (cons 1 infinite-ones)])
  (list-ref infinite-ones 1001))
]}


@defform[(parameterize ([param-expr val-expr] ...) expr)]{

The @racket[parameterize] form implements a kind of dynamic binding.
Each @racket[param-expr] must have type @racket[(parameterof _type)]
where the corresponding @racket[val-expr] has type @racket[_type], and
the parameter produces by @racket[param-expr] is set to
@racket[val-expr] for the dynamic extent of @racket[expr].

@examples[#:eval demo
(define current-mode (make-parameter 'straight))
(define (display-line)
  (display (case (parameter-ref current-mode)
            [(straight) "---"]
            [(curvy) "~~~"])))
(parameterize ([current-mode 'curvy])
  (display-line))
(display-line)
(define f
  (parameterize ([current-mode 'curvy])
    (lambda () (display-line))))
(f)
]}            


@defform[(set! id expr)]{

Mutates @racket[id] to have the value of @racket[expr].

@examples[#:eval demo
(define x 1)
(set! x (+ 1 1))
x]}


@deftogether[(
@defform[(and expr ...)]
@defform[(or expr ...)]
)]{

Boolean combinations with short-circuiting: as soon as an
@racket[expr] produces false in @racket[and] or true in @racket[or],
the remaining @racket[expr]s are not evaluated. The value of
@racket[(and)] is true and @racket[(or)] is false. The @racket[expr]s
must have type @racket[boolean].


@examples[#:eval demo
(and (< 1 2) (< 3 4))
(and (< 2 1) (< 3 (/ 1 0)))
(or (< 2 1) (< 3 4))
(or (< 2 1) (< 1 2) (< 3 (/ 1 0)))
]}


@defform[(list elem ...)]{

Builds a list. All @racket[elem]s must have the same type.

@examples[#:eval demo
(list 1 2 3)
(list "a" "b")
(list (list 1 2) empty (list 3 4))
]}


@defform[(vector elem ...)]{

Builds a vector. All @racket[elem]s must have the same type.

@examples[#:eval demo
(vector 1 2 3)
(vector "a" "b")
(vector (list 1 2) empty (list 3 4))
]}


@defform[(values elem ...)]{

Combines multiple values into @deftech{tuple}, where a tuple
containing one value is equivalent to the value. Match a @tech{tuple}
result using @racket[define-values].

The type of each @racket[elem] is independent.

@examples[#:eval demo
(values 1 'two "three")
]}


@defform*/subs[#:literals (quote else)
               [(type-case tyid/abs val-expr
                  [variant-id (field-id ...) expr] ...)
                (type-case tyid/abs val-expr
                  [variant-id (field-id ...) expr] ...
                  [else expr])]
               ([tyid/abs id
                          (id type ...)])]{

Dispatches based on the variant of the result of @racket[val-expr],
where @racket[val-expr] must have type @racket[tyid/abs], and
@racket[tyid/abs] must refer to a type defined via
@racket[define-type]. The result is the value of @racket[expr] for the
@racket[variant-id] that is instantiated by @racket[val-expr] or the
@racket[expr] in an @racket[else] clause if no @racket[variant-id]
matches. Each @racket[field-id] is bound to a corresponding (by
position) value of a field within the variant instance for use in the
same clause's @racket[expr].

The number of @racket[field-id]s must match the number of fields
declared for @racket[variant-id] in the definition of
@racket[tyid/abs], and either every @racket[variant-id] of
@racket[tyid/abs] must have a clause in the @racket[type-case] form
or an @racket[else] clause must be present.

@examples[#:eval demo
(define-type Shape
  [circle (radius : number)]
  [rectangle (width : number)
             (height : number)])
(define (area [s : Shape])
  (type-case Shape s
    [circle (r) (* (* r r) 3.14)]
    [rectangle (w h) (* w h)]))
(area (circle 1))
(area (rectangle 2 3))
]}


@defform[#:literals (lambda)
         (try expr (lambda () handle-expr))]{

Either returns @racket[expr]'s result or catches an exception raised
by @racket[expr] and calls @racket[handle-expr].

@examples[#:eval demo
(try 1 (lambda () 2))
(try (/ 1 0) (lambda () 2))
(try (begin (error 'demo "oops") 1) (lambda () 2))
(eval:error (try (begin (error 'demo "oops") 1) (lambda () (/ 2 0))))
]}


@deftogether[(
@defform[(test expr expr)]
@defform[(test/exn expr string-expr)]
)]{

The @racket[test] form checks whether the value of the first
@racket[expr] matches the value of the second @racket[expr], and
reports a test failure or success. The @racket[test/exn] form checks
whether the @racket[expr] raises an exception whose error message
includes the string produced by @racket[string-expr].

The @racket[test] and @racket[test/exn] forms have type @racket[void],
although they do not actually produce a void value; instead, they
produce results suitable for automatic display through a top-level
expression, and the @racket[void] type merely prevents your program
from using the result.

See also @racket[print-only-errors] and @racket[module+].}


@defform[(time expr)]{

Shows the time taken to evaluate @racket[expr] and returns the value
of @racket[expr].}


@defform[(let/cc id expr)]{

Equivalent to @racket[(call/cc (lambda (id) expr))].}


@; ----------------------------------------

@section{Predefined Functions and Constants}


@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Booleans}

@deftogether[(
@defthing[true boolean]
@defthing[false boolean]
)]{

Aliases for @racket[#t] and @racket[#f].}


@defthing[not (boolean -> boolean)]{

Boolean negation.

@examples[#:eval demo
(not true)]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Lists}

@deftogether[(
@defthing[empty (listof 'a)]
@defthing[empty? ((listof 'a) -> boolean)]
@defthing[cons ('a (listof 'a) -> (listof 'a))]
@defthing[cons? ((listof 'a) -> boolean)]
@defthing[first ((listof 'a) -> 'a)]
@defthing[rest ((listof 'a) -> (listof 'a))]
)]{

Essential list primitives: a list is either @racket[empty] or
@racket[cons] of an item onto a list. The @racket[empty?] predicate
recognizes the empty list, a @racket[cons] recognizes any other list.
The @racket[first] and @racket[rest] functions select back out the two
arguments to @racket[cons].

@examples[#:eval demo
empty
(cons 1 empty)
(first (cons 1 empty))
(rest (cons 1 empty))
(define my-list (cons 1 (cons 2 (cons 3 empty))))
my-list
(first my-list)
(rest my-list)
(first (rest my-list))
(define also-my-list (list 1 2 3))
also-my-list
(rest also-my-list)
]}


@deftogether[(
@defthing[second ((listof 'a) -> 'a)]
@defthing[third ((listof 'a) -> 'a)]
@defthing[fourth ((listof 'a) -> 'a)]
@defthing[list-ref ((listof 'a) number -> 'a)]
)]{

Shorthands for accessing the @racket[first] of the @racket[rest] of a
list, and so on. The second argument to @racket[list-ref] specifies
the number of @racket[rest]s to use before a @racket[first], so it
effectively counts list items from @racket[0].

@examples[#:eval demo
(define my-list (list 1 2 3))
(second my-list)
(list-ref my-list 2)
]}


@defthing[length ((listof 'a) -> number)]{

Returns the number of items in a list.

@examples[#:eval demo
(define my-list (cons 1 (cons 2 (cons 3 empty))))
(length my-list)
]}

@defthing[append ((listof 'a) (listof 'a) -> (listof 'a))]{

Produces a list that has the items of the first given list followed by
the items of the second given list.

@examples[#:eval demo
(define my-list (list 1 2 3))
(define my-other-list (list 3 4 5))
(append my-list my-other-list)
(append my-other-list my-list)
]}

@defthing[reverse ((listof 'a) -> (listof 'a))]{

Returns a list that has the same elements as the given one, but in
reverse order.

@examples[#:eval demo
(reverse (list 1 2 3))]}


@defthing[member ('a (listof 'a) -> boolean)]{

Determines whether a value is an item in a list. Item are compared
using @racket[equal?].

@examples[#:eval demo
(member 1 (list 1 2 3))
(member 4 (list 1 2 3))]}


@defthing[map (('a -> 'b) (listof 'a) -> (listof 'b))]{

Applies a function in order to each element of a list and forms a new
list with the results.

@examples[#:eval demo
(map add1 (list 1 2 3))
(map to-string (list 1 2 3))]}


@defthing[map2 (('a 'b -> 'c) (listof 'a) (listof 'b) -> (listof 'c))]{

Applies a function in order to each pair of elements from two lists in
``parallel,'' forming a new list with the results. An exception is
raised if the two lists have different lengths.

@examples[#:eval demo
(map2 + (list 1 2 3) (list 3 4 5))]}


@defthing[filter (('a -> boolean) (listof 'a) -> (listof 'a))]{

Returns a list containing (in order) the items of a given list for
which a given function returns true.

@examples[#:eval demo
(filter even? (list 1 2 3 4))
(filter odd? (list 1 2 3 4))
]}


@deftogether[(
@defthing[foldl (('a 'b -> 'b) 'b (listof 'a) -> 'b)]
@defthing[foldr (('a 'b -> 'b) 'b (listof 'a) -> 'b)]
)]{

Applies a function to an accumulated value and each element of a list,
each time obtaining a new accumulated value. The second argument to
@racket[foldl] or @racket[foldr] is the initial accumulated value, and
it is provided as the first argument in each call to the given
function. While @racket[foldl] applies the function or items in the
list from from to last, @racket[foldr] applies the function or items
in the list from last to first.

@examples[#:eval demo
(foldl + 10 (list 1 2 3))
(foldl (lambda (n r) (cons (to-string n) r)) empty (list 1 2 3))
(foldr (lambda (n r) (cons (to-string n) r)) empty (list 1 2 3))
]}


@defthing[build-list (number (number -> 'a) -> (listof 'a))]{

Creates a list of items produced by calling a given function on
numbers starting from @racket[0] a given number of times.

@examples[#:eval demo
(build-list 5 (lambda (v) (* v 10)))
]}
 

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Numbers}

@deftogether[(
@defthing[+ (number number -> number)]
@defthing[- (number number -> number)]
@defthing[* (number number -> number)]
@defthing[/ (number number -> number)]
@defthing[modulo (number number -> number)]
@defthing[remainder (number number -> number)]
@defthing[min (number number -> number)]
@defthing[max (number number -> number)]
@defthing[floor (number -> number)]
@defthing[ceiling (number -> number)]
@defthing[add1 (number -> number)]
@defthing[sub1 (number -> number)]
)]{

Standard arithmetic functions.

@examples[#:eval demo
(+ 1 2)
(- 10 9)
(/ 10 5)
(modulo 10 3)
(remainder 10 3)
(min 1 2)
(max 1 2)
(floor 10.1)
(ceiling 10.1)
(ceiling 10.1)
(add1 10)
(sub1 10)
]}

@deftogether[(
@defthing[= (number number -> boolean)]
@defthing[> (number number -> boolean)]
@defthing[< (number number -> boolean)]
@defthing[>= (number number -> boolean)]
@defthing[<= (number number -> boolean)]
@defthing[zero? (number -> boolean)]
@defthing[odd? (number -> boolean)]
@defthing[even? (number -> boolean)]
)]{

Standard number comparisons and predicates.

@examples[#:eval demo
(= 1 1)
(> 1 2)
(< 1 2)
(zero? 1)
(odd? 1)
(even? 1)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Symbols}

@defthing[symbol=? (symbol symbol -> boolean)]{

Compares symbols

@examples[#:eval demo
(symbol=? 'apple 'apple)
(symbol=? 'apple 'Apple)
]}

@deftogether[(
@defthing[string->symbol (string -> symbol)]
@defthing[symbol->string (symbol -> string)]
)]{

Converts between symbols and strings.

@examples[#:eval demo
(string->symbol "apple")
(symbol->string 'apple)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Strings}

@deftogether[(
@defthing[string=? (string string -> boolean)]
@defthing[string-append (string string -> string)]
@defthing[string-length (string -> number)]
@defthing[substring (string number number -> string)]
@defthing[string-ref (string number -> char)]
)]{

Standard string primitives.

@examples[#:eval demo
(string=? "apple" "apple")
(string-append "apple" "banana")
(string-length "apple")
(substring "apple" 1 3)
(string-ref "apple" 0)
]}


@defthing[to-string ('a -> string)]{

Converts any value to its printed form as a string.

@examples[#:eval demo
(to-string 1)
(to-string 'two)
(to-string "three")
(to-string (list 1 2 3))
(to-string '(1 two "three"))
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Characters}

@defthing[char=? (char char -> boolean)]{

Compares characters.

@examples[#:eval demo
(char=? #\a #\b)
]}


@deftogether[(
@defthing[string->list (string -> (listof char))]
@defthing[list->string ((listof char) -> string)]
)]{

Converts between a string and a list of characters.

@examples[#:eval demo
(string->list "apple")
(list->string (list #\a #\b #\c))
]}


@; - - - - - - - - - - - - - - - - - - - - -
@subsection{S-Expressions}

A @deftech{S-expression} typically represents program text. For example,
placing a @litchar{'} in from of any @racketmodname[plai-typed]
expression (which is the same as wrapping it with @racket[quote])
creates an S-expression that contains the identifiers (as symbols),
parenthesization (as lists), and other constants as the expression
text. Various @racket[plai-typed] values, including symbols, numbers,
and lists, can be coerced to and from S-expression form.

The representation of an S-expression is always the same as some other
@racketmodname[plai-typed] value, so conversion to and from an
S-expression is effectively a cast. For example, the
@racket[s-exp-symbol?] function determines whether an S-expression is
a symbol; in that case, @racket[s-exp->symbol] acts the identity
function to produce the symbol, while any other value passed to
@racket[s-exp->symbol] raises an exception. The @racket[symbol->s-exp]
function similarly acts as the identity function to view a symbol as
an S-expression.

@(define-syntax-rule (converter what s-exp-X? s-exp->X X->s-exp)
  @elem{
   Checks whether an @tech{S-expression} corresponds to a single symbol
   and casts it from or to such a form. If @racket[s-exp->X] is
   given an S-expression for which @racket[s-exp-X?] returns false,
   then @racket[s-exp->X] raises an exception.})

@deftogether[(
@defthing[s-exp-symbol? (s-expression -> boolean)]
@defthing[s-exp->symbol (s-expression -> symbol)]
@defthing[symbol->s-exp (symbol -> s-expression)]
)]{

@converter[@elem{a single symbol} s-exp-symbol? s-exp->symbol symbol->s-exp]

@examples[#:eval demo
(s-exp-symbol? `apple)
(s-exp->symbol `apple)
(eval:error (s-exp->symbol '1))
(symbol->s-exp 'apple)
]}

@deftogether[(
@defthing[s-exp-number? (s-expression -> boolean)]
@defthing[s-exp->number (s-expression -> number)]
@defthing[number->s-exp (number -> s-expression)]
)]{

@converter[@elem{a single number} s-exp-number? s-exp->number number->s-exp]

@examples[#:eval demo
(s-exp-number? '1)
(s-exp->number '1)
(number->s-exp 1)
]}

@deftogether[(
@defthing[s-exp-string? (s-expression -> boolean)]
@defthing[s-exp->string (s-expression -> string)]
@defthing[string->s-exp (string -> s-expression)]
)]{

@converter[@elem{a single string} s-exp-string? s-exp->string string->s-exp]

@examples[#:eval demo
(s-exp-string? '"apple")
(s-exp->string '"apple")
(string->s-exp "apple")
]}

@deftogether[(
@defthing[s-exp-boolean? (s-expression -> boolean)]
@defthing[s-exp->boolean (s-expression -> boolean)]
@defthing[boolean->s-exp (boolean -> s-expression)]
)]{

@converter[@elem{a single boolean} s-exp-boolean? s-exp->boolean boolean->s-exp]

@examples[#:eval demo
(s-exp-boolean? '#f)
(s-exp->boolean '#f)
(boolean->s-exp #f)
(s-exp-boolean? `false)
(s-exp-symbol? `false)
]}

@deftogether[(
@defthing[s-exp-list? (s-expression -> boolean)]
@defthing[s-exp->list (s-expression -> (listof s-expression))]
@defthing[list->s-exp ((listof s-expression) -> s-expression)]
)]{

@converter[@elem{an immediate list} s-exp-list? s-exp->list list->s-exp]
A list produced by @racket[s-exp->list] always contains S-expression items.

@examples[#:eval demo
(s-exp-list? '(1 2 3))
(s-exp-list? '1)
(s-exp->list '(1 2 3))
(list->s-exp (list '1 '2 '3))
(eval:error (list->s-exp (list 1 2 3)))
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Vector}

@deftogether[(
@defthing[make-vector (number 'a -> (vectorof 'a))]
@defthing[vector-ref ((vectorof 'a) number -> 'a)]
@defthing[vector-set! ((vectorof 'a) number 'a -> void)]
@defthing[vector-length ((vectorof 'a) -> number)]
)]{

A vector is similar to a list, but it support constant-time access to
any item in the vector and does not support constant-time extension.
In addition, vectors are mutable.

The @racket[make-vector] function creates a vector of a given size and
initializes all vector items to a given value. The @racket[vector-ref]
function accesses the value in a vector slot, and @racket[vector-set!]
changes the value in a slot. The @racket[vector-length] function
reports the number of slots in the vector.

@examples[#:eval demo
(define vec (make-vector 10 "apple"))
(vector-length vec)
(vector-ref vec 5)
(vector-set! vec 5 "banana")
(vector-ref vec 5)
(vector-ref vec 6)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Boxes}

@deftogether[(
@defthing[box ('a -> (boxof 'a))]
@defthing[unbox ((boxof 'a) -> 'a)]
@defthing[set-box! ((boxof 'a) 'a -> void)]
)]{

A box is like a vector with a single slot. Boxes are used primarily to
allow mutation. For example, the value of a field in a variant
instance cannot be modified, but the field's value can be a box, and
then the box's content can be modified.

The @racket[box] function creates a box with an initial value for its
slot, @racket[unbox] accesses the current value in a box's slot, and
@racket[set-box!] changes the value.

@examples[#:eval demo
(define b (box "apple"))
(define b2 b)
(unbox b)
(set-box! b "banana")
(unbox b)
(unbox b2)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Tuples}

@deftogether[(
@defthing[pair ('a 'b -> ('a * 'b))]
@defthing[fst (('a * 'b) -> 'a)]
@defthing[snd (('a * 'b) -> 'b)]
)]{

Shorthands for two-element tuples: the @racket[pair] function creates
a tuple, and the @racket[fst] and @racket[snd] functions access tuple
items.

@examples[#:eval demo
(define p (pair 1 "apple"))
p
(fst p)
(snd p)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Optional Values}

@deftogether[(
@defthing[none (-> (optionof 'a))]
@defthing[some ('a -> (optionof 'a))]
@defthing[some-v ((optionof 'a) -> 'a)]
@defthing[none? ((optionof 'a) -> bool)]
@defthing[some? ((optionof 'a) -> bool)]
)]{

Defined as
@racketblock[
(define-type (optionof 'a)
  [none]
  [some (v : 'a)])
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Hash Tables}

@deftogether[(
@defthing[make-hash ((listof ('a * 'b)) -> (hashof 'a 'b))]
@defthing[hash ((listof ('a * 'b)) -> (hashof 'a 'b))]
@defthing[hash-ref ((hashof 'a 'b) 'a -> (optionof 'b))]
)]{

The @racket[make-hash] function creates a mutable hash table that is
initialized with a given mapping of keys to values (as a list of
tuples pairing keys to values). The @racket[hash] function similarly
creates an immutable hash table that supports constant-time functional
update.

The @racket[hash-ref] function works on either kind of hash table to
find the value for a given key. If the hash table contains a mapping
for a given key, @racket[hash-ref] returns the key's value wrapped
with @racket[some]. Otherwise, @racket[hash-ref] returns
@racket[(none)].

@examples[#:eval demo
(define m-ht (make-hash (list (pair 1 "apple") (pair 2 "banana"))))
(define i-ht (hash (list (pair 1 "apple") (pair 2 "banana"))))
(hash-ref m-ht 1)
(hash-ref i-ht 1)
(hash-ref m-ht 3)
]}

@deftogether[(
@defthing[hash-set! ((hashof 'a 'b) 'a 'b -> void)]
@defthing[hash-remove! ((hashof 'a 'b) 'a -> void)]
)]{

Changes the mapping of a mutable hash table. The @racket[hash-set!]
operation adds or changes the value for a given key, while
@racket[hash-remove!] deletes the mapping (if any) of a given key.

Providing an immutable hash table triggers an exception.

@examples[#:eval demo
(define m-ht (make-hash (list (pair 1 "apple") (pair 2 "banana"))))
(hash-ref m-ht 1)
(hash-ref m-ht 3)
(hash-set! m-ht 3 "coconut")
(hash-set! m-ht 1 "Apple")
(hash-ref m-ht 1)
(hash-ref m-ht 3)
]}

@deftogether[(
@defthing[hash-set ((hashof 'a 'b) 'a 'b -> (hashof 'a 'b))]
@defthing[hash-remove ((hashof 'a 'b) 'a -> (hashof 'a 'b))]
)]{

Produces an immutable hash table that is like a given one, but with a
mapping changed, added, or removed. The @racket[hash-set] operation
adds or changes the value for a given key in the result hash table, while
@racket[hash-remove] deletes the mapping (if any) of a given key
in the result hash table.

@examples[#:eval demo
(define i-ht (hash (list (pair 1 "apple") (pair 2 "banana"))))
(hash-ref i-ht 1)
(define i-ht2 (hash-set (hash-set i-ht 1 "Apple")
                        3 "coconut"))
(hash-ref i-ht2 1)
(hash-ref i-ht2 3)
(hash-ref i-ht 3)
]}

@defthing[hash-keys ((hashof 'a 'b) -> (listof 'a))]{

Returns the keys mapped by a hash table, which can be mutable or
immutable.

@examples[#:eval demo
(define i-ht (hash (list (pair 1 "apple") (pair 2 "banana"))))
(hash-keys i-ht)
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Parameters}

@deftogether[(
@defthing[make-parameter ('a -> (parameterof 'a))]
@defthing[parameter-ref ((parameterof 'a) -> 'a)]
@defthing[parameter-set! ((parameterof 'a) 'a -> void)]
)]{

A parameter implements a kind dynamic binding. The
@racket[make-parameter] function creates a fresh parameter,
@racket[parameter-ref] accesses the parameter's current value, and
@racket[parameter-set!] changes the parameter's current value (i.e.,
at the nearest dynamic binding established with @racket[parameterize],
if any).

See also @racket[parameterize].}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Equality}

@deftogether[(
@defthing[equal? ('a 'a -> boolean)]
)]{

Compares two values for equality. Roughly, two values are
@racket[equal?] when they print the same, but opaque values such as
functions are @racket[equal?] only if they are @racket[eq?].

@examples[#:eval demo
(equal? "apple" "apple")
(equal? (values 1 'two "three") (values 1 'two "three"))
]}


@defthing[eq? ('a 'a -> boolean)]{

Checks whether two values are exactly the same value, which amounts to
checking pointer equality. The @racket[eq?] function is well-defined
on symbols (where it is equivalent to @racket[equal?]), mutable data
structures (where pointer equality corresponds to shared mutation),
and the result of a single expression (such as comparing a
identifier's value to itself), but it should be avoided otherwise.

@examples[#:eval demo
(eq? 'apple 'apple)
(let ([get-one (lambda () 1)])
  (eq? get-one get-one))
]}

@; - - - - - - - - - - - - - - - - - - - - -
@subsection{Other Functions}

@defthing[identity ('a -> 'a)]{Identity primitive.}

@defthing[error (symbol string -> 'a)]{Error primitive.}

@defthing[display ('a -> void)]{Output primitive.}

@defthing[read (-> s-expression)]{Input primitive.}

@defthing[print-only-errors (boolean -> void)]{

Enables or disables the printing of tests that pass. Tests that fail
always cause printing.}

@defthing[call/cc ((('a -> 'b) -> 'a) -> 'a)]{

Passes the current continuation to the given function, and returns
that function's result.

The current continuation is itself represented as a function. Applying
a continuation function discards the current continuation and replaces
it with the called one, supplying the given value to that
continuation.}

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

Type for functions. Each @racket[type] before the @racket[->]
corresponds to a function argument, and the @racket[type] after
@racket[->] corresponds to a function result.}

@defform/none[#:literals (*) (type * ...+)]{

Type for @tech{tuples}. Each @racket[*]-separated @racket[type] is
the type of an element in the tuple.}

@defform/none[()]{

Type for the empty @tech{tuple}.}


@defform[(listof type)]{Type for lists of elements, where @racket[type] is the type of one element.}
@defform[(boxof type)]{Type for mutable boxes, where @racket[type] is the type of the box's content.}
@defform[(vectorof type)]{Type for vectors of elements, where @racket[type] is the type of one element.}
@defform[(parameterof type)]{Type for parameters, where @racket[type] is the type of the parameter's value.}

@defform[(hashof type type)]{

Type for hash tables, where the first @racket[type] corresponds to
keys and the second @racket[type] correspond to values.}

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

Syntactic literals are for use in declarations such as @racket[define]
and @racket[require]; see @racket[define] and @racket[require] for
more information.}

@; ----------------------------------------

@section{Type Checking and Inference}

Type checking and inference is just as in ML (Hindley-Milner), with
a few small exceptions:

@itemize[

 @item{Functions can take multiple arguments, instead of requring a @tech{tuple}
   of arguments. Thus, @racket[(number number -> number)] is a different type
   than either @racket[((number * number) -> number)], which is the tuple
   variant, or @racket[(number -> (number -> number))], which is the curried
   variant.}

 @item{Since all top-level definitions are in the same
   mutually-recursive scope, the type of a definition's right-hand
   side is not directly unified with references to the defined
   identifier on the right-hand side. Instead, every reference to an
   identifier---even a reference in the identifier's definition---is
   unified with a instantiation of a polymorphic type inferred for the
   definition.

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
   current call) is allowed. Recursive types, however, are prohibited.

   The usual value restriction applies for inferring polymorphic
   types, where expression matching the following grammar
   (@emph{before} macro expansion, unfortunately) are considered
   values:

   @racketgrammar[
      #:literals (lambda list values cons empty quote)
      value-expr (lambda (id/ty ...) expr)
                 (lambda (id/ty ...) : type expr) 
                 (values value-expr ...)
                 (list value-expr ...)
                 empty
                 (cons value-expr value-expr)
                 (hash value-expr ...)
                 (variant-id value ...)
                 (quote datum)
                 id
                 string
                 character
                 number
                 boolean
   ]

   where @racket[_variant-id] is @racket[none], @racket[some],
   or a constructor bound by @racket[define-type].}

 @item{Variables are mutable when @racket[set!] is used, but
  assignment via @racket[set!] is disallowed on a variable after a
  polymorphic type has been inferred for it (e.g., in an interaction
  after type checking is complete).}

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

@; ----------------------------------------

@include-section["untyped.scrbl"]

@close-eval[demo]
