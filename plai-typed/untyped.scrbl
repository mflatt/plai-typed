#lang scribble/manual
@(require (for-label (only-meta-in 0 plai-typed/untyped)
                     (only-in racket/base only-in)))

@title{Untyped with Typed Syntax}

@defmodulelang[plai-typed/untyped]{
The @racketmodname[plai-typed/untyped] language supports the same syntax as
@racketmodname[plai-typed], but it performs no type checking.}

The @racket[define-syntax], @racket[define-syntax-rule],
@racket[module+], and @racket[require] forms from
@racketmodname[plai-typed/untyped] are the bindings from
@racketmodname[racket/base] instead of @racketmodname[plai-typed].

@deftogether[(
@defidform[typed-in]
@defidform[opaque-type-in]
)]{

Forms for @racket[require] that simulate the ones from
@racketmodname[plai-typed] by expanding to uses of @racket[only-in].}
