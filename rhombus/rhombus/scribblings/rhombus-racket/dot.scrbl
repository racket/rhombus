#lang scribble/manual
@(require (for-label rhombus/dot
                     racket/base
                     racket/contract/base
                     rhombus/parse))

@title{Dynamic Dot from Racket}

@defmodule[rhombus/dot]

The Rhombus dot operator can be used via @racket[rhombus-expression],
but the @racketmodname[rhombus/dot] module provides a simpler and more
convenient interface for dynamic access of object fields, methods, and
properties.

@defproc[(dynamic-dot-ref [obj any/c] [name symbol?]) any/c]{

 Accesses a field, method, or property @racket[name] from @racket[obj].
 An exception is raised if no such member exists in @racket[obj].

}

@defproc[(dynamic-dot-set! [obj any/c] [name symbol?] [val any/c]) any/c]{

 Modifies @racket[obj] to set the field or property @racket[name] in
 @racket[obj] to @racket[val]. An exception is raised if no such mutable
 member exists in @racket[obj].

}
