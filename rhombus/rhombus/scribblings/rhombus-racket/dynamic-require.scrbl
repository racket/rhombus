#lang scribble/manual
@(require (for-label rhombus/dynamic-require
                     rhombus/parse
                     racket/base
                     racket/contract/base)
          "rhombus-names.rhm")

@title[#:tag "dynamic-require"]{Dynamic Require of Rhombus Exports}

@defmodule[rhombus/dynamic-require]

The @racketmodname[rhombus/dynamic-require] module provides a function
similar to Racket's @racket[dynamic-require], but for accessing Rhombus
module exports, potentially through dotted names.

@defproc[(rhombus-dynamic-require [mod module-path?]
                                  [name (or/c symbol? (listof symbol?))])
         any/c]{

 Although @racket[dynamic-require] works for Rhombus exports that
 correspond to plain variables, @racket[dynamic-require] cannot get the
 value of a Rhombus binding like the one constructor function default by
 @|rhombus_class|, and it cannot access dotted paths that go through a
 Rhombus namespace.

 The @racket[rhombus-dynamic-require] handles Rhombus bindings more
 generally. When @racket[name] is a list, it access an export
 that corresponds to using the dot operator between the elements of the
 list.

 Static access is normally better than dynamic access, so consider using
 @racket[rhombus-expression] instead of @racket[rhombus-dynamic-require].

}
