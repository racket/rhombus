#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(fun meta(s): italic(s))

@title{Representing Static Information}

Static information for an expression or binding is represented in
key–value form. When static information is associated with a binding, it
is propagated to each use of the bound variable, so we can refer to an
expression @meta{E} that has static information without loss of
generality.

Rhombus uses several built-in static-information keys:

@itemlist(

  @item{@rhombus(statinfo_meta.dot_provider_key) --- names a compile-time function
   that macro-expands any use of @rhombus(.) after @meta{E}. For example,
   assuming a @rhombus(class Posn(x, y)) declaration, @rhombus(p :: Posn)
   associates @rhombus(statinfo_meta.dot_provider_key) with uses of @rhombus(p) to
   expand @rhombus(p.x) and @rhombus(p.y) to field accesses. An expression
   is a @tech{dot provider} when it has static information mapped by
   @rhombus(statinfo_meta.dot_provider_key).},

  @item{@rhombus(statinfo_meta.call_result_key) --- provides static information
   to be attached to a function-call expression where the function position
   is @meta{E}. (The arguments in the call do not matter.) For example,
   @rhombus(class Posn(x, y)) associates @rhombus(statinfo_meta.call_result_key)
   with @rhombus(Posn) itself, and the associated value is static
   information with @rhombus(statinfo_meta.dot_provider_key). So, @rhombus(Posn(1, 2))
   gets @rhombus(statinfo_meta.dot_provider_key) that makes @rhombus(Posn(1, 2).x)
   select the @rhombus(1).},

  @item{@rhombus(statinfo_meta.ref_result_key) --- provides static information to be
   attached to a @litchar{[}...@litchar{]} map reference where E is to the left
   of the @litchar{[}...@litchar{]}. (The index expression inside @litchar{[}...@litchar{]} does
   not matter.) For example @rhombus(ps :: List.of(Posn)) associates
   @rhombus(statinfo_meta.ref_result_key) to @rhombus(ps), where the associated value
   includes is static information with @rhombus(statinfo_meta.dot_provider_key). So,
   @rhombus(ps[i].x) is allowed an selects an @rhombus(x) field from the @rhombus(Posn)
   instance produced by @rhombus(ps[i]).},

  @item{@rhombus(statinfo_meta.map_ref_key) and @rhombus(statinfo_meta.map_set_key) --- names a form to
   use for a @litchar{[}...@litchar{]} map-like reference or assignment where E is to
   the left of the @litchar{[}...@litchar{]}. (The index expression inside
   @litchar{[}...@litchar{]} does not matter.) For example, @rhombus(p :: Array) associates
   @rhombus(statinfo_meta.map_ref_key) to @rhombus(p) so that @rhombus(p[i]) uses an array-specific
   referencing operation, and it associates @rhombus(statinfo_meta.map_ref) to
   @rhombus(p) so that @rhombus(p[i] = n) uses an array-specific assignment operation.},

  @item{@rhombus(statinfo_meta.map_append_key) --- names a form to specialize the
   @rhombus(++) operator for appending maps, lists, and similar values. For example,
   @rhombus(p :: Array) associates @rhombus(statinfo_meta.map_append_key) to @rhombus(p)
   so that @rhombus(p ++ q) uses an array-specific appending operation.}

)

Static information is associated to a binding through a binding
operator/macro, and it can be associated to an expression through a
binding or through an expression operator/macro that adds static
information to its parsed form (i.e., expansion). For example, the
@rhombus(::) operator associates static information through an annotation. An
annotation pairs a predicate with a set of static information to
associate with any variable that is bound with the annotation. That's
why a binding @rhombus(p :: Posn) makes every reference to @rhombus(p) a dot provider: the annotation
@rhombus(Posn) indicates that every binding with the annotation gets a dot
provider to access @rhombus(x) and @rhombus(y). When @rhombus(::) is used in an expression,
then static information indicated by the annotation is similarly
associated with the overall @rhombus(::) expression, which is why
@rhombus((e :: Posn)) is a dot provider for any expression @rhombus(e). Function-call forms
and map-reference forms similarly attach static information to
their parsed forms, sometimes, based on static information attached to
the first subexpression.

Note that static information is associated with an expression, not a
value. So, if @rhombus(Posn) is passed as an argument to a function (instead
of being called directly as a constructor), then the function
ultimately receives a value and knows nothing of the expression that
generated the value. That is, no part of the function's implementation
can take advantage of the fact that directly calling @rhombus(Posn) would have
formed a dot provider. The function might have an annotation on its
argument that indicates a dot-provider constructor, but that's a
feature of the formal argument, and not of an actual value.
