#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(fun meta(s): italic(s))

@title(~tag: "static-info-key"){Representing Static Information}

Static information for an expression or binding is represented in
key--value form and encoded within @tech{syntax objects} at expansion time.
When static information is associated with a binding, it
is propagated to each use of the bound variable, so we can refer to an
expression @meta{E} that has static information without loss of
generality.

The set of static-information keys is unlimited, but Rhombus built-in
forms use several built-in keys. These keys are used directly only via
low-level annotation and binding macros. Nevertheless, we list a few
keys to help make the concept more concrete.

@itemlist(

  @item{@rhombus(statinfo_meta.dot_provider_key): Names a compile-time function
   that macro-expands any use of @rhombus(.) after @meta{E}. For example,
   assuming a @rhombus(class Posn(x, y)) declaration, @rhombus(p :: Posn)
   associates @rhombus(statinfo_meta.dot_provider_key) with uses of @rhombus(p) to
   expand @rhombus(p.x) and @rhombus(p.y) to field accesses. An expression
   is a @tech{dot provider} when it has static information mapped by
   @rhombus(statinfo_meta.dot_provider_key).}

  @item{@rhombus(statinfo_meta.call_result_key): Provides static information
   to be attached to a function-call expression where the function position
   is @meta{E}. (The arguments in the call do not matter.) For example,
   @rhombus(class Posn(x, y)) associates @rhombus(statinfo_meta.call_result_key)
   with @rhombus(Posn) itself, and the associated value is static
   information with @rhombus(statinfo_meta.dot_provider_key). So, @rhombus(Posn(1, 2))
   gets @rhombus(statinfo_meta.dot_provider_key) that makes @rhombus(Posn(1, 2).x)
   select the @rhombus(1).}

  @item{@rhombus(statinfo_meta.index_result_key): Provides static information to be
   attached to a @brackets indexing reference where E is to the left
   of the @brackets. (The index expression inside @brackets does
   not matter.) For example @rhombus(ps :: List.of(Posn)) associates
   @rhombus(statinfo_meta.index_result_key) to @rhombus(ps), where the associated value
   is static information with @rhombus(statinfo_meta.dot_provider_key). So,
   @rhombus(ps[i].x) is allowed and selects an @rhombus(x) field from the @rhombus(Posn)
   instance produced by @rhombus(ps[i]).}

  @item{@rhombus(statinfo_meta.index_get_key) and @rhombus(statinfo_meta.index_set_key): Names a form to
   use for a @brackets indexing reference or assignment where E is to
   the left of the @brackets. (The index expression inside
   @brackets does not matter.) For example, @rhombus(p :: Array) associates
   @rhombus(statinfo_meta.index_get_key) to @rhombus(p) so that @rhombus(p[i]) uses an array-specific
   referencing operation, and it associates @rhombus(statinfo_meta.index_set_key) to
   @rhombus(p) so that @rhombus(p[i] = n) uses an array-specific assignment operation.}

  @item{@rhombus(statinfo_meta.append_key): Names a form to specialize the
   @rhombus(++) operator for appending maps, lists, and similar values. For example,
   @rhombus(p :: Array) associates @rhombus(statinfo_meta.append_key) to @rhombus(p)
   so that @rhombus(p ++ q) uses an array-specific appending operation.}

)

