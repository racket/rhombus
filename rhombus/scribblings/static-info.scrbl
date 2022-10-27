#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def meta(s): italic(s))

@title(~tag: "static-info"){Defining and Using Static Information}

A binding or an expression can have associated @deftech{static
 information} that is used to enable, reject, or resolve certain
expression forms. For example, an expression can be used to the left of
a @rhombus(.) only when it has static information to specify how a field
name after @rhombus(.) resolves to an field accessor. See the
@secref("annotation") for an overview of static information and its
role.

@section{Representing Static Information}

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

@section(~tag: "static-info-rules"){Rules for Static Information}

Exactly how static information is associated to expressions and
bindings depends on the expression or binding form. So, it's not
possible to write down exhaustive rules for for Rhombus static
information (in the same way that it's not possible to write down a
full grammar, since the grammar can be extended via macros). A binding
or expression form's documentation should define what static
information it associates with a name or expression.

Here's a summary of the static-information behavior of classes:

@itemlist(

  @item{A class name bound by @rhombus(class) acts as a @tech{namespace}. It
   provides field-accessor functions, as in @rhombus(Posn.x).},

  @item{As an annotation, a class name turns any binding or
   expression using the annotation into a dot provider. A class name
   followed by @rhombus(.of) has the same effect; in addition, it associates
   any information implied by the argument annotations as static
   information for fields accessed from the binding or exression
   through a dot. For example, assuming a
   @rhombus(class Rect(top_left, side)) declaration,
   @rhombus(r :: Rect.of(Posn, Integer)) causes
   @rhombus(r.top_left) to have @rhombus(Posn) information, with means that
   @rhombus(r.top_left.x) works.},

  @item{When a class field has an annotation, then that annotation's
   static information is associated with a field accessed through the @rhombus(.) operator.
   In the @rhombus(Line) example, the @rhombus(p2) field of @rhombus(Line) has a @rhombus(Posn)
   annotation, so a @rhombus(l1 :: Line) binding means that @rhombus(l1.p2) is a dot
   provider to access @rhombus(x) and @rhombus(y).},

  @item{When a class field has an annotation, then that annotation's
   static information is associated as result information for a field
   accessor accessed through the @rhombus(.) operator. For example, @rhombus(Line.p1) gets the
   @rhombus(Posn) annotation's static information as its call-result
   information, so @rhombus(Line.p1(e)) has that information, which means that
   @rhombus(Line.p1(e)) is a dot provider.},

  @item{When a class field has an annotation, and when the class
   name is used as a pattern form in a binding, then the annotation's
   static information is associated with the argument pattern. For
   example, @rhombus(Line(p1, p2)) as a binding pattern associates @rhombus(Posn)
   information to @rhombus(p1) and to @rhombus(p2), which means that they're dot
   providers.},

  @item{When a class name is used as a binding pattern, any ``downward''
   static information that flows the binding is checked for static
   information keyed by the class's accessors, and that information is
   propagated as ``downward'' information to the corresponding binding
   subpattern. For example, if @rhombus(Rect(tl, s)) as a binding receives
   ``downward'' information that associates (the internal key for)
   @rhombus(Rect.top_left) to @rhombus(Posn)-annotation information, then
   the binding form @rhombus(tl) receives @rhombus(Posn)-annotation
   information.},

)

More rules about static information in general:

@itemlist(
  
  @item{A expression that is parentheses wrapped around an inner expression
   has the same static information as the inner expression.},

  @item{When a function call's function position has result static
   information, the function call as a whole is given that static
   information. For example, since @rhombus(Line.p1) has result information
   that describes a dot provider, @rhombus(Line.p1(e)) is a dot provider.},

  @item{When a @rhombus(fun) defintition form includes a result annotation, then the
   annotation's information is associated to the defined function name
   as call-result information. For example, if a function defintion
   starts @rhombus(fun flip(x) :: Posn), then @rhombus(Posn) static information is
   associated to @rhombus(flip) as result information, so @rhombus(flip(x)) is a dot
   provider. The same applies to a @rhombus(def) form that behaves like a
   @rhombus(fun) definition form.},

  @item{When the right-hand side of a @rhombus(val), @rhombus(def), or @rhombus(let) has a single
   group, and when that goes does not start with a definition-form
   name, then static information from that right-hand side expression
   is propagated to the binding side. For example, @rhombus(val p: Posn(1, 2))
   associated that static information of @rhombus(Posn(1, 2)) with @rhombus(p), which
   among other things means that @rhombus(p.x) will be allowed.}

)

The @rhombus(List), @rhombus(Array), and @rhombus(Map) expression and
binding forms are analogous to class-name forms. For example,
@rhombus(Array) as a constructor in an expression form associates
reference-result information to the overall @rhombus(Array) expression,
as does @litchar{[}...@litchar{]} for constructing a list. In a list
binding pattern, when @rhombus(...) is used after a binding subpattern,
the ``upward'' static information of the subpattern for each identifier
wrapped as reference-result information for the identifier outside the
list pattern, since each; for example @rhombus([p :: Posn, ...]) as a
binding pattern causes @rhombus(p) to have static information that says
its reference result as @rhombus(Posn)-annotation information. The
@rhombus(List.of), @rhombus(Array.of), and @rhombus(Map.of) annotation
forms in bindings propagate ``downward'' reference-result information to
nested annotations. ``Downward'' static information is used by
@rhombus(List) or @litchar{[}...@litchar{]} pattern constructions only
in the case that there's a single element binding pattern followed by
@rhombus(...), while @rhombus(Array) and @rhombus(Map) as pattern
constructors cannot use ``downward'' information.

The @rhombus(::, ~bind) binding form and the @rhombus(matching, ~annot) annotation form
allow static information to flow both ``downward'' and ``upward''
through both annotations and binding patterns.

@section{Binding Static Information}

See @secref("bind-macro-protocol") for information on how binding macros
receive and produce static information.
