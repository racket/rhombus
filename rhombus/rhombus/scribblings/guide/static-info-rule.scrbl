#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "static-info-rules"){Rules for Static Information}

Exactly how static information is associated to expressions and
bindings depends on the expression or binding form. So, it's not
possible to write down exhaustive rules for for Rhombus static
information (in the same way that it's not possible to write down a
full grammar, since the grammar can be extended via macros). A binding
or expression form's documentation should define what static
information it associates with a name or expression.

Here's a summary of the static-information behavior of classes:

@itemlist(

  @item{A class name bound by @rhombus(class) acts as a @tech(~doc: ref_doc){namespace}. It
   provides field-accessor functions, as in @rhombus(Posn.x).}

  @item{As an annotation, a class name turns any binding or
   expression using the annotation into a dot provider. A class name
   followed by @rhombus(.of) has the same effect; in addition, it associates
   any information implied by the argument annotations as static
   information for fields accessed from the binding or exression
   through a dot. For example, assuming a
   @rhombus(class Rect(top_left, side)) declaration,
   @rhombus(r :: Rect.of(Posn, Int), ~bind) causes
   @rhombus(r.top_left) to have @rhombus(Posn) information, with means that
   @rhombus(r.top_left.x) works.}

  @item{When a class field has an annotation, then that annotation's
   static information is associated with a field accessed through the @rhombus(.) operator.
   In the @rhombus(Line) example, the @rhombus(p2) field of @rhombus(Line) has a @rhombus(Posn)
   annotation, so a @rhombus(l1 :: Line, ~bind) binding means that @rhombus(l1.p2) is a dot
   provider to access @rhombus(x) and @rhombus(y).}

  @item{When a class field has an annotation, then that annotation's
   static information is associated as result information for a field
   accessor accessed through the @rhombus(.) operator. For example, @rhombus(Line.p1) gets the
   @rhombus(Posn) annotation's static information as its call-result
   information, so @rhombus(Line.p1(e)) has that information, which means that
   @rhombus(Line.p1(e)) is a dot provider.}

  @item{When a class field has an annotation, and when the class
   name is used as a pattern form in a binding, then the annotation's
   static information is associated with the argument pattern. For
   example, @rhombus(Line(p1, p2)) as a binding pattern associates @rhombus(Posn)
   information to @rhombus(p1) and to @rhombus(p2), which means that they're dot
   providers.}

  @item{When a class name is used as a binding pattern, any ``downward''
   static information that flows the binding is checked for static
   information keyed by the class's accessors, and that information is
   propagated as ``downward'' information to the corresponding binding
   subpattern. For example, if @rhombus(Rect(tl, s)) as a binding receives
   ``downward'' information that associates (the internal key for)
   @rhombus(Rect.top_left) to @rhombus(Posn)-annotation information, then
   the binding form @rhombus(tl) receives @rhombus(Posn)-annotation
   information.}

)

More rules about static information in general:

@itemlist(

  @item{A expression that is parentheses wrapped around an inner expression
   has the same static information as the inner expression.}

  @item{When a function call's function position has result static
   information, the function call as a whole is given that static
   information. For example, since @rhombus(Line.p1) has result information
   that describes a dot provider, @rhombus(Line.p1(e)) is a dot provider.}

  @item{When a @rhombus(fun) defintition form includes a result annotation, then the
   annotation's information is associated to the defined function name
   as call-result information. For example, if a function defintion
   starts @rhombus(fun flip(x) :: Posn), then @rhombus(Posn) static information is
   associated to @rhombus(flip) as result information, so @rhombus(flip(x)) is a dot
   provider. The same applies to a @rhombus(def) form that behaves like a
   @rhombus(fun) definition form.}

  @item{When the right-hand side of a @rhombus(def) or @rhombus(let) has a single
   group, and when that goes does not start with a definition-form
   name, then static information from that right-hand side expression
   is propagated to the binding side. For example, @rhombus(def p: Posn(1, 2))
   associated that static information of @rhombus(Posn(1, 2)) with @rhombus(p), which
   among other things means that @rhombus(p.x) will be allowed.}

)

The @rhombus(List), @rhombus(Array), and @rhombus(Map) expression and
binding forms are analogous to class-name forms. For example,
@rhombus(Array) as a constructor in an expression form associates
reference-result information to the overall @rhombus(Array) expression,
as does @brackets for constructing a list. In a list
binding pattern, when @rhombus(..., ~bind) is used after a binding subpattern,
the ``upward'' static information of the subpattern for each identifier
wrapped as reference-result information for the identifier outside the
list pattern, since each; for example @rhombus([p :: Posn, ...], ~bind) as a
binding pattern causes @rhombus(p) to have static information that says
its reference result as @rhombus(Posn)-annotation information. The
@rhombus(List.of, ~annot), @rhombus(Array.of, ~annot), and @rhombus(Map.of, ~annot) annotation
forms in bindings propagate ``downward'' reference-result information to
nested annotations. ``Downward'' static information is used by
@rhombus(List, ~bind) or @brackets pattern constructions only
in the case that there's a single element binding pattern followed by
@rhombus(..., ~bind), while @rhombus(Array, ~bind) and @rhombus(Map, ~bind) as pattern
constructors cannot use ``downward'' information.

The @rhombus(::, ~bind) binding form and the @rhombus(matching, ~annot) annotation form
allow static information to flow both ``downward'' and ``upward''
through both annotations and binding patterns.
