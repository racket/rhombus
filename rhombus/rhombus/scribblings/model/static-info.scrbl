#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "static-info-rule"){Role of Static Information}

A binding or an expression can have associated @tech{static
 information} that is used to enable, reject (in @rhombus(use_static) mode), or resolve certain
expression forms. For example, an expression to the left of
a @rhombus(.) can have static information to specify how a field
name after @rhombus(.) resolves to an field accessor. See the
@secref(~doc: guide_doc, "annotation") for an introduction to static information and its
role.

Static information is associated to a binding through a binding
operator, and it can be associated to an expression through a
binding that it uses or through an expression form that adds static
information to its parsed form (i.e., expansion). For example, the
@rhombus(::) operator associates static information through an annotation. An
annotation, meanwhile, pairs a predicate with a set of static information to
associate with any variable that is bound with the annotation. That's
why a binding @rhombus(p :: Posn, ~bind) (as in
@seclink(~doc: guide_doc, "annotation"){the introduction section})
makes every reference to @rhombus(p) a @tech{dot provider}: the annotation
@rhombus(Posn) indicates that every binding with the annotation gets a dot
provider to access the @rhombus(x) and @rhombus(y) fields of @rhombus(Posn). When @rhombus(::) is used in an expression,
then static information indicated by the annotation is similarly
associated with the overall @rhombus(::) expression, which is why
@rhombus(e :: Posn) is a dot provider for any expression @rhombus(e). Function-call forms
and map-reference forms similarly attach static information to
their parsed forms, sometimes, based on static information attached to
the function or map subexpression.

Static information is associated with an expression, not a
value. So, if @rhombus(Posn) is passed as an argument to a function (instead
of being called directly as a constructor), then the function
ultimately receives a value and knows nothing of the expression that
generated the value. That is, no part of the function's implementation
can take advantage of the fact that directly calling @rhombus(Posn) would have
formed a dot provider. The function might have an annotation on its
argument that indicates a dot-provider constructor, but that's a
feature of the formal argument, and not of an actual value.
