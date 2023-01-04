#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@title{Identifier Spaces}

An identifier can have different meanings in different contexts, such as
expression versus binding, because an identifier can be bound in a
specific @deftech{space}. Spaces have symbolic names, such as
@rhombus(rhombus/expr, ~datum) and @rhombus(rhombus/bind, ~datum), which
generally correspond to the @tech{namespace} that provides a binding
form for the space. There is also a default space, which effectively
binds in all spaces. For example, in an expression context, an
identifier is resolved by first checking the
@rhombus(rhombus/expr, ~datum) space and then the default space.

Binding forms like @rhombus(def), @rhombus(expr.macro), and
@rhombus(bind.macro) bind an identifier in the default space. Variants
like @rhombus(expr.only.macro) bind in a specific space like
@rhombus(rhombus/expr, ~datum).
Normally, when an identifier has a binding in some space, it should also
have a binding in the default space. Otherwise, local binding of the
same identifier name can lead to ambigious references. So, binding an
identifier with @rhombus(def) and then @rhombus(bind.only.macro) is a
good way to give an identifier meaning in both expression and binding
contexts, but using only @rhombus(expr.only.rule) plus
@rhombus(bind.only.macro) tends to be a bad idea.

Expressions, definitions, and declarations use the same space,
@rhombus(rhombus/expr, ~datum), since those contexts tend to overlap.
Most other contexts have their own spaces, even though some of them also
overlap with expression positions, such as class and interface clauses.
