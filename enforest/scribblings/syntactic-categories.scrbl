#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Syntactic Categories}

Rhombus expansion involves various syntactic categories that determine
different kinds of expansion contexts. The specific set of contexts
depends on the language, and not the Rhombus expander, but here are some
possible contexts:

@itemlist(
  @item{declarations (in a module's immediate body or at the top level)},
  @item{definitions},
  @item{expressions},
  @item{bindings (like @rhombus(match) patterns, but everywhere)}
)

In Racket's expander, a few core contexts are reflected by
@racket_syntax_local_context, but the Racket expander has only one kind
of transformer that is represented by one kind of compile-time value: a
procedure of arity 1. Nevertheless, some macros work only in, say,
definition positions or in a module body. Rhombus expansion instead
expects different kinds of compile-time values for different expansion
contexts, so a mapping can declare where it's meant to be used.

The Rhombus expander is parameterized over the way that different
kinds of compile-time values for different contexts are recognized,
but they are expected to be implemented through structure-type
properties. A compile-time value can then implement multiple kinds of
transformers to create a mapping that is works in multiple contexts.
For example, the example @rhombus(<>) operator is useful in both expression
and binding contexts, with a suitable meaning in each context.

Different contexts may also consult different mapping spaces in the
sense of @racket_provide_for_space. Contexts like declarations,
definitions, and expressions are likely to use the default space, while
binding, require, and provide contexts might use their own spaces. For
example, in the prototype language supplied with this proposal,
@rhombus(operator) and @rhombus(bind.macro) can both bind @rhombus(<>)
because the former binds in the default space and the latter in the
binding space. The Rhombus expander itself is, again, parameterized over
the way that mapping spaces are used.

The relevant syntactic category for a shrubbery is determined by its
surrounding forms, and not inherent to the shrubbery. For example,
@rhombus(Posn(x, y)) or @rhombus(x <> y) in the example mean one thing
as an expression and another as a binding. Exactly where the contexts
reside in a module depends on a specific Rhombus language that is built
on the Rhombus expander. Meanwhile, a full Rhombus language can have
different or more syntactic categories than the ones listed above. A
Rhombus language likely allows extensions to create even more contexts,
just like Racket program can have more contexts through syntactic
extensions, such as @racket_match or Typed Racket.
