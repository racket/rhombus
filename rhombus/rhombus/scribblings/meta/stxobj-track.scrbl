#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta:
      rhombus/rx.rx)

@title(~tag: "stxobj-track"){Syntax Tracking}

As macros in a space are expanded, the resulting syntax object acquires
an @rhombus(#'origin) property to track the history of expansion.
Specifically, when the expander dispatches to a macro bound to
@rhombus(name), then the result of the macro gets an @rhombus(#'origin)
property with the use-site @rhombus(name) identifier. If a result
already has an @rhombus(#'origin) property, then the existing value is
combined with @rhombus(name) using @rhombus(Pair), and an
@rhombus(#'origin) property value is in general a tree of identifiers.
This information is used by DrRacket, for example, to draw binding
arrows from uses or names to definitions of names.

When a new macro is defined using a form like @rhombus(annot.macro),
@rhombus(bind.macro), or one bound using
@rhombus(macro_definer, ~space_clause) in @rhombus(space.enforest), then
the macro may receive arguments that are automatically parsed, depending
on the shape of the macro pattern. Specifically, the left-hand argument
is always parsed for an infix macro, while the right-hand side is always
parsed if its pattern is @rhombus($, ~bind) followed by an identifier. A
macro automatically propagates an @rhombus(#'origin) property value from
each automatically parsed argument to the macro's result. The macro can
propagate other @rhombus(#'origin) properties explicitly using
@rhombus(Syntax.track_origin). That kind of propagation is not needed,
however, if a macro expands to a use of other forms that already handle
expansion tracking.

For example, the following prefix variant of @rhombus(||, ~annot) does
not need specific tracking. Even though its @rhombus(a) and @rhombus(b)
pattern bindings are not automatically parsed, the terms are intact in
the resulting syntax, and @rhombus(||, ~annot) will track appropriately,
and binding arrows can be drawn for @rhombus(either),
@rhombus(String, ~annot) and @rhombus(Int, ~annot):

@rhombusblock(
  annot.macro 'either($a, $b)':
    '($a) || ($b)'

  1 :: either(String, Int)
)

The following infix macro receives already-parsed arguments, so even
though it pulls apart its arguments and creates a new packed
representation of the annotation, the macro expander will record the
connection to the @rhombus(a) and @rhombus(b) expansions:

@rhombusblock(
  annot.macro '$a => $b':
    let (a_pred, a_statinfo) = annot_meta.unpack_predicate(a)
    let (b_pred, b_statinfo) = annot_meta.unpack_predicate(b)
    annot_meta.pack_predicate(
      'fun (v): if $a_pred(v) | $b_pred(v) | #true',
      '()'
    )

  1 :: Int => PosInt
  "a" :: Int => PosInt
)

The following macro needs to use @rhombus(Syntax.track_origin),
otherwise no biding arrows will be shown for @rhombus(Int, ~annot)
and @rhombus(PosInt, ~annot) in the use of @rhombus(implies):

@rhombusblock(
  annot.macro 'implies($(a :: annot_meta.Parsed),
                       $(b :: annot_meta.Parsed))':
    let (a_pred, a_statinfo) = annot_meta.unpack_predicate(a)
    let (b_pred, b_statinfo) = annot_meta.unpack_predicate(b)
    annot_meta.pack_predicate(
      'fun (v): if $a_pred(v) | $b_pred(v) | #true',
      '()'
    ).track_origin([a, b])

  1 :: implies(Int, PosInt)
)

Since tracking is often needed when using a packing function like
@rhombus(annot_meta.pack_predicate), @rhombus(annot_meta.pack_predicate)
accepts a @rhombus(~track) argument that provides a slight shorthand and
as a reminder to consider the need for tracking.

The annotation examples above show macros working within one
@tech{space}. Tracking is practically always needed when bridging
spaces. For example, the @rhombus(rx) impleemntation parses a subsequent
regexp form, and it uses @rhombus(Syntax.track_origin) to connect a
returned expression to parsed regexp. Ultimately, all @rhombus(#'origin)
information must be attached to an expansion or definition form, since
those are the only primitive forms. Tracking in expressions is somewhat
special, meanwhile, because subexpressions appear intact within an
enclosing expression, so origin information does not need to be
explicitly lifted to the enclosing expression.
