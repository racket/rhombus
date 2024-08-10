#lang rhombus/scribble/manual

@(import:
    meta_label:
      rhombus open
      pict
      pict/rhombus open)

@(def rhombus_eval = make_rhombus_eval())
@examples(
  ~eval: rhombus_eval
  ~hidden:
    import:
      pict open
      pict/rhombus open
)

@title(~tag: "rhombus"){Code as Picts}

@docmodule(pict/rhombus)

The @rhombusmodname(pict/rhombus) library provides a @rhombus(rhombus)
form form rendering literal shrubbery forms as a pict.

@doc(
  expr.macro 'rhombus($form)'
  expr.macro 'rhombus($form, $space)'

  grammar space:
    ~var
    ~datum
    ~value
    ~result
    $builtin_space_keyword
    $symbol
    $symbol / $space
){

 Produces a @tech{pict} that renders the text of @rhombus(form).
 Comments in @rhombus(form) are discarded, and extra spacing is removed,
 but the text of shrubbery elements that have multiple representations
 (such as @rhombus(10), @rhombus(0xA), and @rhombus(000010)) is preserved
 verbatim.

 Within @rhombus(form), @rhombus(#,) followed by a parenthesized
 expression is an escape where the expression must produce a pict. The
 pict is rendered in place of the escape.

 If @rhombus(space) is provided as @rhombus(~var), then @rhombus(form)
 is italicized. If @rhombus(space) is @rhombus(~datum), then
 @rhombus(form) does not get a color. If @rhombus(space) is
 @rhombus(~value) or @rhombus(~result), then a corersponding uniform
 color is applied. Any other @rhombus(space) determines the initial space
 for rendering @rhombus(form) sensitive to @rhombus(meta_label, ~expo)
 imports.

@examples(
  ~eval: rhombus_eval,
  rhombus(1+2)
  rhombus(arg, ~var)
  rhombus(3, ~result)
)

}

@doc(
  expr.macro 'rhombusblock($form, ...)'
){

 Produces a @tech{pict} that renders the text of @rhombus(form)
 verbatim, preserving whitespace and comments, except that @rhombus(#,)
 followed by a parenthesized expression is an escape that must produce a
 pict to render in place of the escape.

 Prefix a use of @rhombus(rhombus) with @litchar("@") to avoid needing
 to include @litchar{,} between multiple groups that are on separate
 lines.

@examples(
  ~eval: rhombus_eval,
  ~repl:
    @rhombusblock(
      // This is an example function:
      fun add1(n):
        n + 1
    )
  ~defn:
    ~fake:
      fun make_example(n, res):
        @rhombusblock(
          check: add1(#,(@rhombus(#,))(n))
                 ~is #,(@rhombus(#,))(res)
        )
      fun make_example(n, res):
        @rhombusblock(
          check: add1(#,(n))
                 ~is #,(res)
        )
  ~repl:
    make_example(@rhombus(3), @rhombus(4))
    make_example(@rhombus(3).scale(2), @rhombus(4).scale(2))
)

}

@doc(
  Parameter.def current_paren_color :: Color || String:
    "brown"
  Parameter.def current_literal_color :: Color || String:
    "forestgreen"
  Parameter.def current_identifier_color :: Color || String:
    "black"
  Parameter.def current_comment_color :: Color || String:
    "chocolate"
  Parameter.def current_result_color :: Color || String:
    "darkblue"
  Parameter.def current_rhombus_colorize :: Any.to_boolean:
    #true

  Parameter.def current_rhombus_tt :: Function.of_arity(1)
){

 Parameters that control the output of @rhombus(rhombus) and
 @rhombus(rhombusblock).

 The value of @rhombus(current_rhombus_tt) is a function that takes a
 string and produces a pict for a shrubbery element. The result may be
 colorized, and the current font may have been adjusted to italic for
 @rhombus(~var) mode.

}
