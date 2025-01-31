#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "implicit"){Implicit Forms}

Rhombus parsing is driven by bindings even for forms that have no
apparent name, such as a literal expression like @rhombus(7) or square
brackets like @rhombus([1, 2, 3]). In those cases without an apparent
name an implicit form is used to give meaning to the term, whether in
an expression position, binding position, or other kind of position. For
positions that allow only prefix operators, such as definition
positions, only prefix implicits are used.
Here are all of the implicit forms:

@itemlist(

 @item{@rhombus(#%literal) --- used for anything other than an
       identifier, keyword, operator, or compound form}

 @item{@rhombus(#%parens) --- used for @parens}

 @item{@rhombus(#%brackets) --- used for @brackets}

 @item{@rhombus(#%braces) --- used for @braces}

 @item{@rhombus(#%quotes) --- used for @quotes}

 @item{@rhombus(#%call) --- used as an infix form when a parsed form
       is followed immediately by a @parens term}

 @item{@rhombus(#%index) --- used as an infix form when a parsed form
       is followed immediately by a @brackets term}

 @item{@rhombus(#%comp) --- used as an infix form when a
       parsed form is followed immediately by a @braces term;
       this implicit form is not bound by @rhombuslangname(rhombus)}

 @item{@rhombus(#%juxtapose) --- used as an infix form when a
       parsed form is followed immediately by a non-compound term;
       this implicit form is not bound by @rhombuslangname(rhombus)}

 @item{@rhombus(#%block) --- used for a block formed with
       @litchar{:} (by itself as a would-be parsed term); this
       implicit form is bound by @rhombuslangname(rhombus) to
       as an expression or binding to always report an error}

 @item{@rhombus(#%alts) --- used for a block formed with
       @litchar{|} (by itself as a would-be parsed term); this
       implicit form is not bound by @rhombuslangname(rhombus)}
)


@doc(
  expr.macro '#%literal $literal'
  bind.macro '#%literal $literal'
  annot.macro '#%literal $literal'
  repet.macro '#%literal $literal'
){

 Produces the value @rhombus(literal) as an expression or matches
 values that are @rhombus(==) to @rhombus(literal) as a binding; the
 annotation form reports an error that a literal is not allowed as an
 annotation. A literal also works as a @tech{repetition} of depth 0,
 which can be useful for repeating a constant alongside a repetition of
 greater depth.

 A @rhombus(literal) is any individual term other than an identifier,
 keyword, operator, parenthesized term, bracketed term, braced term,
 quoted term, block, or alternatives.

@examples(
  7
  #%literal 7
  fun only_sevens(7): "yes"
  only_sevens(7)
  ~error:
    only_sevens(8)
)

}

@doc(
  ~nonterminal:
    arg: -> ~annot
    results: -> ~annot
  ~also_meta
  expr.macro '#%parens ($expr)'
  bind.macro '#%parens ($bind)'
  annot.macro '#%parens ($annot)'
  entry_point.macro '#%parens ($entry_point)'
  immediate_callee.macro '#%parens ($immediate_callee)'
  expr.macro '#%parens ($term ... _ $term ...)'
  entry_point.macro '#%parens ($term ... _ $term ...)'
  immediate_callee.macro '#%parens ($term ... _ $term ...)'
  annot.macro '#%parens ($arg, ...) #,(@rhombus(->, ~annot)) $results'
){

 Produces the same value as @rhombus(expr), same binding as
 @rhombus(bind), and so on. Multiple expression, bindings, etc.,
 are disallowed.

 The expression case with an immediate @rhombus(_) in parentheses among
 other @rhombus(term)s is a special case for a function shorthand, and it
 takes precedence over parsing the parenthesized sequence as an
 @rhombus(expr). See @rhombus(_) for more information.

 The @tech{entry point} and @tech{immediate callee} bindings allow
 parentheses to be used around such forms, and they allow the function
 shorthand to cooperate in those positions.

 The @rhombus(#%parens, ~annot) annotation form cooperates with
 @rhombus(->, ~annot) to enable multiple argument annotations in
 parentheses. A @rhombus(->, ~annot) annotation is assumed whenever the
 parenthesized term for @rhombus(#%parens, ~annot) is followed by
 @rhombus(->, ~annot).

@examples(
  (1+2)
  #%parens (1+2)
  def (x) = 1+2
  x
)

}
