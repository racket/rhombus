#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title(~tag: "implicit"){Implicit Forms}

Rhombus parsing is driven by bindings even for forms that have no
apparent name, such as a literal expression like @rhombus(7) or square
brackets like @rhombus([1, 2, 3]). In those cases without an apparent
name an implement form is used to give meaning to the term, whether in
an expression position, binding position, or other kind of position.
Here are all of the implicit forms:

@itemlist(

 @item{@rhombus(#{#%literal}) --- used for anything other than an
       identifier, keyword, operator, or compound form}

 @item{@rhombus(#{#%parens}) --- used for @rhombus(())}

 @item{@rhombus(#{#%brackets}) --- used for @rhombus([])}
 
 @item{@rhombus(#{#%braces}) --- used for @rhombus({})}

 @item{@rhombus(#{#%quotes}) --- used for @rhombus('')}

 @item{@rhombus(#{#%call}) --- used as an infix form when a parsed form
       is followed immediately by a @rhombus(()) term}

 @item{@rhombus(#{#%ref}) --- used as an infix form when a pared form
       is followed immediately by a @rhombus([]) term}

 @item{@rhombus(#{#%comp}) --- used as an infix form when a
       parsed form is followed immediately by a @rhombus({}) term;
       this implicit form is not bound by @rhombusmodname(rhombus)}

 @item{@rhombus(#{#%juxtapose}) --- used as an infix form when a
       parsed form is followed immediately by a non-compound term;
       this implicit form is not bound by @rhombusmodname(rhombus)}

 @item{@rhombus(#{#%block}) --- used for a block formed with
       @litchar{:} (by itself as a would-be parsed term); this
       implicit form is not bound by @rhombusmodname(rhombus)}

 @item{@rhombus(#{#%alts}) --- used for a block formed with
       @litchar{|} (by itself as a would-be parsed term); this
       implicit form is not bound by @rhombusmodname(rhombus)}

 @item{@rhombus(#{#%body}) --- used by forms that contain a
        @rhombus(body) sequence, such as @rhombus(begin), the body of
        @rhombus(fun), and the result part of a @rhombus(match)
        clause; the lexical context of the @litchar{:} or @litchar{|}
        that forms a block determines the @rhombus(#{#%body}) binding
        that is used}
)


@doc(
  expr.macro '#{#%literal} $literal'
  bind.macro '#{#%literal} $literal'
  annot.macro '#{#%literal} $literal'
  repet.macro '#{#%literal} $literal'
){

 Produces the value @rhombus(literal) as an expression or matches
 values that are @rhombus(==) to @rhombus(literal) as a binding; the
 annotation form reports an error that a literal is not allowed as an
 annotation. A literal also works as a @tech{repetition} of depth 0,
 which can be useful for repeating a constant alongside a repetition of
 greater depth.

 A @rhombus(literal) is any individual term other than an identifier,
 keyword, operator, parenthesized term, bracketed term, quoted term,
 or braced term.

@examples(
  7
  #{#%literal} 7
  fun only_sevens(7): "yes"
  only_sevens(7)
  ~error: only_sevens(8)
)

}

@doc(
  expr.macro '#{#%parens} ($expr)'
  bind.macro '#{#%parens} ($binding)'
  annot.macro '#{#%parens} ($annotation)'
){

@provided_also_meta()

 Produces the same value as @rhombus(expr), same binding as
 @rhombus(binding), and so on. Multiple expression, bindings, etc.,
 are disallowed.

@examples(
  (1+2)
  #{#%parens} (1+2)
  def (x): 1+2
  x
)
 
}

@doc(
  expr.macro '#{#%body}:
                $body
                ...'
){

 Returns the result of the @rhombus(body) block, which may include local
 definitions.

}
