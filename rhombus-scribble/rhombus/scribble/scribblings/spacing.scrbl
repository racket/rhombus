#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "spacing"){Spacing}

@doc(~include rhombus/scribble/private/spacing: linebreak){

 Forces a line break within a @tech{paragraph}.

}

@doc(~include rhombus/scribble/private/spacing: nonbreaking){

 Equivalent to calling @rhombus(elem) with a @tech{style} that prevents
 automatic line breaks within the element's content.

}

@doc(~include rhombus/scribble/private/spacing: hspace){

 Creates an @rhombus(Element, ~annot) that takes up horizontal
 whitespace equivalent to @rhombus(n) fixed-width characters.

}

@doc(
  def #{~} :: Element
  def #{~-~} :: Element
  def #{?-} :: Element
  def #{._} :: Element
  def #{.__} :: Element
){

 Constants for find-grained control of spacing and word breaks:

@itemlist(

 @item{@rhombus(#{~}) --- a non-breaking space}

 @item{@rhombus(#{~-~}) --- a non-breaking hyphen}

 @item{@rhombus(#{?-}) --- a hyphen that appears only if it's at a line
   break}

 @item{@rhombus(#{._}) --- a period for ending an abbreviation in the
   middle of a sentence}

 @item{@rhombus(#{.__}) --- a period for ending a sentence}

)

}
