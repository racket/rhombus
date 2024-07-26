#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "spacing"){Spacing}

@doc(~include rhombus/scribble/private/spacing: linebreak){
}

@doc(~include rhombus/scribble/private/spacing: nonbreaking){
}

@doc(~include rhombus/scribble/private/spacing: hspace){
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
