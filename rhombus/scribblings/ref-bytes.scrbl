#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Byte Strings}

A @deftech{byte string} is a sequence of bytes (i.e., integers between 0
and 255 inclusive). A byte string works with map-referencing @brackets
to access a byte via @rhombus(#%ref). A byte string also works with the
@rhombus(++) operator to append bytes strings. A byte string can be used
as @tech{sequence}, in which case it supplies its bytes in order.

@dispatch_table(
  "byte string"
  @rhombus(Bytes)
  [bstr.length(), Bytes.length(bstr)]
)


@doc(
  annot.macro 'Bytes'
){

  Matches byte strings.

}


@doc(
  fun Bytes.length(bstr :: Bytes) : NonnegInt
){

 Returns the number of bytes in @rhombus(bstr).

@examples(
  Bytes.length(#"hello")
  #"hello".length()
)

}
