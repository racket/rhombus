#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Source Locations}

@dispatch_table(
  "source location"
  @rhombus(Srcloc)
  [srcloc.source, Srcloc.source(srcloc)]
  [srcloc.line, Srcloc.line(srcloc)]
  [srcloc.column, Srcloc.column(srcloc)]
  [srcloc.position, Srcloc.position(srcloc)]
  [srcloc.span, Srcloc.span(srcloc)]
)

@doc(
  annot.macro 'Srcloc'
){

 Matches a source location value.
}

@doc(
  fun Srcloc(source,
             line :: Maybe(PosInt),
             column :: Maybe(NonnegInt),
             position :: Maybe(PosInt),
             span :: Maybe(NonnegInt))
    :: Srcloc
){

 Constructs a source location.

}

@doc(
  bind.macro 'Srcloc($source_binding,
                     $line_binding,
                     $column_binding,
                     $position_binding,
                     $span_binding)'
){

 Matches a source location where the components match the corresponding binding forms.

}

@doc(
  fun Srcloc.source(srcloc :: Srcloc)
  fun Srcloc.line(srcloc :: Srcloc) :: Maybe(PosInt)
  fun Srcloc.column(srcloc :: Srcloc) :: Maybe(NonnegInt)
  fun Srcloc.position(srcloc :: Srcloc) :: Maybe(PosInt)
  fun Srcloc.span(srcloc :: Srcloc) :: Maybe(NonnegInt)
){

 Extracts a component of a source location.

}
