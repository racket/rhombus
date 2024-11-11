#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Source Locations}

@doc(
  annot.macro 'Srcloc'
){

 Matches a source location value.
}

@doc(
  fun Srcloc(source :: Any,
             line :: maybe(PosInt),
             column :: maybe(NonnegInt),
             position :: maybe(PosInt),
             span :: maybe(NonnegInt))
    :: Srcloc
){

 Constructs a source location.

}

@doc(
  ~nonterminal:
    source_bind: def bind ~defn
    line_bind: def bind ~defn
    column_bind: def bind ~defn
    position_bind: def bind ~defn
    span_bind: def bind ~defn
  bind.macro 'Srcloc($source_bind,
                     $line_bind,
                     $column_bind,
                     $position_bind,
                     $span_bind)'
){

 Matches a source location where the components match the corresponding binding forms.

}

@doc(
  method (srcloc :: Srcloc).source() :: Any
  method (srcloc :: Srcloc).line() :: maybe(PosInt)
  method (srcloc :: Srcloc).column() :: maybe(NonnegInt)
  method (srcloc :: Srcloc).position() :: maybe(PosInt)
  method (srcloc :: Srcloc).span() :: maybe(NonnegInt)
){

 Extracts a component of a source location.

}


@doc(
  method (srcloc :: Srcloc).to_report_string() :: String
){

 Converts a source location into a human-readable string suitable for
 error messages and other reporting.

@examples(
  Srcloc("demo.rhm", 1, 14, 500, 10).to_report_string()
)

}
