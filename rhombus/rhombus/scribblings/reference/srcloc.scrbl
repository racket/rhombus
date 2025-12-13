#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Source Locations}

A @deftech{source location} represents a source (typically using a
@rhombus(Path, ~annot), an optional line number (counts from 1),
optional column within the line (counts from 0), position within an
overall source (counts from 1), and optional span.

@doc(
  annot.macro 'Srcloc'
){

 Matches a source location value.
}

@doc(
  fun Srcloc(source :: Any,
             line :: maybe(PosInt),
             column :: maybe(Nat),
             position :: maybe(PosInt),
             span :: maybe(Nat))
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
  method (srcloc :: Srcloc).column() :: maybe(Nat)
  method (srcloc :: Srcloc).position() :: maybe(PosInt)
  method (srcloc :: Srcloc).span() :: maybe(Nat)
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
