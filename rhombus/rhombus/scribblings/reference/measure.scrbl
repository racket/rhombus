#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm".body
    meta_label:
      rhombus/measure
      rhombus/memory)

@title(~style: #'toc, ~tag: "measure"){Measuring Time and Space}

@docmodule(rhombus/measure)

@(~version_at_least "8.14.0.4")

@doc(
  expr.macro 'measure.time:
                $option; ...
                $body
                ...'

  grammar option:
    ~gc
){

 Returns the same value(s) as the @rhombus(body) sequence, but first
 prints to the current output port the amount of time elapsed during the
 evaluation of @rhombus(body) sequence.

 If the @rhombus(~gc) option is specified, then @rhombus(memory.gc) is
 called before the @rhombus(body) sequence, and time required for the
 garbage collection is not counted as part of the reported time.

@examples(
  import rhombus/measure
  measure.time:
    for all:
      each i: 0..10000000
      "useless"
)

}

@doc(
  expr.macro 'measure.memory:
                $body
                ...'
){

 Returns the same value(s) as the @rhombus(body) sequence, but first
 prints to the current output port information about allocation during
 the evaluation of @rhombus(body) sequence.

@examples(
  import rhombus/measure
  measure.memory:
    for all:
      each i: 0..10
      [1, 2]
)

}

@doc(
  ~nonterminal:
    option: measure.time

  expr.macro 'measure.time_and_memory:
                $option; ...
                $body
                ...'
){

 Combines @rhombus(measure.time) and @rhombus(measure.memory).

}
