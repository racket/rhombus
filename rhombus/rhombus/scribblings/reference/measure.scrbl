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

@doc(
  fun measure.cpu_milliseconds() :: Fixnum
  fun measure.real_milliseconds() :: Flonum
  fun measure.gc_milliseconds() :: Fixnum
){

 The @rhombus(measure.cpu_milliseconds) function returns an amount of
 processor time in @tech{fixnum} milliseconds that has been consumed by
 on the underlying operating system, including both user and system time.
 The precision of the result is platform-specific, and since the result
 is a fixnum, the value increases only over a limited (though reasonably
 long) time on a 32-bit platform.

 The @rhombus(measure.real_milliseconds) function returns the number of
 milliseconds elapsed in real time since an unspecified starting time.
 Unlike @rhombus(system.milliseconds), which is sensitive to the system
 clock and may therefore retreat or advance more quickly than real time
 if the system clock is adjusted, results from
 @rhombus(measure.real_milliseconds) will always advance with real time
 within a process, but results across processes are not comparable.

 The @rhombus(measure.gc_milliseconds) function returns the amount of
 processor time in fixnum milliseconds that has been consumed by garbage
 collection so far. This time is a portion of the time reported by
 @rhombus(measure.cpu_milliseconds), and is similarly limited.

}
