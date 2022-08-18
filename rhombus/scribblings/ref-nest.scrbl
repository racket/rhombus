#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Nested Bindings}

@doc(
  defn.macro 'nest $identifier:
                $body_or_export
                ...',
  grammar body_or_export:
    $body
    $export
){

 Similar to the same @rhombus(body_or_export) sequence spliced into the
 enclosing context, but definitions within the body are not visible
 outside the body, and @rhombus(export) declarations are allowed and
 determine exports for the @rhombus(identifier) immediately after
 @rhombus(nest). An exported @rhombus(name, ~var) can be reached using
 @rhombus(identifier$$(rhombus(.))$$(rhombus(name, ~var))). The name
 @rhombus(identifier) also wth with @rhombus(import).

@examples(
  nest math:
    export: pi tau
    val pi: 3.14
    val tau: 6.28,
  math.pi,
  begin:
    import: .math open
    [pi, tau]
)

}

