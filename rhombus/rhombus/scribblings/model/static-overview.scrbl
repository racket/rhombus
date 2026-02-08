#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~style: #'toc, ~tag: "static-info"){Type Model}

Rhombus is not a statically typed language, but it has a @deftech{type}
model in the sense of facilities for connecting expressions with the
values those expressions will produce. This connection allows a field
access to be resolved at compile time to a field of a particular class,
for example, instead of waiting until run time to discover the target
object's class. The main distinction between this capability and a
statically typed language, at least in the usual sense, is that Rhombus
offers only limited guarantees that static predictions about values will
be correct. Rhombus is a safe language,@margin_note{Although Rhombus is
 safe by default, unsafe facilities or the use of
 @rhombus(pragma ~unsafe, ~decl) can opt into unsafe mode.} because
run-time checks will enforce predictions where the compiler cannot prove
that they will hold, but limited guarantees mean that the checks can
fail.

The connection between run-time values and syntactic forms and run-time
values is implemented in Rhombus by a set of protocols layered on the
@seclink("syntax-model"){syntax model}:

@itemlist(

 @item{@deftech{static information} as a key--value mapping associated
 with an expression or a binding, where expression information is
 communicated to enclosing forms, and binding information is communicated
 to uses of bound names;}

 @item{macro-extensible @tech{binding forms}, where the parsing protocol
 for bindings communicates static information about an input expression
 (such as the right-hand side of a @rhombus(def) form) and static
 information about identifiers that are bound from those inputs (such as
 identifiers on the left-hand side of a @rhombus(def) form); and}

 @item{@tech{annotations}, which pair a run-time predicate or converter
 with static information in a way that can be plugged into both
 expression and binding forms.}

)
 
@local_table_of_contents()

@include_section("static-info.scrbl")
@include_section("static-info-keys.scrbl")
@include_section("static-info-rule.scrbl")
@include_section("annotation-macro.scrbl")
@include_section("bind-macro-protocol.scrbl")
