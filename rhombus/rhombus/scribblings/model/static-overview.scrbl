#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~style: #'toc, ~tag: "static-info"){Static Information Model}

@deftech{Static information} is implemented in Rhombus by a set of
protocols layered on the @seclink("syntax-model"){syntax model}. The
protocols include

@itemlist(

 @item{a key--value mapping associated with and expressions or binding,
 where expression information is communicated to enclosing forms and
 binding information is communicated to references;}

 @item{macro-extensible @tech{binding forms}, where the parsing protocol
 for bindings communicates static information about an input expression
 (such as the right-hande side of a @rhombus(def) form) and static
 information about identfiers that are bound from those inputs (such as
 identifers on the left-hand side of a @rhombus(def) form); and}

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
