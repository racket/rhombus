#lang rhombus/scribble/manual
@(import:
    "racket_names.rkt" open
    rhombus/scribblings/nonterminal open
    meta_label:
      rhombus open
      rhombus/rkt_obj)

@title(~tag: "rkt_obj"){Racket Classes and Objects from Rhombus}

@docmodule(rhombus/rkt_obj)

The @rhombusmodname(rhombus/rkt_obj) library provides Rhombus syntactic
forms for instantiating Racket classes and working with instances.

@doc(
  expr.macro 'rkt_obj.new $expr($keyword: $expr, ...)'
){

 Rhombus syntax for Racket's @racket_new, which instantiates a Racket
 class with keyword initialization arguments.

 The Racket @racket_new form uses identifiers for ``keyword'' arguments,
 but only because it predates the addition of keywords to Racket. Along
 the same lines as noted in @secref("data"), a Racket ``keyword''
 initialization argument such as @tt{fast?} can be written with
 @rhombus(rkt_obj.new) as @rhombus(~#{fast?}).

}

@doc(
  expr.macro 'rkt_obj.make_object $expr($expr, ...)'
){

 Rhombus syntax for Racket's @racket_make_object, which instantiates a
 Racket class with by-position initialization arguments.

}

@doc(
  expr.macro 'rkt_obj.send $expr . $name($arg, ...)'
  grammar arg
  | $expr
  | $keyword: $body
){

 Rhombus syntax for Racket's @racket_send, which calls a method of an
 object.

}

@doc(
  ~nonterminal:
    object_expr: block expr
    class_expr: block expr
  expr.macro '$object_expr rkt_obj.is_a $class_expr'
){

 Rhombus operator version of Racket's @racket_is_a.

}
