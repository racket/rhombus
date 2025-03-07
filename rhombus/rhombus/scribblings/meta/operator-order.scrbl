#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Operator Orders}

An @deftech{operator order} declares precedence relationships to other
operator orders or a default order. An operator defined with a form such
as @rhombus(operator, ~defn) or @rhombus(macro, ~defn) can select an
operator order using @rhombus(~order), and then precedence
specifications else using the operator order's name apply to the newly
declared operator. The operator also inherits precedence declarations
and an associativity declared in its operator order, but an operator
declaration can override those.

An operator order is not specific to a @tech{space}, and operators bound
in multple spaces (such as via @rhombus(bind.macro, ~defn) or
@rhombus(annot.macro, ~defn)) can use the same set of operator orders.

@doc(
  space.transform operator_order
){

 The @tech{space} for bindings of identifiers that can be used in
 operator order positions.

}

@doc(
  ~meta
  def operator_order_meta.space :: SpaceMeta
){

 A compile-time value that identifies the same space as
 @rhombus(operator_order, ~space). See also @rhombus(SpaceMeta, ~annot).

}


@doc(
  defn.macro 'operator_order.def $id_name:
                $option
                ...'
  grammar option:
    ~stronger_than $other ...
    ~stronger_than: $other ...; ...
    ~weaker_than $other ...
    ~weaker_than: $other ...; ...
    ~same_as $other ...
    ~same_as: $other ...; ...
    ~same_on_left_as $other ...
    ~same_on_left_as: $other ...; ...
    ~same_on_right_as $other ...
    ~same_on_right_as: $other ...; ...
    ~associativity $assoc
    ~associativity: $assoc
){

 Defines @rhombus(id_name) as an @tech{operator order} with precedence
 relationships to other operation orders declared by @rhombus(option)s.
 Each @rhombus(other) in an @rhombus(option) must by either
 @rhombus(~other) or refer to a previously defined operator order; an
 @rhombus(other) cannot be an operator name.

}

@doc(
  ~nonterminal:
    order_id_name: namespace id_name ~defn
  defn.macro 'operator_order.def_set $id_name:
                $order_id_name ...
                ...'
){

 Defines @rhombus(id_name) as a shorthand for a specification that lists
 all of the individual @rhombus(order_id_name)s. The defined
 @rhombus(id_name) can be used in @rhombus(~stronger), @rhombus(~weaker),
 etc., options to declare precedence relationships, but it cannot be used
 in @rhombus(~order) (i.e., an operator has at more one operator order).

}

@doc(
  operator_order.def equivalence
  operator_order.def order_comparison
  operator_order.def_set comparison:
    equivalence
    order_comparison
){

 Precedence orders for equality operators @rhombus(==) or
 @rhombus(is_now) and ordering comparisons such as @rhombus(<) or
 @rhombus(.=). The @rhombus(comparison, ~operator_order) shorthand can be
 used to declare a relationship to both
 @rhombus(equivalence, ~operator_order) and
 @rhombus(order_comparison, ~operator_order), and declaring a
 relationship to @rhombus(order_comparison, ~operator_order) is rarely
 useful, but @rhombus(logical_negation, ~operator_order) refers
 specifically @rhombus(equivalence, ~operator_order).

}

@doc(
  operator_order.def addition:
    ~weaker_than:
      integer_division
      multiplication
      exponentiation
    ~stronger_than:
      comparison
  operator_order.def integer_division:
    ~weaker_than:
      multiplication
      exponentiation
    ~stronger_than:
      comparison
  operator_order.def multiplication:
    ~weaker_than:
      exponentiation
    ~stronger_than:
      comparison
  operator_order.def exponentiation:
    ~associativity: ~right
    ~stronger_than:
      comparison
  operator_order.def_set arithmetic:
    addition
    integer_division
    multiplication
    exponentiation
){

 Precedence orders for arithmetic operators such as @rhombus(+),
 @rhombus(mod),  @rhombus(*), and @rhombus(**).

}

@doc(
  operator_order.def logical_disjunction:
    ~weaker_than:
      comparison
      concatenation
      enumeration
      arithmetic
      logical_conjunction
      logical_negation
  operator_order.def logical_conjunction:
    ~weaker_than:
      comparison
      concatenation
      enumeration
      arithmetic
      logical_negation
  operator_order.def logical_negation:
    ~stronger_than:
      equivalence
){

 Precedence orders for operators such as @rhombus(||),
 @rhombus(&&), and @rhombus(!).

}

@doc(
  operator_order.def assignment:
    ~weaker_than: ~other
){

 Precedence orders for operators such as @rhombus(:=).

}

@doc(
  operator_order.def enumeration:
    ~weaker_than: arithmetic
){

 Precedence orders for operators such as @rhombus(..).

}

@doc(
  operator_order.def concatenation:
    ~weaker_than:
      comparison
      enumeration
      arithmetic
){

 Precedence orders for operators such as @rhombus(++) and @rhombus(+&).

}

@doc(
  operator_order.def member_access:
    ~stronger_than: ~other
){

 The precedence for an operator such as @rhombus(.) or
 @rhombus(?.).

}

@doc(
  operator_order.def pipeline:
    ~weaker_than: ~other
){

 The precedence for an operator such as @rhombus(|>) or
 @rhombus(?>).

}

@doc(
  operator_order.def bitwise_test:
    ~weaker_than:
      comparison
      bitwise_shift
      bitwise_disjunction
      bitwise_conjunction
      bitwise_negation
  operator_order.def bitwise_shift:
    ~weaker_than:
      comparison
      bitwise_disjunction
      bitwise_conjunction
      bitwise_negation
  operator_order.def bitwise_disjunction:
    ~weaker_than:
      comparison
      bitwise_conjunction
      bitwise_negation
  operator_order.def bitwise_conjunction:
    ~weaker_than:
      comparison
      bitwise_negation
  operator_order.def bitwise_negation:
    ~weaker_than:
      comparison
){

 Precedence orders for operators such as @rhombus(bits.(?)),
 @rhombus(bits.(<<)), @rhombus(bits.or), @rhombus(bits.and), and
 @rhombus(bits.not).

}
