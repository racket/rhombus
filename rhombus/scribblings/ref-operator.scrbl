#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(def dots: @rhombus(..., ~bind))
@(def dots_expr: @rhombus(...))

@title(~tag: "ref-operator"){Operators}

@doc(
  defn.macro 'operator ($operator_path $binding) $maybe_res_ann:
                $option; ...
                $body
                ...'
  defn.macro 'operator ($binding $operator_path $binding) $maybe_res_ann:
                $option; ...
                $body
                ...'
  defn.macro 'operator
              | ($operator_path $binding) $maybe_res_ann:
                  $option; ...
                  $body
                  ...
              | ($binding $operator_path $binding) $maybe_res_ann:
                  $option; ...
                  $body
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

  grammar other:
    $identifier
    $operator
    ~other

  grammar assoc:
    ~left
    ~right
    ~none

){

 Binds @rhombus(operator_path) as a operator, either prefix or infix.
 The operator is function-like in the sense that it receives argument
 values. To bind an operator that is not function-like, see
 @rhombus(macro) or @rhombus(expr.macro).

 The @rhombus(maybe_res_ann) parts are the same as in @rhombus(fun)
 definitions. The operator is also a @tech{repetition} operator,
 in which case its arguments must be repetitions, and the depth of the
 resulting repetition is the maximum of the argument repetition depths.

 When an operator definition includes both a prefix and infix variant
 with @litchar{|}, the variants can be in either order.

 At the start of the operator body, @rhombus(option)s can declare
 precedence and, in the case of an infix operator, an associativity for
 the operator. Each @rhombus(option) keyword can appear at most once. In
 a precedence specification, @rhombus(~other) stands for any operator not
 otherwise mentioned.

 See @secref("namespaces") for information on @rhombus(operator_path).

@examples(
  operator (x ^^^ y):
    x +& y +& x
  "a" ^^^ "b"
  operator (x List.(^^^) y):
    x ++ y ++ x
  begin:
    import: .List open
    [1, 2] ^^^ [3]
)

}
