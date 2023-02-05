#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(def dots: @rhombus(..., ~bind))
@(def dots_expr: @rhombus(...))

@title(~tag: "ref-operator"){Operators}

@doc(
  defn.macro 'operator $op_case'
  defn.macro 'operator 
              | $op_case
              | ...'
  defn.macro 'operator $op_or_id_path:
                $option; ...
                match
                | $op_case
                | ...'

  grammar op_case:  
    ($op_or_id_path $binding_term) $implementation
    ($binding_term $op_or_id_path $binding_term) $implementation
    ($binding_term $op_or_id_path) $implementation

  grammar implementation:
    $maybe_res_ann:
      $option; ...
      $body
      ...

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

  grammar op_or_id_path:
    $operator_path
    $identifier_path
){

 Binds @rhombus(op_or_id_path) as a operator, either
 prefix, infix, postfix, or a combination. See @secref("namespaces") for
 information on @rhombus(operator_path) and @rhombus(identifier_path).

 The operator is function-like in the sense that it receives argument
 values. (To bind an operator that is not function-like, see
 @rhombus(macro) or @rhombus(expr.macro).) Each argument is specified by
 a binding that must be written as a single shrubbery term that is not an
 operator in the shrubbery sense; parentheses can be used around other
 binding forms for arguments. If two identifier terms appear within the
 parentheses that follow @rhombus(operator), a prefix operator is
 defined using the first identifier as its name.

 The @rhombus(maybe_res_ann) parts are the same as in @rhombus(fun)
 definitions. The new operator is also bound a @tech{repetition}
 operator, in which case its arguments must be repetitions, and the depth
 of the resulting repetition is the maximum of the argument repetition
 depths.

 When multple cases are provided via @vbar, an operator can be defined
 as both prefix and infix or prefix and postfix (but not infix and
 postfix). The prefix cases and infix/postfix cases can be mixed in any
 order, but when the operator is used as a prefix or infix/postfix
 operator, cases are tried in the relative order that they are written.
 Similar to the @rhombus(form) form, multiple cases can be placed under
 @rhombus(match) within a block, with the advantage that a result
 annotation can be written once for all cases.

 At the start of an operator body, @rhombus(option)s can declare
 precedence and, in the case of an infix operator, an associativity for
 the operator. Each @rhombus(option) keyword can appear at most once. In
 a precedence specification, @rhombus(~other) stands for any operator not
 otherwise mentioned. When multiple cases are povided using @vbar, then
 only the first prefix case and the first infix/postfix case can supply
 options; alternatively, when cases are nested under @rhombus(match),
 then options that apply to all cases can be written before
 @rhombus(match). Options can appear both before @rhombus(match) and in
 individual clauses, as long as all precedence and all associatvity
 options are in one or the other.

@examples(
  ~defn:
    operator (x ^^^ y):
      x +& y +& x
  ~repl:
    "a" ^^^ "b"
    1 ^^^ 2
  ~defn:  
    operator (x wings y):
      x +& y +& x
  ~repl:
    "a" wings "b"
  ~defn:      
    operator ((x :: String) ^^^ (y :: String)):
      x +& y +& x
  ~repl:
    "a" ^^^ "b"
    ~error:
      1 ^^^ 2
  ~defn:      
    operator (x List.(^^^) y):
      x ++ y ++ x
  ~repl:
    begin:
      import: .List open
      [1, 2] ^^^ [3]
  ~defn:
    operator
    | (x ^^^ y):
        x +& y +& x
    | (^^^ y):
        "--" +& y +& "--"
  ~repl:
    "a" ^^^ "b"
    ^^^ "b"
  ~defn:
    operator ^^^:
      ~weaker_than: +
      match
      | (x ^^^ y):
          x +& y +& x
      | (^^^ y):
          "--" +& y +& "--"
  ~repl:
    1 ^^^ 2 + 3
    ^^^ 4 + 5
)

}
