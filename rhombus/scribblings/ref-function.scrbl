#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(val dots: @rhombus(..., ~bind))
@(val dots_expr: @rhombus(...))

@title(~tag: "ref-function"){Functions}

An expression followed by a parenthesized sequence of expressions is
parsed as an implicit use of the @rhombus(#{#%call}) form, which is
normally bound to implement function calls.

@doc(
  expr.macro '$fun_expr #{#%call} ($arg, ..., $rest, ...)',

  grammar arg:
    $arg_expr
    $keyword: $arg_expr,

  grammar rest:
    & $list_expr
    ~& $map_expr
    $repetition $$(@litchar{,}) $$(dots_expr)
){

  A function call. Each @rhombus(arg_expr) alone is a by-position
  argument, and each @rhombus(keyword: arg_expr) combination is a
  by-keyword argument.

  If the @rhombus(rest) sequence contains @rhombus(& list_expr) or
  @rhombus(repetition $$(@litchar{,}) $$(dots_expr)), then the
  elements of the list or repetition are used as additional
  by-position arguments, in order after the @rhombus(arg_expr)
  arguments.
  Only one positional rest argument is allowed in the call.

  If the @rhombus(rest) sequence contains @rhombus(~& map_expr), then
  all the keys in @rhombus(map_expr) must be keywords, and they must
  not overlap with the directly supplied @rhombus(keyword)s.
  The keyword-value pairs are passed into the function as additional
  keyword arguments.
  Only one keyword rest argument is allowed in the call.

 @see_implicit(@rhombus(#{#%call}), @rhombus(()), "expression", ~is_infix: #true)

@examples(
  List.length([1, 2, 3]),
  List.length #{#%call} ([1, 2, 3])
)

}

@doc(
  defn.macro 'fun $identifier_path($kwopt_binding, ..., $rest, ...) $maybe_result_annotation:
                $body
                ...',
  defn.macro 'fun
              | $identifier_path($binding, ..., $rest, ...) $maybe_result_annotation:
                  $body
                  ...
              | ...',

  expr.macro 'fun ($kwopt_binding, ..., $rest, ...) $maybe_result_annotation:
                $body
                ...',

  expr.macro 'fun
              | ($binding, ..., $rest, ...) $maybe_result_annotation:
                  $body
                  ...
              | ...',

  grammar identifier_path:
    $identifier
    $identifier_path . $identifier,

  grammar kwopt_binding:
    $binding
    $keyword: $binding
    $binding $$(@tt{=}) $default_expr
    $keyword: $binding $$(@tt{=}) $default_expr,
  
  grammar maybe_result_annotation:
    :: $annotation
    -: $annotation
    $$("ϵ"),

  grammar rest:
    & $list_binding
    ~& $map_binding
    $repetition_binding $$(@litchar{,}) $$(dots)
){

 Binds @rhombus(identifier_path) as a function, or when
 @rhombus(identifier_path) is not supplied, serves as an expression that
 produces a function value.

 See @secref("namespaces") for information on @rhombus(identifier_path).

@examples(
  fun f(x):
    x+1,
  f(0),
  fun List.number_of_items(l):
    List.length(l),
  List.number_of_items(["a", "b", "c"])
)

@examples(
  ~label: #false,
  val identity: fun (x): x,
  identity(1),
)

@examples(
  ~label: #false,
  fun curried_add(x):
    fun(y):
      x + y,
  curried_add(1)(2)
)

 When @litchar{|} is not used, then arguments can have default values.
 Bindings for earlier arguments are visible in each
 @rhombus(default_expr), but not bindings for later arguments;
 accordingly, matching actions are interleaved with binding effects (such
 as rejecting a non-matching argument) left-to-right, except that the
 result of a @rhombus(default_expr) is subject to the same constraints
 imposed by annotations and patterns for its argument as an explicitly
 supplied argument would be.

@examples(
  fun f(x, y = x+1):
    [x, y],
  f(0),
  f(0, 2),
)

@examples(
  ~label: #false,
  fun transform([x, y],
                ~scale: factor = 1,
                ~dx: dx = 0,
                ~dy: dy = 0):
    [factor*x + dx, factor*y + dy],
  transform([1, 2]),
  transform([1, 2], ~dx: 7),
  transform([1, 2], ~dx: 7, ~scale: 2)
)

 When alternatives are specified with multiple @litchar{|} clauses, the
 alternatives are tried in order when the function is called. The
 alternatives can differ by number of arguments as well as keywords,
 annotations, and binding patterns.

@examples(
  fun | hello(name):
          "Hello, " +& name
      | hello(first, last):
          hello(first +& " " +& last),
  hello("World"),
  hello("Inigo", "Montoya"),
)

@examples(
  ~label: #false,
  fun | is_passing(n :: Number): n >= 70
      | is_passing(pf :: Boolean): pf,
  is_passing(80) && is_passing(#true)
)

When a @rhombus(rest) sequence contains @rhombus(& list_binding) or
@rhombus(repetition_binding $$(@litchar{,}) $$(dots)), then the
function or function alternative accepts any number of additional
by-position arguments.
For @rhombus(& list_binding), the additional arguments are collected
into a list value, and that list value is bound to the
@rhombus(list_binding).
For @rhombus(repetition_binding $$(@litchar{,}) $$(dots)), each
variable in @rhombus(repetition_binding) is bound to a repetition that
repeats access to that piece of each additional argument.
Only one positional rest argument is allowed in the function
alternative.

When a @rhombus(rest) sequence contains @rhombus(~& map_binding), then
the function or function alternative accepts any number of additional
keyword arguments. The additional keywords and associated argument
values are collected into a map value to be bound to
@rhombus(map_binding).

@examples(
  ~label: #false,
  fun
  | is_sorted([]): #true
  | is_sorted([head]): #true
  | is_sorted([head, next, & tail]):
      head <= next && is_sorted([next, & tail]),
  is_sorted([1, 2, 3, 3, 5]),
  is_sorted([1, 2, 9, 3, 5])
)

@examples(
  ~label: #false,
  fun
  | is_sorted([]): #true
  | is_sorted([head]): #true
  | is_sorted([head, next, tail, ...]):
      head <= next && is_sorted([next, tail, ...]),
  is_sorted([1, 2, 3, 3, 5]),
  is_sorted([1, 2, 9, 3, 5])
)

}

@doc(
  defn.macro 'operator ($operator_path $binding) $maybe_result_annotation:
                $body
                ...',
  defn.macro 'operator ($binding $operator_path $binding) $maybe_result_annotation:
                $body
                ...',
  defn.macro 'operator
              | ($operator_path $binding) $maybe_result_annotation:
                  $body
                  ...
              | ($binding $operator_path $binding) $maybe_result_annotation:
                  $body
                  ...',

){

 Binds @rhombus(operator_path) as an operator, either prefix or infix.
 The @rhombus(maybe_result_annotation) parts are the same as in
 @rhombus(fun) definitions.

 When an operator definition includes both a prefix and infix variant
 with @litchar{|}, the variants can be in either order.

 See @secref("namespaces") for information on @rhombus(operator_path).

@examples(
  operator (x ^^^ y):
    x +& y +& x,
  "a" ^^^ "b",
  operator (x List.(^^^) y):
    x ++ y ++ x,
  begin:
    import: .List open
    [1, 2] ^^^ [3]
)
  
}
