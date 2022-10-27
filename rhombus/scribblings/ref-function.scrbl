#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(val dots: @rhombus(..., ~bind))
@(val dots_expr: @rhombus(...))

@title(~tag: "ref-function"){Functions}

An expression followed by a parenthesized sequence of expressions is
parsed as an implicit use of the @rhombus(#{#%call}) form, which is
normally bound to implement function calls.

@dispatch_table(
  "function",
  @rhombus(Function),
  [f.map(args, ...), Function.map(f, args, ...)]
)

@doc(
  annotation.macro 'Function'
){

 Matches any function.

}


@doc(
  expr.macro '$fun_expr #{#%call} ($arg, ...)',

  grammar arg:
    $arg_expr
    $keyword: $arg_expr
    $repetition $$(@litchar{,}) $$(dots_expr)
    & $list_expr
    ~& $map_expr
){

  A function call. Each @rhombus(arg_expr) alone is a by-position
  argument, and each @rhombus(keyword: arg_expr) combination is a
  by-keyword argument.

  If the @rhombus(arg) sequence contains @rhombus(& list_expr) or
  @rhombus(repetition $$(@litchar{,}) $$(dots_expr)), then the
  elements of the list or @tech{repetition} are spliced into the
  call as separate by-position arguments.

  If the @rhombus(arg) sequence contains @rhombus(~& map_expr), then all
  the keys in the map produced by @rhombus(map_expr) must be keywords, and
  they must not overlap with the directly supplied @rhombus(keyword)s or
  keywords in maps from other @rhombus(~& map_expr) arguments. The
  keyword-value pairs are passed into the function as additional keyword
  arguments.

 @see_implicit(@rhombus(#{#%call}), @rhombus(()), "expression", ~is_infix: #true)

@examples(
  List.length([1, 2, 3]),
  List.length #{#%call} ([1, 2, 3])
)

}

@doc(
  defn.macro 'fun $identifier_path($kwopt_binding, ..., $rest, ...) $maybe_res_ann:
                $body
                ...',
  defn.macro 'fun
              | $identifier_path($binding, ..., $rest, ...) $maybe_res_ann:
                  $body
                  ...
              | ...',

  expr.macro 'fun ($kwopt_binding, ..., $rest, ...) $maybe_res_ann:
                $body
                ...',

  expr.macro 'fun
              | ($binding, ..., $rest, ...) $maybe_res_ann:
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
  
  grammar maybe_res_ann:
    :: $annotation
    -: $annotation
    $$("ϵ"),

  grammar rest:
    $repetition_binding $$(@litchar{,}) $$(dots)
    & $list_binding
    ~& $map_binding

){

 Binds @rhombus(identifier_path) as a function, or when
 @rhombus(identifier_path) is not supplied, serves as an expression that
 produces a function value.

 In addition to any annotations that may be included in argument
 bindings, when @rhombus(maybe_res_ann) is present, it provides an
 annotation for the function's result. In the case of a checked
 annotation using @rhombus(::), the function's body is @emph{not} in
 tail position with respect to a call to the function, since a check
 will be applied to the function's result.

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
Only one by-position rest binding, @rhombus(& list_binding) or
@rhombus(repetition_binding $$(@litchar{,}) $$(dots_expr)), can appear
in a @rhombus(rest) sequence.

When a @rhombus(rest) sequence contains @rhombus(~& map_binding), then
the function or function alternative accepts any number of additional
keyword arguments. The additional keywords and associated argument
values are collected into a map value to be bound to
@rhombus(map_binding).
Only one @rhombus(~& map_binding) can appear in a @rhombus(rest) sequence.

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
  callable.macro 'fun ($kwopt_binding, ..., $rest, ...) $maybe_res_ann:
                    $body
                    ...',

  callable.macro 'fun
                  | ($binding, ..., $rest, ...) $maybe_res_ann:
                      $body
                      ...
                  | ...'
){

 The callable form of @rhombus(fun, ~callable) is the same as the
 expression form of @rhombus(fun). This binding as a callable allows a
 @rhombus(fun, ~callable) form is to work and cooperate with contexts
 such as @rhombus(constructor, ~class_clause).

}


@doc(
  defn.macro 'operator ($operator_path $binding) $maybe_res_ann:
                $body
                ...',
  defn.macro 'operator ($binding $operator_path $binding) $maybe_res_ann:
                $body
                ...',
  defn.macro 'operator
              | ($operator_path $binding) $maybe_res_ann:
                  $body
                  ...
              | ($binding $operator_path $binding) $maybe_res_ann:
                  $body
                  ...',

){

 Binds @rhombus(operator_path) as an operator, either prefix or infix.
 The @rhombus(maybe_res_ann) parts are the same as in @rhombus(fun)
 definitions.

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


@doc(
  fun Function.map(f :: Function, args :: List, ...) :: List,
){

 Applies @rhombus(f) to each element of each @rhombus(args), iterating
 through the @rhombus(args) lists together, so @rhombus(f) must take as
 many arguments as the number of given @rhombus(args) lists. The result
 is a list of values, which is the result of each call to @rhombus(f) in
 order.

@examples(
  Map.values({"a": 1, "b": 2})
)

}
