#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Functions}

@doc[
  defn.macro 'fun $identifier($kwopt_binding, ...) $maybe_result_annotation:
                $body
                ...',
  defn.macro 'fun
              | $identifier($binding, ...) $maybe_result_annotation:
                  $body
                  ...
              | ...',

  expr.macro 'fun ($kwopt_binding, ...) $maybe_result_annotation:
                $body
                ...',

  expr.macro 'fun
              | ($arg_binding, ...) $maybe_result_annotation:
                  $body
                  ...
              | ...',
  
  grammar kwopt_binding:
    $binding
    $keyword: $binding
    $binding $$(@tt{=}) $default_expr
    $keyword: $binding $$(@tt{=}) $default_expr,
  
  grammar maybe_result_annotation:
    :: $annotation
    -: $annotation
    $$("ϵ")
]{

 Binds @rhombus[identifier] as a function, or when @rhombus[identifier]
 is not supplied, serves as an expression that produces a function value.

@examples[
  fun f(x):
    x+1,
  f(0),
]

@examples[
  ~label: #false,
  val identity: fun (x): x,
  identity(1),
]

@examples[
  ~label: #false,
  fun curried_add(x):
    fun(y):
      x + y,
  curried_add(1)(2)
]

 When @litchar{|} is not used, then arguments can have keywords and/or
 default values. Bindings for earlier arguments are visible in each
 @rhombus[default_expr], but not bindings for later arguments;
 accordingly, matching actions are interleaved with binding effects (such
 as rejecting a non-matching argument) left-to-right, except that the
 result of a @rhombus[default_expr] is subject to the same constraints
 imposed by annotations and patterns for its argument as an explicitly
 supplied argument would be.

@examples[
  fun f(x, y = x+1):
    [x, y],
  f(0),
  f(0, 2),
]

@examples[
  ~label: #false,
  fun transform([x, y],
                ~scale: factor = 1,
                ~dx: dx = 0,
                ~dy: dy = 0):
    [factor*x + dx, factor*y + dy],
  transform([1, 2]),
  transform([1, 2], ~dx: 7),
  transform([1, 2], ~dx: 7, ~scale: 2)
]

 When alternatives are specified with multiple @litchar{|} clauses, the
 alternatives are tried in order when the function is called. The
 alternatives can differ by number of arguments as well as annotations
 and binding patterns.

@examples[
  fun | hello(name):
          "Hello, " & name
      | hello(first, last):
          hello(first & " " & last),
  hello("World"),
  hello("Inigo", "Montoya"),
]

@examples[
  ~label: #false,
  fun | is_passing(n :: Number): n >= 70
      | is_passing(pf :: Boolean): pf,
  is_passing(80) && is_passing(#true)
]

}

@doc[
  defn.macro 'operator ($opname $arg_binding) $maybe_result_annotation:
                $body
                ...',
  defn.macro 'operator ($arg_binding $opname $arg_binding) $maybe_result_annotation:
                $body
                ...',
  defn.macro 'operator
              | ($opname $arg_binding) $maybe_result_annotation:
                  $body
                  ...
              | ($arg_binding $opname $arg_binding) $maybe_result_annotation:
                  $body
                  ...',
]{

 Binds @rhombus[opname] as an operator, either prefix or infix. The
 @rhombus[arg_binding] and @rhombus[maybe_result_annotation] parts are
 the same as in @rhombus[function] definitions.

 When an operator definition includes both a prefix and infix variant
 with @litchar{|}, the variants can be in either order.

}
