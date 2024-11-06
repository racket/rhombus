#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/doc.DocSpec)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title(~tag: "ref-function"){Functions}

An expression followed by a parenthesized sequence of expressions is
parsed as an implicit use of the @rhombus(#%call) form, which is
normally bound to implement function calls.

@dispatch_table(
  "function"
  Function
  f.map(args, ...)
  f.for_each(args, ...)
)

@doc(
  annot.macro 'Function'
  annot.macro 'Function.of_arity($expr_or_keyword, $expr_or_keyword, ...)'
  grammar expr_or_keyword:
    $expr
    $keyword
){

 The @rhombus(Function, ~annot) annotation matches any function.

 The @rhombus(Function.of_arity, ~annot) variant requires that each
 @rhombus(expr) produces a nonnegative integer, and then function must
 accept that many by-position arguments. The function must require only
 keywords that are provided as @rhombus(keyword)s, and it must accept all
 @rhombus(keyword)s that are listed. Each @rhombus(keyword) must be
 distinct.

 See also @rhombus(->, ~annot) and @rhombus(Function.all_of, ~annot).

@margin_note_block{Due to the current limitation in the function arity
 protocol, a function must require an exact set of keywords across all
 arities, even though Rhombus multi-case @rhombus(fun)s allow
 non-uniform keyword arguments in different cases. In a Rhombus
 multi-case @rhombus(fun), the required set of keywords is the
 ``intersection'' of required keywords in all cases.}

@examples(
  math.cos is_a Function
  math.cos is_a Function.of_arity(1)
  math.atan is_a Function.of_arity(1, 2)
  (fun (x, ~y): #void) is_a Function.of_arity(1, ~y)
  (fun (x, ~y): #void) is_a Function.of_arity(1)
  (fun (x, ~y = 0): #void) is_a Function.of_arity(1)
  (fun (x, ~y = 0): #void) is_a Function.of_arity(1, ~y, ~z)
)

}

@doc(
  ~nonterminal:
    id: block
    fun_expr: block expr
    arg_expr: block expr
    repet_arg: #%call arg
    list_expr: block expr
    map_expr: block expr

  expr.macro '$fun_expr #%call ($arg, ...)'
  repet.macro '$fun_expr #%call ($repet_arg, ...)'
  expr.macro '$fun_expr #%call ($arg, ..., _, $arg, ...)'

  grammar arg:
    $arg_expr
    $keyword
    $keyword: $arg_expr
    $keyword: $body; ...
    $repet #,(@litchar{,}) $ellipses
    & $list_expr
    ~& $map_expr
  grammar ellipses:
    $ellipsis
    $ellipses #,(@litchar{,}) $ellipsis
  grammar ellipsis:
    #,(dots_expr)
){

  A function call. Each @rhombus(arg_expr) alone is a by-position
  argument, and each @rhombus(keyword: arg_expr) combination is a
  by-keyword argument. Function calls can serve as repetitions,
  where @rhombus(repet_arg) is like @rhombus(arg), but with repetitions
  in place of expressions.

  If the @rhombus(arg) sequence contains @rhombus(& list_expr) or
  @rhombus(repet #,(@litchar{,}) ellipses), then the
  elements of the list or @tech{repetition} are spliced into the
  call as separate by-position arguments.

  If the @rhombus(arg) sequence contains @rhombus(~& map_expr), then all
  the keys in the immutable map produced by @rhombus(map_expr) must be keywords, and
  they must not overlap with the directly supplied @rhombus(keyword)s or
  keywords in maps from other @rhombus(~& map_expr) arguments. The
  keyword-value pairs are passed into the function as additional keyword
  arguments.

 Parallel to @rhombus(fun), a single @rhombus(keyword) as an
 @rhombus(arg) is equivalent to the form @rhombus(keyword: id)
 @margin_note{This kind of double duty of a single @rhombus(keyword)
 is sometimes referred to as ``punning.''}
 where @rhombus(id) is composed from @rhombus(keyword) by taking its
 string form and lexical context.

 The case with an immediate @rhombus(_) group among other
 @rhombus(arg)s is a special case for a function shorthand, and it takes
 precedence over parsing the @rhombus(_) as an @rhombus(arg). See
 @rhombus(_) for more information.

 See also @rhombus(use_static).

 @see_implicit(@rhombus(#%call), @parens, "expression", ~is_infix: #true)

@examples(
  List.length([1, 2, 3])
  List.length #%call ([1, 2, 3])
)

}


@doc(
  ~nonterminal:
    arg_expr: block expr
    fun_expr: block expr
  expr.macro '$arg_expr |> $immediate_callee'
  expr.macro '$arg_expr |> $fun_expr'
){

 The @rhombus(|>) operator applies its second argument as a function to
 its first argument. That is, @rhombus(arg_expr |> fun_expr) is
 equivalent to @rhombus(fun_expr(arg_expr)), except that
 @rhombus(arg_expr) is evaluated before @rhombus(fun_expr), following
 the textual order.
 @margin_note{This form is known as a ``pipeline.'' Accordingly,
 @rhombus(|>) is the ``pipe-forward'' operator.}
 The conversion is performed syntactically so that
 static checking and propagation of static information may apply, but
 @rhombus(arg_expr) and @rhombus(fun_expr) are parsed as expressions before the
 conversion. The @rhombus(|>) operator declares weaker precedence than
 all other operators.

 Alternatively, the right-hand side can be an @tech{immediate callee},
 in which case the static information for @rhombus(arg_expr) is
 supplied to it.

 A @rhombus(_) function shorthand can be especially useful with
 @rhombus(|>).

@examples(
  [1, 2, 3] |> List.length
  [3, 1, 2]
    |> List.sort(_)
    |> ([0] ++ _ ++ [100])
    |> to_string.map(_)
)

}

@doc(
  ~nonterminal:
    id: block
    default_expr: block expr
    default_body: block body
    list_expr: block expr
    map_expr: block expr
    list_bind: def bind ~defn
    map_bind: def bind ~defn
    repet_bind: def bind ~defn

  defn.macro 'fun $id_name($bind, ...):
                $body
                ...'
  defn.macro 'fun $id_name $case_maybe_kw_opt:
                $option; ...
                $body
                ...'
  defn.macro 'fun
              | $id_name $case_maybe_kw:
                  $who_option; ...
                  $body
                  ...
              | ...'
  defn.macro 'fun $id_name $maybe_res_annot
              | $id_name $case_maybe_kw:
                  $who_option; ...
                  $body
                  ...
              | ...'
  defn.macro 'fun $id_name $maybe_res_annot:
                $option; ...
              | $id_name $case_maybe_kw:
                  $who_option; ...
                  $body
                  ...
              | ...'

  expr.macro 'fun ($bind, ...):
                $name_option; ...
                $body
                ...'
  expr.macro 'fun $case_maybe_kw_opt:
                $name_option; ...
                $body
                ...'

  expr.macro 'fun $maybe_res_annot
              | $case_maybe_kw:
                  $who_option; ...
                  $body
                  ...
              | ...'
  expr.macro 'fun $maybe_res_annot:
                $name_option; ...
              | $case_maybe_kw:
                  $who_option; ...
                  $body
                  ...
              | ...'

  grammar case_maybe_kw_opt:
    ($bind_maybe_kw_opt, ..., $rest, ...) $maybe_res_annot

  grammar case_maybe_kw:
    ($bind_maybe_kw, ..., $rest, ...) $maybe_res_annot

  grammar option:
    ~doc
    ~doc:
      $desc_body
      ...
    name_option

  grammar name_option:
    ~name $op_or_id_name
    ~name: $op_or_id_name
    who_option

  grammar who_option:
    ~who $id
    ~who: $id

  grammar bind_maybe_kw_opt:
    $bind
    $keyword: $bind
    $bind = $default_expr
    $bind: $default_body; ...
    $keyword: $bind = $default_expr
    $keyword: $bind: $default_body; ...
    $keyword
    $keyword = $default_expr

  grammar bind_maybe_kw:
    $bind
    $keyword: $bind
    $keyword

  grammar maybe_res_annot:
    #,(@rhombus(::, ~bind)) $annot
    #,(@rhombus(:~, ~bind)) $annot
    #,(@rhombus(::, ~bind)) #,(@rhombus(values, ~annot))($annot, ...)
    #,(@rhombus(:~, ~bind)) #,(@rhombus(values, ~annot))($annot, ...)
    #,(@rhombus(::, ~bind)) ($annot, ...)
    #,(@rhombus(:~, ~bind)) ($annot, ...)
    #,(epsilon)

  grammar rest:
    $repet_bind #,(@litchar{,}) $ellipsis
    & $list_bind
    ~& $map_bind

  grammar ellipsis:
    #,(dots)

){

 Binds @rhombus(id_name) as a function in the @rhombus(expr, ~space)
 @tech{space}, or when
 @rhombus(id_name) is not supplied, serves as an expression that
 produces a function value.

 The first case shown for the definition and expression forms shown
 above are subsumed by the second case, but they describe the most common
 forms of function definitions and expressions.

@examples(
  ~defn:
    fun f(x):
      x+1
  ~repl:
    f(0)
  ~defn:
    fun List.number_of_items(l):
      List.length(l)
  ~repl:
    List.number_of_items(["a", "b", "c"])
  ~defn:
    def identity = fun (x): x
  ~repl:
    identity(1)
  ~defn:
    fun curried_add(x):
      fun (y):
        x + y
  ~repl:
    curried_add(1)(2)
)

 When @vbar is not used, then arguments can have default values
 as specified after a @rhombus(=) or in a block after the argument name.
 Bindings for earlier arguments are visible in each
 @rhombus(default_expr) or @rhombus(default_body), but not bindings for later arguments;
 accordingly, matching actions are interleaved with binding effects (such
 as rejecting a non-matching argument) left-to-right, except that the
 result of a @rhombus(default_expr) is subject to the same constraints
 imposed by annotations and patterns for its argument as an explicitly
 supplied argument would be. An argument form @rhombus(keyword) or
 @rhombus(keyword = default_expr) is equivalent to the form
 @rhombus(keyword: id) or @rhombus(keyword: id = default_expr) where
 @rhombus(id) is composed from @rhombus(keyword) by taking its
 string form and lexical context.
 A @rhombus(::) or @rhombus(:~) is not allowed in @rhombus(default_expr),
 unless it is nested in another term, since that might be misread or
 confused as an annotation in @rhombus(bind) for an identifier; for similar
 reasons, @rhombus(bind) and @rhombus(default_expr) cannot contain an
 immediate @rhombus(=).

 A constraint not reflected in the grammar is that all optional
 by-position arguments must follow all required by-position arguments.
 To define a function that works otherwise, use the @vbar form.

@examples(
  ~defn:
    fun f(x, y = x+1):
      [x, y]
  ~repl:
    f(0)
    f(0, 2)
  ~defn:
    fun transform([x, y],
                  ~scale: factor = 1,
                  ~dx: dx = 0,
                  ~dy: dy = 0):
      [factor*x + dx, factor*y + dy]
  ~repl:
    transform([1, 2])
    transform([1, 2], ~dx: 7)
    transform([1, 2], ~dx: 7, ~scale: 2)
  ~defn:
    ~error:
      fun invalid(x = 1, y):
        x+y
  ~defn:
    fun
    | valid(y): valid(1, y)
    | valid(x, y): x+y
  ~repl:
    valid(2)
    valid(3, 2)
)

 When alternatives are specified with multiple @vbar clauses, the
 clauses can be provided immediately after @rhombus(fun) or after the
 name and a @rhombus(maybe_res_annot) as described further below.

@examples(
  ~defn:
    fun
    | hello(name):
        "Hello, " +& name
    | hello(first, last):
        hello(first +& " " +& last)
  ~repl:
    hello("World")
    hello("Inigo", "Montoya")
  ~defn:
    fun
    | is_passing(n :: Number): n >= 70
    | is_passing(pf :: Boolean): pf
  ~repl:
    is_passing(80) && is_passing(#true)
)

When a @rhombus(rest) sequence contains @rhombus(& list_bind) or
@rhombus(repet_bind #,(@litchar{,}) #,(dots)), then the
function or function alternative accepts any number of additional
by-position arguments.
For @rhombus(& list_bind), the additional arguments are collected
into a list value, and that list value is bound to the
@rhombus(list_bind). Static information associated by @rhombus(List)
is propagated to @rhombus(list_bind).
For @rhombus(repet_bind #,(@litchar{,}) #,(dots)), each
variable in @rhombus(repet_bind) is bound to a repetition that
repeats access to that piece of each additional argument.
Only one by-position rest binding, @rhombus(& list_bind) or
@rhombus(repet_bind #,(@litchar{,}) #,(dots_expr)), can appear
in a @rhombus(rest) sequence.

When a @rhombus(rest) sequence contains @rhombus(~& map_bind), then
the function or function alternative accepts any number of additional
keyword arguments. The additional keywords and associated argument
values are collected into an immutable map value to be bound to
@rhombus(map_bind). Static information associated by @rhombus(Map) is
propagated to @rhombus(map_bind).
Only one @rhombus(~& map_bind) can appear in a @rhombus(rest) sequence.

@examples(
  ~defn:
    fun
    | is_sorted([] || [_]):
        #true
    | is_sorted([head, next, & tail]):
        head <= next && is_sorted([next, & tail])
  ~repl:
    is_sorted([1, 2, 3, 3, 5])
    is_sorted([1, 2, 9, 3, 5])
  ~defn:
    fun
    | is_sorted([] || [_]):
        #true
    | is_sorted([head, next, tail, ...]):
        head <= next && is_sorted([next, tail, ...])
  ~repl:
    is_sorted([1, 2, 3, 3, 5])
    is_sorted([1, 2, 9, 3, 5])
)

 When @rhombus(maybe_res_annot) is present, it provides an annotation for
 the function's result, but only for the corresponding case if a
 @rhombus(maybe_res_annot) is present in a multi-case function written with
 @(vbar). In the case of a checked annotation using @rhombus(::), the
 function's body is @emph{not} in tail position with respect to a call to
 the function, since a check will be applied to the function's result.
 When @rhombus(maybe_res_annot) is present for a function declared with
 cases afterward, a @rhombus(maybe_res_annot) applies to all cases, in addition to any
 @rhombus(maybe_res_annot) supplied for a specific case. A
 @rhombus(maybe_res_annot) that has a parenthesized sequence of
 @rhombus(annot)s (with our without @rhombus(values)) describes
 multiple result values with an annotation for each individual result.

@examples(
  ~defn:
    fun hello :: String
    | hello(name):
        "Hello, " +& name
    | hello():
        #false
  ~repl:
    hello("World")
    ~error:
      hello()
  ~defn:
    fun things_to_say :: (String, String)
    | things_to_say():
        values("Hi", "Bye")
    | things_to_say(more):
        values("Hi", "Bye", more)
  ~repl:
    things_to_say()
    ~error:
      things_to_say("Nachos")
)

 When @rhombus(~doc) is present as an @rhombus(option) and @rhombus(fun) is used in a
 declaration context, then a @rhombus(doc, ~datum) submodule splice is
 generated with @rhombusmodname(rhombus/doc) as the submodule's language.
 In the submodule, @rhombus(id_name) is defined as a
 @rhombus(DocSpec, ~annot) value to record the function's arguments and
 result annotation (if any). If a @rhombus(maybe_res_annot) is present
 with @rhombus(:~, ~bind), it is converted to @rhombus(::, ~bind) in the
 recorded function shape. Tools such as Rhombus Scribble can import
 the submodule to extract the recorded information.

 When @rhombus(~name) is present as an @rhombus(option) or
 @rhombus(name_option), the given @rhombus(op_or_id_name) is used for
 reporting annotaton failures on arguments and results, and it is also
 used when printing the function. Otherwise, @rhombus(id_name) is used.
 Supplying @rhombus(~name) does not change the name that is bound, which
 is always the initial @rhombus(id_name).

 When @rhombus(~who) is present as an @rhombus(option),
 @rhombus(name_option), or @rhombus(who_option), the given @rhombus(id)
 is bound to a symbol form of the function name---that is, to the symbol
 form of the defined @rhombus(id_name) or the @rhombus(op_or_id_name)
 provided with @rhombus(~name). A @rhombus(~who) binding is particularly
 useful as a symbol that can be provided to @rhombus(error). The
 @rhombus(id) for @rhombus(~who) is bound after the declaration, which
 means that it is either outside multiple cases (and potentially shadowed
 by arguments in those cases) or inside of a single case (where it
 potentially shadows argument bindings).

@examples(
  ~defn:
    fun trivial(x :: Int):
      ~name: easy
      ~who: who
      if x == 0
      | error(~who: who, "zero")
      | x
  ~repl:
    trivial(1)
    trivial
    ~error:
      trivial("one")
    ~error:
      trivial(0)
)

 See also @rhombus(_) for information about function shorthands using
 @rhombus(_). For example, @rhombus((_ div _)) is a shorthand for
 @rhombus(fun (x, y): x div y).

}


@doc(
  entry_point.macro 'fun $args_and_body'

  immediate_callee.macro 'fun $args_and_body'
){

 The @tech{entry point} and @tech{immediate callee} forms of
 @rhombus(fun, ~entry_point) have the same syntax and behavior as the
 expression form of @rhombus(fun).

 A binding as an @deftech{entry point} allows a form to work and cooperate
 with contexts such as @rhombus(constructor, ~class_clause) that
 syntactically require a function. That is, an entry point is a syntactic
 concept. Its corresponding run-time representation is normally a function,
 but an entry point may need to be manipulated statically, such as adding
 an extra argument to make it serve as a method.
 Besides @rhombus(fun, ~entry_point), the @rhombus(macro, ~entry_point) form is
 also bound as entry point.

 A binding as an @tech{immediate callee} allows a form to work and
 cooperate with contexts such as the right-hand side of the @rhombus(|>)
 operator to improve static-information propagation.

}


@doc(
  ~nonterminal:
    annot: ::
    list_annot: :: annot
    map_annot: :: annot
  ~literal: = _ :: values
  annot.macro '$args -> $results'
  grammar args:
    $annot
    ($arg, ..., $rest_arg, ...)
    (~any)
  grammar results:
    $annot
    ($result, ..., $rest_result, ...)
    #,(@rhombus(values, ~annot)) ($result, ..., $rest_result, ...)
    ~any
    (~any)
  grammar arg:
    plain_arg
    named_arg
    rest_arg
  grammar plain_arg:
    $annot
    $annot = _
    $keyword: $annot
    $keyword: $annot = _
  grammar named_arg:
    $id :: $annot
    $id :: $annot = _
    $keyword: $id :: $annot
    $keyword: $id :: $annot = _
  grammar rest_arg:
    $annot #,(@litchar{,}) $ellipsis
    & $list_annot
    ~& $map_annot
    & $id :: $list_annot
    ~& $id :: $map_annot
  grammar result:
    $annot
    $id :: $annot
  grammar rest_result:
    $annot #,(@litchar{,}) $ellipsis
    & $list_annot
    & $id :: $list_annot
  grammar ellipsis:
    #,(dots_expr)
){

 A @tech(~doc: guide_doc){converter annotation} that is immediately satisfied by a
 function that has a compatible argument count and keyword arguments.
 When a function converted by the annotation is called, then the argument
 annotations are applied to the actual arguments, and the result
 annotations are applied to the results. An error is reported if the
 number of actual results does not match the number of result
 annotations.

@examples(
  ~repl:
    def f :: Real -> Int = (fun (x): x)
    f(1)
    ~error:
      f("hello")
    ~error:
      f(1.5)
  ~repl:
    ~error:
      def f2 :: (Int, Int) -> Int = (fun (x): x)
)

 An @rhombus(arg) that starts with a @rhombus(keyword) represents a
 keyword argument. An @rhombus(arg) that ends @rhombus(= _) is an
 optional argument; the default value is not specified, and it is left up
 to the called function; along the same lines, the @rhombus(annot) before
 @rhombus(= _) is not applied to the argument default.

@examples(
  ~repl:
    def g :: (Int, Int, ~mode: Symbol) -> Int:
      fun (x, y, ~mode: mode):
        (x + y) * (if mode == #'minus | -1 | 1)
    g(1, 2, ~mode: #'minus)
    ~error:
      g(1, 2, ~mode: 3)
  ~repl:
    def g2 :: (Int, Int, ~mode: Symbol = _) -> Int:
      fun (x, y, ~mode: mode = "plus"):
        (x + y) * (if mode == #'minus | -1 | 1)
    g2(1, 2)
)

 When an @rhombus(arg) has an @rhombus(id) and @rhombus(::), the
 argument is named for use in later argument annotations, including
 result annotations. As in @rhombus(fun), each name refers to the
 argument after any conversion implied by its annotation.

@examples(
  def both :: (x :: String, satisfying(fun (y): x < y)) -> List:
    fun (x, y): [x, y]
  both("apple", "banana")
  ~error:
    both("apple", "aardvark")
)

 An @rhombus(arg) written with @rhombus(&) or @rhombus(~&) stands for
 any number of by-position and by-keyword arguments, respectively.
 By-position arguments for @rhombus(&) are gathered into a list, and
 by-keyword arguments for @rhombus(~&) are gathered into a map whose keys
 are keywords. Alternatively, extra by-position arguments can be covered
 by an @rhombus(annot) followed by @dots_expr. Arguments gathers with
 @rhombus(&) or @rhombus(~&) can be named for later reference.

@examples(
  ~repl:
    (fun (x, y, z, ...): 0) :: (Int, Int) -> Int
    ~error:
      (fun (x, y, z, ...): 0) :: (Int, Int, ...) -> Int
  ~repl:
    def k :: (~& kws :: Map) -> satisfying(fun (r):
                                             r.length() == kws.length()):
      fun (~& kws):
        kws.keys()
    k(~a: 1, ~b: 2, ~c: 3)
)

 If @rhombus(args) is @rhombus((~any)), then no constraint is placed on
 the function arguments. Note that @rhombus((~any)) is different than
 @rhombus((Any, ...)) or @rhombus((& Any)), which require that the
 function accept any number of arguments. The arity of the converted
 function is the same as the original function.

@examples(
  def m :: (~any) -> Int:
    fun (x, y = x): x + y
  m(1)
  m(1, 2)
  ~error:
    m(1.0)
  m is_a Function.of_arity(3)
)

 Result annotations are analogous to argument annotations, except that
 keywords cannot be used for results. A multiple-annotation result
 sequence in parentheses can be preceded optionally with
 @rhombus(values, ~annot). Note that using @rhombus(Any, ~annot) as the result
 annotation implies a check that the converted function produces a single
 result when it is called. In the special case that the result annotation
 sequence is @rhombus(~any), @rhombus((~any)), or equivalent
 to @rhombus((Any, ...), ~annot) or @rhombus((& Any), ~annot),
 then a call to the original function converted by the @rhombus(->, ~annot)
 annotation is a tail call with respect to the converting wrapper.

@examples(
  def n_values :: (Int) -> (Any, ...):
    fun (n):
      values(& 0..n)
  n_values(1)
  n_values(3)
)

 To accept multiple argument annotations in parentheses,
 @rhombus(->, ~annot) relies on help from @rhombus(#%parens, ~annot).
 Relying on @rhombus(#%parens, ~annot) for non-annotation to the left of
 @rhombus(->, ~annot) is why @rhombus(~any) for an argument must be in
 parentheses.

 See also @rhombus(Function.all_of, ~annot), which can be used not only
 to join multiple @rhombus(->), but to provide a name that the converted
 function uses for reporting failed annotation checks.

}

@doc(
  ~nonterminal:
    arrow_annot: :: annot
  annot.macro 'Function.all_of($annot_or_name, ...)'
  grammar annot_or_name:
    $arrow_annot
    ~name:
      $body
      ...
){

 Creates an annotation that is satisfied by a function that satisfies
 every @rhombus(arrow_annot), each of which should correspond to a
 @rhombus(->, ~annot) annotation (or one that is ultimately defined by
 expansion to @rhombus(->, ~annot)).

 The difference between using @rhombus(Function.all_of, ~annot) to
 combine the @rhombus(arrow_annot)s and using @rhombus(&&, ~annot) is
 that @rhombus(&&, ~annot) would effectively apply every
 @rhombus(arrow_annot) to every call of the function, while
 @rhombus(Function.all_of, ~annot) selects only the first
 @rhombus(arrow_annot) whose argument annotations are satisfied by the
 supplied arguments for each call to the function.

 If a @rhombus(~name) form is among the @rhombus(annot_or_name)s, it can
 appear only once. The @rhombus(body) forms under @rhombus(~name) are
 evaluated only when an error is to be reported, and the result must
 satisfy @rhombus(error.Who, ~annot). The @rhombus(~name) clause is
 removed from the textual representation of
 @rhombus(Function.all_of, ~annot) when reporting a failure to match the
 overall annotation. The textual representation of
 @rhombus(Function.all_of, ~annot) is further reduced to
 @rhombus(arrow_annot) when only one is provided (with or without
 @rhombus(~name)).

@examples(
  ~repl:
    def mutable saved = 0
    def f :: Function.all_of(() -> Int,
                             Int -> Void):
      fun | (): saved
          | (v): saved := v
    f(1)
    f()
    saved := #false
    ~error:
      f()
  ~repl:
    def f :: Function.all_of(Int -> Int,
                             String -> String):
      fun (x): x
    f(1)
    f("apple")
    ~error:
      f(#'apple)
  ~repl:
    fun build(filter :: Function.all_of(Int -> Int,
                                        String -> String,
                                        ~name: "filter for build")):
      [filter(1), filter("apple")]
    ~error:
      build(fun (x): "apple")
)

}

@doc(
  fun Function.map(f :: Function,
                   args0 :: List, args :: List, ...)
    :: List,
  fun Function.for_each(f :: Function,
                        args0 :: List, args :: List, ...)
    :: Void,
){

 Applies @rhombus(f) to each element of each @rhombus(args) (including @rhombus(args0)), iterating
 through the @rhombus(args) lists together, so @rhombus(f) must take as
 many arguments as the number of given @rhombus(args) lists. For
 @rhombus(Function.map), the result is a list containing the result of
 each call to @rhombus(f) in order. For @rhombus(Function.for_each), the
 result is @rhombus(#void), and the result of each call to @rhombus(f) is
 ignored.

@examples(
  Function.map((_ + _), [1, 2, 3], [4, 5, 6])
  (_ + _).map([1, 2, 3], [4, 5, 6])
  println.for_each([1, 2, 3])
)

}

@doc(
  fun Function.pass(& _, ~& _) :: Void
){

 Accepts any arguments and returns @rhombus(#void).

 In a call @rhombus(Function.pass(#,(@rhombus(arg, ~var)), ...)), if
 every @rhombus(arg, ~var) is either a simple expression or
 repetition, and if the implicit @rhombus(#%call, ~datum) form is the
 default @rhombus(#%call), no intermediate @tech{lists} are
 produced. This makes @rhombus(Function.pass) suitable for
 ``side-effecting'' repetitions, as an alternative to @rhombus(for).

@examples(
  Function.pass()
  Function.pass(
    "a",
    ~weird: "call",
    & ["these", "are", "ignored"],
    ~& {#'~and: "thrown"},
    "away",
  )
  block:
    let [[x, ...], ...] = [[1, 2, 3], [4, 5]]
    // a good alternative to `for`
    Function.pass(println(x), ..., ...)
)

}


@doc(
  interface Callable
){

@provided_interface_only()

 An interface that a class can implement (publicly or privately) to make
 instances of the class callable as functions. The interface has one
 abstract method, which must be overridden to implement the behavior of
 function calls:

@itemlist(

 @item{@rhombus(call) --- receives the arguments that are passed to the
  instance that is called as a function, and the method's result is the
  result of the function call.}

)

@examples(
  ~defn:
    class Posn(x, y):
      private implements Callable
      private override method call(dx, dy):
        Posn(x + dx, y + dy)
  ~repl:
    def p = Posn(1, 2)
    p
    p(3, 4)
)

}
