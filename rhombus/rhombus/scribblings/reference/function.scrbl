#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/unsafe open)

@(def dots = @rhombus(..., ~bind))
@(def dots_expr = @rhombus(...))

@title(~tag: "ref-function"){Functions}

A @deftech{function} is normally written with the @rhombus(fun, ~defn) definition
form or @rhombus(fun) expression form, but see also
@rhombus(Callable, ~class).

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
                  $case_option; ...
                  $body
                  ...
              | ...'
  defn.macro 'fun $id_name $maybe_res_annot
              | $id_name $case_maybe_kw:
                  $case_option; ...
                  $body
                  ...
              | ...'
  defn.macro 'fun $id_name $maybe_res_annot:
                $option; ...
              | $id_name $case_maybe_kw:
                  $case_option; ...
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
    case_option

  grammar name_option:
    ~name $op_or_id_name
    ~name: $op_or_id_name
    who_option

  grammar case_option:
    ~unsafe:
      $unsafe_body
      ...
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
    #,(@rhombus(&, ~bind)) $list_bind
    #,(@rhombus(~&, ~bind)) $map_bind

  grammar ellipsis:
    #,(dots)

){

 Binds @rhombus(id_name) as a function in the @rhombus(expr, ~space)
 @tech(~doc: meta_doc){space}. Note that when @rhombus(id_name) is not supplied,
 @rhombus(fun, ~expr) is an expression form instead of a definition form.

 The first case for the definition form shown above is subsumed by the
 second case, but it describes the most common form of function
 definition.

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

When a @rhombus(rest) sequence contains @rhombus(#,(@rhombus(&, ~bind)) list_bind) or
@rhombus(repet_bind #,(@litchar{,}) #,(dots)), then the
function or function alternative accepts any number of additional
by-position arguments.
For @rhombus(#,(@rhombus(&, ~bind)) list_bind), the additional arguments are collected
into a list value, and that list value is bound to the
@rhombus(list_bind). Static information associated by @rhombus(List)
is propagated to @rhombus(list_bind).
For @rhombus(repet_bind #,(@litchar{,}) #,(dots)), each
variable in @rhombus(repet_bind) is bound to a repetition that
repeats access to that piece of each additional argument.
Only one by-position rest binding, @rhombus(#,(@rhombus(&, ~bind)) list_bind) or
@rhombus(repet_bind #,(@litchar{,}) #,(dots)), can appear
in a @rhombus(rest) sequence.

When a @rhombus(rest) sequence contains @rhombus(#,(@rhombus(~&, ~bind)) map_bind), then
the function or function alternative accepts any number of additional
keyword arguments. The additional keywords and associated argument
values are collected into an immutable map value to be bound to
@rhombus(map_bind). Static information associated by @rhombus(Map) is
propagated to @rhombus(map_bind).
Only one @rhombus(#,(@rhombus(~&, ~bind)) map_bind) can appear in a @rhombus(rest) sequence.

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
 reporting annotation failures on arguments and results, and it is also
 used when printing the function. Otherwise, @rhombus(id_name) is used.
 Supplying @rhombus(~name) does not change the name that is bound, which
 is always the initial @rhombus(id_name).

 When @rhombus(~who) is present as an @rhombus(option),
 @rhombus(name_option), or @rhombus(case_option), the given @rhombus(id)
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

 When @rhombus(~unsafe) is present as a @rhombus(case_option), an
 alternative implementation is provided that may be used for calls to the
 function in an unsafe context (see @rhombus(use_unsafe)). The
 alternative implementation as a @rhombus(unsafe_body) sequence is
 intended to produce the same result as the case's main @rhombus(body)
 sequence, but it may skip annotation checks. Annotation on the function
 arguments are not checked (and no conversions are applied) for unsafe
 calls, even when the annotation is accoated with an argument via
 @rhombus(::, ~bind) as opposed to @rhombus(:~). The
 @rhombus(unsafe_body) sequence itself is in unsafe mode. Argument
 bindings are restricted when a @rhombus(~unsafe) option is present: the
 binding must be either an immediate identifier, an identifier annotated
 with @rhombus(::, ~bind) or @rhombus(:~, ~bind), or a single-identifier
 binding form that accepts any argument, such as @rhombus(_, ~bind).
 Keyword are allowed only when a function does not have multiple cases,
 and @rhombus(&, ~bind) or @rhombus(~&) arguments are not allowed. (A
 final repetition argument using @rhombus(ellipsis) is allowed.) When a
 multi-case function has @rhombus(~unsafe) for any case, then it must
 have @rhombus(~unsafe) for all cases. If the function has a result
 annotation, it is not checked (and no conversion is applied) for the
 unsafe result.

@examples(
  ~defn:
    import rhombus/fixnum
    fun fixnum_sum(ns :: List.of(Fixnum)) :: Fixnum:
      ~unsafe:
        // assume all fixnums and that result fits:
        for values(res = 0) (n in ns):
          res fixnum.(+) n
      // use generic arithmetic, check fixnum result at end:
      for values(res = 0) (n in ns):
          res + n
  ~repl:
    fixnum_sum([1, 2, 3])
    ~error:
      :
        fixnum_sum([1, 2, 3 ** 100]) // seriously bad in unsafe mode
)

}


@doc(
  ~nonterminal:
    name_option: fun ~defn
    case_maybe_kw_opt: fun ~defn
    case_maybe_kw: fun ~defn
    maybe_res_annot: fun ~defn
    who_option: fun ~defn

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
){

 Produces a function value. Note that when an @rhombus(id_name) appears
 after @rhombus(fun, ~defn) or after a @vbar, @rhombus(fun, ~defn) is
 a definition form instead of an expression form.

 The first case shown for the expression form
 above is subsumed by the second case, but it describes the most common
 forms of function expression.

@examples(
  ~defn:
    def identity = fun (x): x
  ~repl:
    identity(1)
  ~defn:
    fun curried_add(x):  // definition
      fun (y):           // expression
        x + y
  ~repl:
    curried_add(1)(2)
)

 Optional and keyword argument support for a @rhombus(fun, ~expr)
 expression is the same as for a @rhombus(fun, ~defn). The
 @rhombus(fun, ~expr) expression form does not support a @rhombus(~doc)
 option.

 See also @rhombus(_) for information about function shorthands using
 @rhombus(_). For example, @rhombus((_ div _)) is a shorthand for
 @rhombus(fun (x, y): x div y).

}


@doc(
  entry_point.macro 'fun $args_and_body'

  immediate_callee.macro 'fun $args_and_body'
){

 The @tech{entry point} and @tech(~doc: meta_doc){immediate callee} forms of
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

 A binding as an @tech(~doc: meta_doc){immediate callee} allows a form to work and
 cooperate with contexts such as the right-hand side of the @rhombus(|>)
 operator to improve static-information propagation.

}


@doc(
  method (f :: Function).map(args0 :: List, args :: List, ...)
    :: List
  method (f :: Function).for_each(args0 :: List, args :: List, ...)
    :: Void
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
  method (f :: Function).arity()
    :: values(Int, List.of(Keyword), maybe(List.of(Keyword)))
  method (f :: Function).reduce_arity(
    by_pos_mask :: Int,
    required_kws :: List.of(Keyword),
    allowed_kws :: maybe(List.of(Keyword)),
    ~name: name :: Symbol = Function.name(f),
    ~realm: realm :: Symbol = #'rhombus
  ) :: Function
){

 The @rhombus(Function.arity) method reports information about the
 arguments that a function accepts:

@itemlist(

 @item{A bitwise @rhombus(by_pos_mask) for the number(s) of by-position
  arguments that a function accepts. If the function accepts
  @rhombus(n, ~var) arguments, then
  @rhombus(by_pos_mask bits.and (1 bits.(<<) @rhombus(n, ~var))) is
  non-zero.}

 @item{A @rhombus(required_kws) list of keyword arguments that the
  function requires, where the keywords are sorted by @rhombus(<).}

 @item{An @rhombus(allowed_kws) as @rhombus(#false) if the function
  accepts any keyword, or a list of keyword arguments that the function
  accepts, including the required keywords, where the keywords are sorted
  by @rhombus(<).}

)

 The @rhombus(Function.arity) method returns a function that is like
 @rhombus(f), but with its allowed arguments more restricted. The given
 @rhombus(by_pos_mask) must have only bits that are set of the mask of
 @rhombus(f), the given @rhombus(required_kws) must contain at least the
 keywords that @rhombus(f) requires and must contain a subset of the
 keywords that @rhombus(f) accepts, and @rhombus(allowed_kws) must
 contain a subset of the keywords that @rhombus(f) allows (so it can be
 @rhombus(#false) only if @rhombus(f) accepts any keyword).

@examples(
  ~repl:
    Function.arity(fun (x): 0)
    Function.arity(fun | (x): 0 | (x, y): 1)
    Function.arity(fun (x, ...): 0)
    Function.arity(fun (x, ~y: y = 1): 0)
    Function.arity(Function.arity)
    Function.arity(fun (& args, ~& kws): 0)
  ~repl:
    def f = fun | (x): 0 | (x, y): 1
    def g = Function.reduce_arity(f, 2, [], [])
    g(1)
    f(1, 2)
    ~error:
      g(1, 2)
)

}


@doc(
  method (f :: Function).name() :: maybe(Symbol)
  method (f :: Function).realm() :: Symbol
  method (f :: Function).rename(
    name :: Symbol,
    ~realm: realm :: Symbol = #'rhombus
  ) :: Function
){

 The @rhombus(Function.name) method reports a function's name, which is
 used for printing the function. The @rhombus(Function.realm) method
 reports a function's realm, which is used for error message adjusting.
 The @rhombus(Function.rename) method returns a function that is like
 @rhombus(f), but with the given name. A function's realm may affect
 how the function name is printed in different contexts.

}


@doc(
  ~nonterminal:
    pre_fun_expr: block expr
    post_fun_expr: block expr
  expr.macro '$post_fun_expr compose $pre_fun_expr'
  expr.macro '$post_fun_expr ∘ $pre_fun_expr'
  expr.macro '$post_fun_expr compose_values $pre_fun_expr'
){

 Produces a function that that composes the result functions of
 @rhombus(pre_fun_expr) and @rhombus(post_fun_expr) so that

@rhombusblock(
  (post_fun_expr compose pre_fun_expr)(#,(@rhombus(arg, ~var)), ...)
)

 is the same as

@rhombusblock(
  post_fun_expr(pre_fun_expr(#,(@rhombus(arg, ~var)), ...))
)

 With the @rhombus(compose) operator or its alias @rhombus(∘),
 @rhombus(pre_fun_expr) must produce a single result when called. With
 @rhombus(compose), @rhombus(pre_fun_expr) can produce multiple values,
 and the values are all delivered as arguments to
 @rhombus(post_fun_expr).

 In static mode (see @rhombus(use_static)) when using @rhombus(compose)
 or @rhombus(∘), if @rhombus(post_fun_expr) with has a known arity, it
 must accept a single argument. In all modes and including
 @rhombus(compose_values), static information about the arity of
 @rhombus(pre_fun_expr) is propagated the the operation's result, and
 static information about the call result of @rhombus(post_fun_expr) is
 propagated to the operation's result.

@examples(
  ~repl:
    def nstr = to_string compose math.abs
    nstr(-100)
  ~repl:
    def partition_to_list = List compose_values List.partition
    partition_to_list([1, -2, -3, 4], (_ > 0))
)

}
