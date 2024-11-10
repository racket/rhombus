#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def dots = @rhombus(..., ~bind))

@title(~tag: "function-annot"){Function Annotations}

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
    annot: ::
    list_annot: :: annot
    map_annot: :: annot
  annot.macro '$args -> $results'
  grammar args:
    $annot
    ($arg, ..., $rest_arg, ...)
    (~any)
  grammar results:
    $annot
    ($result, ..., $rest_result, ...)
    #,(@rhombus(values, ~annot))($result, ..., $rest_result, ...)
    ~any
    (~any)
  grammar arg:
    plain_arg
    named_arg
    rest_arg
  grammar plain_arg:
    $annot
    $annot = #,(@rhombus(_, ~bind))
    $keyword: $annot
    $keyword: $annot = #,(@rhombus(_, ~bind))
  grammar named_arg:
    $id #,(@rhombus(::, ~bind)) $annot
    $id #,(@rhombus(::, ~bind)) $annot = #,(@rhombus(_, ~bind))
    $keyword: $id #,(@rhombus(::, ~bind)) $annot
    $keyword: $id #,(@rhombus(::, ~bind)) $annot = #,(@rhombus(_, ~bind))
  grammar rest_arg:
    $annot #,(@litchar{,}) $ellipsis
    #,(@rhombus(&, ~bind)) $list_annot
    #,(@rhombus(~&, ~bind)) $map_annot
    #,(@rhombus(&, ~bind)) $id #,(@rhombus(::, ~bind)) $list_annot
    #,(@rhombus(~&, ~bind)) $id #,(@rhombus(::, ~bind)) $map_annot
  grammar result:
    $annot
    $id #,(@rhombus(::, ~bind)) $annot
  grammar rest_result:
    $annot #,(@litchar{,}) $ellipsis
    #,(@rhombus(&, ~bind)) $list_annot
    #,(@rhombus(&, ~bind)) $id #,(@rhombus(::, ~bind)) $list_annot
  grammar ellipsis:
    #,(dots)
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
 keyword argument. An @rhombus(arg) that ends @rhombus(= #,(@rhombus(_, ~bind))) is an
 optional argument; the default value is not specified, and it is left up
 to the called function; along the same lines, the @rhombus(annot) before
 @rhombus(= #,(@rhombus(_, ~bind))) is not applied to the argument default.

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

 When an @rhombus(arg) has an @rhombus(id) and @rhombus(::, ~bind), the
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

 An @rhombus(arg) written with @rhombus(&, ~bind) or @rhombus(~&, ~bind) stands for
 any number of by-position and by-keyword arguments, respectively.
 By-position arguments for @rhombus(&, ~bind) are gathered into a list, and
 by-keyword arguments for @rhombus(~&, ~bind) are gathered into a map whose keys
 are keywords. Alternatively, extra by-position arguments can be covered
 by an @rhombus(annot) followed by @dots. Arguments gathers with
 @rhombus(&, ~bind) or @rhombus(~&, ~bind) can be named for later reference.

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
 @rhombus((Any, ...), ~annot) or @rhombus((#,(@rhombus(&, ~bind)) Any), ~annot), which require that the
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
 to @rhombus((Any, ...), ~annot) or @rhombus((#,(@rhombus(&, ~bind)) Any), ~annot),
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
 to join multiple @rhombus(->, ~annot), but to provide a name that the converted
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
