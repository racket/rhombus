#lang scribble/rhombus/manual
@(import:
   "util.rhm" open
   "common.rhm" open)

@(def ann_eval: make_rhombus_eval())

@demo(
  ~eval: ann_eval
  ~hidden:
    class Posn(x, y)
    import: rhombus/meta open
)

@title(~tag: "annotation-macro"){Annotations and Static Information}

Annotations produce @tech{static information} when used with the @rhombus(::)
binding or expression operator. Similar to binding macros, which can
either be simple expansions or use lower-level machines, an annotation
macro can use lower-level machinery to explicitly produce static
information or manipulate static information produced by subannotation
forms.

The @rhombus(annot.macro) form
defines an annotation. In the simplest case, the expansion of an
annotation can be another annotation:

@demo(
  ~eval: ann_eval
  ~defn:
    annot.macro 'AlsoPosn': 'Posn'
  ~repl:
    Posn(1, 2) :: AlsoPosn
)

Note that @rhombus(annot.macro) defines only an annotation. To make
@rhombus(AlsoPosn) also a binding operator, you can use @rhombus(bind.macro):

@demo(
  ~eval: ann_eval
  ~defn:
    bind.macro 'AlsoPosn ($x, $y)':
      'Posn($x, $y)'
  ~repl:
    def AlsoPosn(x, y): Posn(1, 2)
    x
)

To define an annotation with explicit control over the associated
predicate, use @rhombus(annot_meta.pack_predicate). This
implementation if @rhombus(IsPosn) creates a new predicate that uses
@rhombus(is_a) with @rhombus(Posn), so it checks whether something is a
@rhombus(Posn) instance, but it doesn't act as a @rhombus(Posn)-like
binding form or constructor:

@demo(
  ~eval: ann_eval
  ~defn:
    annot.macro 'IsPosn':
      annot_meta.pack_predicate('fun (x): x is_a Posn')
  ~repl:
    fun get_x(p :: IsPosn): Posn.x(p)
    get_x(Posn(1, 2))
    ~error: get_x(10)
)

The @rhombus(annot_meta.pack_predicate) takes an optional second
argument, which is static information to associate with uses of the
annotation. Static information (the second argument to
@rhombus(annot_meta.pack_predicate)) is a parenthesized sequence of
parenthesized two-group elements, where the first group in each element
is a key and the second element is a value.

A value for @rhombus(statinfo_meta.dot_provider_key) should be a syntax object
naming a dot-provider transformer. So, if we want to define a
@rhombus(Vector) annotation that is another view on @rhombus(Posn) where
the ``fields'' are @rhombus(angle) and @rhombus(magnitude) instead of
@rhombus(x) and @rhombus(y), we start with an annotation definition that
refers to a @rhombus(vector_dot_provider) that we will define:

@demo(
  ~eval: ann_eval
  ~defn:
    annot.macro 'Vector':
      annot_meta.pack_predicate('fun (x): x is_a Posn',
                                '(($(statinfo_meta.dot_provider_key),
                                   vector_dot_provider))')
)

A dot-provider transformer is defined using @rhombus(dot.macro). A
dot-provider transformer always receives three parts, which are the
parsed expression to the left of the dot, the dot itself, and an
identifier to the right of the dot. The dot provider associated with
@rhombus(Vector) access @rhombus(angle) and @rhombus(magnitude)
``fields'' by calling helper functions:

@demo(
  ~eval: ann_eval
  ~defn:
    dot.macro 'vector_dot_provider $left . $right':
      match right
      | 'angle': 'vector_angle($left)'
      | 'magnitude': 'vector_magnitude($left)'
  ~defn:
    fun vector_angle(Posn(x, y)): math.atan(y, x)
    fun vector_magnitude(Posn(x, y)): math.sqrt(x*x + y*y)
  )

With those pieces in place, a binding using @rhombus(:: Vector) creates
a dot provider:

@demo(
  ~eval: ann_eval
  ~defn:
    def vec :: Vector: Posn(3, 4)
  ~repl:
    vec.angle
    vec.magnitude
)

A macro can explicitly associate static information with an expression
by using @rhombus(statinfo_meta.wrap):

@demo(
  ~eval: ann_eval
  ~defn:
    expr.macro 'or_zero $p $tail ...':
      def expansion: '$p || Posn(0,0)'
      statinfo_meta.wrap(expansion,
                         '(($(statinfo_meta.dot_provider_key),
                            vector_dot_provider))')
  ~repl:
    or_zero(Posn(3, 4)).magnitude
    or_zero(#false).magnitude
)

A similar effect could be acheived by expanding to
@rhombus('($p || Posn(0,0)) :: Vector'), but for better or worse, this
implementation of @rhombus(or_zero) omits an extra predicate on the
result of the expression, and instead claims that it will always work as
a @rhombus(Vector).

If a name is otherwise bound but has no static information associated
with the binding, the @rhombus(statinfo.macro) form can associate
static information. In the following example, @rhombus(zero) is defined
without a result annotation, but @rhombus(statinfo.macro) is used to
associate static information to @rhombus(zero) using
@rhombus(statinfo_meta.call_result_key). The value for
@rhombus(statinfo_meta.call_result_key) should be static information itself,
so we use @rhombus(statinfo_meta.pack) to pack it from a syntax-object
representation.

@aside{The @rhombus(statinfo_meta.wrap) and
 @rhombus(annot_meta.pack_predicate) functions automatically pack for
 you, because they expect a syntax object that represents static
 information. The overall right-hand side result for
 @rhombus(statinfo.macro) is similarly automatically packed.}

@rhombusblock(
  fun zero(): Posn(0, 0)
  statinfo.macro 'zero':
     '($(statinfo_meta.call_result_key),
       $(statinfo_meta.pack('(($(statinfo_meta.dot_provider_key),
                               vector_dot_provider))')))'
  zero().magnitude  // prints 0
)

The @rhombus(statinfo.macro) form expects @rhombus('') containing an
identifier or operator, not a more funciton-like pattern, because it's
mean to define a constant association between a name and static
information.


A dot provider like @rhombus(vector_dot_provider) normally would not be
exposed outside of the module that implements @rhombus(Vector). But if a
dot provider is used directly, then it receives itself as the left
argument:

@rhombusblock(
  dot.macro 'hello $left . $right':
    match right
    | 'english': '"Hi"'
    | 'chinese': '"你好"'
    | 'spanish': '"Hola"'

  hello.chinese  // prints "你好"
  hello.spanish  // prints "Hola"
  hello.english  // prints "Hello"
  // hello.greek  // would be a compile-time match error
)

A direct use like this makes sense when a dot provider is not associated
with a run-time value. Attempting to use @rhombus(hello) in an
expression position other than before a @rhombus(.) results in a static
error.

@close_eval(ann_eval)

