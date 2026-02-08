#lang rhombus/scribble/manual
@(import:
   "common.rhm" open)

@(def ann_eval = make_rhombus_eval())
@examples(
  ~eval: ann_eval
  ~hidden:
    class Posn(x, y)
    import:
      rhombus/meta open
)

@title(~tag: "annotation-macro"){Annotations and Low-Level Protocol}

An @deftech{annotation} pairs a run-time predicate or converter with
static information. For example, the annotation @rhombus(Int, ~annot)
encapsulates a run-time predicate that can be applied using
@rhombus(is_a) or @rhombus(::), and it provides static information about
@rhombus(<) ordering. Annotations are recognized by various expression
and binding forms, such as the @rhombus(::) expression form,
@rhombus(::, ~bind) binding form, or @rhombus(fun) expression form (with
its optional result accnotation); those forms extract the components of
an annotation to apply in their expansions.

A @deftech{predicate annotation} has a predicate that accepts or rejects
values as satisfying the annotation. As a special case, an
@deftech{always-satisfied annotation} such as @rhombus(Any, ~annot) is
one where the predicate always returns true. A @deftech{converter
 annotation} includes a predicate that may reject some values, but also a
conversion function that potentially replaces a given value with a
converted value. A converter annotation's static information applies to
converted values, but a converter annotation may also include static
information that applies to any input (before conversion) that satisfies
the annotation's predicate.

New annotations can be defined as macros with @rhombus(annot.macro).
With annotation constructors such as @rhombus(&&, ~annot),
@rhombus(satisfying, ~annot), and @rhombus(converting, ~annot), most
annotation macros can be implemented by rewriting into existing
annotation forms as shown in @secref(~doc: guide_doc, "bind-macro"). The
@rhombus(annot.macro) form also supports a low-level protocol. A
macro opts into the low-level protocol by returning a result built with
@rhombus(annot_meta.pack_predicate) or @rhombus(annot_meta.pack_converter).

A simple predicate annotation can be implemented with just
@rhombus(annot_meta.pack_predicate). The following
implementation of @rhombus(IsPosn) creates a new predicate that uses
@rhombus(is_a) with @rhombus(Posn), so it checks whether something is a
@rhombus(Posn) instance, but it doesn't act as a @rhombus(Posn)-like
binding form or constructor:

@examples(
  ~eval: ann_eval
  ~defn:
    annot.macro 'IsPosn':
      annot_meta.pack_predicate('fun (x): x is_a Posn')
  ~repl:
    fun get_x(p :: IsPosn):
      Posn.x(p)
    get_x(Posn(1, 2))
    ~error:
      get_x(10)
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

@examples(
  ~eval: ann_eval
  ~defn:
    annot.macro 'Vector':
      annot_meta.pack_predicate(
        'fun (x): x is_a Posn',
        '(($statinfo_meta.dot_provider_key,
           vector_dot_provider))'
      )
)

A dot-provider transformer is defined using @rhombus(dot.macro). A
dot-provider transformer always receives three parts, which are the
parsed expression to the left of the dot, the dot itself, and an
identifier to the right of the dot. The dot provider associated with
@rhombus(Vector) access @rhombus(angle) and @rhombus(magnitude)
``fields'' by calling helper functions:

@examples(
  ~eval: ann_eval
  ~defn:
    dot.macro 'vector_dot_provider $left . $right':
      match right
      | 'angle': 'vector_angle($left)'
      | 'magnitude': 'vector_magnitude($left)'
  ~defn:
    fun vector_angle(Posn(x, y)):
      math.atan(y, x)
    fun vector_magnitude(Posn(x, y)):
      math.sqrt(x*x + y*y)
  )

With those pieces in place, a binding using @rhombus(:: Vector) creates
a dot provider:

@examples(
  ~eval: ann_eval
  ~defn:
    def vec :: Vector = Posn(3, 4)
  ~repl:
    vec.angle
    vec.magnitude
)

A macro can explicitly associate static information with an expression
by using @rhombus(statinfo_meta.wrap):

@examples(
  ~eval: ann_eval
  ~defn:
    expr.macro 'or_zero $p $tail ...':
      statinfo_meta.wrap(
        '$p || Posn(0,0)',
        '(($statinfo_meta.dot_provider_key,
           vector_dot_provider))'
      )
  ~repl:
    or_zero(Posn(3, 4)).magnitude
    or_zero(#false).magnitude
)

A similar effect could be achieved by expanding to
@rhombus('($p || Posn(0,0)) :: Vector'), but for better or worse, this
implementation of @rhombus(or_zero) omits an extra predicate on the
result of the expression, and instead claims that it will always work as
a @rhombus(Vector).

If a name is otherwise bound but has no static information associated
with the binding, the @rhombus(statinfo.macro) form can associate
static information. In the following example, @rhombus(zero) is defined
without a result annotation, @margin_note{The example uses @rhombus(def),
plus @rhombus(dynamic) to ensure that @rhombus(def) cannot infer any
static information to bind automatically.} but @rhombus(statinfo.macro) is used to
associate static information to @rhombus(zero) using
@rhombus(statinfo_meta.call_result_key). The value for
@rhombus(statinfo_meta.call_result_key) should be static information itself,
so we use @rhombus(statinfo_meta.pack) to pack it from a syntax-object
representation.

@margin_note_block{The @rhombus(statinfo_meta.wrap) and
 @rhombus(annot_meta.pack_predicate) functions automatically pack for
 you, because they expect a syntax object that represents static
 information. The overall right-hand side result for
 @rhombus(statinfo.macro) is similarly automatically packed.}

@examples(
  ~eval: ann_eval
  ~defn:
    def zero:
      dynamic(fun ():
                Posn(0, 0))
    statinfo.macro 'zero':
      '(($statinfo_meta.call_result_key,
         $(statinfo_meta.pack(
             '(($statinfo_meta.dot_provider_key,
                vector_dot_provider))'
           ))))'
  ~repl:
    zero().magnitude
)

The @rhombus(statinfo.macro) form expects @rhombus('') containing an
identifier or operator, not a more function-like pattern, because it's
mean to define a constant association between a name and static
information.

An annotation macro can create a converter annotation directly using
@rhombus(annot_meta.pack_converter). Converter annotations build on the
binding protocol (see @secref("bind-macro-protocol")), which includes a
predicate, a conversion function, and static information implied for any
input value that succeeds for the binding, and the annotation itself
provides static information for the result.

When a macro parses annotations, it can use
@rhombus(annot_meta.unpack_converter) to handle all forms of
annotations, since predicate annotations can be automatically
generalized to converter form. A converter annotation will not unpack
with @rhombus(annot_meta.unpack_predicate). Use
@rhombus(annot_meta.is_predicate) and @rhombus(annot_meta.is_converter)
to detect annotation shapes and specialize transformations. Use
@rhombus(annot_meta.is_always_satisfied) to detect an
@tech{always-satisfied annotation}.

@(close_eval(ann_eval))
