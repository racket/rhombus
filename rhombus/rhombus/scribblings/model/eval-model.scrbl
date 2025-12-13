#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "prog_step.rhm" open
    lib("scribble/core.rkt"):
      expose:
        style
        #{background-color-property}
    meta_label:
      rhombus/custodian open
      rhombus/network open
      rhombus/thread open)

@(def reduces = elem("→"))
@(def rspace = elem(~style: style("ghost", PairList[]), "→"))

@(fun _redex(c):
    elem(~style: style(#false, PairList[#{background-color-property}("mistyrose")]), c))
@(macro 'redex($a)':
    '_redex(rhombus($a))')

@(def hole = elem("[]"))
@(fun _sub(c, e):
    elem([c, "[", e, "]"]))
@(macro 'sub($a, $b)':
    '_sub(rhombus($a), @rhombus($ b))')
@(fun frame(n):
    elem([_C, subscript(to_string(n))]))

@(def _C = @rhombus(C, ~var))
@(def o1 = @rhombus(⟨o1⟩, ~var))
@(def o2 = @rhombus(⟨o2⟩, ~var))
@(def p1 = @rhombus(⟨p1⟩, ~var))

@// ------------------------------------------------------------------------
@title(~tag: "eval-model"){Evaluation Model}

Rhombus evaluation can be viewed as the simplification of expressions
to obtain values. For example, just as an elementary-school student
simplifies

@verbatim(~indent: 2){
  1 + 1 = 2
}

Rhombus evaluation simplifies

@rhombusblock(
  1 + 1 #,(reduces) 2
)

The arrow @reduces replaces the more traditional @tt{=} to
emphasize that evaluation proceeds in a particular direction toward
simpler expressions. In particular, a @deftech{value}, such as the number @rhombus(2),
is an expression that evaluation simplifies no further.

@// ------------------------------------------------------------------------
@section(~tag: "cont-model"){Subexpression Evaluation and Continuations}

Some simplifications require more than one step. For example:

@rhombusblock(
  4 - #,(redex((1 + 1))) #,(reduces) #,(redex(4 - 2)) #,(reduces) 2
)

An expression that is not a @tech{value} can always be partitioned
into two parts: a @deftech{redex} (``reducible expression''),
which is the part that can change in a
single-step simplification (highlighted), and the
@deftech{continuation}, which is the evaluation
context surrounding the redex. In @rhombus(4 - (1 + 1)), the redex is @rhombus((1 + 1)), and
the continuation is @rhombus(4 - #,(hole)), where @hole takes the place
of the redex as it is reduced. That is, the continuation says how to ``continue''
after the redex is reduced to a value.

Before some expressions can be evaluated, some or all of their sub-expressions must be
evaluated. For example, in the expression @rhombus(4 - (1 + 1)), the
use of @rhombus(-) cannot be reduced until the subexpression
@rhombus((1 + 1)) is reduced.
Thus, the specification of each syntactic form specifies how (some of)
its sub-expressions are evaluated and then how the results are
combined to reduce the form away.

The @deftech{dynamic extent} of an expression is the sequence of
evaluation steps during which the expression contains the @tech{redex}.

@// ------------------------------------------------------------------------
@section{Tail Position}

An expression @rhombus(expr1, ~var) is in @deftech{tail position} with
respect to an enclosing expression @rhombus(expr2, ~var) if, whenever
@rhombus(expr1, ~var) becomes a redex, its continuation is the same
as was the enclosing @rhombus(expr2, ~var)'s continuation.

For example, the @rhombus((1 + 1)) expression is @italic{not} in @tech{tail
 position} with respect to @rhombus(4 - (1 + 1)). To illustrate, we use
the notation @sub(#,(_C), #,(@rhombus(expr, ~var))) to mean the expression that is produced by
substituting @rhombus(expr, ~var) in place of @hole in some continuation
@rhombus(C, ~var):

@rhombusblock(
  #,(sub(#,(_C), 4 - (1 + 1))) #,(reduces) #,(sub(#,(_C), 4 - 2))
)

In this case, the continuation for reducing @rhombus((1 + 1)) is
@sub(#,(@_C), (4 - #,(hole))), not just @rhombus(C, ~var).
The requirement specified in the first paragraph above is not met.

In contrast, @rhombus((1 + 1)) is in @tech{tail position} with respect
to @rhombus(if 0 == 0 | (1 + 1) | 3) because, for any continuation @_C,

@rhombusblock(
  #,(sub(#,(_C), if 0 == 0 | (1 + 1) | 3)) #,(reduces) #,(sub(#,(_C), if #true | (1 + 1) | 3)) #,(reduces) #,(sub(#,(_C), 1 + 1))
)

The requirement specified in the first paragraph is met.
The steps in this reduction sequence are driven by the definition of
@rhombus(if), and they do not depend on the continuation
@_C. The ``then'' branch of an @rhombus(if) form is always in
tail position with respect to the @rhombus(if) form. Due to a
similar reduction rule for @rhombus(if) and @rhombus(#false), the ``else''
branch of an @rhombus(if) form is also in tail position.

Tail-position specifications provide a guarantee about the
asymptotic space consumption of a computation. In general, the
specification of tail positions accompanies the description of
each syntactic form, such as @rhombus(if); subexpressions of a
form are not in tail position unless documented otherwise.

@// ------------------------------------------------------------------------
@section(~tag: "values-model"){Multiple Return Values}

A Rhombus expression can evaluate to @deftech{multiple values}, to
provide symmetry with the fact that a function can accept multiple arguments.

Most @tech{continuations} expect a certain number of result
@tech{values}, although some continuations can accept
an arbitrary number. Indeed, most continuations, such as
@rhombus(#,(hole) + 1), expect a single value. The continuation

@rhombusblock(
  block:
    let (x, y) = #,(hole)
    #,(@rhombus(body, ~var))
)

expects two result
values; the first result replaces @rhombus(x) in
@rhombus(body, ~var), and the second replaces @rhombus(y) in
@rhombus(body, ~var). The continuation

@rhombusblock(
  block:
    #,(hole)
    1 + 2
)

accepts any number of result values, because it ignores the
result(s).

In general, the specification of a syntactic form indicates the
number of values that it produces and the number that it
expects from each of its sub-expressions. In addition, some functions
(notably @rhombus(values)) produce multiple values, and some
functions (notably @rhombus(call_with_values)) create continuations
internally that accept a certain number of values.

@// ------------------------------------------------------------------------
@section{Top-Level Variables}

Given

@verbatim(~indent: 2){
  x = 10
}

then an algebra student simplifies @tt{x + 1} as follows:

@verbatim(~indent: 2){
  x + 1 = 10 + 1 = 11
}

Rhombus works much the same way, in that a set of @tech{top-level
 variables} (see also @secref("vars-and-locs")) are available for substitutions on demand during
evaluation. For example, given

@rhombusblock(
  def x = 10
)

then

@rhombusblock(
  #,(redex(x)) + 1 #,(reduces) #,(redex(10 + 1)) #,(reduces) 11
)

In Rhombus, the way definitions are created is just as important as the way
they are used. Rhombus evaluation thus keeps track of both
definitions and the current expression, and it extends the set of
definitions in response to evaluating forms such as @rhombus(def).

Each evaluation step, then, transforms the current set of definitions and
program into a new set of definitions and program. Before a
@rhombus(def) can be moved into the set of definitions, its
expression (i.e., its right-hand side) must be reduced to a @tech{value}.
(The left-hand side is not an expression position, and so it is not evaluated.)

@(
  prog_steps:
    ~step:
      ~expr:
        def x = #,(redex(9 + 1))
        x + 1
    ~step:
      ~expr:
        #,(redex (def x = 10))
        x + 1
    ~step:
      ~defn:
        def x = 10
      ~expr:
        #,(redex(x)) + 1
    ~step:
      ~defn:
        def x = 10
      ~expr:
        #,(redex(10 + 1))
    ~step:
      ~defn:
        def x = 10
      ~expr:
        11
)

Using @rhombus(def) again, a program can change the value associated with an
existing @tech{top-level variable}:

@(
  prog_steps:
    ~step:
      ~defn:
        def x = 10
      ~expr:
        #,(redex(def x = 8))
        x
    ~step:
      ~defn:
        def x = 8
      ~expr:
        #,(redex(x))
    ~step:
      ~defn:
        def x = 8
      ~expr:
        8
)

@// ------------------------------------------------------------------------
@section{Objects and Imperative Update}

In addition to @rhombus(def) for imperative update of @tech{top-level
 variables}, various functions and operators enable the modification of elements
within a mutable compound data structure. For example, @rhombus([]) with @rhombus(:=)
modifies the content of an array.

To explain such modifications to data, we must distinguish between
@tech{values}, which are the results of expressions, and
@deftech{objects}, which actually hold data.

A few kinds of objects can serve directly as values, including
booleans, @rhombus(#void), and small exact integers. More generally,
however, a value is a reference to an object stored somewhere
else. For example, a value can refer to a particular array that
currently holds the value @rhombus(10) in its first slot. If an
object is modified via one value,
then the modification is visible through
all the values that reference the object.

In the evaluation model, a set of objects must be carried along
with each step in evaluation, just like the definition set. Operations
that create objects, such as @rhombus(Array), add to the set of
objects:

@(
  prog_steps:
    ~step:
      ~obj:«»
      ~expr:
        def x = #,(redex(Array(10, 20)))
        def y = x
        x[0] := 11
        y[0]
    ~step:
      ~obj:
        def #,(o1) = Array(10, 20)
      ~expr:
        #,(redex(def x = #,(o1)))
        def y = x
        x[0] := 11
        y[0]
    ~step:
      ~obj:
        def #,(o1) = Array(10, 20)
      ~defn:
        def x = #,(o1)
      ~expr:
        def y = #,(redex(x))
        x[0] := 11
        y[0]
    ~step:
      ~obj:
        def #,(o1) = Array(10, 20)
      ~defn:
        def x = #,(o1)
      ~expr:
        #,(redex(def y = #,(o1)))
        x[0] := 11
        y[0]
    ~step:
      ~obj:
        def #,(o1) = Array(10, 20)
      ~defn:
        def x = #,(o1)
        def y = #,(o1)
      ~expr:
        #,(redex(x))[0] := 11
        y[0]
    ~step:
      ~obj:
        def #,(o1) = Array(10, 20)
      ~defn:
        def x = #,(o1)
        def y = #,(o1)
      ~expr:
        #,(redex(#,(o1)[0] := 11))
        y[0]
    ~step:
      ~obj:
        def #,(o1) = Array(11, 20)
      ~defn:
        def x = #,(o1)
        def y = #,(o1)
      ~expr:
        #,(redex(y))[0]
    ~step:
      ~obj:
        def #,(o1) = Array(11, 20)
      ~defn:
        def x = #,(o1)
        def y = #,(o1)
      ~expr:
        #,(redex(#,(o1)[0]))
    ~step:
      ~obj:
        def #,(o1) = Array(11, 20)
      ~defn:
        def x = #,(o1)
        def y = #,(o1)
      ~expr:
        11
)


The distinction between a @tech{top-level variable} and an object
reference is crucial. A top-level variable is not a
@tech{value}, so it must be evaluated. Each time
a @tech{variable} expression is evaluated, the
value of the variable is extracted from the current set of definitions. An object
reference, in contrast, is a value and therefore needs no further
evaluation. The evaluation steps above use @o1
for an object reference to distinguish it from a
variable name.

An object reference can never appear directly in a text-based source
program. A program representation created with
@rhombus(Syntax.make), however, can embed direct references to
existing objects.

@// ------------------------------------------------------------------------
@section(~tag: "gc-model"){Garbage Collection}

@margin_note{See @secref(~doc: ref_doc, "memory") for functions related to
garbage collection.}

In the program state

@(
  prog_steps:
    ~step:
      ~obj:
        def #,(o1) = Array(10, 20)
        def #,(o2) = Array(0)
      ~defn:
        def x = #,(o1)
      ~expr:
        1 + x
)

evaluation cannot depend on @o2, because it is not part of the program
to evaluate, and it is not referenced by any definition that is
accessible by the program. The @tech{object} is said to not be
@deftech{reachable}. The object @o2 may therefore be removed from
the program state by @deftech{garbage collection}.

A few special compound datatypes hold @deftech{weak references} to
objects. Such weak references are treated specially by the garbage
collector in determining which objects are reachable for the
remainder of the computation. If an object is reachable @italic{only}
via a weak reference, then the object can be reclaimed, and the
weak reference is replaced by a different value
(typically @rhombus(#false)).

As a special case, a @tech(~doc: ref_doc){fixnum} is always considered reachable by
the garbage collector. Many other values are always reachable due to
the way they are implemented and used: A @tech(~doc: ref_doc){character}
is always reachable, and @rhombus(==) 
characters are always @rhombus(===). Similarly, @rhombus(PairList[]),
@rhombus(#true), @rhombus(#false), @rhombus(Port.eof), and @rhombus(#void) are
always reachable. Values produced by @rhombus(#%literal) remain reachable
when the @rhombus(#%literal) expression itself is reachable.

@// ------------------------------------------------------------------------
@section{Function Calls and Local Variables}

Given

@verbatim(~indent: 2){
  f(x) = x + 10
}

an algebra student simplifies @tt{f(7)} as follows:

@verbatim(~indent: 2){
  f(7) = 7 + 10 = 17
}

The key step in this simplification is to take the body of the defined
function @tt{f} and replace each @tt{x} with the actual
@tech{value} @tt{7}.

Rhombus function calls work much the same way. A function is
an @tech{object}, so evaluating @rhombus(f(7)) starts with a
@tech{variable} lookup:

@(
  prog_steps:
    ~step:
      ~obj:
        def #,(p1) = fun (x): x + 10
      ~defn:
        def f = #,(p1)
      ~expr:
        #,(redex(f))(7)
    ~step:
      ~obj:
        def #,(p1) = fun (x): x + 10
      ~defn:
        def f = #,(p1)
      ~expr:
        #,(redex(#,(p1)(7)))
    ~step:
      ~obj:
        def #,(p1) = fun (x): x + 10
      ~defn:
        def f = #,(p1)
      ~expr:
        #,(redex(7 + 10))
    ~step:
      ~obj:
        def #,(p1) = fun (x): x + 10
      ~defn:
        def f = #,(p1)
      ~expr:
        17
)

If a variable like @rhombus(x) is made @rhombus(mutable, ~bind), however,
the @tech{value} associated with the variable can be changed in the body of a function by using
@rhombus(:=), as in the example @rhombus(fun (mutable x): x := 3; x).
Since the value associated with a @rhombus(mutable, ~bind) argument variable @rhombus(x) should be
able to change, we cannot just substitute the value in for @rhombus(x) when
we first call the function.

Instead, a new @deftech{location} is created for each variable
on each function call. The argument value is placed in the
location, and each instance of the variable in the
function body is replaced with the new location:

@(
  prog_steps:
    ~step:
      ~obj:
        def #,(p1) = fun (mutable x): x + 10
      ~defn:
        def f = #,(p1)
      ~expr:
        #,(redex(f))(7)
    ~step:
      ~obj:
        def #,(p1) = fun (mutable x): x + 10
      ~defn:
        def f = #,(p1)
      ~expr:
        #,(redex(#,(p1)(7)))
    ~step:
      ~obj:
        def #,(p1) = fun (mutable x): x + 10
      ~defn:
        def f = #,(p1)
        def xloc = 7
      ~expr:
        #,(redex(xloc)) + 10
    ~step:
      ~obj:
        def #,(p1) = fun (mutable x): x + 10
      ~defn:
        def f = #,(p1)
        def xloc = 7
      ~expr:
        #,(redex(7 + 10))
    ~step:
      ~obj:
        def #,(p1) = fun (mutable x): x + 10
      ~defn:
        def f = #,(p1)
        def xloc = 7
      ~expr:
        17
)

A @tech{location} is the same as a @tech{top-level variable}, but when
a location is generated, it (conceptually) uses a name that has
not been used before and that cannot be generated again or
accessed directly.

Generating a location in this way means that @rhombus(:=)
evaluates for @tech{local variables}, including argument
variables, in the same way as for
@tech{top-level variables}, because the local variable is
always replaced with a location by the time the @rhombus(:=)
form is evaluated:

@(
  prog_steps:
    ~step:
      ~obj:
        def #,(p1) = fun (mutable x): x := 3; x
      ~defn:
        def f = #,(p1)
      ~expr:
        #,(redex(f))(7)
    ~step:
      ~obj:
        def #,(p1) = fun (mutable x): x := 3; x
      ~defn:
        def f = #,(p1)
      ~expr:
        #,(redex(#,(p1)(7)))
    ~step:
      ~obj:
        def #,(p1) = fun (mutable x): x := 3; x
      ~defn:
        def f = #,(p1)
        def xloc = 7
      ~expr:
        #,(redex(xloc := 3))
        xloc
    ~step:
      ~obj:
        def #,(p1) = fun (mutable x): x := 3; x
      ~defn:
        def f = #,(p1)
        def xloc = 3
      ~expr:
        #,(redex(xloc))
    ~step:
      ~obj:
        def #,(p1) = fun (mutable x): x := 3; x
      ~defn:
        def f = #,(p1)
        def xloc = 3
      ~expr:
        3
)

The @tech{location}-generation and substitution step of function
call requires that the argument is a @tech{value}. Therefore,
in @rhombus((fun (mutable x): x + 10)(1 + 2)), the @rhombus(1 + 2)
subexpression must be simplified to the value @rhombus(3), and
then @rhombus(3) can be placed into a location for
@rhombus(x). In other words, Rhombus is a @deftech{call-by-value}
language.

Evaluation of a local-variable form, such as

@rhombusblock(
  block:
    let mutable x = 1 + 2
    #,(@rhombus(expr, ~var))
)

is the same as for a function call. After @rhombus(1 + 2)
produces a value, it is stored in a fresh location
that replaces every instance of @rhombus(x) in @rhombus(expr, ~var).

@// ------------------------------------------------------------------------
@section(~tag: "vars-and-locs"){Variables and Locations}

A @deftech{variable} is a placeholder for a @tech{value}, and
expressions in an initial program refer to variables. A
@deftech{top-level variable} is both a variable and a
@tech{location}. Any other variable is always replaced by a
location at run-time---conceptually, even in the case of
variables that are not @rhombus(mutable, ~bind). Thus, evaluation of expressions
involves only locations. A single @deftech{local variable}
(i.e., a non-top-level, non-module-level variable), such as an
argument variable, can correspond to different locations
during different calls.

For example, in the program

@rhombusblock(
  def y = (block: let x = 5; x) + 6
)

both @rhombus(y) and @rhombus(x) are variables. The @rhombus(y)
variable is a top-level variable, and the @rhombus(x) is
a local variable. When this code is evaluated, a
location is created for @rhombus(x) to hold the value
@rhombus(5), and a location is also created for @rhombus(y) to
hold the value @rhombus(11).

The replacement of a variable with a location during
evaluation implements Rhombus's @deftech{lexical scoping}.
@margin_note{For the purposes of substituting @rhombus(xloc) for @rhombus(x),
all variable bindings must use distinct names, so no @rhombus(x) that
is really a different variable will get replaced. Ensuring that
distinction is one of the jobs of the macro expander; see @secref("syntax-model").}
For example, when an argument variable @rhombus(x) is replaced by
the location @rhombus(xloc), it is replaced @italic{throughout} the
body of the function, including any nested @rhombus(fun)
forms. As a result, future references to the variable always
access the same location.

@// ------------------------------------------------------------------------
@section(~tag: "module-eval-model"){Modules and Module-Level Variables}

Most definitions in Rhombus are within @deftech{modules}. In terms of evaluation,
a module is essentially a prefix on a defined name, so that different
modules can define the same name. That is, a @deftech{module-level
 variable} is like a @tech{top-level variable} from the perspective of
evaluation.

One difference between a module and a top-level definition
is that a module can be @deftech(~key: "declare"){declared}
without instantiating its module-level definitions.
Evaluation of a @rhombus(import) @deftech{instantiates}
(i.e., triggers the @deftech{instantiation} of) the declared
module, which creates variables that correspond to its
module-level definitions.

For example, given the module declaration

@rhombusblock(
  // in "m.rhm"
  #,(hash_lang()) #,(rhombuslangname(rhombus))
  def x = 10
)

the evaluation of @rhombus(import "m.rkt") creates the variable @rhombus(x)
and installs @rhombus(10) as its value. This @rhombus(x) is unrelated to
any top-level definition of @rhombus(x) (as if it were given a unique,
module-specific prefix).

@// ------------------------------------------------------------------------
@subsection(~tag: "module-phase"){Phases}

The purpose of @deftech{phases} is to
address the necessary separation of names defined at evaluation time versus
names defined at expansion time.

A module can be @tech{instantiate}d in multiple phases. A
phase is an integer that, like a module name, is effectively a prefix on the names
of module-level definitions. Phase 0 is the run-time phase.

A top-level @rhombus(import)
instantiates a module at phase 0, if the module is not
already instantiated at phase 0.  A top-level
@rhombus(import meta) instantiates a module at
phase 1 (if it is not already instantiated at that
phase); @rhombus(meta, ~impo) also has a different binding
effect on further program parsing, as described in
@secref("intro-binding").

Within a module, some definitions are already shifted by a phase: the
@rhombus(meta) form shifts expressions and definitions by a relative phase +1.
Thus, if the module is instantiated at phase 1,
the variables defined with @rhombus(meta) are created at phase 2,
and so on. Moreover, this relative phase acts as another layer of
prefixing, so that @rhombus(x) defined with @rhombus(def) and
@rhombus(x) defined with @rhombus(meta def) can co-exist in a module
without colliding. A @rhombus(meta) form can be nested
within a @rhombus(meta) form, in which case the inner definitions and
expressions are in relative phase +2, and so on. Higher phases are
mainly related to program parsing instead of normal evaluation.

If a module instantiated at phase @math{n}
@rhombus(import)s another module, then the imported module is
first instantiated at phase @math{n}, and so on
transitively. (Module @rhombus(import)s cannot form cycles.) If a
module instantiated at phase @math{n} imports
another module @rhombus(M, ~var) with @rhombus(meta), then @rhombus(M, ~var) becomes
@deftech{available} at phase @math{n+1}, and it later may be
instantiated at phase @math{n+1}.  If a module that is
available at phase @math{n} (for @math{n>0}) @rhombus(import)s
another module @rhombus(M, ~var) with @rhombus(meta -1), then @rhombus(M, ~var) becomes
available at phase @math{n-1}, and so
on. Instantiations of available modules above
phase 0 are triggered on demand as described in
@secref("mod-parse").

A final distinction among module instantiations is that
multiple instantiations may exist at phase 1 and
higher. These instantiations are created by the parsing of
module forms (see @secref("mod-parse")), and are, again, conceptually
distinguished by prefixes.

Top-level variables can exist in multiple phases in the same way as
within modules. For example, @rhombus(def) within @rhombus(meta) creates a
phase 1 variable. Furthermore, reflective operations like
@rhombus(Evaluator.make_rhombus) and @rhombus(eval) provide access to
top-level variables in higher phases, while module
instantiations (triggered by @rhombus(import)) relative to such
top-levels are in correspondingly higher phases.

@// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection(~tag: "module-redeclare"){Module Redeclarations}

@section_indexed(["modules", "redefine"])

When a module is declared using a name with which a module is already
declared, the new declaration's definitions replace and extend the old
declarations. If a variable in the old declaration has no counterpart
in the new declaration, the old variable continues to exist, but its
binding is not included in the @tech{lexical information} for the
module body. If a new variable definition has a counterpart in the old
declaration, it effectively assigns to the old variable.

If a module is @tech{instantiate}d in the current namespace's
@tech{base phase} before the module is redeclared, the redeclaration
of the module is immediately instantiated in that
@tech{phase}.

If the current @tech{inspector} does not manage a module's declaration
inspector (see @secref("modprotect")), then the module cannot be
redeclared. Even if redeclaration succeeds, instantiation of a module that is
previously instantiated may fail if instantiation for the
redeclaration attempts to modify variables that are constant.

@// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection(~tag: "submodules"){Submodules}

A @rhombus(module) form within a module declares a @deftech{submodule}. A submodule is
accessed relative to its enclosing module, usually with the
@rhombus(!, ~impo) operator. Submodules can be nested to any depth.

Although a submodule is lexically nested within a module, a subsmodule
declared with @rhombus(~lang) cannot access the bindings of its enclosing module directly.
In that case, unless a submodule imports from its enclosing module or vice versa, then
@tech{visits} or @tech{instantiations} of the two modules are
independent, and their implementations may even be loaded from a compiled
form at different times.

See @rhombus(module) for more information.

@// ------------------------------------------------------------------------
@section(~tag: "mark-model"){Continuation Frames and Marks}

@margin_note{See @rhombus(Continuation.Marks, ~annot) for continuation-mark forms and functions.}

Every continuation @_C can be partitioned into @deftech{continuation
 frames} @frame(1), @frame(2), ..., @frame("n") such that @_C =
@_sub(frame(1), _sub(frame(2), _sub("...", frame("n")))), and no frame
@frame("i") can be itself partitioned into smaller continuations.
Evaluation steps add frames to and remove frames from the current
continuation, typically one at a time.

Each frame is conceptually annotated with a set of
@deftech{continuation marks}. A mark consists of a key and its value.
The key is an arbitrary value, and each frame includes at most one
mark for any given key. Various operations set and extract marks from
continuations, so that marks can be used to attach information to a
@tech{dynamic extent}. For example, marks can be used to record information
for a ``stack trace'' to be presented when an exception is thrown, or
to implement dynamic scope.

@// ------------------------------------------------------------------------
@section(~tag: "prompt-model"){Prompts, Delimited Continuations, and Barriers}

@margin_note{See @secref(~doc: ref_doc, "Continuations") for continuation and prompt functions.}

A @deftech{prompt} is a special kind of continuation frame that is
annotated with a specific @deftech{prompt tag} (essentially a
@tech{continuation mark}). Various operations allow the capture of frames in
the continuation from the redex position out to the nearest enclosing
prompt with a particular prompt tag; such a continuation is sometimes
called a @deftech{delimited continuation}. Other operations allow the
current continuation to be extended with a captured continuation
(specifically, a @deftech{composable continuation}). Yet other
operations abort the computation to the nearest enclosing prompt with
a particular tag, or replace the continuation to the nearest enclosing
prompt with another one. When a delimited continuation is captured,
the marks associated with the relevant frames are also captured.

A @deftech{continuation barrier} is another kind of continuation frame
that prohibits certain replacements of the current continuation with
another. Specifically, a continuation can be replaced by another only
when the replacement does not introduce any continuation barriers.
A continuation barrier
thus prevents ``downward jumps'' into a continuation that is protected
by a barrier. Certain operations install barriers automatically; in
particular, when an exception handler is called, a continuation
barrier prohibits the continuation of the handler from capturing the
continuation past the exception point.

An @deftech{escape continuation} is a derived concept. It
combines a prompt for escape purposes with a continuation for
mark-gathering purposes. As the name implies, escape continuations are
used only to abort to the point of capture.

@// ------------------------------------------------------------------------
@section(~tag: "thread-model"){Threads}

@margin_note{See @secref(~doc: ref_doc, "concurrency") for thread and synchronization functions.}

Rhombus supports multiple @deftech{threads} of evaluation.  Threads run
concurrently, in the sense that one thread can preempt another without
its cooperation, independent of whether the threads all run on the same processor
(i.e., the same underlying operating system process and thread) as
@tech(~doc: ref_doc){coroutine threads} or potentially on different
processors as @tech(~doc: ref_doc){parallel threads}.

Threads are created explicitly by forms such as @rhombus(thread).
In terms of the evaluation model, each step in evaluation
actually deals with multiple concurrent
expressions, up to one per thread, rather than a single expression. The expressions all
share the same objects and top-level variables, so that they can
communicate through shared state, and @defterm{sequential consistency} is
guaranteed for coroutine threads (i.e., the result is consistent with some global sequence
imposed on all evaluation steps across threads). Most evaluation steps involve a
single step in a single thread, but certain synchronization
primitives require multiple threads to progress together in one step; for example,
an exchange of a value through a @tech{channel} progresses in two
threads simultaneously.

Unless otherwise noted, all constant-time functions and operations
provided by Rhombus are thread-safe in the sense that they are
@defterm{atomic}: they happen as a single evaluation step.
For example, @rhombus(:=) assigns to a variable as an atomic action
with respect to all threads, so that no thread can see a
``half-assigned'' variable. Similarly, @rhombus([]) with @rhombus(:=) assigns to
an array atomically. Note that the evaluation of a @rhombus(:=)
expression with its subexpression is not necessarily atomic, because
evaluating the subexpression involves a separate step of evaluation.
Only the assignment action itself (which takes after the subexpression
is evaluated to obtain a value) is atomic. Similarly, a function
call can involve multiple steps that are not atomic, even if
the function itself performs an atomic action.

The @rhombus([]) plus @rhombus(:=) combination is not atomic on
a @rhombus(MutableMap), but the map is
protected by a lock; see @secref(~doc: ref_doc, "Maps") for more information.
Port operations are generally not atomic, but they are thread-safe in
the sense that a byte consumed by one thread from an input port will
not be returned also to another thread, and methods like
@rhombus(Port.Input.Progress.commit) and
@rhombus(Port.Output.write_bytes) offer specific concurrency guarantees.

In addition to the state that is shared among all threads, each thread
has its own private state that is accessed through @deftech{thread
 cells}. A thread cell is similar to a normal mutable object, but a
change to the value inside a thread cell is seen only when extracting
a value from that cell in the same thread. A thread cell can be
@deftech{preserved}; when a new thread is created, the creating
thread's value for a preserved thread cell serves as the initial value
for the cell in the created thread. For a non-preserved thread cell, a
new thread sees the same initial value (specified when the thread cell
is created) as all other threads.

@// ------------------------------------------------------------------------
@section(~tag: "parameter-model"){Context Parameters}

@margin_note{See @secref(~doc: ref_doc, "context-parameters") for context-parameter forms and functions.}

@deftech{Context parameters} are a derived concept in Rhombus; they
are defined in terms of @tech{continuation marks} and @tech{thread
 cells}. However, parameters are also ``built in,'' due to the fact that some
primitive functions consult parameter values. For example, the
default output stream for primitive output operations is specified by
a parameter.

A parameter is a setting that is both thread-specific and
continuation-specific. In the empty continuation, each parameter
corresponds to a @tech{preserved} @tech{thread cell}; a corresponding
@deftech{parameter function} accesses and sets the thread cell's
value for the current thread.

In a non-empty continuation, a parameter's value is determined through
a @deftech{parameterization} that is associated with the nearest
enclosing continuation frame via a continuation mark (whose key is
not directly accessible). A parameterization maps each parameter to a
preserved thread cell, and the combination of the thread cell and the current
thread yields the parameter's value. A parameter function sets or
accesses the relevant thread cell for its parameter.

Various operations, such as @rhombus(parameterize), install a parameterization into
the current continuation's frame.

@// ------------------------------------------------------------------------
@section(~tag: "exn-model"){Exceptions}

@margin_note{See @secref(~doc: ref_doc, "Exceptions") for exception forms, functions, and types.}

@deftech{Exceptions} are a derived concept in Rhombus; they
are defined in terms of continuations, prompts, and continuation
marks.  However, exceptions are also ``built in,'' due to the fact that
primitive forms and functions may throw exceptions.

An @deftech{exception handler} to @deftech{catch} exceptions can be associated
with a continuation frame though a @tech{continuation mark} (whose key
is not directly accessible). When an exception is thrown, the current
continuation's marks determine a chain of exception-handler
functions that are consulted to handle the exception.
A handler for uncaught exceptions is designated through a built-in @tech{context parameter}.

One potential action of an exception handler is to abort the
current @tech{continuation} up to an enclosing @tech{prompt} with a
particular @tech{prompt tag}.  The default handler for uncaught
exceptions, in particular, aborts to a particular tag for which a
prompt is always present, because the prompt is installed in the
outermost frame of the continuation for any new thread.

@// ------------------------------------------------------------------------
@section(~tag: "custodian-model"){Custodians}

@margin_note{See @secref(~doc: ref_doc, "custodian") for custodian functions.}

A @deftech{custodian} manages a collection of objects such as @tech(~doc: ref_doc){threads},
@rhombus(Port.FileStream, ~annot) objects,
@rhombus(TCPListener, ~annot) objects, and
@rhombus(UDP, ~annot) objects.
Whenever a thread, etc., is created, it is placed under the management
of the @deftech{current custodian} as determined by the
@rhombus(Custodian.current) @tech{context parameter}.

Except for the root custodian, every custodian itself is
managed by a custodian, so that custodians form a hierarchy.
Every object managed by a subordinate custodian is also managed by the
custodian's owner.

When a custodian is shut down via
@rhombus(Custodian.shutdown_all), it forcibly and immediately closes
the ports, TCP connections, etc., that it manages, as well as
terminating (or suspending) its threads. A custodian that has been
shut down cannot manage new objects.  After the current custodian is shut
down, if a function is called that attempts to create a managed resource (e.g.,
@rhombus(Port.Input.open_file), @rhombus(thread)), then the
@rhombus(Exn.Fail.Contract) exception is thrown.

@//|=={
A thread can have multiple managing custodians, and a suspended thread
created with @racket[thread/suspend-to-kill] can have zero
custodians. Extra custodians become associated with a thread through
@racket[thread-resume] (see @secref["threadkill"]). When a thread
has multiple custodians, it is not necessarily killed by a
@racket[custodian-shutdown-all]. Instead, shut-down custodians are removed
from the thread's managing custodian set, and the thread is killed when its
managing set becomes empty.
}==|

The values managed by a custodian are semi-weakly held by the
custodian: a @tech{will} can be executed for a value that is
managed by a custodian. A custodian only weakly
references its subordinate custodians; if a subordinate custodian is
unreferenced but has its own subordinates, then the custodian may be
garbage collected, at which point its subordinates become immediately
subordinate to the collected custodian's superordinate (owner) custodian.

In addition to the other entities managed by a custodian, a
@deftech{custodian box} created with @rhombus(Custodian.Box)
strongly holds onto a value placed in the box until the box's
custodian is shut down. However, the custodian only weakly retains the box
itself, so the box and its content can be collected if there
are no other references to them.

@//|=={

When Rhombus is compiled with support for per-custodian memory
accounting (see @racket[custodian-memory-accounting-available?]), the
@racket[current-memory-use] function can report a custodian-specific
result.  This result determines how much memory is occupied by objects
that are @tech{reachable} from the custodian's managed values, especially its
threads, and including its sub-custodians' managed values. If an
object is reachable from two custodians where neither is an ancestor
of the other, an object is arbitrarily charged to one or the other,
and the choice can change after each collection; objects reachable
from both a custodian and its descendant, however, are reliably
charged to the custodian and not to the descendants, unless the
custodian can reach the objects only through a descendant custodian or
a descendant's thread.  Reachability for per-custodian accounting does
not include weak references, references to threads managed by other
custodians, references to other custodians, or references to custodian
boxes for other custodians.

}==|
