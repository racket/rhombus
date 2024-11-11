#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      "fake.rhm" open)

@title{Documentation Entries}

In general, documented bindings are shown in a blue box where the bound
name is in @bold{boldface}. For example, documentation for
@rhombus(my_contract) could appear as follows:

@nested(~style: #'inset){
@doc(
  non_target:
    def my_constant :: Int
){}
}

The syntactic category of a binding is shown to the right within the
box, and it is shown as ``value'' in the example above. That syntactic
category implies a @tech{space} for the binding, and it determines how
the rest of the entry should be interpreted. In the case of value
bindings, documentation is written as the start of a @rhombus(def) form.
Functions are typically documented using a @rhombus(fun) form:

@nested(~style: #'inset){
@doc(
  non_target:
    fun fib(n :: NonnegInt) :: NonnegInt
){}
}

When a function supports multiple argument counts in a way that is not
easy expressed as optional arguments or reetitions, its documentation
may shown as separate @rhombus(fun) forms, instead of using multi-case
@vbar syntax:

@nested(~style: #'inset){
@doc(
  non_target:
    fun fib(n :: NonnegInt) :: NonnegInt
  non_target:
    fun fib(n :: Flonum) :: Flonum
){}
}

Different documentation forms might even be used for the same binding,
since a macro implementation of a binding can make it act in different
ways in different contexts. A binding's various forms are always shown
in the same blue box, but a single blue box may document multiple
bindings.

@section{Documenting Syntactic Forms}

Syntactic forms, such as in the ``expression'' and ``definition''
categories, are documented with a grammar template, where non-italicized
portions are literal (including the documented binding in bold), and
italicized portions are metavariables. Metavariables stand for syntax
that matched a separately defined grammar. A metavariable may be
hyperlinked to its definition, like @nontermref(bind) below, or it may
have a locally defined grammar, such as @rhombus(peano_num, ~var) below.

@nested(~style: #'inset){
@doc(
  non_target:
    defn.macro 'def_peano $bind = $peano_num'
  grammar peano_num:
    0
    $peano_num + 1
){}
}

A grammar like the one for @rhombus(peano_num, ~var) describes syntactic
sequences, not values. So, @rhombus(0), @rhombus(0 + 1), and
@rhombus(0 + 1 + 1) fit the grammar for @rhombus(peano_num, ~var), but
@rhombus(2) does not.

@section(~tag: "doc_method"){Documenting Methods}

When a binding is documented with @rhombus(method, ~class_clause), it
corresponds to a function that is reachable both through a dotted name
and by using @rhombus(.) after an object expression. For example, if a
@rhombus(Fish.swim) method can be called as @rhombus(Fish.swim) or as
@rhombus(f.swim) where @rhombus(f) produces a @rhombus(Fish) object, it
would be documented as follows:

@nested(~style: #'inset){
@doc(
  non_target:
    method (obj :: Fish).swim(distance :: Real)
){}
}

Sometimes, the object that a method accepts is different from the
namespace where the method is bound. For example, @rhombus(Fish.swim)
might actually accept any object that satisfies @rhombus(FishOrWhale)
and works with @rhombus(f.swim) where @rhombus(f) produces a value that
satisifes @rhombus(FishOrWhale). In that case, it would be document as
follows:

@nested(~style: #'inset){
@doc(
  non_target:
    method Fish.swim(obj :: FishOrWhale, distance :: Real)
){}
}

Note that if this description of @rhombus(Fish.swim) started with
@rhombus(fun) instead of @rhombus(method, ~class_clause), it would
describe a function that works when called as @rhombus(Fish.swim), but
not via @rhombus(f.swim).

In this @rhombus(fun)-like form of @rhombus(method, ~class_clause), the
dotted name's prefix will always correspond to a class or annotation.
The annotation of the first argument can be more permissive, as in the
above example, since @rhombus(FishOrWhale) presumably accepts any value
that satisfies @rhombus(Fish). The argument annotation can also be more
constrained, however, as in the following example (assuming that only
some @rhombus(Fish) objects satisfy @rhombus(FastFish)):

@nested(~style: #'inset){
@doc(
  non_target:
    method Fish.escape(obj :: FastFish)
){}
}

When a method's first argument has a more constraining annotation than
the one that is a prefix of the method's name, then objects satisfying
the prefix annotation can be used to access the method, even when they
do not satisfy the argument annotation. Calling the method through such
an object will result in an annotation-satisfaction exception, however.
