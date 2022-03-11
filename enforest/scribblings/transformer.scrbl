#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Operator and Macro Transformers}

Some contexts in a Rhombus language (likely including expression
contexts) will support infix, prefix, and postfix operators. The Rhombus
expander provides an _enforestation_ framework for parsing forms that
involve multiple operators, each with a declared precedence and
associativity. The enforestation process also allows an operator
transformer to completely take over parsing of terms that follow the
operator within a shrubbery group. An ``operator'' in this sense can be
named by either a shrubbery operator or a shrubbery identifier, so the
prefix-operator protocol suffices for defining macros in a traditional
sense.

For an infix operator, enforestation always parses the left-hand
argument (i.e., the part before the operator) in the same context as the
operator's context. For example, the left-hand argument to an infix
expression operator @rhombus[+] or @rhombus[.] is always parsed as an
expression. For the right-hand (or only, in the case of prefix)
argument, the operator's mapping selects one of two protocols:
_automatic_, where the right-hand argument is also parsed in the same
context, or _macro_, where the operator's transformer receives the full
sequence of terms remaining in the enclosing group. An operator using
the macro protocol parses remaining terms as it sees fit, and then it
returns the still-remaining terms that it does not consume. For example,
@rhombus[+] for expressions is likely implemented as an automatic infix
operator, since both of its arguments are also expressions, while
@rhombus[.] is likely implemented as a macro infix operator so that it's
right-hand ``argument'' is always parsed as a field identifier. In the
earlier @rhombus[<>] and @rhombus[->] examples, @rhombus[<>] is
implemented as an automatic infix operator for expressions, while
@rhombus[<>] for bindings and @rhombus[->] for expressions were
implemented as macro infix operators.

Roughly, an operator that uses the macro protocol takes on some of the
burden of dealing with precedence, at least for terms after the
operator. For operators like @rhombus[.] or @rhombus[->], this is no
problem, because the right-hand side has a fixed shape. Other operators
may need to call back into the enforestation algorithm, and the Rhombus
expander provides facilities to enable that.

A postfix operator is implemented as a macro infix operator that
consumes no additional terms after the operator. For example, a postfix
@rhombus[!] might be defined (shadowing the normal @rhombus[!] for
``not'') as follows:

@(rhombusblock:
    fun
    | factorial(0): 1
    | factorial(n): n*factorial(n-1)
         
    expr.macro '$a ! $tail ...':
      values('factorial($a)', tail)

    10! + 1 // = 3628801
  )

Since the Rhombus expander provides a way for macro transformers to
resume enforestation, all operators could be implemented with the
macro protocol. The automatic protocol is just a convenient shortcut.

Some contexts might constrain the allowed forms of operators to prefix
or infix, constrain the names used for operators, and/or eschew one of
the operator protocols. For example, declaration and definition contexts
might allow only macro prefix operators with identifier names. The
@Rhombus implementation makes that choice, and it also allows
expression forms with operators to appear in the same places as
declaration and definition forms.
