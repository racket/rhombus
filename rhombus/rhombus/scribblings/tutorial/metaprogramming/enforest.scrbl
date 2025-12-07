#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/bnf
    "racket-term.rkt".double_shrub)

@(def op_eval = make_rhombus_eval())

@examples(
  ~eval: op_eval
  ~hidden:
    import rhombus/meta open
)

@title(~tag: "enforest"){Enforestation}

For expressions and other contexts that involve infix operators, the
@tech{parse} process can further divided into two interleaved processes:

@itemlist(

 @item{@deftech{Enforestation} handles operator precedence within a
@bnf.nt{group} so that, for example, @rhombus(1 + 2 * 3) is recognized
as an addition expression overall with a multiplication subexpression.}

 @item{@deftech{Expansion} consumes an enforestation result, such as
 @rhombus(+) having two subexpression inputs, and it determines how to
 combine the pieces, such as generating a @rhombus(math.sum) call to
 perform addition.}

)

These processes are interleaved because the meaning of
an operator is determined by the scope in which it is used. While
@rhombus(*)'s dominant use is clearly multiplication, it may also be used in a regular
expression context to mean repetition or, in a language for writing typing rules,
it might be used for the type of tuples.
Accordingly, Rhombus operators acquire a specific precedence in a specific
scope via the definition form for the operator.
As we will see, macro definition forms like
@rhombus(operator, ~defn) and @rhombus(expr.macro, ~defn) include
precedence specifications, so the operator or macro that's being defined
has a specification of its behavior at both the enforestation and expansion layers.
And, while precedence and associativity are the most common use cases to
motivate enforestation, it is actually a general process that converts
a linear sequence of terms into a tree that can be programmed in a general way.

The enforestation and expansion layers also both work on shrubbery
representations. Although enforestation may deliver fully parsed
subexpressions to the expander for a simple infix or prefix operator, in
the most general case, enforestation delivers only a parsed left
argument to an infix operator. An expander can determine how to treat
any shrubbery forms to the right of an infix or prefix operator.
Expansion might choose to recur to enforestation to parse some terms, it
may consume some parts directly, and it may leave any number of tokens
in the enclosing @bnf.nt{group}'s stream to be parsed further. For
example, parsing @rhombus([1, 2, 3].remove(2) |> println) enforests the
left-hand side of @rhombus(.) to the expression @rhombus([1, 2, 3]), but
the right-hand side of @rhombus(.) starts with a method name
@rhombus(remove), not an expression. The @rhombus(.) expander determines
that @rhombus(remove(2)) should be consumed in order to expand into a method
call, but it leaves @rhombus(|> println) for further parsing. Accordingly,
once the @rhombus(.) operator finishes, the expansion and enforestation process
picks up from there with the @rhombus(|>) operator.

Going forward, we can mostly forget about the internal composition of
@tech{enforestation} and @tech{expansion} and think in terms of
@tech{parsing} shrubbery representations, but sometimes it is useful to
keep the interplay of the two processes in mind.

@section{Defining Operators}

The simplest way to define a new operator is using the
@rhombus(operator) form, which is analogous to
@rhombus(fun) for defining a function:

@examples(
  ~eval: op_eval
  ~defn:
    fun revlist(a, b):
      [b, a]
  ~repl:
    revlist("apple", "banana")
  ~defn:
    operator a <~> b:
      [b, a]
  ~repl:
    "apple" <~> "banana"
)

@margin_note_block{The term ``operator'' is somewhat overloaded. At the
 shrubbery level, @tech{operator} refers to a syntactic category that is
 distinct from identifiers. A Rhombus operator in the
 @rhombus(operator,~defn) sense can be an identifier, like
 @rhombus(mod).}

Although they are normally defined at the top of a module, operator
definitions can be in a local-definition context, just like function
definitions.

@examples(
  ~eval: op_eval
  ~repl:
    [
      block:
        operator a <~> b:
          ["just", a]
        "apple" <~> "banana",
      block:
        operator a <~> b:
          [b, "only"]
        "apple" <~> "banana"
    ]
)

As defined, @rhombus(<~>) can be used with single-term expressions to
the left or right:

@examples(
  ~eval: op_eval
  ~repl:
    ("apple" ++ " pie") <~> ("banana" ++ " cream cake")
)

If we leave out parentheses, however, we get an error:

@examples(
  ~eval: op_eval
  ~repl:
    ~error:
      "apple" ++ " pie" <~> "banana" ++ " cream cake"
)

We could declare @rhombus(<~>) to be weaker than @rhombus(++)
specifically, but let's declare weaker precedence than anything that
uses the @rhombus(concatenation, ~operator_order) order, which is the
order that @rhombus(++) adopts.

@margin_note_block{Precedence relationships are pairwise, and they are
 not required to form an @defterm{order} in the mathematical sense, much
 less fit on a numerical scale. Operator orders like
 @rhombus(concatenation, ~operator_order) provide an indirection to
 specify pairwise relationships among groups of operators.}

@examples(
  ~eval: op_eval
  ~defn:
    operator a <~> b:
      ~weaker_than: concatenation
      [b, a]
  ~repl:
    "apple" ++ " pie" <~> "banana" ++ " cream cake"
    "apple" +& 1 <~> "banana" +& 2
)

The @rhombus(operator, ~defn) form supports prefix and postfix
operators, too, and that works the way you'd expect.

@section{Expression Macros}

Like a function, an operator defined with @rhombus(operator, ~defn)
receives value arguments, and it has no control over the order of
evaluation or the parsing of those arguments at a place where the operator is used. A
macro has that kind of control, because it can rearrange the syntax of each
use of the macro.

An infix macro with @rhombus(expr.macro, ~defn) is similar to an infix
operator defined with @rhombus(operator, ~defn), except that a syntax
pattern is used for the definition, and the body must produce a syntax
object. The prefix operator's name is extracted from the middle of the pattern.

@margin_note_block{The @rhombus(macro, ~defn) form is a simplified
 variant of @rhombus(expr.macro) that is exported by
 @rhombuslangname(rhombus), so it can be used without switching to
 @rhombuslangname(rhombus/and_meta) or importing
 @rhombusmodname(rhombus/meta). Unlike @rhombus(expr.macro, ~defn),
 @rhombus(macro, ~defn) does not allow arbitrary parsing-time code, and
 it restricts its body to have an immediate template that escapes using
 @rhombus($) only to refer to pattern bindings. For this first example,
 @rhombus(macro, ~defn) would work just as well as
 @rhombus(expr.macro, ~defn).}

@examples(
  ~eval: op_eval
  ~defn:
    expr.macro '$a <~~~> $b':
      ~weaker_than: concatenation
      'block:
         let b_val = $b // eval RHS first
         let a_val = $a
         [b_val, a_val]'
  ~repl:
    "apple" <~~~> "banana"
)

In this example, the parser takes a syntax-object representation
of the input stream, @rhombus('"apple" <~~~> "banana"'), and matches it
to the pattern @rhombus('$a <~~~> $b'). The resulting expansion with
@rhombus("apple") in place of @rhombus($a) and @rhombus("banana") in place of @rhombus($b) is
incorporated back into the input stream for further parsing.

The @rhombus(block) and
@rhombus(let) forms are not really necessary to achieve the intended
ordering here, since @rhombus('[$b, $a]') would also cause the argument
for @rhombus(b) to be evaluated before the argument for @rhombus(a).
Either way, the reverse order illustrates how the macro has
control, in contrast to an operator defined with
@rhombus(operator, ~defn), and we can expose the change in order by using
arguments that have side effects.

@examples(
  ~eval: op_eval
  ~defn:
    fun trace(v):
      showln(v)
      v
  ~repl:    
    trace("apple") <~> trace("banana")
    trace("apple") <~~~> trace("banana")
)

The last example above illustrates a subtlety of @rhombus(expr.macro, ~defn)
patterns. The @rhombus(a) and @rhombus(b) arguments are matched to
multi-term expressions @rhombus(trace("apple")) and
@rhombus(trace("banana")), which would not happen with the same pattern
in @rhombus(match):

@examples(
  ~eval: op_eval
  ~repl:
    ~error:
      match 'trace("apple") <~~~> trace("banana")'
      | '$a <~~~> $b': // `$a` matches a single term
          "ok"
)

The @rhombus(<~~~>) macro works because @rhombus(expr.macro, ~defn)
recognizes a specific shape as a shorthand for @tech{parsing} left- and
right-hand expressions first, taking into account the
@rhombus(~weaker_than) precedence declaration. It completes the pattern
match only after expressions to the left and right of @rhombus(<~~~>) are
parsed. Those parsed terms as then passed along to the macro as
single-term representations of already-parsed expressions. We will
@seclink("macro-patterns"){return later} to the precise rules for patterns and the specific shapes that
@rhombus(expr.macro, ~defn) recognizes and automatically adjusts.
For now, it suffices to realize that @rhombus(expr.macro, ~defn)
has some smarts to make the common case easy.

@section(~tag: "ex-log_as"){Exercise}

Implement a @rhombus(log_as) infix macro that prints the result of its
left-hand argument expression, but first prints its right-hand argument
expression as a debugging label (without evaluating the expression), and
print @litchar{=>} in between. In other words, make this test pass:

@rhombusblock(
  check: 1 + 5 log_as 2 * 3
         ~prints "2 * 3 => 6"
)

You may find it helpful to first experiment with this expression:

@rhombusblock(
  print('1 + 2 * 3'.to_source_string())
)

Be sure to generate a string for the right-hand argument of
@rhombus(log_as) at compile time, instead of delaying the string
conversion to run time.

Solution: @local_file("log_as_soln.rhm").


@section(~tag: "ex-operator"){Exercise}

It may have occurred to you that @rhombus(operator, ~defn) can be
implemented by generating (1) a function to hold the operator body and (2) an
@rhombus(expr.macro, ~defn) form to expand a use of the operator to a
function call. Your task in this exercise is to define
@rhombus(my_operator) that way.

The starting code @local_file("my_operator.rhm") solves a few problems for you:

@itemlist(

 @item{A suitable pattern for @rhombus(my_operator) is set up already.
 This pattern limits @rhombus(my_operator) to
 defining an infix operator with plain identifiers to represent
 the arguments. The @rhombus(Name, ~stxclass) syntax class matches both
 identifiers and operators. Since @rhombus($body) is not only alone
 within its @bnf.nt{group} but also alone within it @bnf.nt{block}, it
 can match a sequence of @bnf.nt{group}s.}

 @item{You will need to generate an @rhombus(expr.macro, ~defn) form, which
 uses @quotes itself. So, the @rhombus(my_operator) implementation uses
 @guillemet_quotes for the result template to avoid an opening
 @litchar{'} in generated code from being treated as a closing @litchar{'} for
 the overall template.}

 @item{Generating an @rhombus(expr.macro, ~defn) form will be tricky for a
 second reason: you need @rhombus($) sometimes as an escape for the
 @guillemet_quotes template, and sometimes as a literal @rhombus($) that
 should appear in the expansion. Use @rhombus($('$')) to generate a
 literal @rhombus($) in the macro expansion; that works because a
 @rhombus($) by itself in @rhombus('$') is not treated as an escape.}

)

Solution: @local_file("my_operator_soln.rhm").


@// ==================================================

@close_eval(op_eval)
