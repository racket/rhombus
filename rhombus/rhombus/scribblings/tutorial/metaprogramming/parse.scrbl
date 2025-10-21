#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/bnf
    "racket-term.rkt".double_shrub)

@(def def_eval = make_rhombus_eval())

@title(~tag: "parse"){Post-Shrubbery Parsing}

Generally ``parsing'' programs means converting a character
stream to an AST that can be executed. However, given that Rhombus has a
@tech{bicameral} approach to syntax, we will define @tech{parse} to mean
specifically a second stage of program elaboration.

@itemlist(

 @item{For the first stage, we use the terms @deftech{read} or @deftech{reading} to refer to the
 process of converting a character sequence into an abstract shrubbery
 form. (This terminology is inherited from Lisp and Scheme.)

 @tabular(
  ~column_properties: [#'top],
  [[@hspace(1), @elem{@litchar{fun (x): x+x}}, @elem{@hspace(1) ⇒ @hspace(1)}, double_shrub]]
 )

 A Rhombus module---or any language implemented using shrubbery
 notation---is fully @tech{read} before it is processed further, so
 shrubbery-level syntax errors are always reported first. This is similar
 to the way that programs in some languages are fully @defterm{tokenized}
 before they are parsed further.}

 @item{For the second stage, we use @deftech{parse} or @deftech{parsing} to refer to the
 process of converting an abstract shrubbery into a core language AST.

@tabular(
  ~column_properties: [#'top],
  [[@hspace(1), double_shrub, @elem{@hspace(1) ⇒ @hspace(1)}, @tt{(λx . plus x x)}]]
 )

 Conceptually, the core Rhombus AST is
 the call-by-value λ-calculus with a handful of extensions, such as conditionals and
 quoted values (which is also the same as the core Racket AST).

 Alternatively, everything documented as part of
 @rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus))) could be
 considered the core language, ignoring the fact that much of that
 language is implemented internally as an expansion into other parts and
 into an even simpler language. This alternative view is less tidy
 theoretically, because it means that ``core Rhombus'' is a very big
 language, but it's a productive view for our purposes.

 From this productive perspective,

 @tabular(
  ~column_properties: [#'top],
  [[@hspace(1),double_shrub]]
 )

 is fully parsed, and we could just as well render the representation
 of that program as

 @tabular(
  ~column_properties: [#'top],
  [[@hspace(1), @rhombus('fun (x): x + x')]]
 )

 meaning the core function form with a core addition form in its body.}

)

@section(~tag: "patten+template"){Patterns and Templates}

Before we get to parsing expressions, where we'll have to worry about
infix operators, let's first consider parsing definitions. For example,
let's suppose that instead of the separate @rhombus(class) forms written
in @local_file("interp1.rhm"), we'd like to write

@rhombusblock(
  datatype Expr
  | Id(name :: Symbol)
  | Plus(left :: Expr, right :: Expr)
  | Equals(left :: Expr, right :: Expr)
  | Let(name :: Symbol, rhs :: Expr, body :: Expr)
  | Fun(arg :: Symbol, body :: Expr)
  | Call(fun :: Expr, arg :: Expr)
  | Literal(val :: Any)
)

As the example illustrates, our new @rhombus(datatype) form will expect
an identifier, like @rhombus(Expr), followed by an @bnf.nt{alts}, which
is a sequence of @litchar{|} blocks. Each @litchar{|} block has a single
@bnf.nt{group} containing an identifier (@rhombus(Id), @rhombus(Plus),
etc.) followed by a parenthesized sequence of field specifications. We
don't need to do anything with the fields except propagate them to
@rhombus(class), so let's treat each field specification as a generic
@bnf.nt{term} sequence.

In short, a valid use of @rhombus(datatype) will match this @tech{syntax
 object} pattern:

@rhombusblock(
  'datatype $name
   | $variant($field ..., ...)
   | ...'
)

The pattern has many ellipses:

@itemlist(

 @item{The @rhombus(...) immediately after @rhombus($field) means that
 @rhombus(field) stands for a repetition of @bnf.nt{term}s, instead
 of a single @bnf.nt{term}.}

 @item{There's another @rhombus(...) after the @litchar{,} to mean
 repetitions of @rhombus($field ...), which implies that @rhombus(field)
 stands for a comma-separated repetition of repetitions, i.e., 
 a repetition of depth 2.}

 @item{The @rhombus(...) after @litchar{|} means a @litchar{|}-separated
 repetition of @rhombus($variant($field ..., ...)), so @rhombus(field)
 actually standards for a repetition of depth 3! Meanwhile,
 @rhombus(variant) is a also repetition of depth 1. For any match,
 the count associated with the @rhombus(variant) repetition will be the
 same as the count for the outermost repetition count of
 @rhombus(field).}

)

The repetitions @rhombus(variant) and @rhombus(field) will not record
the @litchar{,} or @litchar{|} separators or @parens that were part of
the match (or, more properly, the corresponding abstract structure of a
shrubbery form that was read previously). The repetitions only record
the matching portions of a syntax object inside those pieces.

If we have a match for that pattern, then we can generate the desired
result using the following @tech{syntax object} template:

@rhombusblock(
  'class $name():
     nonfinal
   class $variant($field ..., ...):
     extends $name
   ...'
)

The @rhombus($variant($field ..., ...)) part of the template
reconstructs the same fragment that it matched in the pattern. The final
@rhombus(...) is in its own group, which means that the preceding group
should be replicated, and thus a @rhombus(class) form is generated for each
@rhombus(variant).

The matching and repetition rules for ellipses are sophisticated in order
to facilitate writing and using clean and expressive patterns like that one.
In particular, take a careful look at
the variable @rhombus(name) as it is used near the end of the template.
It stands for a single matching @bnf.nt{term}
from the pattern, but it is inside @rhombus(...). That's allowed: as long as
a template @rhombus(...) follows something that tells it how many times
to repeat---which would be @rhombus(variant) or @rhombus(field) in this
case---then a non-repetition component is copied as many times as
needed.

Let's check that the pattern and template work as intended:

@examples(
  ~eval:def_eval
  ~defn:
    def input:
      'datatype Expr
       | Id(name :: Symbol)
       | Fun(arg :: Symbol, body :: Expr)
       | Call(fun :: Expr, arg :: Expr)'
  ~repl:
    match input
    | 'datatype $name
       | $variant($field ..., ...)
       | ...':
        "matches the definition of " ++ to_string(name)
  ~repl:
    :
      // same idea in pattern-matching definition form via `def`:
      def 'datatype $name
           | $variant($field ..., ...)
           | ...':
        input
  ~repl:
    :
      // which lets us pull out the pieces to have a look at them
      name
    [variant, ...]
    [[[field, ...], ...], ...]
  ~repl:
    :
      // and even use them in a syntax object to build the classes
      'class $name():
         nonfinal
       class $variant($field ..., ...):
         extends $name
       ...'    
)

@section{Shorthands and Syntax Classes}

The three-level repetition of @rhombus(field) in our @rhombus(datatype)
example is somewhat tedious. The repetition of multiple fields for a
variant is important, and the repetition for multiple variants is also
important, but the innermost repetition is just because a field
description like @rhombus(name :: Symbol) has multiple @bnf.nt{term}s.

To reduce tedious ellipses, such as in the innermost repetition for
@rhombus(field), an escape that is alone in its @bnf.nt{group} in a
pattern can be matched to an entire @bnf.nt{group}. Similarly, a
template allows a whole group to be spliced in place of an escape.

By relying on the whole-group convention, our pattern can be a little
simpler:

@examples(
  ~eval:def_eval
  ~repl:
    def 'datatype $name
         | $variant($field, ...) // dropped one layer of ...
         | ...':
      input
    [[field, ...], ...]
  ~repl:
    'class $name():
       nonfinal
     class $variant($field, ...):
       extends $name
     ...'    
)

Note how the @rhombus([[field, ...], ...]) interaction shows that each
@rhombus(field) in the repetition is a @bnf.nt{group} syntax
object that contains multiple space-separated @bnf.nt{term}s.

Sometimes these shorthands are not exatly what you want, or sometimes
you want to be more strict about what is allowed at some point to
match a pattern variable. In those cases, you can annotate the pattern variable
with a @deftech{syntax class}, which is written by putting the pattern variable
in parentheses, then adding @rhombus(::) followed by the syntax class.

Returning to our example, the @rhombus($name) and @rhombus($variant) escapes in our example will
not match multiple @bnf.nt{term}s, because they are not alone in their
respective @bnf.nt{group}s within the pattern. If we wanted to insist
that @rhombus($field) is matched to a single @bnf.nt{term} despite being
alone in its @bnf.nt{group}, the we could annotate the escape with the
@rhombus(Term, ~stxclass) syntax class by using
@rhombus($(field :: Term)). Similarly, we could use
@rhombus($(field :: Group)) to be explicit about the possibility of
matching a multi-@bnf.nt{term} @bnf.nt{group}. Along the same lines,
we'd like to insist that @rhombus(name) and @rhombus(variant) match an
identifier (as opposed to a number, string, or other @bnf.nt{term}) by
using the @rhombus(Identifier, ~stxclass) syntax class.

@examples(
  ~eval:def_eval
  ~defn:
    fun my_expand(input):
      // match via pattern
      match input
      | 'datatype $(name :: Identifier)
         | $(variant :: Identifier)($field, ...)
         | ...':
          // result via template
          'class $name():
             nonfinal
           class $variant($field, ...):
             extends $name
           ...'
  ~repl:
    my_expand('datatype Expr
               | Id(name :: Symbol)
               | Call(fun :: Expr, arg :: Expr)')
  ~repl:
    ~error:
      my_expand('datatype Expr
                 | 1()
                 | 2()')  
)

You can @seclink("syntax-classes", ~doc: rhombus_doc){define your own}
syntax classes to support new syntactic categories that might be important
for your own constructs, but the built-in set of syntax classes is sufficient
for our purposes in the tutorial. 

@section{Parsing-Time Expressions}

We can match a syntax object in a function and produce a replacement
like @rhombus(my_expand) in the previous example, but that's a run-time
operation. It's too late to affect the way the surrounding program is
parsed and its definitions are discovered. To implement a definition
macro, we need to perform the same matching and substitution at
@deftech{parsing time} instead of @deftech{run time}.

To write parsing-time functions and expressions, we must import the
@rhombusmodname(rhombus/meta) module with
@rhombus(import rhombus/meta open), or we can use the language
@rhombuslangname(rhombus/and_meta) after @hash_lang() instead of
@rhombuslangname(rhombus). The @rhombusmodname(rhombus/meta) module
provides @rhombus(meta), which shifts the evaluation time of its body
from run time to parsing time---a.k.a., @deftech{expand time} or
@deftech{compile time}, since those are all the same time relative to
run time.

@rhombusblock(
  #,(@hash_lang()) #,(@rhombuslangname(rhombus/and_meta))

  println("running")

  meta:
    println("parsing")
)

@margin_note{If you run this module in DrRacket, then
``parsing'' is likely to print twice, because DrRacket's strategy for
debugging programs involves compiling them twice. If you
 turn off debugging via the @onscreen{Choose Languague...} item in the
 @onscreen{Language} menu, then ``parsing'' prints only once in
 DrRacket.}
This example module prints ``running'' when it is run, but it has to be
parsed before it can be run, and so ``parsing'' prints first---even though it
is later in the module. 

If we put @rhombus(my_expand) from the previous example into a
@rhombus(meta) block, then it can be called at parse time,
but the function's behavior is only half of the story. There still needs to be a
connection between @rhombus(my_expand) and uses of @rhombus(datatype) in the
run-time part of the module. We need to specifically use a macro-defining
form to hook into the parsing process and create the connection.

@section{Definition Macros}

The @rhombus(defn.macro, ~defn) form combines @rhombus(meta),
@rhombus(match) for a syntax object pattern, and a hook into the parsing
process for run-time definitions. That combination creates a
@deftech{macro} that applies to definition contexts.

In the following example, @rhombus(defn.macro) creates a parsing-time
function and registers a connection to the name @rhombus(datatype),
because @rhombus(datatype) is the head of the pattern after
@rhombus(defn.macro). When the parser later encounters
@rhombus(datatype) in a definition position, it matches the
@rhombus(datatype) use to the pattern, it and evaluates the body of the
@rhombus(defn.macro) form to get a replacement set of definitions.

@rhombusblock(
  #,(@hash_lang()) #,(@rhombuslangname(rhombus/and_meta))

  defn.macro 'datatype $(name :: Identifier)
              | $(variant :: Identifier)($field, ...)
              | ...':
    // result via template
    'class $name():
       nonfinal
     class $variant($field, ...):
       extends $name
     ...'

  datatype Expr
  | Id(name :: Symbol)
  | Fun(arg :: Symbol, body :: Expr)
  | Call(fun :: Expr, arg :: Expr)

  fun interp(e :: Expr, env :: Map):
    match e
    | Id(name):
        env[name]
    | Fun(arg, body):
        fun (arg_val): interp(body, env ++ { arg: arg_val })
    | Call(fun, arg):
        interp(fun, env)(interp(arg, env))
)

In the @rhombus(defn.macro) form that defines @rhombus(datatype), the
template @quotes expression in the body of the definition is a
parsing-type expression. The code @emph{inside} the @quotes represents a
run-time definition, since it will be spliced in place of a use of
@rhombus(datatype) in a run-time definition position.

@section(~tag: "ex-defn-macro"){Exercise}

Start with with program @local_file("interp_defn_macro.rhm"). If you
replace @rhombus(Id) in @rhombus(datatype Expr) with @rhombus(7), then
you get a nice error message from @rhombus(datatype). But if you replace
@rhombus(name :: Symbol) with @rhombus(7 :: Symbol), then you get an
error with poor reporting, because it comes from @rhombus(class).

Refine the @rhombus(field) part of the @rhombus(datatype) macro's
pattern to insist that a field is an identifier, followed by @rhombus(::), and then
another identifier. That way, any other shape will trigger an error from
@rhombus(datatype) instead of @rhombus(class). Experiment with different, incorrect
syntax to see how the error messages improve.

Solution: @local_file("interp_defn_macro_soln.rhm").

@// ==================================================

@close_eval(def_eval)
