#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def sc_eval = make_rhombus_eval())

@demo(
  ~eval: sc_eval
  ~hidden:
    import:
      rhombus/meta open
    class Posn(x, y)
)

@title(~tag: "syntax-classes"){Syntax Patterns and Classes}

As shown in @secref("syntax"), a variable can be bound in a syntax
pattern by escaping from the pattern with @rhombus($, ~bind). A
@rhombus($, ~bind) can also be followed by a more complex escape. We
have seen the use of the @rhombus(::, ~unquote_bind) operator, for
example. It takes a pattern variable and a @tech{syntax class} name to
specify the kind of syntax the pattern variable can match. The syntax
classes @rhombus(Term, ~stxclass), @rhombus(Group, ~stxclass), and
@rhombus(Multi, ~stxclass) are built in, among others.

@demo(
  ~defn:
    def '$(x :: Term)' = '1'
)

Quotes create a syntax pattern, and they work nested inside an escape to
create a nested pattern. Nesting immediately within an
@rhombus($, ~unquote_bind) escape allows matching a literal
@rhombus($, ~datum) or @rhombus(..., ~datum), analogous to the way those
literals can be included when constructing syntax.

@demo(
  ~defn:
    fun get_amt('$('$') $amt'): // matches `$` followed by any term
      amt
  ~repl:
    def price_tag = '$('$') 17'
    price_tag
    get_amt(price_tag)
    ~error:
      get_amt('€ 17')
)

Nested patterns are more useful with operators like
@rhombus(||, ~unquote_bind) and @rhombus(&&, ~unquote_bind), which
take two syntax bindings and ensure that at least one matches or that
both match, respectively. In particular, combining
@rhombus(&&, ~unquote_bind) with an identifier can give a name to a
nested match.

@demo(
  ~defn:
    :
      // matches a parenthesized `*` term and names it `mult`
      fun get_mult('1 + $(mult && '($_ * $_)')'):
        mult
  ~repl:
    get_mult('1 + (2 * 3)')
    ~error:
      get_mult('1 + (2 / 3)')
)

When a multi-term syntax pattern is used in a @rhombus($, ~bind) escape
in a term context, the multi-term pattern is spliced into the enclosing
group pattern. The @rhombus(||, ~unquote_bind) operator can try
spliced sequences that have different lengths.

@demo(
  ~defn:
    // matches an optional `+ 1` at the front
    fun get_area_code('$('+ 1' || '') ($code) $_ - $_'):
      code
  ~repl:
    get_area_code('+ 1 (801) 555 - 1212')
    get_area_code('(801) 555 - 1212')
)

Although Rhombus supports new binding operators through
@rhombus(unquote_bind.macro), syntax classes provide a better way to
organize most syntax abstractions. To define a new syntax class, use the
@rhombus(syntax_class) form:

@demo(
  ~defn:
    syntax_class Arithmetic
    | '$x + $y'
    | '$x - $y'
)

Defining a syntax class in this way makes it available for use in syntax
patterns, such as in @rhombus(def) or @rhombus(match). The syntax class must be defined
at the same phase as the referencing pattern. To define a syntax class
for use in a macro definition, place it inside a
@rhombus(meta) block.

@demo(
  ~eval: sc_eval
  ~defn:
    meta:
      syntax_class Arithmetic
      | '$x + $y'
      | '$x - $y'
)

Once defined, a syntax class can be used to annotate a pattern
variable that matches any of pattern alternatives specified in the
syntax class.

@demo(
  ~eval: sc_eval
  ~defn:    
    expr.macro 'add_one_to_expr $(a :: Arithmetic)':
      '$a + 1'
  ~repl:
    add_one_to_expr 1 + 1
    add_one_to_expr 1 - 2
    ~error: add_one_to_expr 2 > 3
)

The @rhombus($)-escaped variables in a syntax class's patterns bind to
matched syntax objects as fields of the class. They can be accessed
from a pattern variable using dot notation.

@demo(
  ~eval: sc_eval
  ~defn:
    expr.macro 'right_operand $(a :: Arithmetic)':
      a.y
  ~eval:
    right_operand 2 + 3
    right_operand 8 - 4
)

A field is accessible only when it appears in every pattern
alternative of a syntax class.

@demo(
  ~eval: sc_eval
  ~defn:
    syntax_class Arithmetic
    | '$x + $y + $z'
    | '$x - $y'
  ~repl:
    def '$(a :: Arithmetic)' = '1 + 2 + 3'
    a.y
    ~error: a.z
)

In other words, the fields of a syntax class are defined by the intersection 
of all escaped pattern variables found in the pattern alternatives. That's more
flexible than @rhombus(||, ~unquote_bind), which does not bind identifiers
from either of its arguments.
