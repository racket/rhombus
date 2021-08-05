- Feature Name: Candidate Rhombus prototype using Shrubbery Notation
- Start Date: 2021-07-30
- RFC PR: [racket/rhombus-brainstorming#163](https://github.com/racket/rhombus-brainstorming/pull/163)

# Summary
[summary]: #summary

[Shrubbery notation](https://github.com/mflatt/rhombus-brainstorming/blob/shrubbery/shrubbery/0000-shrubbery.md)
and the
[Rhombus expander proposal](https://github.com/mflatt/rhombus-brainstorming/blob/enforestation/enforestation/0000-enforestation.md)
provide a foundation for building a language. This proposal describes a prototype
language built on those pieces.

# Motivation
[motivation]: #motivation

Let's try putting things together. There's much more to do for a full
language, but this proposal probably has enough to gauge its
particular theme and direction.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

## Shrubbery notation

Before we start, here's a recap on shrubbery notation.

_If you install
[https://github.com/mflatt/shrubbery-rhombus-0](https://github.com/mflatt/shrubbery-rhombus-0),
then you can run `#lang shrubbery` (not `#lang rhombus` for this part)
to see how example shrubberies parse into an S-expression
representation. Unfortunately, it needs a development version of
Racket right now, so you may need to install a
[snapshot build](https://snapshot.racket-lang.org/)._

Numbers are decimal, either integer or floating-point, or they're
hexadecimal integers written with `0x`:

```
0
42
-42
1_048_576
3.14157
.5
6.022e23
0xf00ba7ba2
```

Identifiers are Unicode alphanumeric and `_` with an initial character
that is not numeric.

```
pi
scissor7
π
underscore_case
camelCase
```

These characters are used for shrubbery structure and are not
available for use in operators:

```
( ) [ ] { }   ; ,   : |   \   " '  # @
```

Any other Unicode punctuation or symbol is fair game for an
operator:

```
+
.
->
>=
!^$&%$
```

To avoid potential confusion with operators alongside numbers,
however, an operator that ends in `+`, `-`, or `.` must consist only
of that character. So, `++` and `...` are operators, but `!+` is not.
Similar problems happen with comments, so an operator cannot contain
`//` or `/*` or have multiple characters and end in `/`.

Keywords are like identifiers, but wrapped with `'`:

```
'base'
'stronger_than'
```

Booleans:

```
#true
#false
```

Strings and byte strings:

```
"This is a string, just like you'd expect"
#"a byte string"
```

Comments are C-style, but block comments are nestable:

```
// This is a line comment

/* This is a multiline
   comment that /* continues */
   on further lines */
```

If you need anything more at the lexeme level (such as fancier
numbers), escape to S-expression notation with `#{` ... `}`.

Shrubbery notation is whitespace-sensitive, and it uses line breaks
and indentation for grouping. A line with more indentation starts a
_block_, and it's always either after a line that ends `:` or on a
line starts with `|`:

```
begin:
  group within block
  another group within block

if is_rotten(apple)
 | get_another()
 | take_bite()
   be_happy()

match x
 | 0:
    def zero = x
    x + zero
 | n:
    n + 1
```

_Even if you don't normally use DrRacket, you should proabably try it
while reading this proposal, since that's where the syntax coloring
and indentation support for `#lang shrubbery` and `#lang rhombus` are
currently implemented. Syntax coloring uses the same lexer as the
language's implementation, so it can't get out of sync. For
indentation, hit Tab to cycle through the possible indentations for a
line (based on preceding lines). If multiple lines are selected, then
Tab cycles through possibilities for the first selected line and
shifts remaining lines by same amount. There should be some way to
normalize indentation while preserving the current parse, but that's
not yet implemented._

A `|` at a new indentation starts two blocks: one that has multiple
`|`-starting parts within the same block, plus a subblock on the
right-hand side of the `|`. Each subsequent `|` at the same
indentation creates a new subblock. Shrubbery notation allows `|` only
within a block whose groups all contain an immediate `|` subblock.
A block of `|` subblocks is an _alts-block_.

Each line within a block forms a _group_. Groups are important,
because post-shrubbery parsing and macro expansion are constrained to
operate on groups (although a group can contain nested blocks, etc.)
Groups at the same level of indentation as a previous line continue
that group's block. A `|` can have multiple groups in the subblock to
its right, but the block started by `|` turns out to be the only thing
in its own group.

A `:` doesn't have to be followed by a new line, but it starts a new
block, anyway. Similarly, a `|` that starts a block doesn't have to be
on a new line. These examples parse the same as the previous
examples:

```
begin: group within block
       another group within block

if is_rotten(apple) | get_another() | take_bite()
                                      be_happy()

match x | 0: def zero = x
             x + zero
        | n: n + 1
```

Extra `:`s are ok and end up being ignored. For example, `match x:`
would work the same as `match x` just before a `|` that's on the same
line or the next line. Standard style is to not use redundant `:`s.

Within a block, a `;` can be used instead of a new line to start a new
group, so these examples also parse the same:

```
begin: group within block; another group within block

if is_rotten(apple) | get_another() | take_bite(); be_happy()

match x | 0: def zero = x; x + zero
        | n: n + 1
```

You can add extra `;`s, such as at the end of lines, since `;` will
never create an empty group.

Finally, `:` plus indentation can be written instead with `{` ... `}`,
so blocks can be fully braced, if you like. In the following example,
three pairs of braces replace three `:`s, while the other pairs are
allowed but redundant due to `|` rules, and every block in the example now has braces
(but this is definitely not the intended style):

```
begin { group within block; another group within block }

if is_rotten(apple) { | { get_another() } | { take_bite(); be_happy() } }

match x { | { 0 { def zero = x; x + zero } }
          | { n { n + 1 } } }
```

Parentheses `(` ... `)` and square brackets `[` ... `]` similarly
combine a sequence of groups. Unlike `{` ... `}`, a comma `,` can be
used to separate groups on one line between `(` ... `)` or `[` ...
`]`. Also unlike `{` ... `}`, a `,` is _required_ to separate groups
within between `(` ... `)` or `[` ... `]`, even if they're not on the same
line. You can't have extra `,`s, except after the last group.

```
f(1, 2,
  3, 4)

["apples",
 "bananas",
 "cookies",
 "milk"]

map(add_five, [1, 2, 3, 4,])
```

Indentation still works for creating blocks within `{` ... `}`, `(`
... `)` or `[` ... `]`:

```
map(fun (x):
      x + 5,
    [1, 2, 3, 4])
```

There are some subtleties related to the “precedence” of `|`, `;`, and
`,`, but they're likely to work as you expect in a given example. When
in doubt, you can add parentheses or curly braces.

## Modules

A Rhombus module, which implements a program or a library, always
starts `#lang rhombus`. If you write an expression at the top of a
module, then its value gets printed out.

```
#lang rhombus

1+4  // prints 5

"Hello, world!"  // prints "Hello, world!", including the quotes
```

The ways to define names in a module include `val` and `fun`.
The `val` form defines an immutable variable, and it expects an
identifier to define followed by a block. The `fun` form defines
a function when it see an identifier, parentheses, and then a block.

```
#lang rhombus

val fahrenheit_freezing: 32

fun fahrenheit_to_celsius(f):
  (f - 32) * 5/9

fahrenheit_to_celsius(fahrenheit_freezing)  // prints 0
```

_If you have installed the `shrubbery-rhombus-0` package, then the
interactions window in DrRacket will work to call
`fahrenheit_to_celsius`. For now, though, you have to add an extra
line with just `;` to evaluate a form._

A Rhombus module can export definitions to other modules using
`export`, and it can import other modules using `import`. The
`#lang rhombus` line is a kind of `import` already, so normally more
`import`s are written at the top of a module, and then `export`s,
and then the definitions.

```
// f2c.rhm
#lang rhombus

export:
  fahrenheit_freezing
  fahrenheit_to_celsius

val fahrenheit_freezing: 32

fun fahrenheit_to_celsius(f):
  (f - 32) * 5/9
```

```
// freezing.rhm
#lang rhombus

import:
  "f2c.rhm"

f2c.fahrenheit_to_celsius(f2cfahrenheit_freezing)  // prints 0
```

Unlike Racket, imported bindings must accessed using a prefix name and
then `.`, at least by default. The prefix is inferred from a module
path by taking its last component and removing any extension, so
that's why the import of `"f2c.rhm"` leads to the `f2c` prefix. To
supply an explicit prefix, use `=`:

```
import:
  convert = "f2c.rhm"

convert.fahrenheit_to_celsius(convert.fahrenheit_freezing)
```

Using `=` with no name before it imports without a prefix, but this
kind of “namespace dumping” is considered bad style in most cases:

```
import:
  = "f2c.rhm"

fahrenheit_to_celsius(fahrenheit_freezing)
```

_If the left-dangling `=` as above is slightly uncomfortable, that may
be good as a discoragement again dumping._

Module paths in Racket are written with a `/` separator. In Rhombus,
`.` as an import operator combines module-path elements, and the last
element is the one tat determines the default import prefix:

```
import:
  racket.math

math.pi  // prints 3.141592653589793
```

_The use of `.` with an import name as a hierarchical reference is not
the same as the `.` described in the next section. See also the
current [rationale](#rationale-and-alternatives)._


## Definitions

Besides `val` and `fun`, `struct` is a definition form that
defines a new structure type. By convention, structure names start
with a capital letter.

```
struct Posn(x, y)
```

A structure-type name can be used like a function to construct an
instance of the structure type. The structure-type name followed by
`.` and a field name gets a function to extract the field value from
an instance of the structure.

```
val origin: Posn(0, 0)

origin  // prints Posn(0, 0)

Posn.x(origin)  // prints 0
```

_Printing is not yet implemented in the `shrubbery-rhombus-0` package,
so you'll see Racket printing for now._

A structure-type name can also be used after a function-argument name
and `::` to constrain the values that are allowed for the argument.
This `flip` function will report an error if its argument is not a
`Posn` instance:

```
fun flip(p :: Posn):
  Posn(Posn.y(p), Posn.x(p))

// flip(0)  // would be a run-time error
flip(Posn(1, 2))  // prints Posn(2, 1)
```

Furthermore, the `:: Posn` declaration on `p` makes `.` work directly
to access fields of `p`:

```
fun flip(p :: Posn):
  Posn(p.y, p.x)
```

The use of `::` in this way is not specific to `fun`. The `::`
binding operator works in any binding position, including the one for
`val`:

```
val (flipped :: Posn):  flip(Posn(1, 2))

flipped.x  // prints 2
```

Finally, a structure-type name like `Posn` can also work in binding
positions as a pattern-matching form. Here's a implementation of
`flip` that uses pattern matching for its argument:

```
fun flip(Posn(x, y)):
  Posn(y, x)

// flip(0)  // would be a run-time error
flip(Posn(1, 2))  // prints Posn(2, 1)
```

As a function-argument pattern, `Posn(x, y)` both requires the
argument to be a `Posn` instance and binds the identifiers `x` and `y`
to the values of the instance's fields.

As you would expect, the fields in a `Posn` binding pattern are
themselves patterns. Here's a function that works only on the origin:

```
fun flip_origin(Posn(0, 0)):
  origin

// flip_origin(Posn(1, 2))  // would be a run-time error
flip_origin(origin)  // prints Posn(0, 0)
```

Finally, a function can have a result contract, which is written with
`::` after the parentheses for the function's argument. Every return
value from the function is checked against the contract. Beware that a
function's body does not count as being tail position when the
function is declared with a result contract.

```
fun same_posn(p) :: Posn:
  p

same_posn(origin)  // prints Posn(0, 0)
// same_posn(5)    // woudl be a run-time error
```

The `def` form is a kind of do-what-I-mean form that acts like
`val`, `fun`, or certain other definition forms depending on
the shape of the terms after `def`. It's sensitive to binding
forms, though, so it will not treat the immediate use of a pattern
constructor as a function definition.

```
def pin: Posn(3, 4)

def distance(Posn(x, y), Posn(x2, y2)):
  def dx: x2-x
  def dy: y2-y
  sqrt(dx*dx + dy*dy)

distance(origin, pin)  // prints 5

def Posn(pin_x, pin_y): pin
pin_x  // prints 3
```

_Is `def` a good idea? See the current [rationale](#rationale-and-alternatives)._

The `let` form is like `def`, but it makes bindings available
only _after_ the definition, and it shadows any binding before, which
is useful for binding a sequence of results to the same name. The
`let` form does not change the binding region of other
definitions, so a `def` after `let` binds a name that is
visible before the `let` form.

```
def get_after(): after

def accum: 0
let accum: accum+1
let accum: accum+1
accum  // prints 2

def after: 3
get_after()  // prints 3
```

## Contracts and the dot operator

Besides structure types defined with `struct`, a few predefined
contracts work with the `::` contract operator, including `Integer`
(meaning exact integer), `Number`, and `String`.

_The currently prototype doesn't offer a direct way to define new
predicate-based contracts, but that will be straightforward. Since a
binding operator gets an opportunity to adjust the value delivered to
a variable, non-predicate contracts fit naturally into the language's
framework._

The `::` operator also works in expression positions. In that case,
the right-hand expression must produce a value that satisfies the
right-hand contract, otherwise a run-time contract exception is
raised.

When `struct` defines a new structure type, a contract can be
associated with each field. The contract is checked when an instance
is created.

```
struct Posn(x :: Integer, y :: Integer)

Posn(1, 2)       // prints Posn(1, 2)
// Posn(1, "2")  // would be a run-time error
```

Naturally, structure-type contracts can be used as field contracts,
and then the `.` operator can be chained in the expected way:

```
struct Line(p1 :: Posn, p2 :: Posn)

def l1 :: Line:
  Line(Posn(1, 2), Posn(3, 4))

l1.p2.x  // prints 3
```

More generally, the left-hand side of `.` needs to be an expression
that can act as a _dot provider_. A structure-type name is a dot
provider, and it provides access to field-accessor functions, as in
`Posn.x` (which doesn't get a specific `x`, but produces a function
that can be called on a `Posn` instance to extract its `x` field). An
identifier that is bound using `::` and a structure-type name is also a
dot provider, and it provides access to fields of a structure instance.
More generally, a contract that is associated to a binding or
expression with `::` might give the binding or expression a dot
provider. See [a separate document](dot-provider.md) for more
information on dot providers.

_Using `.` to reach an imported binding, as in
`f2c.fahrenheit_to_celsius`, is a different kind of `.` than the infix
expression operator. See the current
[rationale](#rationale-and-alternatives)._


## Function expressions

The `fun` form also works in a function position (as λ). Just
like `fun` in JavaScript, the expression variant omits a function
name.

```
val curried_add: fun (x):
                   fun (y):
                     x+y

curried_add(10)(20)  // prints 30
```

Naturally, keyword and optional arguments (as described in the next
section) work with `fun` expressions, too.

## Keyword and optional arguments

A function argument can be made optional by using `=` after the
argument's pattern and providing a default-value expression after `=`:

```
fun scale(Posn(x, y), factor = 1):
  Posn(factor * x, factor * y)

scale(Posn(1, 2))     // prints Posn(1, 2)
scale(Posn(1, 2), 3)  // prints Posn(3, 6)
```

By-keyword arguments are often useful for functions that have multiple
optional arguments. A keyword argument is indicated by prefixing a
formal or actual argument with a shrubbery keyword, which is written
between `'`s, and then starting a block with `:`.

```
fun transform(Posn(x, y),
              'scale': factor = 1,
              'dx': dx = 0,
              'dy': dy = 0):
  Posn(factor*x + dx, factor*y + dy)

transform(Posn(1, 2))           // prints Posn(1, 2)
transform(Posn(1, 2), 'dx': 7)  // prints Posn(8, 2)
transform(Posn(1, 2), 'dx': 7, 'scale': 2)  // prints Posn(9, 4)
```

Since a keyword by itself is not allowed as an expression or pattern,
there is no possibility that a keyword will be inadvertently treated
as an actual argument or binding pattern by itself.

_Why `'` for keywords? See the rationale in the 
[Shrubbery notation](https://github.com/mflatt/rhombus-brainstorming/blob/shrubbery/shrubbery/0000-shrubbery.md)
proposal._

_The keyword prefix and `=` for default values are not binding
operators. They are specific to the syntax of `fun`._

If an argument name is the same as its keyword (just without the
`'`s), then the `:` argument name can be omitted. That only works for
an argument that would otherwise be just an identifier and maybe a
default value, because keywords don't work as variable names
in binding patterns.

```
fun transform(Posn(x, y),
              'scale': factor = 1,
              'dx' = 0,
              'dy' = 0):
  Posn(factor*x + dx, factor*y + dy)
```

## Conditionals and pattern-matching dispatch

The `&&` and `||` operators are short-circuiting “and” and ”or” forms.
As in Racket, `||` returns the first non-`#false` value, and `&&`
returns the last non-`#false` value. 

```
1 < 2 && "ok"  // prints "ok"
```

Comparison operators and `!` (for “not”) have higher precedence than
`&&` and `||`, while `&&` has higher precedence than `||`. Arithmetic
operators have higher precedence than comparison operators, `||`,
`&&`, but they have no precedence relative to `!`. The `==` operator
is numerical comparison like Racket's `=`, while `===` operator is
Racket's `equal?`. Comparison operators are non-associative and have
no precedence relationship with each other.

The `if` form expects a test expression followed by an alts-block with
two `|`s. The first `|` holds the “then” branch, and the second `|`
holds the “else” branch:

```
if 1 == 2
 | "same"
 | "different"
```

Shrubbery reminder: You can write a `:` after the test expression for
`if`, but a `:` is not needed when the first indented line starts with
`|`.

Although an `if` could be nested further in the “else” branch to
implement an “if” ... “else if” ... “else if” ... combination, the `cond`
form supports that combination better. It expects an alts-block where each
`|` has a test expression followed by a block. Evaluating the `cond` form dispatches
to the block after first test that produces a non-`#false` value. The
`'else'` keyword can be used in place of a last test.

```
fun fib(n):
  cond
   | n == 0: 1
   | n == 1: 1
   | 'else': fib(n-1) + fib(n-2)

fib(5) // prints 8
```

If there's no `'else'` case and no matching case, then `cond` reports
an error at run time (unlike Racket, which returns void in that case).
Note that `'else'` is a keyword, and not an identifier. If it were an
identifier, then `else` might get bound in some context to `#false`,
which would be confusing.

Although `cond` is better than `if` for `fib`, the `match` form is
even better. The `match` form expects an expression and then an
alts-block where each `|` has a binding pattern followed by a block.
The `match` form evaluates that first expression, and dispatches to
the first block whose pattern accepts the expression's value. Similar
to `cond`, `match` supports `'else`' in place of a final binding
pattern:

```
fun fib(n):
  match n
   | 0: 1
   | 1: 1
   | 'else': fib(n-1) + fib(n-2)
```

This kind of immediate pattern-matching dispatch on a function
argument is common enough that `fun` supports it directly,
fusing the function declaration and the pattern match, like this:

```
fun 
 | fib(0): 1
 | fib(1): 1
 | fib(n): fib(n-1) + fib(n-2)
```

There's no `'else'` for this fused form. Also, the function name and
all relevant argument positions have to be repeated in every case, but
that's often a readable trade-off. Match-dispatching functions cannot
have optional or keyword arguments, but different cases can have
different numbers of arguments, and a call will find a matching case
with the right number of arguments.

```
fun
 | hello(name):
    "Hello, " +$ name    // +$ coerces to strings and concatenates
 | hello(first, last):
    hello(first +$ " " +$ last)

hello("World")             // prints "Hello, World"
hello("Inigo", "Montoya")  // prints "Hello, Inigo Montoya"
```

## Multiple values

The `values` form returns multiple values:

```
values(1, "apple") // prints 1 and "apple"
```

When an expression in a module body returns multiple values, each one
is printed, the same as in `#lang racket`.

When the `val` binding form is followed by parentheses with _N_
groups, then the right-hand side should produce _N_ values, and each
value is matched against the corresponding group.

```
val (n, s): values(1, "apple")

n  // prints 1
s  // prints "apple"
```

A definition binding with with `val` or `def` can also use
`values` in the outermost pattern, and that's the same as not writing
`values`, but makes the receiver and sender side look more the same:

```
def values(n, s): values(1, "apple")

n  // prints 1
s  // prints "apple"
```

As in Racket, multiple values are not a tuple value. They must be
specifically received as values. The `values` binding pattern works
only with definition forms that recognize it, and not, for example, as
a function argument.

_Get rid of multiple values? See the current
[rationale](#rationale-and-alternatives)._

## Mutable variables

Variables are immutable unless they are declared with the `mutable`
binding operator. The `=` infix operator assigns to a mutable variable
while also returning the variable's new value.

```
def mutable todays_weather: "sunny"

todays_weather            // prints "sunny"
todays_weather = "rainy"  // prints "rainy"
todays_weather            // prints "rainy"

def f(mutable x):
  x = x + 8
  x

f(10)  // prints 18

// f = 5 // would be an error: f is not mutable
```

_The `=` operator should also cooperate with `.` when a structure-type
field is declared `mutable`, but that's not yet implemented._

## Operators

The `operator` form defines a prefix or infix operator for
expressions, similar to a function definition:

```
operator (x <> y):
  Posn(x, y)

1 <> 2  // prints Posn(1, 2)

operator (<<>> x):
  Posn(x, x)

<<>> 3  // prints Posn(3, 3)
```

An “operator” name does not have to be a shrubbery operator. It can be
an identifier:

```
operator (x mod y):
  x - floor(x / y) * y

10 mod 3  // prints 1
```

_It allowing identifiers as “operators” a good idea? See the current
[rationale](#rationale-and-alternatives)._

The `operator` form must be followed by parentheses and then a block.
Inside the parentheses, there must be exactly two or three terms, and
the next-to-last term must be an operator or identifier to define. The
arguments can be described by binding patterns, but in that case, they
may need parentheses around the pattern to ensure that they form a
single term in next to the operator being defined:

```
operator ((x :: Integer) <> (y :: Integer)):
  Posn(x, y)

// 1 <> "apple"  // would be a run-time error
```

An operator can be defined for both infix and prefix behavior in much
the same way that functions can be defined to accept one or two
arguments:

```
operator 
 | ((x :: Integer) <> (y :: Integer)):
     Posn(x, y)
 | (<> (x ::Integer)):
     Posn(x, x)

1 <> 2  // prints Posn(1, 2)
<> 3    // prints Posn(3, 3)
```

Operator precedence is declared in relationship to other operators
when the operator is defined. With no precedence defined, `<>` cannot
appear near an arithmetic operator like `*`:

```
// 1 <> 2 * 3  // would be a syntax error
1 <> (2 * 3)   // prints Posn(1, 6)
```

The initially defined operators mostly have the usual precedence: `*`
and `/` are stronger than `+` and `-`, while `+` and `-` have the same
predence and are left-associative. The `*` and `/` operator have the
same precedence as long as `*` appears only to the left of `/`,

A precedence declaration in `operator` takes the form of keyword
blocks at the start of the operator's body. The possible keyword
options for prefix operators are `'weaker_than'`, `'stronger_than'`,
`'same_as'`, or `'same_as_on_left'`. For infix operators, those options
 apply, as well as `'same_as_on_right'` and `'associativity'`. Operators
listed with keywords like `'weaker_than'`
can be grouped on lines however is convenient.

```
operator (x <> y):
  'weaker_than': * / 
                 + -
  'associativity': 'right'
  Posn(x, y)

1 <> 2 * 3  // prints Posn(1, 6)
1 <> 2 <> 3 // prints Posn(1, Posn(2, 3))
```

Use the keyword `'other'` in `'weaker_than'`, `'stronger_than'`, or
`'same_as'` to declare a precedence relationship for operators not
otherwise mentioned.

To export an operator, use an `operator` block with `export`:

```
export:
  operator: <>
```

_Why require `operator:` here? See the current
[rationale](#rationale-and-alternatives)._

On the import side, to refer to an operator that has a prefix, put the
operator after `.` in parentheses:

```
import:
  "posn.rhm"

1 posn.(<>) 2
```

If the point of an operator is terseness, an import prefix may defeat
the point. Using a library that supplies operators may be one reason
to import with a leading `=` to avoid a prefix on the imports.


## Syntax objects

The `?` operator quotes an immediately following shrubbery term to
produce a syntax object. The syntax object holds an unparsed
shrubbery, not a parsed Rhombus expression.

```
?1         // prints a shrubbery: 1
?hello     // prints a shrubbery: hello
?(1 + 2)   // prints a shrubbery: (1 + 2)
?(x:
    y)     // prints a shrubbery: x { y }
// ?1 + 2  // would be a run-time error, since ?1 is not a number
```

The `?` operator is more precisely a quasiquoting operator. The `¿`
unquotes the immediate following terms. That is, the term after `¿` is
a Rhombus expression whose value replaces the `¿` and its argument
within the quoted form.

```
?(1 + ¿(2 + 3))  // prints a shrubbery: (1 + 5)
```

_Mac users with a U.S. keyboard layout: Option-? produces “¿”._

In case `¿` is too difficult to type, the `??` operator is an alias
for `¿`.

```
?(1 + ??(2 + 3))  // prints a shrubbery: (1 + 5)
```

A `¿` only unquotes when it is followed by a term, otherwise the `¿`
itself remains quoted.

```
?(1 + ¿(? ¿) 2)  // prints a shrubbery: (1 + ¿ 2)
```

Besides `¿`, `...` is treated specially within a `?`-quoted term
(except, like `¿` when it's the only thing in the term). When `...`
immediately follows a term that includes at least one `¿`, the value
of the expression after that `¿` must produce a parenthesized-group
syntax object. Then, instead of the parenthesized group in place of
`¿`, the term before `...` is replicated as many times as the
parenthesized group has terms, and each of those terms is used in one
replication.

```
def seq: ?(1 2 3)

?((hi ¿seq) ...) // prints a shrubbery: ((hi 1) (hi 2) (hi 3))
```

There's a subtlety here: could `seq` have zero elements? If so,
replicating the `hi` form zero times within a group would leave an
empty group, but a shrubbery never has an empty group. The same
problem happens with any attempt to represent an empty sequence in the
first place. To manage this gap, a parenthesized shrubbery form with
zero groups is treated as equivalent for `¿` replication to a
parenthesized form with an empty group. Similarly, when a `?` form
describes a single parenthesized group that turns out to be
empty, it instead produces a parenthesized form with zero groups.
Attempting to generate a group with no terms within a parenthesized
form with multiple groups is an error.

```
def seq: ?()

?((hi ¿seq) ...)          // prints a shrubbery: ()
// ?(x, (hi ¿seq) ..., y) // would be a run-time error
```

Square-bracket forms and block forms have the same special cases to
deal with an empty group as parenthesis forms.

When `...` is the only term in a group, and when that group follows
another, then `...` replicates the preceding group. For example,
putting `...` after a `,` in parentheses means that it follows a the
group before the `,`, which effectively replicates that group with its
separating comma:

```
def seq: ?(1 2 3)

?(hi ¿seq, ...) // prints a shrubbery: (hi 1, hi 2, hi 3)
```

Along the same lines, `...` just after a `|` can replicate a preceding
`|` block:

```
def seq: ?(1 2 3)

?{ | ¿seq | ... } // prints a shrubbery: { | 1 | 2 | 3 }
```

In other words, `...` in various places within a quoted shrubbery
works the way you'd expect it to work.

When `?` is used in a binding position, it constructs a pattern that
matches syntax objects, and it binds variables that are escaped in the
pattern with `¿`.

```
val ?(¿x + ¿y): ?(1 + (2 + 3))

x  // prints a shrubbery: 1
y  // prints a shrubbery: (2 + 3)
```

_This is like `match`, and not like `syntax-case`. See the current
[rationale](#rationale-and-alternatives)._

A `¿`-escaped variable in a `?` pattern matches one term. Keep in mind
that `?` creates syntax objects containing shrubberies that are not
yet parsed, so a variable will not be matched to a multi-term sequence
that would be parsed as an expression. For example, a pattern variable
`y` by itself cannot be matched to a sequence `2 + 3`:

```
// val ?(¿x + ¿y): ?(1 + 2 + 3)  // would be a run-time error
```

Meanwhile, `...` works the way you would expect in a pattern, matching
any `...`-replicated pattern variables to form a sequence of matches:

```
val ?(¿x + ¿y ...): ?(1 + 2 + 3)

x  // prints a shrubbery: 1
y  // prints a shrubbery: (2 + 3)
```

_In the current implementation of `?` patterns, a `¿` escape must be
followed by an identifier. Generalizing the position after `¿` can
open the door to more `syntax-parse` goodness._

## Expression macros

Macros extend the syntax available for expressions, bindings,
definitions, and more. Each kind of macro extension has a different
protocol and a different form for defining a macro. In the case of
expressions, a macro receives the sequence of terms starting with the
macro operator/identifier within a group, and the macro returns two
values: an expansion and the remaining terms that the macro did not
consume.

For example, here's a `thunk` macro that expects a block and wraps as
a zero-argument function:

```
expression_macro ?(thunk { ¿body ... } ¿tail ...):
  vals(?(fun () { ¿body ... } ), tail)

thunk { 1 + "oops" } // prints a function
thunk { 1 + 3 } ()   // prints 4
```

The `expression_macro` form expects a `?` and then parentheses to
create a pattern that matches a sequence of terms. Either the first or
second term within the pattern is an _unescaped_ identifier or
operator to be defined; conceptually, it's unescaped because the macro
matches a sequence of terms that use that identifier or operator
literally. If the first term in the pattern is an unescaped identifier
or operator, a prefix macro is defined; otherwise, the second term
must be unescaped, and an infix macro is defined.

In the second use of `thunk` above, the `thunk` macro consumes the
block containing `1 + 3`, but it does not consume the `()`, so parsing
continues and produces a call to thunk. Note that returning `tail`
from the macro is the same as returning `?(¿tail ...)`.

_The current implementation's reporting of errors is especially bad
for a macro use that doesn't match the macro's pattern, but there
appears to be no fundamental obstacle to improving error reporting._

A postfix macro can be implemented as an infix operator that
consumes no additional terms after the operator. For example, a
postfix `!` might be defined (shadowing the normal `!` for “not”) like
this:

```
expression_macro ?(¿a ! ¿tail ...):
  vals(?(factorial(¿a)), tail)

fun
 | factorial(0): 1
 | factorial(n): n*factorial(n-1)
         
10! + 1 // = 3628801
```

When the macro transformer for `!` is called, `a` will be bound to a
syntax object representing a parsed Rhombus expression, as opposed to
an unparsed shrubbery. Currently, there's no way for a transformer to
inspect a parsed Rhombus expression (except by escaping to Racket).
When the parsed expression is injected back into an unparsed
shrubbery, as happens in `?(factorial(¿a))`, it will later simply
parse as itself.

_The current implementation does not track the category of a parsed
term, which would enable reporting a specific error if a parsed
expression is put into a non-expression context. Probably it should do
that._

The `def` form turns itself into `expression_macro` when it is
followed by a `?` pattern that would be suitable for
`expression_macro`.

Besides `expression_macro`, there is also `expression_operator`. The
`expression_operator` form provides the same parsing benefits for a
right-hand expression argument as `expression_macro` provides
already for the left-hand argument of an infix macro. Normally, in
that case, you might as well use `operator`, but `expression_operator`
provides control over evaluator order. For example, this `+<=`
operator is like `+`, but evaluates its right-hand side before it's
left-hand side:

```
expression_operator ?(¿a +<= ¿b):  
  ?(¿b + ¿a)

1 +<= 2                       // prints 3
// (1+"oops") +<= (2+"ouch")  // would complain about "ouch", not "oops"
```

In the same way that `operator` supports operators that are both
prefix and infix, you can use an alt-block with `expression_macro` or
`expression_operator` to create a prefix-and-infix macro.

_It would make sense for `expression_operator` to further support
multiple cases that differ by pattern instead of infix vs. prefix,
but the implementation does not yet do that._

## Definition and declaration macros

The `definition_macro` form defines a definition macro. It is similar
to `expression_macro` in prefix form, except that the name must be an
identifier (never an operator), and the result syntax object should
represent a block, which is spliced into the definition context where
the macro is used.

Here's the classic `def_five` macro:


```
definition_macro ?(def_five ¿id):
  ?{
    def ¿id: 5
  }

def_five v
v  // prints 5
```

Declarations macros are written with `declaration_macro`, and the
block produced by expansion can use forms like `import` and
`export`.

By distinguishing between expression macros, definition macros, and
declaration macros, Rhombus can report errors for out-of-place uses
earlier and more clearly than Racket.

_In the Racket-level interface, it's possible to bind an identifier to
both expression and definition transformers, etc. Adding a Rhombus
syntax for that combination should be straightforward._


## Binding macros

Macros can extend binding-position syntax, too, via `binding_macro`
and `binding_operator`. In the simplest case, a binding operator is
implemented by expanding to other binding operators, like this
definition of `$` as a prefix operator to constrain a pattern to
number inputs:

```
binding_operator ?($ ¿n):
  ?(¿n :: Number)

val $salary: 100.0

salary  // prints 100.0
```

More expressive binding operators can use a lower-level protocol where
a binding is represented by transformers that generate checking and
binding code. It gets complicated, so the details are pushed out to [a
separate document](binding-macros.md). After an expressive set of
binding forms are implemented with the low-level interface, however,
many others can be implemented though simple expansion.


## More
[more]: #more

Probably `[` ... `]` notation should be used for literal lists as
expression and list patterns as bindings.

Array-reference notation _expr_ `[` _key_ `]` should probably be a
kind of general accessor form, where contract annotations can
specialize to a specific reference operator (such as Racket's
`list-ref`, `vector-ref`, `hash-ref`, etc.).

Maybe function declarations should allow result contracts, which would
be checked before returning from the function, and then a function
call could have a static contract—generalizing the static contract on
constructor calls. Meanwhile, an unchecked mode (not quite as far as
unsafe) could elide all contract checks that would otherwise guard
`::` annotations.

Contract annotations could be supported in `export`.


# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

There's no reference, but the prototype implementation is included
with the proposal.

You can most easily run a prototype by
installing a package, which will likely evolve faster than the
proposal:
[https://github.com/mflatt/shrubbery-rhombus-0](https://github.com/mflatt/shrubbery-rhombus-0)


# Drawbacks
[drawbacks]: #drawbacks

This is a large proposal, even setting aside that it builds on two
other proposals that have been co-designed with this one. The proposal
stakes out a point in the design space that is (hopefully) near some
local maximum, but many details for some dimension of the language are
constrained by choices in other dimensions.


# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

## Overloading `.` for hierarchical naming and field access

While the use of `.` for hierarchical naming in something like
`f2c.fahrenheit_to_celsius` may seem related to the use of `.` for a
field access like `p.x`. They're fundamentally different ideas,
however, and they're supported by different mechanisms in the Rhombus
expander. The overloading of `.` here is Java-like; Rust, in contrast,
uses `::` for import paths and `.` for field access.

The two uses of `.` cannot be unified, because they work in different
ways. In examples like `Posn(1, 2).x` or `p.x`, the left-hand side of
`.` is an expression that is already parsed. In `expr.macro` or `1
posn.(<>) 2`, the left-hand side of `.` is not a definition or
expression, and `.` as a binary operator would not be able to produce
a binary operator (at least not while implementing the target
operator's intended precdence).

## Define

The `def` form may or may not be a good idea. Some programmers may
prefer the generality of `def`, while others may prefer the
specificity of `val`, `fun`, and `expression_macro`. Some
programmers may dislike that there's more than one way. But having
`def` also makes it easier to have `let`. A `let` modifier
that could be applied to any definition form creates a lot of extra
complexity, because definitions can expand to multiple bindings, and
some of them need to refer to each other.

## The dot operator and dot providers

The rules for propagating dot-provider information are reminiscent of
type rules in Turnstile (Chang et al.), but must more limited. The
goal here is to make `.` notation reasonably consistent while keeping
`.` uses as efficient as Racket-style accessors.

Most rules can be viewed as macro bindings that expand to provide
dot-provider information in the expansion for consumption by the
immediate enclosing expression. Indeed, that is mostly the
implementation. The rule that's ad hoc and does not fit that pattern
is the one for for `val` or `def` with an right-hand side that is an
immediate call; the rule is in terms of an unparsed expression on the
right-hand side. The rule is nevertheless included, otherwise

```
def origin: Point(0, 0)
```

would need an extra `:: Point` in the middle to make `origin.x` and
`origin.y` work, and that would look silly. The rule is generalized to
functions with return contracts so that other constructor functions
can have the same status as the structure type name as a constructor.

## Multiple values

Multiple return values are a double-edged sword in Racket: often
useful, but also often a pain to abstract over. It's tempting to
simplify by getting rid of multiple values, but then we'd lose the
benefits that the compiler provides for multiple-value results, and
interoperation with Racket libraries would be slightly more difficult.

## Operator names

Identifier-named infix operators are supported because they cause no
particular prolem for parsing and may be useful, such as for the `mod`
operator. It may make sense to require infix forms to use shrubbery
operators, though, to reduce the space of possible parses of a
shrubbery independent of binding.

## Exporting operators

It might make sense to export operators just the same as identifiers,
which don't require an `identifier:` block:

```
export:
  fahrenheit_freezing
  fahrenheit_to_celsius
```

The difference right now is how the enforestation process works.
Enforestation is relevant to `export` because export operators are
supported. The Rhombus expander's enforestation process allows
identifiers that are not bound as export operators to expand as
themselves. It does not treat operators that way. Maybe that should be
an option to `define-enforest`, or maybe `operator:` is a good idea
for clarity

## Syntax patterns

Syntax patterns with `?` continue in the `match` tradition of
requiring an escape to create a pattern variable, instead of using the
macro-by-example and `syntax-case` default of treating an identifier
as a pattern variable. Although the macro-by-example convention
creates macros that look prettier, the implicit unquoting of variables
in patterns and templates confuses many newcomers. Unification with
`match` is a big advantage of escaped pattern variables.


# Prior art
[prior-art]: #prior-art

Yes. Lots.

# Unresolved questions
[unresolved-questions]: #unresolved-questions

Various questions are inlined in italics within the document, most
with a tentative resolution.


# Future possibilities
[future-possibilities]: #future-possibilities

See [more](#more).
