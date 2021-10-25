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

Main content:

* [Motivation](#motivation)
* [Guide-level explanation](#guide-level-explanation)
* [Reference-level explanation](#reference-level-explanation)
* [Drawbacks](#drawbacks)
* [Rationale and alternatives](#rationale-and-alternatives)

# Motivation
[motivation]: #motivation

Let’s try putting things together. There’s much more to do for a full
language, but this proposal probably has enough to gauge its
particular theme and direction.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Explanation content:

* [Shrubbery notation](#shrubbery-notation)
* [Modules](#modules)
* [Definitions](#definitions)
* [Annotations and the dot operator](#annotations-and-the-dot-operator)
* [Function expressions](#function-expressions)
* [Keyword and optional arguments](#keyword-and-optional-arguments)
* [Conditionals and pattern-matching dispatch](#conditionals-and-pattern-matching-dispatch)
* [Multiple values](#multiple-values)
* [Mutable variables](#mutable-variables)
* [Operators](#operators)
* [Lists](#lists)
* [Arrays and maps](#arrays-and-maps)
* [Sets](#sets)
* [Syntax objects](#syntax-objects)
* [Expression macros](#expression-macros)
* [Definition and declaration macros](#definition-and-declaration-macros)
* [Binding and annotation macros](#binding-and-annotation-macros)
* [Annotations versus binding patterns](#annotations-versus-binding-patterns)
* [More](#more)

## Shrubbery notation
[shrubbery-notation]: #shrubbery-notation

Before we start, here’s a recap on shrubbery notation.

_If you install
[https://github.com/mflatt/shrubbery-rhombus-0.git](https://github.com/mflatt/shrubbery-rhombus-0),
then you can run `#lang shrubbery` (not `#lang rhombus` for this part)
to see how example shrubberies parse into an S-expression
representation. Unfortunately, it needs a development version of
Racket right now, so you may need to install a
[snapshot build](https://snapshot.racket-lang.org/)._

Numbers are decimal, either integer or floating-point, or they’re
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

These characters are used for shrubbery structure and are mostly not
available for use in operators:

```
( ) [ ] { }   ; ,   : |   « »  \   " ~  # @
```

The `:` and `|` characters can be used as part of an operator, and any
other Unicode punctuation or symbol character is fair game for an
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

Keywords are like identifiers, but prefixed with `~` and no space:

```
~base
~stronger_than
```

Booleans:

```
#true
#false
```

Strings and byte strings:

```
"This is a string, just like you’d expect"
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
_block_, and it’s always after a line that ends `:`, that ends `|`
(much less commonly), or that starts `|`. You can think of a
more-indented `|` as being preceded implicitly by a `:` at the end of
the previous line. A `|` counts as being indented by half a column, so
the `|`s below are indented even when they are written right under
`if`, `match`, or `cond`:

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
   def zero: x
   x + zero
| n:
   n + 1

cond
| // check the weather
  is_raining():
    take_umbrella()
| // check the destination
  going_to_beach():
    wear_sunscreen()
    take_umbrella()
| // assume a hat is enough
  _:
    wear_hat() 
```

_Even if you don’t normally use DrRacket, you should proabably try it
while reading this proposal, since that’s where the syntax coloring
and indentation support for `#lang shrubbery` and `#lang rhombus` are
currently implemented. Syntax coloring uses the same lexer as the
language’s implementation, so it can’t get out of sync. For
indentation, hit Tab to cycle through the possible indentations for a
line (based on preceding lines). If multiple lines are selected, then
Tab cycles through possibilities for the first selected line and
shifts remaining lines by same amount. There should be some way to
normalize indentation while preserving the current parse, but that’s
not yet implemented._

A `|` is allowed only within a block, and it also starts a subblock on
the right-hand side of the `|`. Each subsequent `|` at the same
indentation creates a new subblock. Shrubbery notation allows `|` only
within a block whose groups all contain an immediate `|` subblock. A
block of `|` subblocks is an _alts-block_. A `:` isn’t needed or
allowed before the first `|` in an _alts-block_, because the `|`
itself is enough of an indication that a _alts-block_ is starting.

Each line within a block forms a _group_. Groups are important,
because post-shrubbery parsing and macro expansion are constrained to
operate on groups (although a group can contain nested blocks, etc.)
Groups at the same level of indentation as a previous line continue
that group’s block. A `|` can have multiple groups in the subblock to
its right, but the block started by `|` turns out to be the only thing
in its own group.

A `:` doesn’t have to be followed by a new line, but it starts a new
block, anyway. Similarly, a `|` that starts a block doesn’t have to be
on a new line. These examples parse the same as the previous
examples:

```
begin: group within block
       another group within block

if is_rotten(apple) | get_another() | take_bite()
                                      be_happy()

match x | 0: def zero: x
             x + zero
        | n: n + 1

cond | is_raining(): take_umbrella()
     | going_to_beach(): wear_sunscreen()
                         take_umbrella()
     | _: wear_hat() 
```

Within a block, a `;` can be used instead of a new line to start a new
group, so these examples also parse the same:

```
begin: group within block; another group within block

if is_rotten(apple) | get_another() | take_bite(); be_happy()

match x | 0: def zero: x
             x + zero
        | n: n + 1

cond | is_raining(): take_umbrella()
     | going_to_beach(): wear_sunscreen(); take_umbrella()
     | _: wear_hat() 
```

You can add extra `;`s, such as at the end of lines, since `;` will
never create an empty group.

Finally, anything that can be written with newlines and indentation
can be written on a single line, but `«` and `»` may be required to
delimit a block using `«` just after `:` or `|` and `»` at the end of
the block. Normally, parentheses work just as well, but the `match`
example above illustrates a rare case where `«` and `»` would be
needed to fit on a single line. Without `«` and `»`, the following
form would put `x + zero` insinde the definition of `zero`:

```
match x | 0: def zero:« x »; x + zero | n: n + 1
```

Parentheses `(` ... `)`, square brackets `[` ... `]`, and curly braces
`{` ... `}` combine a sequence of groups. A comma `,` can be used to
separate groups on one line between the opener and closer.
Furthermore, a `,` is _required_ to separate groups, even if they’re
not on the same line. You can’t have extra `,`s, except after the last
group.

```
f(1, 2,
  3, 4)

["apples",
 "bananas",
 "cookies",
 "milk"]

map(add_five, [1, 2, 3, 4,])
```

Indentation still works for creating blocks within `(` ... `)`, `[`
... `]`, or `{` ... `}`:

```
map(fun (x):
      x + 5,
    [1, 2, 3, 4])
```

There are some subtleties related to the “precedence” of `:`, `|`,
`;`, and `,`, but they’re likely to work as you expect in a given
example.

## Modules
[modules]: #modules

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
`fahrenheit_to_celsius`. In interactions, a single input line is
accepted as complete as long as it's openers and closers are balanced,
and as long as it doesn't contain `:` outside of an opener--closer
pair. A blank line terminates multi-line input. For multi-line input
where the first line would otherwise parse as complete, add `:` at
the front, either on the same line or its own line._

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
that’s why the import of `"f2c.rhm"` leads to the `f2c` prefix. To
supply an explicit prefix, use the `prefix` modifier:

```
import:
  "f2c.rhm": prefix convert

convert.fahrenheit_to_celsius(convert.fahrenheit_freezing)
```

Use the `no_prefix` modifier to import without a prefix, but this kind
of “namespace dumping” is considered bad style in most cases:

```
import:
  "f2c.rhm": no_prefix

fahrenheit_to_celsius(fahrenheit_freezing)
```

Module paths are written with a `/` separator as in Racket, and the
last path element is the one that determines the default import
prefix:

```
import:
  racket/math

math.pi  // prints 3.141592653589793
```

_The use of `.` with an import name as a hierarchical reference is not
the same as the `.` described in the next section. We stick with `/`
for module paths to avoid overloading `.` further. See also the
current [rationale](#rationale-and-alternatives)._

There’s a lot more to the syntax or `import` and `export` for
renaming, re-exporting, and so on. See [a separate
document](import-export.md) for more information.

## Definitions
[definitions]: #definitions

Besides `val` and `fun`, `struct` is a definition form that
defines a new structure type. By convention, structure names start
with a capital letter.

```
struct Posn(x, y)
```

A structure-type name can be used like a function to construct an
instance of the structure type. A structure-instance expression
followed by `.` and a field name extracts the field value from the
structure instance.

```
val origin: Posn(0, 0)

origin    // prints Posn(0, 0)

origin.x  // prints 0
```

A structure-type name followed by `.` and a field name gets an
accessor function to extract the field value from an instance of the
structure:

```
Posn.x(origin)  // prints 0
```

Comparing `Posn.x` to a function that uses `.x` on its argument, the
difference is that `Posn.x` works only on `Posn` instances. That
constraint makes field access via `Posn.x` more efficient than a
generic lookup of a field with `.x`.

An _annotation_ associated with a binding or expression can make field
access with `.x` the same as using a structure-specific accessor.
Annotations are particularly encouraged for a function argument that
is a structure instances, and the annotation is written after the
argument name with `-:` and the structure type name:

```
fun flip(p -: Posn):
  Posn(p.y, p.x)

flip(Posn(1, 2))  // prints Posn(2, 1)
```

Using `-:` makes an assertion about values that are provided as
arguments, but that assertion is not checked when the argument is
provided. In effect, the annotation simply selects a structure-specific field
accessor for `.x`. If `flip` is called with `0`, then a run-time error
will occur at the point that `p.y` attempts to access the `y` field of
a `Posn` instance:

```
// flip(0)  // would be a run-time error from `.y`
```

The `::` binding operator is another way to annotate a variable.
Unlike `-:`, `::` installs a run-time check that a value supplied for
the variable satisfies its annotation. The following variant of the
`flip` function will report an error if its argument is not a `Posn`
instance, and the error is from `flip` instead of delayed to the
access of `y`:

```
fun flip(p :: Posn):
  Posn(p.y, p.x)

// flip(0)  // would be a run-time error from `flip`
```

A run-time check implied by `::` can be expensive, depending on the
annotation and context. In the case of `flip`, this check is unlikely
to matter, but if a programmer uses `::` everywhere to try to get
maximum checking and maximum guarantees, it’s easy to create expensive
function boundaries. Rhombus programmers are encouraged to use `-:`
when the goal is to hint for better performance, and use `::` only
where a defensive check is needed, such as for the arguments of an
exported function.

_The [rationale](#static-dynamic) says more about `-:` and `::`._

The use of `-:` or `::` as above is not specific to `fun`. The `-:`
and `::` binding operators work in any binding position, including the
one for `val`:

```
val (flipped -: Posn):  flip(Posn(1, 2))

flipped.x  // prints 2
```

The `struct Posn(x, y)` definition does not place any constraints on
its `x` and `y` fields, so using `Posn` as a annotation similarly does
not imply any annotations on the field results. Instead of using just
`Posn` as a annotation, however, you can use `Posn.of` followed by
parentheses containing annotations for the `x` and `y` fields. More
generally, a `struct` definition binds the name so that `.of` accesses
an annotation constructor.

```
fun flip_ints(p :: Posn.of(Integer, Integer)):
  Posn(p.y, p.x)

flip_ints(Posn(1, 2))       // prints Posn(2, 1)
// flip_ints(Posn("a", 2))  // would be a run-time error
```

Finally, a structure-type name like `Posn` can also work in binding
positions as a pattern-matching form. Here’s a implementation of
`flip` that uses pattern matching for its argument:

```
fun flip(Posn(x, y)):
  Posn(y, x)

// flip(0)  // would be a run-time error
flip(Posn(1, 2))  // prints Posn(2, 1)
```

As a function-argument pattern, `Posn(x, y)` both requires the
argument to be a `Posn` instance and binds the identifiers `x` and `y`
to the values of the instance’s fields. There’s no need to skip the
check that the argument is a `Posn`, because the check is anyway part
of extracting `x` and `y` fields.

As you would expect, the fields in a `Posn` binding pattern are
themselves patterns. Here’s a function that works only on the origin:

```
fun flip_origin(Posn(0, 0)):
  origin

// flip_origin(Posn(1, 2))  // would be a run-time error
flip_origin(origin)  // prints Posn(0, 0)
```

Finally, a function can have a result annotation, which is written
with `-:` or `::` after the parentheses for the function’s argument.
With a `::` result annotation, every return value from the function is
checked against the annotation. Beware that a function’s body does not
count as being tail position when the function is declared with a `::`
result annotation.

```
fun same_posn(p) -: Posn:
  p

same_posn(origin)    // prints Posn(0, 0)
same_posn(5)         // prints 5, since `-:` does not check
same_posn(origin).x  // prints 0 through efficient field access

fun checked_same_posn(p) :: Posn:
  p

checked_same_posn(origin)  // prints Posn(0, 0)
// checked_same_posn(5)    // woudl be a run-time error
```

The `def` form is a kind of do-what-I-mean form that acts like
`val`, `fun`, or certain other definition forms depending on
the shape of the terms after `def`. It’s sensitive to binding
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

_Is `def` a good idea' See the current [rationale](#rationale-and-alternatives)._

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

The identifier `_` is similar to `Posn` and `-:` in the sense that
it’s a binding operator. As a binding, `_` matches any value and binds
no variables. Use it as an argument name or subpattern form when you
don’t need the corresponding argument or value, but `_` nested in a
binding pattern like `::` can still constrain allowed values.

```
fun omnivore(_): "yum"
fun omnivore2(_, _): "yum"
fun nomivore(_ :: Number): "yum"

omnivore(1)        // prints "yum"
omnivore("apple")  // prints "yum"
omnivore2("a", 1)  // prints "yum"
nomivore(1)        // prints "yum"
// nomivore("a")   // would be a run-time error
```


## Annotations and the dot operator
[annotations-and-the-dot-operator]: annotations-and-the-dot-operator


Besides structure types defined with `struct`, a few predefined
annotations work with the `-:` and `::` annotation operators, including
`Integer` (meaning exact integer), `Number`, `String`, `Keyword`, and
`Any` (meaning any value).

The `-:` and `::` operators also work in expression positions. In that
case, the assertion or check is about the expression on the left-hand
side of `-:` or `::`. For `::`, the left-hand expression must produce
a value that satisfies the right-hand annotation, otherwise a run-time
exception is raised. The `is_a` operator takes an annotation like
`::`, but it produces a boolean result indicating whether the result
of the left-hand expression matches the annotation.

```
(flip(origin) -: Posn).x  // prints 0
// (1 :: Posn)            // would be a run-time error

origin is_a Posn  // prints #true
1 is_a Posn       // prints #false
```

_Since a binding operator or `::` as an expression gets an opportunity
to adjust the value delivered to a variable, non-predicate contracts
fit naturally into the annotation framework. Probably `is_a` should
fail on non-predicate annotations._

When `struct` defines a new structure type, an annotation can be
associated with each field. When the annotation is written with `::`,
then the annotation is checked when an instance is created.

```
struct Posn(x :: Integer, y :: Integer)

Posn(1, 2)       // prints Posn(1, 2)
// Posn(1, "2")  // would be a run-time error
```

Naturally, structure-type annotations can be used as field
annotations, and then the `.` operator can be chained for efficient
access:

```
struct Line(p1 -: Posn, p2 -: Posn)

def l1 :: Line:
  Line(Posn(1, 2), Posn(3, 4))

l1.p2.x  // prints 3
```

More generally, `.` access is efficient when the left-hand side of `.` is an
expression that can act as a _dot provider_. A structure-type name is a dot
provider, and it provides access to field-accessor functions, as in
`Posn.x` (which doesn’t get a specific `x`, but produces a function
that can be called on a `Posn` instance to extract its `x` field). An
identifier that is bound using `-:` or `::` and a structure-type name is also a
dot provider, and it provides access to fields of a structure instance.
More generally, an annotation that is associated to a binding or
expression with `-:` or `::` might make the binding or expression a dot
provider. See [a separate document](static-info.md) for more
information on dot providers and other static information, but
only after reading the rest of this document.

The `use_static_dot` definition form binds the `.` operator so that it
works only in efficient mode with a dot provider. If the left-hand
side of the `.` is not a dot provider, then the `.` defined by
`use_static_dot` reports a compile-time error. The `use_dynamic_dot`
form binds `.` to the default `.`, which allows dynamic field lookup
if the left-hand side is not a dot provider.

```
use_static_dot

l1.p2.x  // prints 3
// 1.x   // disallowed statically
```

_Using `.` to reach an imported binding, as in
`f2c.fahrenheit_to_celsius`, is a different kind of `.` than the infix
expression operator. See the current
[rationale](#rationale-and-alternatives)._


## Function expressions
[function-expressions]: #function-expressions

The `fun` form works in an expression position as λ. Just like
`function` in JavaScript, the expression variant omits a function
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
[keyword-and-optional-arguments]: #keyword-and-optional-arguments

A function argument can be made optional by using `=` after the
argument’s pattern and providing a default-value expression after `=`:

```
fun scale(Posn(x, y), factor = 1):
  Posn(factor * x, factor * y)

scale(Posn(1, 2))     // prints Posn(1, 2)
scale(Posn(1, 2), 3)  // prints Posn(3, 6)
```

By-keyword arguments are often useful for functions that have multiple
optional arguments. A keyword argument is indicated by prefixing a
formal or actual argument with a shrubbery keyword, which is written
with a leading `~`, and then starting a block with `:`.

```
fun transform(Posn(x, y),
              ~scale: factor = 1,
              ~dx: dx = 0,
              ~dy: dy = 0):
  Posn(factor*x + dx, factor*y + dy)

transform(Posn(1, 2))          // prints Posn(1, 2)
transform(Posn(1, 2), ~dx: 7)  // prints Posn(8, 2)
transform(Posn(1, 2), ~dx: 7, ~scale: 2)  // prints Posn(9, 4)
```

Since a keyword by itself is not allowed as an expression or pattern,
there is no possibility that a keyword will be inadvertently treated
as an actual argument or binding pattern by itself. The `keyword` form
turns a keyword into an expression that produces the keyword, as in
`keyword(~scale)`.

_The keyword prefix and `=` for default values are not binding
operators. They are specific to the syntax of `fun`._

If an argument name is the same as its keyword (just without the
`~`), then the `:` argument name can be omitted. That only works for
an argument that would otherwise be just an identifier and maybe a
default value, because keywords don’t work as variable names
in binding patterns.

```
fun transform(Posn(x, y),
              ~scale: factor = 1,
              ~dx = 0,
              ~dy = 0):
  Posn(factor*x + dx, factor*y + dy)
```

## Conditionals and pattern-matching dispatch
[conditionals-and-pattern-matching-dispatch]: #conditionals-and-pattern-matching-dispatch

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
is numerical comparison like Racket’s `=`, while `===` operator is
Racket’s `equal'`. Comparison operators are non-associative and have
no precedence relationship with each other.

The `if` form expects a test expression followed by an alts-block with
two `|`s. The first `|` holds the “then” branch, and the second `|`
holds the “else” branch:

```
if 1 == 2
| "same"
| "different"
```

Although an `if` could be nested further in the “else” branch to
implement an “if” ... “else if” ... “else if” ... combination, the `cond`
form supports that combination better. It expects an alts-block where each
`|` has a test expression followed by a block. Evaluating the `cond` form dispatches
to the block after first test that produces a non-`#false` value. The
`~else` keyword can be used in place of a last test.

```
fun fib(n):
  cond
  | n == 0: 1
  | n == 1: 1
  | ~else: fib(n-1) + fib(n-2)

fib(5) // prints 8
```

If there’s no `~else` case and no matching case, then `cond` reports
an error at run time (unlike Racket, which returns void in that case).
Note that `~else` is a keyword, and not an identifier. If it were an
identifier, then `else` might get bound in some context to `#false`,
which would be confusing. As another special case, `_` is allowed in
place of `else`; although it is possible to bind `_`, it takes a
specifical effort because `_` is a binding operator.

Although `cond` is better than `if` for `fib`, the `match` form is
even better. The `match` form expects an expression and then an
alts-block where each `|` has a binding pattern followed by a block.
The `match` form evaluates that first expression, and dispatches to
the first block whose pattern accepts the expression’s value. Similar
to `cond`, `match` supports `~else` in place of a final binding
pattern, but using the binding operator `_` is more common.

```
fun fib(n):
  match n
  | 0: 1
  | 1: 1
  | _: fib(n-1) + fib(n-2)
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

There’s no `~else` for this fused form, but `_` can be useful in
catch-call clauses where the argument is not used. Also, the function name and
all relevant argument positions have to be repeated in every case, but
that’s often a readable trade-off. Match-dispatching functions cannot
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
[multiple-values]: #multiple-values

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
`values` in the outermost pattern, and that’s the same as not writing
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

_Get rid of multiple values' See the current
[rationale](#rationale-and-alternatives)._

## Mutable variables
[mutable-variables]: #mutable-variables

Variables are immutable unless they are declared with the `mutable`
binding operator. The `=` infix operator assigns to a mutable variable
while also returning the variable’s new value.

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
field is declared `mutable`, but that’s not yet implemented._

## Operators
[operators]: #operators

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

_It allowing identifiers as “operators” a good idea' See the current
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
blocks at the start of the operator’s body. The possible keyword
options for prefix operators are `~weaker_than`, `~stronger_than`,
`~same_as`, or `~same_as_on_left`. For infix operators, those options
 apply, as well as `~same_as_on_right` and `~associativity`. Operators
listed with keywords like `~weaker_than`
can be grouped on lines however is convenient.

```
operator (x <> y):
  ~weaker_than: * / 
                + -
  ~associativity: ~right
  Posn(x, y)

1 <> 2 * 3  // prints Posn(1, 6)
1 <> 2 <> 3 // prints Posn(1, Posn(2, 3))
```

Use the keyword `~other` in `~weaker_than`, `~stronger_than`, or
`~same_as` to declare a precedence relationship for operators not
otherwise mentioned.

An operator can be exported the same as identifiers:

```
export:
  <>
```

On the import side, to refer to an operator that has a prefix, put the
operator after `.` in parentheses:

```
import:
  "posn.rhm"

1 posn.(<>) 2
```

If the point of an operator is terseness, an import prefix may defeat
the point. Using a library that supplies operators may be one reason
to import with a leading `=` to avoid a prefix on the imports. To
selectively make an operator accessible without it import’s prefix,
use the `expose` import modifier:

```
import:
  "posn.rhm":
    expose: <>

1 <> 2
```

## Lists
[lists]: #lists

A `[` ... `]` form as an expression creates a list:

```
[1, 2, 3]                // prints [1, 2, 3]
[0, "apple", Posn(1, 2]] // prints [0, "apple", Posn(1, 2]]
```

You can also use the `List` constructor, which takes any number of
arguments:

```
List(1, 2, 3)  // prints [1, 2, 3]
```

A list is a “linked list,” in the sense that getting the _n_th element
takes O(_n_) time, and adding to the front takes constant time. A list
is immutable.

`List` works as an annotation with `-:` and `::`:

```
fun
| classify(_ :: List): "list"
| classify(_ :: Number): "number"
| classify(_): "other"

classify([1])  // prints "list"
classify(1)    // prints "number"
classify("1")  // prints "other"
```

As pattern, `[` ... `]` matches a list, and list elements can be
matched with specific subpatterns. The `List` binding operator works
the same in bindings, too.

```
fun three_sorted([a, b, c]):
  a <= b && b <= c

three_sorted([1, 2, 3]) // prints #true
three_sorted([1, 3, 2]) // prints #false
```

The last element in a `[` ... `]` binding pattern can be `...`, which
means zero or more repetitions of the preceding pattern, and each
variable bound by the preceding pattern is instead bound to a list of
matches.

```
fun
| got_milk([]): #false
| got_milk([head, tail, ...]):
   head === "milk" || got_milk(tail)

got_milk([])                             // prints #false
got_milk(["apple", "milk", "banana"])    // prints #true
got_milk(["apple", "coffee", "banana"])  // prints #false
```

A use of `[` ... `]` or `List` for an expression also supports `...`
in place of a last argument, in which case the preceding argument is
treated as a list that is the tail of the new list.

```
[1, 2, [3, 4], ...]  // prints [1, 2, 3, 4]

fun
| is_sorted([]): #true
| is_sorted([head]): #true
| is_sorted([head, next, tail, ...]):
   head <= next && is_sorted([next, tail, ...])

is_sorted([1, 2, 3, 3, 5]) // prints #true
is_sorted([1, 2, 9, 3, 5]) // prints #false
```

When `[` ... `]`appears after an expression, then instead of forming a
list, it accesses an element of an _map_ value. Lists are
maps that are indexed by natural numbers starting with `0`:

```
val groceries: ["apple", "banana", "milk"]

groceries[0] // prints "apple"
groceries[2] // prints "milk"
```

Indexing with `[` ... `]` is sensitive to binding-based static
information in the same way as `.` For example, a function’s argument
can use a binding pattern that indicates a list of `Posn`s, and then
`.` can be used after `[` ... `]` to efficiently access a field of a `Posn`
instance:

```
fun nth_x([ps -: Posn, ...], n):
  ps[n].x

nth_x([Posn(1, 2), Posn(3, 4), Posn(5, 6)], 1) // prints 3
```

An equivalent way to write `nth_x` is with the `List.of` annotation
constructor. It expects an annotation that every element of the list
must satisfy:

```
fun nth_x(ps -: List.of(Posn), n):
  ps[n].x
```

The `nth_x` function could have been written as follows, but unlike
the previous versions, this one creates an intermediate list `xs`:

```
fun nth_x([Posn(xs, _), ...], n):
  xs[n]

nth_x([Posn(1, 2), Posn(3, 4), Posn(5, 6)], 1) // prints 3
```


## Arrays and maps
[arrays-and-maps]: #arrays-and-maps

The `Array` constructor is similar to `List`, but it creates an array,
which has a fixed length at the time that it’s created and offers
constant-time acc<ess to any element of the array. Like a list, and
array is a map. Unlike a list, an array is mutable, so `[` ...
`]` for indexing can be combined with `=` for assignment.

```
val buckets: Array(1, 2, 3, 4)

buckets[0]      // prints 1
buckets[1] = 5  // prints 5
buckets         // prints Array1, 5, 3, 4)
```

`Array` is also an annotation and a binding contructor, analogous to
`List`, and `Array.of` is an annotation constructor. The `Array` binding
and expression constructors do not support `...`.

The `Map` constructor creates an immutable mapping of arbitrary keys
to values. The term “map” is meant to be generic, and `Map` as a
constructor just uses a default implementation of maps. The `Map`
constructor can be used like a function, in which case it accepts keys
alternating with values:

```
val neighborhood: Map("alice", Posn(4, 5),
                      "bob", Posn(7, 9))

neighborhood["alice"]     // prints Posn(4, 5)
// neighborhood["clara"]  // would be a run-time error
```

Curly braces `{` ... `}` can be used as a shorthand for writing `Map(`
... `)`. Within curcly braces, the key and value are joined by `:`.
(If a key expression needs to use `:` itself, the expression will have
to be in parentheses.)

```
val neighborhood: {"alice": Posn(4, 5),
                   "bob": Posn(7, 9)}
neighborhood["alice"]       // prints Posn(4, 5)
```

To functionally extend a map, use the `++` append operator:

```
val new_neighborhood: neighborhood ++ {"alice": Posn(40, 50)}
new_neighborhood["alice"] // prints Posn(40, 50)
neighborhood["alice"]     // prints Posn(4, 5)
```

When `++` is used with a left-hand side that is statically known to be
the default implementation of maps, and when the right-hand argument
is an immediate map construction with a single element, then the use
of `++` is compiled as an efficient single-key update of the map.
Whether optimized or general, the `++` operator will only combine
certain compatible kinds of maps. For example, `++` will append lists
and combine default-implementation maps, but it will not combine two
vectors or combine a list and a default-implementation map with keys
and values.

`Map` or its curly-braces shorthand is also an annotation and a
binding constructor. As an annotation or binding constructor, `Map`
refers to map values genercially, and not to a specific
implementation. For example, a list can be passed to a function that
expects a `Map` argument.

In a binding use of `Map`, the key positions are *expressions*, not
*bindings*. The binding matches an input that includes the keys, and
each corresponding value is matched to the value binding pattern.

```
fun alice_home({"alice": p}): p

alice_home(neighborhood)  // prints Posn(4, 5)
```

The `Map.of` annotation constructor takes two annotations, one for keys
and one for values:

```
fun locale(who, neighborhood -: Map.of(String, Posn)):
  val p: neighborhood[who]
  p.x +$ ", " +$ p.y

locale("alice", neighborhood)  // prints "4, 5"
```

Unlike `.`, indexed access via `[` ... `]` works even without static
information to say that the access will succeed. Still, static
information can select a more specific and potentially fast indexing
operator. For example, `buckets[0]` above statically resolves to the
use of array lookup, instead of going through a generic function for
maps at run time.

The `make_map` function works similarly to the `Map` constructor, but
it creates a mutable map. A mutable map can be updated using `[` ...
`]` with `=` just like an array.

```
val locations: make_map("alice", Posn(4, 5),
                        "bob", Posn(7, 9))
locations["alice"] = Posn(40, 50)
locations["alice"]  // prints Posn(40, 50)
```

## Sets
[sets]: #sets

When `{` ... `}` is used with elements that do not have `:`, then `{`
... `}` creates a set. (If a set-element expression uses `:`, then it
will need to be in parentheses to avoid being parsed as a key–value
pair.) A set can serve as a map, where the set's elements act as keys
and each key's value is `#true`. There's a `Set` constructor that's
analogous to `Map`, but `Set` accepts just values to include in the
set. The `++` operator effectively unions sets.

```
val friends: {"alice", "bob", "carol"}

if friends["alice"] && friends["carol"]
| "I know both"
| "Who are they?"
// prints "I know both"

val new_friends: friends ++ {"david"}
new_friends["david"]  // prints #true
friends["david"]      // prints #false
```

`Set.of` and `make_set` work as you'd expect. When `[` ... `]` with
`=` is used to modify a mutable set, the “key” is removed from the set
if the assigned value is `#false`, otherwise the “key” is added to the
set.

## Syntax objects
[syntax-objects]: #syntax-objects

The `'` operator quotes an immediately following shrubbery term to
produce a syntax object. The syntax object holds an unparsed
shrubbery, not a parsed Rhombus expression.

```
'1         // prints a shrubbery: 1
'hello     // prints a shrubbery: hello
'(1 + 2)   // prints a shrubbery: (1 + 2)
'(x:
    y)     // prints a shrubbery: x:« y »
// '1 + 2  // would be a run-time error, since '1 is not a number
```

The `'` operator is more precisely a quasiquoting operator. The `$`
unquotes the immediate following terms. That is, the term after `$` is
a Rhombus expression whose value replaces the `$` and its argument
within the quoted form.

```
'(1 + $(2 + 3))  // prints a shrubbery: (1 + 5)
```

A `$` only unquotes when it is followed by a term, otherwise the `$`
itself remains quoted.

```
'(1 + $(' $) 2)  // prints a shrubbery: '(1 + $ 2)
```

Like `$`, `...` is treated specially within a `'`-quoted term (except,
like `$` when it’s the only thing in the term). When `...` immediately
follows a term that includes at least one `$`, the value of the
expression after that `$` must produce a list of syntax objects. Then,
instead of the parenthesized group in place of `$`, the term before
`...` is replicated as many times as the list has items, and each of
those items is used in one replication.

```
def seq: ['1, '2, '3]

'((hi $seq) ...) // prints a shrubbery: ((hi 1) (hi 2) (hi 3))
```

There’s a subtlety here: could `seq` have zero elements' If so,
replicating the `hi` form zero times within a group would leave an
empty group, but a shrubbery never has an empty group. To manage this
gap, a parenthesized shrubbery form with zero groups is treated as
equivalent for `$` replication to a parenthesized form with an empty
group. Similarly, when a `'` form describes a single parenthesized
group that turns out to be empty, it instead produces a parenthesized
form with zero groups. Attempting to generate a group with no terms
within a parenthesized form with multiple groups is an error.

```
def seq: []

'((hi $seq) ...)          // prints a shrubbery: ()
// '(x, (hi $seq) ..., y) // would be a run-time error
```

Square-bracket forms and block forms have the same special cases to
deal with an empty group as parenthesis forms.

When `...` is the only term in a group, and when that group follows
another, then `...` replicates the preceding group. For example,
putting `...` after a `,` in parentheses means that it follows a the
group before the `,`, which effectively replicates that group with its
separating comma:

```
def seq: ['1, '2, '3]

'(hi $seq, ...) // prints a shrubbery: (hi 1, hi 2, hi 3)
```

Along the same lines, `...` just after a `|` can replicate a preceding
`|` block:

```
def seq: ['1, '2, '3]

'(cond | $seq | ...) // prints a shrubbery: cond |« 1  »|« 2  »|« 3 »}
```

In other words, `...` in various places within a quoted shrubbery
works the way you’d expect it to work.

When `'` is used in a binding position, it constructs a pattern that
matches syntax objects, and it binds variables that are escaped in the
pattern with `$`.

```
val ~($x + $y): '(1 + (2 + 3))

x  // prints a shrubbery: 1
y  // prints a shrubbery: (2 + 3)
```

_This is like `match`, and not like `syntax-case`. See the current
[rationale](#rationale-and-alternatives)._

A `$`-escaped variable in a `'` pattern matches one term. Keep in mind
that `'` creates syntax objects containing shrubberies that are not
yet parsed, so a variable will not be matched to a multi-term sequence
that would be parsed as an expression. For example, a pattern variable
`y` by itself cannot be matched to a sequence `2 + 3`:

```
// val '($x + $y): '(1 + 2 + 3)  // would be a run-time error
```

Meanwhile, `...` works the way you would expect in a pattern, matching
any `...`-replicated pattern variables to form a list of matches:

```
val '($x + $y ...): '(1 + 2 + 3)

x  // prints a shrubbery: 1
y  // prints a list: ['2, ' +, '3]
```

_In the current implementation of `'` patterns, a `$` escape must be
followed by an identifier or `_`. Generalizing the position after `$` can
open the door to more `syntax-parse` goodness._

A `......` behaves similarly to `...`, but for a _tail replication_
that can only appear at the end of a group. In a patttern, an escaped
variable must appear before `......`, and instead of binding the
variable to a list, the variable is bound to a syntax object for a
parenthesized term that contains the matched tail. In a template, an
escaped expression must appear before `......`, and it must produce a
syntax object for a parenthesized term.

Use `......` for tail sequences that you don’t need to inspect,
because the syntax-object representation can avoid work proportional
to the length of the matched tail. Avoiding that work can be important
for macros.

```
val '($head $tail ......): '(1 2 3 4 5)

head  // prints a shrubbery: 1
tail  // prints a shrubbery: (2 3 4 5)
'(0 $tail ......) // prints a shrubbery: (0 2 3 4 5)
```

_Using `......` is similar to using `.` in S-expression patterns and
templates. The difference can avoid quadratic expansion costs, which
is all the more important in the Rhombus expansion protocol, which
must thread potentially long sequences into and out of macro
transformers._


## Expression macros
[expression-macros]: #expression-macros

Macros extend the syntax available for expressions, bindings,
definitions, and more. Each kind of macro extension has a different
protocol and a different form for defining a macro. In the case of
expressions, a macro receives the sequence of terms starting with the
macro operator/identifier within a group. A _rule_ macro consumes only
the tokens that its pattern matches, and it’s right-hand side is an
immediate template expression that produces the macro expansion. A
general form of macro is implemented with arbitrary compile-time
computation, and it can return terms that the macro does not consume.

For example, here’s a `thunk` macro that expects a block and wraps as
a zero-argument function

```
import:
  rhombus/macro: no_prefix

expr.rule '(thunk: $body ...):
  '(fun (): $body ...)

thunk: 1 + "oops"  // prints a function
(thunk: 1 + 3)()   // prints 4
```

The `expr.rule` form expects a `'` and then either parentheses or an 
identifier or operator to create a pattern that matches a sequence of
terms. With parentheses after `'`, either the first or
second term within the pattern is an _unescaped_ identifier or
operator to be defined; conceptually, it’s unescaped because the macro
matches a sequence of terms that use that identifier or operator
literally. If the first term in the pattern is an unescaped identifier
or operator, a prefix macro is defined; otherwise, the second term
must be unescaped, and an infix macro is defined. If the part after `'`
is just an identifier or parentheses, then it’s shorthand for a prefix
macro that consumes none of the tail.

The `expr.rule` form must be imported from `rhombus/macro`, but `def`
behaves like `expr.rule` when the part before `:` is valid for
`expr.rule`:

```
// no import needed

def '(thunk: $body ...):
  '(fun (): $body ...)

(thunk: 1 + 3)()   // prints 4
```

A postfix macro can be implemented as an infix operator that
consumes no additional terms after the operator. For example, a
postfix `!` might be defined (shadowing the normal `!` for “not”) like
this:

```
def '($a !):
  '(factorial($a))

fun
| factorial(0): 1
| factorial(n): n*factorial(n-1)

10! + 1 // = 3628801
```

The `expr.rule` or `def` form is a shorthand for a more general
`expr.macro` macro form. With `expr.macro`, the macro implementation
after `:` is compile-time code. Importing `rhombus/macro` imports all
of the same bindings as `rhombus` into the compile-time phase, in
addition to making forms like `expr.macro` available. Normally,
`rhombus/macro` should be imported without a prefix, otherwise a
prefix would have to be used for all Rhombus forms in compile-time
code—even for things like `values` and `'`. In addition, a
macro defined with `expr.macro` receives all remaining terms in the
enclosing group as input, and it must return two values: the expanded
expression and the remaining terms that have not been consumed.

For example, the `!` macro can be equivalently written like this:

```
expr.macro '($a ! $tail ......):
  values('(factorial($a)), '(tail ......))
```

Since an `expr.macro` implementation can use arbitrary compile-time
code, it can inspect the input syntax objects in more way than just
pattern matching. However, already-parsed terms will be opaque. When
the macro transformer for `!` is called, `a` will be bound to a syntax
object representing a parsed Rhombus expression, as opposed to an
unparsed shrubbery. Currently, there’s no way for a transformer to
inspect a parsed Rhombus expression (except by escaping to Racket).
When the parsed expression is injected back into an unparsed
shrubbery, as happens in `'(factorial($a))`, it will later simply
parse as itself.

_The current implementation does not track the category of a parsed
term, which would enable reporting a specific error if a parsed
expression is put into a non-expression context. Probably it should do
that._

The `!` macro matches an unconsumed tail with `......` instead of
`...`. Using `...` would implement the same transformation, but
less efficiently:

```
expr.macro '($a ! $tail ...):
  values('(factorial($a)), '(tail ...))

// changing to 2000 `!`s or so makes parsing take noticeably long:
0 ! ! ! ! ! ! ! ! ! ! ! !
```

Since `......` after `tail` in a pattern binds `tail` to a
parenthesized sequence, the macro’s second result can be written as
just `tail`. That’s convenenient, although not inherently more
efficient:

```
expr.macro '($a ! $tail ......):
  values('(factorial($a)), tail)
```

The `......` operator cannot be used at the end of the input group for
a rule macro, because there’s implicitly a `$tail ......` at the end
of every rule-macro pattern. If you try replacing `...` in the `thunk`
implementation with `......`, it will work, but that’s because the
`......` in that case is under a block created by `:`, and not in the
enclosing input group.

Whether pattern-based or not, an infix-operator macro’s left-hand
input is parsed. A prefix or infix macro’s right-hand input is not
parsed by default. To trigger parsing for a right-hand argument,
include `~parsed_right` as a declaration at the start of the macro
body. Often, in that case, you might as well use `operator`, but
macros provide control over evaluator order. For example, this `+<=`
operator is like `+`, but evaluates its right-hand side before it’s
left-hand side:

```
def '($a +<= $b): 
  ~parsed_right
  '($b + $a)

1 +<= 2                       // prints 3
// (1+"oops") +<= (2+"ouch")  // would complain about "ouch", not "oops"
```

Declaring `~parsed_right` affects a `expr.macro` macro in a second
way: the macro will receive only the left (if any) and right
arguments, and will not receieve or return the tail of the ennclosing
group. In other words, declaring `~parse_right` uses the same
argument and return protocol as a rule-based macro, but the template
part can be implemented by arbitrary compile-time expressions.

In the same way that `operator` supports operators that are both
prefix and infix, you can use an alt-block with `expr.rule` or
`expr.macro` to create a prefix-and-infix macro. Furthermore, an
`expr.rule` or `expr.macro` form can have multiple prefix blocks or
multiple infix blocks, where the each block’s pattern is tried in
order; in that case, only the first prefix block (if any) and first
infix block (if any) can have precedence and associativity
declarations that apply to all cases, and none of the cases can use
`~parsed_right`.

## Definition and declaration macros
[definition-and-declaration-macros]: #definition-and-declaration-macros

The `defn.macro` form defines a definition macro. It is similar
to `expr.macro` in prefix form, except that the name must be an
identifier (never an operator), and the result syntax object should
represent a block, which is spliced into the definition context where
the macro is used.

Here’s the classic `def_five` macro:


```
import:
  rhombus/macro: no_prefix

defn.macro '(def_five $id):
  '(:
      def $id: 5
  )

def_five v
v  // prints 5
```

Declarations macros are written with `decl.macro`, and the
block produced by expansion can use forms like `import` and
`export`.

By distinguishing between expression macros, definition macros, and
declaration macros, Rhombus can report errors for out-of-place uses
earlier and more clearly than Racket.

_In the Racket-level interface, it’s possible to bind an identifier to
both expression and definition transformers, etc. Adding a Rhombus
syntax for that combination should be straightforward._


## Binding and annotation macros
[binding-and-annotation-macros]: #binding-and-annotation-macros

Macros can extend binding-position syntax, too, via `bind.rule` and
`bind.macro`. In the simplest case, a binding operator is implemented
by expanding to other binding operators, like this definition of `$`
as a prefix operator to constrain a pattern to number inputs:

```
import:
  rhombus/macro: no_prefix

bind.rule '($ $n):
  ~parsed_right
  '($n :: Number)

val $salary: 100.0

salary  // prints 100.0
```

More expressive binding operators can use a lower-level protocol where
a binding is represented by transformers that generate checking and
binding code. It gets complicated, and it’s tied up with the
propagation of static information, so the details are pushed out to [a
separate document](binding-macros.md). After an expressive set of
binding forms are implemented with the low-level interface, however,
many others can be implemented though simple expansion.

The `annotation.macro` form is similar to `bind.macro`, but for
annotations. 

```
use_static_dot

annotation.macro 'PosnList: 'List.of(Posn)

fun nth_x(ps -: PosnList, n):
  ps[n].x
```

Details on the low-level annotation protocol are also in [a separate
document](annotation-macros.md).

## Annotations versus binding patterns
[annotations-versus-binding-patterns]: #annotations-versus-binding-patterns

Annotations and binding patterns serve similar and interacting purposes.
The `-:` and `::` binding operators put annotations to work in a binding. For
the other direction, the `matching` annotation operator puts a binding form
to work in a annotation.

For example, suppose you want a annotation `PersonList`, which is a list
of maps, and each map must at least relate `"name"` to a `String` and
`"location"` to a `Posn`. The `Map.of` annotation combination cannot
express a per-key specialization, but the `Map` binding pattern can.

```
annotation.macro 'PersonList: 
  '(List.of(matching(Map(~name: (_ :: String),
                         ~location: (_ :: Posn)])))

val players :: PersonList:
  [Map(~name: "alice", ~location: Posn(1, 2)),
   Map(~name: "bob", ~location: Posn(3, 4))]
```

As another example, here’s how a `ListOf` annotation constructor could be
implemented if `List.of` did not exists already:

```
annotation.macro '(ListOf ($annotation ...) $tail ......):
  values('(matching([_ :: ($annotation ...), $(' ...)])),
         tail)
```

At a lower level, the bridge between binding patterns and annotations is
based on their shared use of [static information](static-info.md) as
described in the [binding API](binding-macros.md) and the [annotation
API](annotation-macros.md).


## More
[more]: #more

Annotation annotations could be supported in `export`.

For bug-finding purposes, it might make sense to have a
`use_checked_assert` form that defines `-:` as `::`.

The datatype support in this proposal is primitive. Some combination
of Racket’s `for` and Clojure’s transducers is the next step.


# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

There’s no reference, but the prototype implementation is included
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

The use of `.` for hierarchical naming in something like
`f2c.fahrenheit_to_celsius` may seem related to the use of `.` for a
field access like `p.x`. They’re different ideas from the perspective
of enforestation, however, and they’re supported by different
mechanisms in the Rhombus expander. The overloading of `.` here is
Java-like; Rust, in contrast, uses `::` for import paths and `.` for
field access.

The two uses of `.` cannot be unified within the Rhombus expander’s
framework, because they work in different ways. In examples like
`Posn(1, 2).x` or `p.x`, the left-hand side of `.` is an expression
that is already parsed. In `expr.macro` or `1 posn.(<>) 2`, the
left-hand side of `.` is not a definition or expression, and `.` as a
binary operator would not be able to produce a binary operator (at
least not while implementing the target operator’s intended
precedence).

The `.` could be further overloaded for module paths, as in
`racket.math` or `racket.base` instead of `racket/math` or
`racket/base`. In that case, `.` corresponds to an infix dot operator
in a module-path context. But it also invitates generalizations that
are difficult to implement on top of Racket without a new layer of
searching (keeping in mind that searching is the root of many evils).
For example, why not `racket.math.pi`, instead of having to import
`racket.math`' If a module imports `racket/math` as `math`, then it can
export `math` (as opposed to `all_in(math)`), and then an importing
module can use `math.pi`; but importing `racket` and then having an
importing use `racket.math.pi` would be a similar sort of problem.
Meanwhile, using `/` instead of `.` in module paths leaves open the
door to using `.` for referencing a submodule. Finally, using `/`
preserves the connection to filesystem paths and the relative-path
string form of module paths.

## Define

The `def` form may or may not be a good idea. Some programmers may
prefer the generality of `def`, while others may prefer the
specificity of `val`, `fun`, and `expr.macro`. Some
programmers may dislike that there’s more than one way. But having
`def` also makes it easier to have `let`. A `let` modifier
that could be applied to any definition form creates a lot of extra
complexity, because definitions can expand to multiple bindings, and
some of them need to refer to each other.

## Static information versus dynamic checking
[static-dynamic]: #static-dynamic

It’s not obvious that a framework for static information falls within
the mission of Rhombus. If the project is about pushing Racket-style
language extensibility to new notations, that can be done while
just changing `posn-x` to `Posn.x`, and so on.

At the same time, static information interacts with notation choices,
and potential effect on a language’s idioms make it difficult to
ignore. Resolving `.` to a static accessor is the most obvious
example; writing `p.x` is nicer than `Posn.x(p)`. Similarly, it’s nice
to use `[` ... `]` for generic index-based lookups and, when useful,
separately indicate what kind of lookup is involved: array, list, etc.
These static choices can be implemented as dynamic mechanisms plus a
sufficiently clever compiler, but Racket is better positioned through
its macro system for dealing with static information.

We know from experience in Typed Racket that the boundary between
static and dynamic enforcement is itself fraught with performance
pitfalls. The inclusion of both `-:` and `::` is an attempt to
navigate that space. We know, in particular, that the way that `::`
annotations look like types can encourage programmers to create
expensive run-time checks. For example, this `sum` runs in quadratic
time:

```
fun
| sum([]): 0
| sum([head :: Number, tail :: Number, ...]):
    head + sum(tail)
```

Encouraging `-:` over `::` is an attempt to avoid this problem.
Removing `::` or changing it to be `-:` would avoid the problem more
reliably, but dynamic checking is sometimes needed. For example, it’s
appropriate to check all arguments to a library function before
enterting the function body, and `::` is a better way to write that
compared to explicit checks in the function body. Also, a predicate
triggered by `::` interacts well with `match` dispatch. The current
bet in this proposal is that programmers can learn the difference
between `-:` and `::` and use each effectively.

Static information can be used to enable things like `.` for efficient
access, but it can also be used to statically reject programs that
would otherwise be allowed. The choices in this proposal shy away from
that direction (which, in the limit, is a type system), but not
rigidly. Continuing `.` as the key example, a `.` is normally allowed
with a left-hand side that has no static information, in which case it
becomes a dynamic field lookup, but using `use_static_dot` opts in to
a syntactic prohibition against dynamic lookup. Static information
might similarly be used to reject programs that directly apply a
function with the wrong number of arguments or wrong keywords, but
if so, that should also be opt-in.

## Propagating static information
[static-information]: #static-information

The rules for propagating static information are reminiscent of type
rules in Turnstile (Chang et al.). At the level of binding operators,
there is “downward”-flowing information from a context, and there is
“upward” information that originates from binding operators. For
expressions, the only “downward” mechanism is binding, and otherwise
static information flows “upward” from subexpressions. By convention,
static information does not flow “upward” out of blocks.

Rhombus macro extensions are free to create and use static information
as they see fit. However, since extensions need to cooperate, and
since arbitrary static-information flows (say, toward a sound type
system with inference) would make that cooperation complex, this
proposal defines some relatively simple rules and conventions that are
hopefully effective without being surprising to programmers or onerous
to language extensions. The convention about not propagating
information from blocks, for example, is easy to reason about
syntactically and avoids a swath of semantic questions (such as how to
unify results from branches of an `if` or `match`) and expansion-order
problems (such as needing to expand the body of a function to get
static information for use at call sites). If the rules work well
enough to make static `.` convenient, then hopefully they work well in
general.

The `val` rule is a special case, in that it takes static information
from its right-hand side, which is in a block, and propagates it
“downward” into the binding on its left-hand side. Without that rule,
an example like

```
val origin: Point(0, 0)
```

would need an extra `-: Point` in the middle to make `origin.x` and
`origin.y` work, and that would look silly. More generally, the idea
is that it should be possible to give a name to the result of an
expression without losing static information that the expression
itself would have.

To allow `val` to propoagate information that way, the usual order of
parsing must be adjusted to parse the right-hand side earlier. Early
expansion could have unexpected effects, however. In the following
example, `p.x` is rejected because the detinition of `p` has not been
seen when the definition of `v` is being parsed, but parsing for `val
v` forces parsing of `p.x`:

````
use_static_dot

fun f(p -: Number):
  val v: p.x
  val p: Posn(5, 6)
  v
````

Of course, if this example did parse, it would cause an error at run
time, because `p` is used before it is defined. If the reference to
`p.x` is delayed at all, such as being under `fun` or even just
delayed in a parsing sense by being under `if`, then the `p.x`
reference parses as expected. The fact that a forwad reference
requires some sort of delay to be sensible suggests that
parse-reordering by `val` can be practical. Still, to further decrease
the likelihood of surprises due to parse ordering, early expansion of
a `val` right-hand side is constrained to the cases where a `val` form
has a block with a single group and the group does not start with a
name that implements a definition form.

## Multiple values

Multiple return values are a double-edged sword in Racket: often
useful, but also often a pain to abstract over. It’s tempting to
simplify by getting rid of multiple values, but then we’d lose the
benefits that the compiler provides for multiple-value results, and
interoperation with Racket libraries would be slightly more difficult.

## Operator names

Identifier-named infix operators are supported because they cause no
particular prolem for parsing and may be useful, such as for the
`is_a` or `mod` operator. It may make sense to require infix forms to
use shrubbery operators, though, to reduce the space of possible
parses of a shrubbery independent of binding.

## Syntax patterns

Syntax patterns with `'` continue in the `match` tradition of
requiring an escape to create a pattern variable, instead of using the
macro-by-example and `syntax-case` default of treating an identifier
as a pattern variable. Although the macro-by-example convention
creates macros that look prettier, the implicit unquoting of variables
in patterns and templates confuses many newcomers. Unification with
`match` is a big advantage of escaped pattern variables.

## Datatypes and genericity

The `List` and `Array` annotations refer to implementations, while `Map`
refers to an abstraction. The intent is that generic code uses `Map`,
not specifying an implementation of the map, in the same way that `[`
... `]` indexing is generic. At the same time, `Map` as a constructor
can provide a sensible default, instead of forcing programmers to pick
a specific implementation. There is some performance penalty to using
generic maps, but the distasterous performance penalty of
`racket/dict` will be avoided by not putting dependent contracts on
the basic operations like indexing and update.

Following the good example of Clojure, Rhombus programmers should use
maps for representing domain data, not structs (even prefabs) as a
Racket programmer would. Maybe there should be a `map` form, as in

```
map Person(~name :: String, ~location :: Posn)
```

to serve as a shorthand for `annotation.macro` plus `fun`:

```
annotation.macro 'Person:
  '(Map.of(~name: String, ~location: Posn))

fun Person(~name: n :: String, ~location: l :: Posn):
  Map(~name: n, ~location: l)
```

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
