Ellipses and Repetitions
========================

Motivation
----------

Ellipses in Racket's `syntax-parse` patterns and `syntax` templates work
differently than in Racket's `match` for data-structure matching. In
an effort to unify these two forms of matching, the current Rhombus
prototype follows the `match` approach, which has the advantage that
it does not introduce a new kind of binding. For example, when the
pattern `'$x + $y'` is used to match the syntax object `'1 + 2'`, then
`x` and `y` are bound as regular variables to the syntax-object values
`'1'` and `'2'`.

When a binding is modified an ellipsis, `syntax-parse` binds the
pattern variable so that it much be referenced using an ellipsis in
a template, while `match` in the original Rhombus prototype caused
the bound variable to hold a list of
matches. For example, the pattern `'($x ...)'` matched against `'(1 2 3)'`
causes `x` to be bound to `['1', '2', '3']`. Here, again, the
advantage is that plain variables and value are used. That
simplification, however, came at some cost:

  * There was a naming mismatch between `x` in a pattern standing for
    something that's replicated versus its value as a list of matches.
    Following the convention of adding `s` to the end of a variable
    name when it holds a list, the right binding form is either
    `'($x ...)'` or `'($xs ...)'`, depending on whether you care more
    about `x` as an individual match in the pattern or `xs` as a
    list-valued variable to use in the body of the match.

    When writing a pattern–template combination in the current Rhombus
    prototype, a programmer can choose to ignore the fact that a
    variable is bound to a list and use a name in singular form:

    ```
    val '($head_n, $tail_n, ...)': '(1, 2, 3)'
    '{f($tail_n), ...}' // => `{f(2), f(3)}`
    ```

    That choice is not available for working with list matches that do not
    feed into a template, however.

  * Mismatches in the number of ellipses for a binding and for a use
    were detected dynamically, instead of statically.

  * Templates could not straightforwardly handle mixtures of binding
    depth nested under the same number of ellipses (matching the
    deepest binding). This generalization has proven occasionally
    useful in Scheme and Racket's `syntax` templates.

  * Exposing the representation of a `match` sequence interfered with
    efficiently transferring a tail pattern match to a tail template
    positions, as in

    ```
    expr.macro '$a ! $tail ...':
    values('factorial($a)', '$tail ...')
    ```

    If `tail` is not treated specially here, then a sequence of `!`
    postfix operations `1 ! ! ! ....` takes quadratic time to expand.
    The initial Rhombus implementation required a separate operator,
    `......`, to enable tail sharing. More recent Rhombus iterations
    choose a different representation for tail-pattern variables than
    a list, but this turns out to create an ambiguity with implicit
    conversions among term, group, and multi-group syntax objects.

Summary
-------

We adopted an alternative design where `...` creates a new kind of
_repetition_ binding that can be referenced only in a repetition
context. A repetition context is created by various forms, such as
syntax templates, list constructions, and function calls, typically
triggered by a `...` (but extensibility means that a repetition
context can be anywhere a macro implementer wants one).

This approach resolves the naming problem with ellipses, since a
variable is modified by ellipses both at a binding and at a use:

```
val [$head_n, $tail_n, ...]: [1, 2, 3]
[tail_n, ...] // => [2, 3]
```

Using `tail_n` above not in a repetition content would be a syntax
error. Here's another example, this time with ellipses in a function
argument list and function call:

```
fun
| add(): 0
| add(x): x
| add(x, y, ...): x + add(y, ...)

add(1, 2, 3)    // prints 6
```

Other problems with binding plain variables to lists in the motivation
(i.e., besides naming) are also resolved with repetitions in a simple
and consistent way. The notion of repetition positions fits well in
the Rhombus language model, both because Rhombus is already set up to
handle different kinds of context and because the enforestation
machinery is easily deployed to those different contexts.

In a sense, this design pushes `match` back toward `syntax-parse` in
creating a distinction between different kinds of binding. However,
the two forms of matching are still unified, because `...` is
available for referencing sequences of data-structure matches as well
as sequences of syntax-object matches. Matching also remains
`match`-like in the way that binding variables are escaped within a
syntax pattern, instead of following the macro-by-example tradition of
treating an identifier as a pattern variable by default.

Design
------

The expansion of a repetition form must inherently provide two pieces
of information: a repetition depth and an expression that produces a
representation of the elements (currently as nested lists). To
cooperate with the propagation of static information, a repetition
expansion also provides static information that applies to each
element of the repetition. Finally, to help with error reporting, a
repetition expansion has a name and a delta between an original
repetition depth and the remaining depth of the form.

A ”repetition binding” is a binding to an identifier in the repetition
space to a transformer procedure that produces a repetition expansion.
As always with different binding spaces, an identifier can be bound in
multiple of them. For example, an identifier bound by a syntax pattern
annotated with a syntax class will have a repetition binding (since a
match in general is a sequence of terms), but is also works in an
expression position combined with `.` to access an attribute. More
generally, an identifier or operator can be bound as a prefix or infix
operator, and repetition expansion is based enforestation as usual.

Repetition forms, repetition binders, and repetition positions from
`#lang rhombus` in the implementation:

 * A syntax pattern recognizes nested ellipses and `$` escapes to
   create repetition bindings that expansion to depths N as determined
   by nesting. (As of the current implementation, more work is needed
   to handle the case when a pattern identifier is annotated with a
   syntax class and a `.` attribute reference is under ellipses.)

 * A syntax template recognizes nested ellipses and `$` escapes to
   create repetition positions that expect a repetition at depth N as
   determined by nesting. (Currently, each repetition under a given
   `...` must be at the same depth. The difference between the nesting
   depth N and repetitions's depth could be used to get behavior more
   like Scheme when a repetition is used under more ellipses than its
   depth.)

 * A list pattern recognizes `...` in place of the last element and
   converts each identifier in the preceding binding form to a
   repetition binding. (This could be generalized to allow `...` in
   other positions, but some generalizations lead to non-linear
   matching.) Patterns with `...` can be nested, leading to repetition
   bindings at greater depths.

 * A list construction recognizes `...` after any element and treats
   the preceding element as a repetition form of depth 1. The
   repetition's elements are spliced into the last , or they form the
   tail of the list if `...` is at the end. A `...` repetition
   reference can appear anywhere in a list construction, and multiple
   `...` repetitions can be used in the same list construction.

 * The list-construction form also works as repetition. In that case,
   its elements are parsed as repetitions, and the depth of the
   overall list repetition is the same as the greatest depth among its
   elements. Referring to repetitions of different depths is handled
   as in `syntax-parse`: shallower repetitions are repeated for
   additional outer instances of other repetitions.

 * Literals and variables are repetitions of depth 0, which means that
   they can be combined with deeper repetitions to "copy" the literal
   or variable reference as needed.

 * A function declaration with `...` in place of the last formal
   argument convert each identifier in the preceding binding form to a
   repetition binding reperseting “rest” argument.

 * A function call recognizes `...` in place of an argument and treats
   the preceding argument as a repetition form of depth 1. The
   repetition elements serve as by-position arguments, spliced in
   among other arguments.

 * A function-call form is also a repetition, analogous to a list
   construction. Since a variable bound to a function works as a
   repetition (of depth 0), a list construction can effectively map a
   function over a repetition, as in `[f(x), ...]` where `x` is a
   repetition of depth 1 and `f` is a variable that is bound to a
   function.

 * Uses of operators (as defined by `operator`), `.`, and `[]` for map
   and array indexing all work as repetition forms analogous to
   function calls. For example, `[x+1, ...]` maps the addition of 1
   over the elements of the repetition `x`. Note that the right-hand
   side of `.` is not an expression position normally, and it is also
   not a repetition position when `.` is used as a repetition.

 * A parenthesized repetition is a repetition.

 * The `List.repet(expr)` form is a repetition of depth 1 and expects
   `expr` to produce a list.

Open Issues
-----------

This design complements most of #225, not replace it, except for the
“Ellipses” parts. In particular, `& lst` syntax seems better and
worthwhile to apply a function to a list compared to
`List.repet(lst), ...`. Also, the design here does not address
keyword “rest” arguments at all.

`List.repet` is a placeholder to demonstrate an idea. The final name
or operator could be different, or the operation could be omitted from
`#lang rhombus`, but the possibility of creating this operator seem
important to the generality of repetitions.

Defining repetition variants of forms like function calls, operator
use, and `.` seems to work well in striking a balance between
generality and simplicity. Trying to automatically adapt all
expression forms to repetitions seems to invite trouble, because some
binding forms will introduce bindings or have non-expression
components (like the right-hand side of `.`). The current
implementation does not include as repetition any predefined form that
has binding or a block (that can contain definitions); it might make
sense to allow `if` with single-expression branches, or maybe it's
better to defer those kinds of computations to a less compact but more
general form like `for`. This issue seems related to Scala-style
implicit functions (see mflatt/shrubbery-rhombus-0#4), which would also
ideally automatically adapt expression forms, but it's not clear how
that can be done in general. Another possibility is that being more
explicit about literal versus repeated parts is better; see also
Steele (2017), and along those lines, maybe code-quoting notation
like `'y+$x'` would be a good approach. (If the difference between
literal and repeated parts is made explicit, it's not clear that the
concept of repetition positions is needed, since the explicit form
turns into some variant of `map` either way.)

Other Discussion
----------------

Some prefer `fun (x ...): body` to `fun (x, ...): body`, omitting a
comma before the `...`, on the grounds that the comma looks strange
and `...` belongs in the group that it modifies. Putting the `...` in
a separate group (i.e., after a comma) is consistent with syntax
pattern matching, however, where `($x ...)` and `($x, ...)` are
distinct and meaningful patterns.

Prior art
---------

Scheme and Racket macros, especially macros-by-example (Kohlbecker & Wand, 1987).

[Macros by example in Rust](https://doc.rust-lang.org/reference/macros-by-example.html).

Scheme and Racket `match`.

Python, as noted in #225:
[function definitions with starargs](https://docs.python.org/3/reference/compound_stmts.html#function-definitions)
and [function calls with *expression and **expression](https://docs.python.org/3/reference/expressions.html#calls).


Contributors
------------

* Ross Angle
* Jack Firth
* Matthew Flatt
* Sid Kasivajhula
* Alex Knauth
* Sorawee Porncharoenwase
* Sam Tobin-Hochstadt

References
----------

Steele, Guy L. (2017). "Computer Science Metanotation, It's Time for a
New Old Language." ClojureConj [talk](https://www.youtube.com/watch?v=dCuZkaaou0Q).

Kohlbecker, E.E. and M. Wand (1987). "Macro-by-example: Deriving
Syntactic Transformations from Their Specifications." POPL.
