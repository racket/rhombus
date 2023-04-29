block-alts
==========================

Summary
-------

An addition to Shrubbery to allow `|` alts after the end of a `:` block.

Motivation
----------

 * Syntax class options, including kind and fields, without `pattern` in between
 * Operator options, including precedence, without `match` in between
 * Macro options, including precedence and op_stx, without `match` in between
 * Function cases with a common result annotation, without `match` in between
 * Using shrubbery syntax for a whitespace-lisp

Design Options
--------------

### Option A: `|` pipe indentation less than preceding block indentation

For a shrubbery like:

```
a:
  b
| c
```

To parse as:

```
(group a
       (block (group b))
       (alts (block (group c))))
```

The indentation of the `|` before `c` must be greater-than-or-equal-to the indentation of `a`, and less than the indentation of `b`. If it's less than `a`, then it might belong to a parent of `a` instead of `a`'s group. If it's at `b` or more, then it would belong to `b`'s group instead of `a`'s group.

#### Parsing shrubberies

```
‹group›	::= ‹term›* ‹tail›

‹tail› 	::= ‹term›
       	 |  ‹block›
       	 |  ‹alts›
       	 |  ‹block› ‹alts›

‹block›	::= `:` ‹group›*

‹alts› 	::= `|` ‹block›*
```

```
_group	= (group _term ... _tail)

_tail 	= _term
      	| _block
      	| _alts
      	| _block _alts

_block	= (block _group ...)

_alts 	= (alts _block ...)
```

#### Parsing written/armored forms of shrubberies

The current code in `rhombus-prototype/shrubbery/write.rkt` is already good.

However, the parser isn't good enough to handle what `write-shrubbery` produces: 

```
a:
  b
| c
```

Is parsed into `(group (block (group b)) (alts (block (group c))))`
which gets written as `a:« b » |« c »`,
but then the current parser implementation isn't good enough to read that back yet.

#### Modifying Rhombus forms to use block-alts

In general, Rhombus forms that currently have the shape:
```
main_id:
  option; ...
  other_id
  | case
  | ...
```

Where `other_id` is something bland like `pattern` or `match`, can be simplified to:

```
main_id:
  option; ...
| case
| ...
```

For example this `syntax_class` form:

```
syntax_class id maybe_args:
  stxclass_option; ...
  pattern
  | pattern_case
  | ...
```

Could by simplified to:

```
syntax_class id maybe_args:
  stxclass_option; ...
| pattern_case
| ...
```

More concretely:

```
syntax_class result:
  kind: ~sequence
| '$(mode :: result_mode): $body':
    field [suffix, ...]: []
| '$(mode :: result_mode) $g ...':
    field body: '$g ...'
    field [suffix, ...]: []
| '~completes':
    field mode: '~completes'
    field body: '#void'
    field [suffix, ...]: ['#void']
```

This `operator` form:

```
operator op_or_id_path:
  option; ...
  match
  | op_case
  | ...
```

Could by simplified to:

```
operator op_or_id_path:
  option; ...
| op_case
| ...
```

More concretely:

```
operator ^^^:
  ~weaker_than: +
| x ^^^ y:
    x +& y +& x
| ^^^ y:
    "--" +& y +& "--"
```

This `macro` form:

```
macro op_or_id_path:
  option; ...
  match
  | macro_case
  | ...
```

Could by simplified to:

```
macro op_or_id_path:
  option; ...
| macro_case
| ...
```

And this `fun` form:

```
fun maybe_res_annot:
  match
  | case_maybe_kw
  | ...
```

Could be simplified to:

```
fun maybe_res_annot
| case_maybe_kw
| ...
```

If a syntax-parse form is added with parse options before the cases, that could be expressed like this:

```
syntax_parse stx_expr:
  parse_option; ...
| pattern_case
| ...
```

Instead of needing some identifier like `pattern` or `match` separating the options from the cases.

### Option B: `|` pipe indentation equal to preceding block indentation

For a shrubbery like:

```
a:
  b
  | c
```

To parse as:

```
(group a
       (block (group b))
       (alts (block (group c))))
```

The indentation of the `|` before `c` must be greater-than the indentation of `a`, and equal to the indentation of `b`. If it's less-than-or-equal-to `a`, then it might belong to a parent of `a` instead of `a`'s group.
If it's between `a` and `b`, then its an indentation error.
If it's equal to `b` then it parses correctly as above.
And if it's at indentation greater than `b`, then it would belong to `b`'s group instead of `a`'s group.

#### Parsing shrubberies

The same grammar as option A, but with differences in how indentation is interpreted.
In particular, for this to work consistently,

```
a
| b
```

Should not be allowed, and instead

```
a
 | b
```

Would be required for it to parse correctly as

```
(group a
       (alts (block (group b))))
```

#### Parsing written/armored forms of shrubberies

This still runs into the same problem option A has with parsing `a:« b » |« c »`.

#### Modifying Rhombus forms to use block-alts

In general, Rhombus forms that currently have the shape:
```
main_id:
  option; ...
  other_id
  | case
  | ...
```

Where `other_id` is something bland like `pattern` or `match`, can be simplified to:

```
main_id:
  option; ...
  | case
  | ...
```

For example this `syntax_class` form:

```
syntax_class id maybe_args:
  stxclass_option; ...
  pattern
  | pattern_case
  | ...
```

Could by simplified to:

```
syntax_class id maybe_args:
  stxclass_option; ...
  | pattern_case
  | ...
```

More concretely:

```
syntax_class result:
  kind: ~sequence
  | '$(mode :: result_mode): $body':
      field [suffix, ...]: []
  | '$(mode :: result_mode) $g ...':
      field body: '$g ...'
      field [suffix, ...]: []
  | '~completes':
      field mode: '~completes'
      field body: '#void'
      field [suffix, ...]: ['#void']
```

This `operator` form:

```
operator op_or_id_path:
  option; ...
  match
  | op_case
  | ...
```

Could by simplified to:

```
operator op_or_id_path:
  option; ...
  | op_case
  | ...
```

More concretely:

```
operator ^^^:
  ~weaker_than: +
  | x ^^^ y:
      x +& y +& x
  | ^^^ y:
      "--" +& y +& "--"
```

This `macro` form:

```
macro op_or_id_path:
  option; ...
  match
  | macro_case
  | ...
```

Could by simplified to:

```
macro op_or_id_path:
  option; ...
  | macro_case
  | ...
```

And this `fun` form:

```
fun maybe_res_annot:
  match
  | case_maybe_kw
  | ...
```

Could be simplified to:

```
fun maybe_res_annot
  | case_maybe_kw
  | ...
```

If a syntax-parse form is added with parse options before the cases, that could be expressed like this:

```
syntax_parse stx_expr:
  parse_option; ...
  | pattern_case
  | ...
```

Instead of needing some identifier like `pattern` or `match` separating the options from the cases.

Evaluation and Tradeoffs
--------------

### Good of A:

Option A fits better into the existing parser which allows `|` on the same indentation level as the head of the group it's a part of. All existing code that uses `|` in that way will continue to parse the same way. Option A also has less "rightward drift" required.

### Bad of A:

Cases like:

```
a:
  b
  | c
```

vs.

```
a:
  b
 | c
```

Have the potential to be confusing, as they are visually similar but parse completely differently.

To someone unfamiliar with how Rhombus deals with `|` alts, those might both look like the `|` alts belong to `a`'s group and not `b`'s group like the first one currently does.

### Good of B

Option B may be easier to read, with the children of a group clearly indented more than the head of the group. This:

```
a and stuff
  | b and more stuff
  | c and even more
```

Is clearer about indentation grouping than what option A allows.

Making `|` alts indentation between `a` and `b` an error prevents the problem in Bad of A. If

```
a:
  b
 | c
```

Is an indentation error, then there's no confusion between that and 

```
a:
  b
  | c
```

Option B might also allow the lexer and parser code to be simpler: the half-integer column values for `|` alts could be turned into simple integers.

### Bad of B

Changing existing Rhombus programs will require some effort.
Especially existing programs that currently use:

```
a:
  b
  | c
```

to mean

```
(group a
       (block (group b
                     (alts (block (group c))))))
```

Might "silently" fail, with the parser interpreting the same text as a different tree structure:

```
(group a
       (block (group b))
       (alts (block (group c))))
```

Instead of giving an error or some other immediate feedback to prompt users to fix their code.

Rightward drift may also become a problem with option B, when programs get larger, more complicated, and more nested, the little bits of extra indentation that option B forces on `|` alts may start to add up.

Prior art and References
---------

Some inspiration from [Lean function cases](https://leanprover.github.io/functional_programming_in_lean/getting-to-know/conveniences.html#pattern-matching-definitions):

```
def length : List α → Nat
| [] => 0
| y :: ys => Nat.succ (length ys)
```

Leading to a [discord discussion between sorawee, Alex Knauth, and notjack on 2023-03-31](https://discord.com/channels/571040468092321801/871953739982995547/1091533258329706569) on forms from `fun` to `operator`, `syntax_class`, and `syntax-parse`.

Also some inspiration from using `syntax-parse` in [unnwraith](https://github.com/AlexKnauth/unnwraith):

```
define (parse stx):
  syntax-parse stx:
    ~datum-literals [λ]
  | x
    ~declare x id
    Var (symbol->string (syntax-e 'x'))
  | λ x: b
    ~declare x id
    Lam (symbol->string (syntax-e 'x')) (parse 'b')
  | f a
    App (parse 'f') (parse 'a')
  | f a ...+ b
    App (parse 'f a ...') (parse 'b')
```

Drawbacks and Unresolved questions
----------------------------------

See "Bad of A" and "Bad of B" in the Evaluation and Tradeoffs section.

Contributors
------------

* sorawee
* Alex Knauth
* notjack
* mflatt
