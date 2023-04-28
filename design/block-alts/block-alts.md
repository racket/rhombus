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

### Option A

Describe a complete approach to the design, introducing its components
and how they might interact.

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

Evaluation and Tradeoffs
--------------

If/when more options are added,
Explain the tradeoffs between the various options.

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

Contributors
------------

* sorawee
* Alex Knauth
* notjack
