#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@title{Syntax Objects}

Syntax objects can be constructed using the expression syntax
@rhombus['$$(@rhombus[term, ~var]) ...; ...'], which creates
a syntax object quoting the @rhombus[term, ~var]s. When a single
@rhombus[term, ~var] is present, the result is a single-term syntax
object. When a single @rhombus[$$(@rhombus[term, ~var]) ...]
group is present with multiple @rhombus[term,~var]s, the result is
a group syntax object. The general case is a multi-group syntax
object.

@examples[
  '1',
  'pi',
  '1 + 2',
  '1 + 2
   3 + 4',
]

A @rhombus[$] as a @rhombus[term,~var] escapes a following expression
whose value replaces the @rhombus[$] term and expression. The value is
normally a syntax objects, but other kinds of values are coerced to a
syntax object. Nested @rhombus[''] forms are allowed around @rhombus[$]
and do @emph{not} change whether the @rhombus[$] escapes.

@examples[
  'x $(if #true | 'y' | 'why') z',
  'x $(1 + 2) z',
  '« x '$(1 + 2)' z »'
]

As a binding form,
@rhombus['$$(@rhombus[term, ~var]) ...; ...'] matches a syntax
object consistent with @rhombus[term,~var]s. A @rhombus[$, ~bind] within
@rhombus[form] escapes to an binding that is matched against the
corresponding portion of a candidate syntax object. Ellipses, etc.

@examples[
  match '1 + 2'
  | '$n + $m': [n, m]
]


@doc[
  annotation.macro 'Syntax'
]{

  Matches syntax objects.

}

@doc[
  expr.macro '$ $expr'
]{

 Only allowed within a @rhombus[''] form, escapes so that the value of
 @rhombus[expr] is used in place of the @rhombus[$] form.

}


@doc[
  bind.macro '$ $identifier',
  bind.macro '$ ($identifier :: $syntax_class)',
]{

 Only allowed within a @rhombus['', ~bind] binding pattern, escapes so that
 @rhombus[identifier] is bound to the corresponding portion of the syntax
 object that matches the @rhombus['', ~bind] form.

 The @rhombus[syntax_class] can be @rhombus[Term, ~stxclass], @rhombus[Id, ~stxclass],
 or @rhombus[Group, ~stxclass], among others.

}

@doc[
  syntax.class Term,
  syntax.class Id,
  syntax.class Op,
  syntax.class Id_Op,
  syntax.class Keyw,
  syntax.class Group,
  syntax.class Multi,
  syntax.class Block,
]{

 Syntax classes, all of which imply a single-term match except for
 @rhombus[Group, ~stxclass], @rhombus[Multi, ~stxclass], and
 @rhombus[Block, ~stxclass].

 The @rhombus[Group, ~stxclass] syntax class can be used only for a
 pattern identifier that is the sole term of its group in a pattern. The
 identifier is bound to a match for the entire group as a group syntax
 object.

 The @rhombus[Multi, ~stxclass] syntax class can be used only for a
 pattern identifier that is the sole term where a sequence of groups is
 allowed, such as in the body of a block. The identifier is bound to a
 match for the entire sequence of groups.

 The @rhombus[Block, ~stxclass] syntax class can be used only for a
 pattern identifier that is the sole term of a block. The identifier is
 bound to a match for the entire block as a single term (i.e., as a
 single-term syntax object that has a block term, and not as a
 multi-group syntax object).

}


@doc[
  expr.macro '«literal_syntax '$term ...; ...'»',
  expr.macro 'literal_syntax ($term ..., ...)'
]{

 Similar to a plain @rhombus[''] form, but @rhombus[$] escapes or
 @rhombus[...] and @rhombus[......] repetition forms are not recognizes
 in the @rhombus[term]s, so that the @rhombus[term]s are all treated as
 literal terms to be quoted.

 There's no difference in result between using @rhombus[''] or
 @rhombus[()] after @rhombus[literal_syntax]---only a difference in
 notation used to describe the syntax object, such as using @litchar{;}
 versus @litchar{,} to separate groups.

@examples[
  literal_syntax 'x',
  literal_syntax (x),
  literal_syntax '1 ... 2',
  literal_syntax '$ $ $'
]}

While many syntax classes available for use are built into the language, rhombus 
also supports user-defined syntax classes with @rhombus[stx_class]. 

@doc[
  defn.macro '«stx_class $name:
                pattern
                | '$expr_pattern'
                | ...»',
  defn.macro '«stx_class $name
               | '$expr_pattern'
               |...»'
]{

Syntax classes can be defined with the use of @rhombus[stx_class] followed by 
an identifier and a block containing the form @rhombus[patterns] followed by
alternatives. They can also be defined with a shorthand syntax where the 
alternatives come right after the identifier. 

In each alternative, there should be a syntax pattern. 
These will define the patterns that will be matched on when the syntax class is 
used to annotate a pattern variable in another syntax pattern. 

Any pattern variables included in these syntax pattern alternatives will be 
available as attributes of the syntax class using dot-notation. For an attribute
to be accessible, it must be pressent in every alternate syntax pattern in the 
syntax class definition.

@examples[
  stx_class Arithmetic
  | '$x + $y'
  | '$x - $y',
  stx_class Labeled:
    pattern
    | '~foo $n'
    | '~bar $n',
  val '$(exp :: Arithmetic)': '1 + 2',
  exp.x,
  val '$(l :: Labeled)': '~bar 5',
  l.n
]}

To use a syntax class inside a macro definion, put the definition inside a 
@rhombus[begin_for_meta] block. Any and all defintions inside a 
@rhombus[begin_for_meta] block will be defined for use inside macros. 

@doc[
  defn.macro 'begin_for_meta:
                $body
                ...'
]{

@examples[
  ~eval: macro.make_for_meta_eval(),
  begin_for_meta:
    stx_class Arithmetic
    | '$x + $y'
    | '$x - $y',
  expr.macro 'right_operand $(exp :: Arithmetic)':
    values(exp.y, ''),
  right_operand 1 + 2
]

Pattern variables annotated with a syntax class can be used on their own 
to generate the syntax they are bound to or inside templates where they 
are escaped with a @rhombus[$]. To use an attribute on a pattern variable 
inside a template, the form must be a @rhombus[$] escape followed by 
parentheses that have the pattern variable identifier, a dot, and then the 
attribute identifier. To use the entire syntax that was matched, escape the 
identifier with @rhombus[$] and follow it with @rhombus[...]. 

@examples[
  ~eval: macro.make_for_meta_eval(),
  begin_for_meta:
    stx_class Arithmetic
    | '$x + $y'
    | '$x - $y',
  expr.macro 'doubled_operands $(e :: Arithmetic)':
    values('$(e.x) * 2 + $(e.y) * 2', ''),
  doubled_operands 3 + 5,
  expr.macro 'add_one_to_expression $(e :: Arithmetic)':
    values('$e ... + 1', ''),
  add_one_to_expression 2 + 2
]}
