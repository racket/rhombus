#lang scribble/rhombus/manual
@(import: "common.rhm" open)

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
  bind.macro '......'
]{

 Used within @rhombus['', ~bind] binding patterns to indicate tail
 repetition.

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


