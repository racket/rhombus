#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Syntax Objects}

@doc[
  annotation.macro 'Syntax
]{

  Matches syntax objects.

}

@doc[
  expr.macro '(' $form)
]{

 Creates a syntax object quoting @rhombus[form].

 A @rhombus[$] within @rhombus[form] escapes to an expression whose
 value replaces the @rhombus[$] form. A @rhombus['] within @rhombus[form]
 increases the quoting depth so that a matching @rhombus[$] within the
 nested @rhombus['] form doesn't escape, but merely decreases the quoting
 depth.

@examples[
  '1,
  'pi,
  '(1 + 2),
  '($(1 + 2)),
  '(' $(1 + $(4-1))),
]

}


@doc[
  expr.macro '($ $expr)
]{

 Only allowed within a @rhombus['] form, escapes so that the value of
 @rhombus[expr] is used in place of the @rhombus[$] form.

}


@doc[
  bind.macro '(' $form)
]{

 Matches a syntax object consistent with @rhombus[form].

 A @rhombus[$, ~bind] within @rhombus[form] escapes to an binding that is
 matched against the corresponding portion of a candidate syntax object.
 Ellipses, etc.

@examples[
  match '(1 + 2)
  | '($n + $m): [n, m]
]

}


@doc[
  bind.macro '($ $identifier),
  bind.macro '($ ($identifier $: $syntax_class)),
]{

 Only allowed within a @rhombus[', ~bind] binding pattern, escapes so that
 @rhombus[identifier] is bound to the corresponding portion of the syntax
 object that matches the @rhombus[', ~bind] form.

 The @rhombus[syntax_class] can be @rhombus[Term, ~stxclass], @rhombus[Id, ~stxclass],
 or @rhombus[Group, ~stxclass], among others.

}

@doc[
  expr.macro '($identifier $: $syntax_class)
]{

 Only allowed with @rhombus[$] within a template, matches and binds
 @rhombus[identifier] only if the canditate syntax-object portion matches
 @rhombus[syntax_class].

}

@doc[
  syntax.class Term,
  syntax.class Id,
  syntax.class Op,
  syntax.class Id_Op,
  syntax.class Keyw,
  syntax.class Group,
  syntax.class Block,
]{

 Syntax classes, all of which imply a term match except for
 @rhombus[Group, ~stxclass] and @rhombus[Block, ~stxclass].

 The @rhombus[Group, ~stxclass] syntax class can be used only for a
 pattern identifier that is the sole term of its group in a pattern. The
 identifier is bound to a match for the entire group.

 The @rhombus[Block, ~stxclass] syntax class can be used only for a
 pattern identifier that is the sole term of a block. The identifier is
 bound to a match for the entire block.

}


@doc[
  bind.macro '(......)
]{

 Used within @rhombus[', ~bind] binding patterns to indicate tail
 repetition.

}


