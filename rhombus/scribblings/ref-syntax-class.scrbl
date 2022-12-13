#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@(def dots: @rhombus(..., ~bind))
@(fun list(x, ...): [x, ...])

@title{Syntax Classes}

@doc(
  defn.macro '«syntax.class $name
               | $clause
               | ...»',
  defn.macro '«syntax.class $name:
                $option; ...
                ~pattern
                | $clause
                | ...»',
  grammar option:
    ~description: $body; ...
    $kind,
  grammar kind:
    ~term
    ~sequence
    ~group
    ~multi
    ~block,
  grammar clause:
    $syntax_pattern
    $syntax_pattern: $pattern_body; ...,
  grammar pattern_body:
    $body
    ~attr $identifier_maybe_rep: $body; ...,
  grammar identifier_maybe_rep:
    $identifier
    [$identifier_maybe_rep, $ellipsis],
  grammar ellipsis:
    $$(dots)
){

 Defines a syntax class that can be used in syntax patterns with
 @rhombus(::). The @rhombus(~pattern) subform is optional in the sense
 that pattern alternatives can be inlined directly in the
 @rhombus(syntax.class) form, but the @rhombus(~pattern) subform makes
 room for additional @rhombus(option)s. Each @rhombus(option) alternative
 can be supplied at most once.

 An optional @rhombus(~description) subform provides a description of
 the syntax class which is used to produce clearer error messages when
 a term is rejected by the syntax class. The result of the
 @rhombus(block) block must be a string or @rhombus(#false), where
 @rhombus(#false) is equivalent to not specifying a
 @rhombus(~description).

 An optional @rhombus(kind) determines where the context within a
 pattern where a syntax class can be used, and it determines the kind
 of match that each pattern specifies. Declaring @rhombus(~term) means
 that each pattern represents a single term, and the syntax
 class can be used in the same way at the @rhombus(Term, ~stxclass)
 syntax class. Declaring @rhombus(~sequence) means that each pattern
 represents a sequence of terms that is spliced within a group;
 @rhombus(~sequence) is the default mode of a syntax class when no
 @rhombus(kind) is specified. Declaring @rhombus(~group) means that
 each pattern represents a @tech{group}, and the syntax class can be
 used in the same places as @rhombus(Group, ~stxclass) (i.e., alone
 within its group). Declaring @rhombus(~multi) means that the pattern
 represents multiple groups, and the syntax class can be used in the
 same way at the @rhombus(Multi, ~stxclass) syntax class. Declaring
 @rhombus(~multi) means that the pattern represents a block, and the
 syntax class can be used in the same way at the @rhombus(Block, ~stxclass)
 syntax class. With @rhombus(~term), each pattern must
 match only a single term, and with @rhombus(~block), each pattern
 must be a block pattern.

 When a variable @rhombus(id, ~var) is bound through a
 @seclink("stxobj"){syntax pattern} with
 @rhombus($($$(@rhombus(id, ~var)) :: $$(@rhombus(stx_class_id, ~var)))),
 it matches a syntax object that matches any of the
 @rhombus(syntax_pattern)s in the definition of
 @rhombus(stx_class_id ,~var), where the @rhombus(syntax_pattern)s are tried
 first to last. A pattern variable that is included in all of the
 @rhombus(syntax_pattern)s is an attribute of the syntax class, which is
 accessed from a binding @rhombus(id, ~var) using dot notation. For
 example, if the pattern variable is @rhombus(attr_id, ~var), its value is
 accessed from @rhombus(id, ~var) using
 @list(@rhombus(id, ~var), @rhombus(.), @rhombus(attr_id, ~var)). To use an attribute
 within a template, parentheses are needed around the variable name,
 @rhombus(.), and attribute name to group them together if the variable
 name is preceded by a @rhombus($) escape:
 @rhombus($($$(@list(@rhombus(id, ~var), @rhombus(.), @rhombus(attr_id, ~var))))).

 A variable bound with a syntax class (within a syntax pattern) can be
 used without dot notation. In that case, the result for
 @rhombus(~sequence) mode is a sequence of syntax objects
 corresponding to the entire match of a @rhombus(syntax_pattern); use
 @rhombus(...) after a @rhombus($)-escaped reference to the variable
 in a syntax template. For other modes, the variable represents a
 single syntax object representing matched syntax.

 Within a @rhombus(clause), custom attributes of a syntax class can be
 defined within a @rhombus(pattern_body), which is a mixture of
 expressions, definitions, and @rhombus(~attr) forms. An
 @rhombus(~attr) form is a definition, but it also creates a custom
 attribute named by an @rhombus(identifier) and at a repetition depth
 determined by surrounding @(dots). The value of the right-hand side
 @rhombus(body) sequence must be nested lists corresponding to the
 repetition depth, with syntax objects as the most nested value (so,
 just a syntax object for repetition depth 0 when not @(dots) are used
 on the left-hand side). Variables bound by the pattern are available
 for use in @rhombus(pattern_body).

@examples(
  ~eval: macro.make_for_meta_eval(),
  meta:
    syntax.class Arithmetic
    | '$x + $y'
    | '$x - $y',
  expr.macro 'doubled_operands $(e :: Arithmetic)':
    values('$(e.x) * 2 + $(e.y) * 2', ''),
  doubled_operands 3 + 5,
  expr.macro 'add_one_to_expression $(e :: Arithmetic)':
    values('$e ... + 1', ''),
  add_one_to_expression 2 + 2,
  meta:
    syntax.class NTerms
    | '~one $a':
        ~attr b:
          '0'
        ~attr average:
          '$(Syntax.unwrap(a) / 2)'
    | '~two $a $b':
        def sum:
          Syntax.unwrap(a) + Syntax.unwrap(b)
        ~attr average:
          '$(sum / 2)',
  expr.macro 'second_term $(e :: NTerms)':
    values(e.b, ''),
  second_term ~two 1 2,
  second_term ~one 3,
  expr.macro 'average $(e :: NTerms)':
    values(e.average, ''),
  average ~two 24 42
)

}
