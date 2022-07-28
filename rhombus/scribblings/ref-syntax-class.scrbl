#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@(val dots: @rhombus[..., ~bind])
@(def list(x, ...): [x, ...])

@title{Syntax Classes}

@doc[
  defn.macro '«syntax.class $name:
                $maybe_description
                pattern
                | $clause
                | ...»',
  defn.macro '«syntax.class $name
               | $clause
               | ...»',
  grammar maybe_description:
    description: $body; ...
    $$("ϵ"),
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
]{

 Defines a syntax class that can be used in syntax patterns with
 @rhombus[::]. The @rhombus[pattern] subform is optional in the sense
 that pattern alternatives can be inlined directly in the
 @rhombus[syntax.class] form (but the @rhombus[pattern] subform makes
 room for additional subforms in the future). 

 The optional @rhombus[description] subform can be used before @rhombus[pattern] 
 to define the description of the syntax class. This description is used to 
 produce clearer error messages when a term is rejected by the syntax class. 
 It takes a block whose result must evaluate to a string or 
 @rhombus[#false]. (Setting a description to @rhombus[#false] is equivalent
 to not specifying a description at all and will mean the error message is not
 customized.)

 When a variable @rhombus[id, ~var] is bound through a
 @seclink["stxobj"]{syntax pattern} with
 @rhombus[$($$(@rhombus[id, ~var]) :: $$(@rhombus[stx_class_id, ~var]))],
 it matches a syntax object that matches any of the
 @rhombus[syntax_pattern]s in the definition of
 @rhombus[stx_class_id ,~var], where the @rhombus[syntax_pattern]s are tried
 first to last. A pattern variable that is included in all of the
 @rhombus[syntax_pattern]s is an attribute of the syntax class, which is
 accessed from a binding @rhombus[id, ~var] using dot notation. For
 example, if the pattern variable is @rhombus[attr_id, ~var], its value is
 accessed from @rhombus[id, ~var] using
 @list[@rhombus[id, ~var], @rhombus[.], @rhombus[attr_id, ~var]]. To use an attribute
 within a template, parentheses are needed around the variable name,
 @rhombus[.], and attribute name to group them together if the variable
 name is preceded by a @rhombus[$] escape:
 @rhombus[$($$(@list[@rhombus[id, ~var], @rhombus[.], @rhombus[attr_id, ~var]]))].

 A variable bound with a syntax class (within a syntax pattern) can be
 used without dot notation. In that case, the result is a sequence of
 syntax objects corresponding to the entire match of a
 @rhombus[syntax_pattern], as opposed to an individual attributes within
 the match. Use @rhombus[...] after a @rhombus[$]-escaped reference to
 the variable in a syntax template.

Within a @rhombus[clause], custom attributes of a syntax class can be
defined within a @rhombus[pattern_body], which is a mixture of
expressions, definitions, and @rhombus[~attr] forms. An
@rhombus[~attr] form is a definition, but it also creates a custom
attribute named by an @rhombus[identifier] and at a repetition depth
determined by surrounding @(dots). The value of the right-hand side
@rhombus[body] sequence must be nested lists corresponding to the
repetition depth, with syntax objects as the most nested value (so,
just a syntax object for repetition depth 0 when not @(dots) are used
on the left-hand side). Variables bound by the pattern are available
for use in @rhombus[pattern_body].

@examples[
  ~eval: macro.make_for_meta_eval(),
  begin_for_meta:
    syntax.class Arithmetic
    | '$x + $y'
    | '$x - $y',
  expr.macro 'doubled_operands $(e :: Arithmetic)':
    values('$(e.x) * 2 + $(e.y) * 2', ''),
  doubled_operands 3 + 5,
  expr.macro 'add_one_to_expression $(e :: Arithmetic)':
    values('$e ... + 1', ''),
  add_one_to_expression 2 + 2,
  begin_for_meta:
    syntax.class NTerms
    | '~one $a':
        ~attr b:
          '0'
        ~attr average:
          '$(unwrap_syntax(a) / 2)'
    | '~two $a $b':
        def sum:
          unwrap_syntax(a) + unwrap_syntax(b)
        ~attr average:
          '$(sum / 2)',
  expr.macro 'second_term $(e :: NTerms)':
    values(e.b, ''),
  second_term ~two 1 2,
  second_term ~one 3,
  expr.macro 'average $(e :: NTerms)':
    values(e.average, ''),
  average ~two 24 42
]}
