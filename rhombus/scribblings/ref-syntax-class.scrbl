#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@(def list(xs, ...): xs)

@title{Syntax Classes}

@doc[
  defn.macro '«syntax.class $name:
                pattern
                | $syntax_pattern
                | ...»',
  defn.macro '«syntax.class $name
               | $syntax_pattern
               | ...»'
]{

 Defines a syntax class that can be used in syntax patterns with
 @rhombus[::]. The @rhombus[pattern] subform is optional in the sense
 that pattern alternatives can be inlined directly in the
 @rhombus[syntax.class] form (but the @rhombus[patttern] subform makes
 room for additional subforms in the future).

 When a variable @rhombus[id, ~var] is bound through a
 @seclink["stxobj"]{syntax pattern} with
 @rhombus[$($$(@rhombus[id, ~var]) :: $$(@rhombus[stx_class_id, ~var]))],
 it matches a syntax object that matches any of the
 @rhombus[syntax_pattern]s in the definition of
 @rhombus[stx_class_id ,~var], the @rhombus[syntax_pattern]s are tried
 first to last. A pattern variable that is included in all of the
 @rhombus[syntax_pattern]s is an attribute of the syntax class, which is
 accessed from a binding @rhombus[id, ~var] using dot notation. For
 example, if the pattern variable is @rhombus[attr_id, ~var], its value is
 accessed from @rhombus[id, ~var] using
 @list[@rhombus[id, ~var], @rhombus[.], @rhombus[attr_id, ~var]]. To use an attribute
 within a template, parentheses are needed around the variable name,
 @rhombus[.], and attribue name to group them together if the variable
 name is preceded by a @rhombus[$] escape:
 @rhombus[$($$(@list[@rhombus[id, ~var], @rhombus[.], @rhombus[attr_id, ~var]]))].

 A variable bound with a syntax class (within a syntax pattern) can be
 used without dot notation. In that case, the result is a sequence of
 syntax objects corresponding to the entire match of a
 @rhombus[syntax_pattern], as opposed to an indvidual attributes within
 the match. Use @rhombus[...] after a @rhombus[$]-escaped reference to
 the variable in a syntax template.

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
  add_one_to_expression 2 + 2
]}
