#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(val macro_eval: macro.make_macro_eval())

@(val dollar: @rhombus($))

@title{Class and Interface Clause Macros}

@doc(
  defn.macro 'class_clause.macro $rule_pattern:
                $option; ...
                $body
                ...',
  defn.macro 'class_clause.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...',
){

 Defines an @rhombus(identifier, ~var), @rhombus(operator, ~var),
 @rhombus(identifier_path, ~var), or @rhombus(operator_path, ~var) as a
 @rhombus(class) clause form, where @rhombus(rule_pattern) and form is the
 same as for @rhombus(defn.macro). The macro pattern is matched to an
 entire group in a definition context.

 The compile-time @rhombus(body) block returns the expansion result. The
 result must be a sequence of groups to be spliced in place of the macro
 use, where each group can be either a another @rhombus(class) clause, an
 expression, a defintion, or an export.

@examples(
  ~eval: macro_eval,
  class_clause.macro 'lazy_method $id(): $body':
    'private field result: #false
     method $id():
       result || (begin:
                    val v: $body
                    result := v
                    v)',
  class Person(name):
    lazy_method greeting(): "Hello, " +& name,
  val ming: Person("Ming"),
  ming.greeting(),
  ming.greeting() === ming.greeting()
)

}

@doc(
  defn.macro 'interface_clause.macro $rule_pattern:
                $option; ...
                $body
                ...',
  defn.macro 'interface_clause.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...',
){

Like @rhombus(class_clause.macro), but for @rhombus(interface) clauses.

}

@doc(
  defn.macro 'class_and_interface_clause.macro $rule_pattern:
                $option; ...
                $body
                ...',
  defn.macro 'class_and_interface_clause.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...',
){

 Like @rhombus(class_clause.macro), but defines for use both in
 @rhombus(class) clauses and @rhombus(interface) clauses.

}

@«macro.close_eval»(macro_eval)
