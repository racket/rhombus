#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Class and Interface Clause Macros}

@doc(
  defn.macro 'class_clause.macro $rule_pattern:
                $option; ...
                $body
                ...'
  defn.macro 'class_clause.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...'
){

 Defines an @rhombus(identifier, ~var) or @rhombus(operator_path, ~var)
 as a @rhombus(class) clause form, where @rhombus(rule_pattern) and form
 is the same as for @rhombus(defn.macro). The macro pattern is matched to
 an entire group in a definition context.

 The compile-time @rhombus(body) block returns the expansion result. The
 result must be a sequence of groups to be spliced in place of the macro
 use, where each group can be either a another @rhombus(class) clause, an
 expression, a defintion, or an export.

@examples(
  ~eval: macro_eval
  class_clause.macro 'lazy_method $id(): $body':
    'private field result: #false
     method $id():
       result || (begin:
                    def v: $body
                    result := v
                    v)'
  class Person(name):
    lazy_method greeting(): "Hello, " +& name
  def ming: Person("Ming")
  ming.greeting()
  ming.greeting() === ming.greeting()
)

}

@doc(
  defn.macro 'interface_clause.macro $rule_pattern:
                $option; ...
                $body
                ...'
  defn.macro 'interface_clause.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...'
){

Like @rhombus(class_clause.macro), but for @rhombus(interface) clauses.

}

@doc(
  defn.macro 'class_and_interface_clause.macro $rule_pattern:
                $option; ...
                $body
                ...'
  defn.macro 'class_and_interface_clause.macro
              | $rule_pattern:
                  $option; ...
                  $body
                  ...
              | ...'
){

 Like @rhombus(class_clause.macro), but defines for use both in
 @rhombus(class) clauses and @rhombus(interface) clauses.

}

@doc(
  defn.macro 'class_clause.only.macro $macro_decl'
  defn.macro 'interface_clause.only.macro $macro_decl'
  defn.macro 'class_and_interface_clause.only.macro $macro_impl'
){

 Like @rhombus(class_clause.macro), @rhombus(interface_clause.macro),
 and @rhombus(class_and_interface_clause.macro), but the identifier is
 bound only in the @rhombus(rhombus/class, ~datum) space,
 @rhombus(rhombus/interface, ~datum) space, or both @tech{spaces},
 respectively.

}


@«macro.close_eval»(macro_eval)
