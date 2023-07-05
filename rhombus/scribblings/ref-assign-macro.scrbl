#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@title{Assignment Macros}

@doc(
  ~nonterminal:
    macro_pattern: macro ~defn

  defn.macro 'assign.macro $macro_patterns'
){

 Like @rhombus(expr.macro), but defines an infix @deftech{assignment
  operator}, similar to @rhombus(:=). An assignment operator is
 used in combination with @rhombus(mutable, ~bind)
 variables and forms like @rhombus(.) and like @brackets via
 @rhombus(#%index). The @rhombus(macro_pattern) is constrained to an
 infix-operator pattern, since the cooperating forms always expect an
 infix use. When the @rhombus(macro_pattern) would imply an
 already-parsed right-hand expression for @rhombus(macro) or
 @rhombus(expr.macro), it corresponds to a parsed expression for
 @rhombus(assign.macro), too.

 The left-hand input to the macro is not a parsed expression, but
 instead a @tech{parsed} value that encapsulates both a mutator and
 accessor for the target mutable component. The expansion of the macro
 can use another assignment operator, or it can use
 @rhombus(assign_meta.unpack_left) to extract functions for the target
 and assemble them into an expression that is packed with
 @rhombus(assign_meta.pack_assignment).

@examples(
  ~eval: macro_eval
  ~defn:
    assign.macro '$left += $right':
      ~weaker_than: ~other
      def (ref, set, name) = assign_meta.unpack_left(left)
      assign_meta.pack_assignment('block:
                                     let v = $ref() + (block:
                                                         let $name = $right
                                                         $name)
                                     $set(v)
                                     v')
  ~repl:
    def mutable x = 12
    x += 4
    x
)

}


@doc(
  fun assign_meta.unpack_left(stx :: Syntax) :: values(Syntax, Syntax, Syntax)
){

@provided_meta()

 Takes a syntax object that represents the parse left-hand side of an
 assignment operator, returning three pieces of information about the
 mutable target:

@itemlist(

 @item{an expression for a function of zero arguments that acts an
  accessor;}

 @item{an expression for a function of one argument that acts a mutator;
  and}

 @item{an identifier that reflects the target's name, which is useful
  for inferring a name for certain kinds of values (such as procedures)
  for the right-hand side.}

)

 An assignment macro defined with @rhombus(assign.macro) can uses these
 pieces to construct an expression, and then wrap the expression via
 @rhombus(assign_meta.pack_assignment) to serve as its result.

}


@doc(
  fun assign_meta.pack_assignment(group :: Syntax) :: Syntax
){

@provided_meta()

 Converts a syntax object, which can be a multi-term syntax object, into
 an @tech{parsed} term that represents an expression to implement an
 assignment operator's expansion.

 See @rhombus(assign.macro) for an example.

}

@doc(
  syntax_class assign_meta.AssignParsed(ref, set, name):
    kind: ~group
    field group
    field [tail, ...]
){

@provided_meta()

 A syntax class that matches by parsing an assignment, where the input
 starts with an assignment operator and continues as the operator expects
 (typically with a right-hand expression). The syntax-class arguments
 @rhombus(ref), @rhombus(set), and @rhombus(name) must be an expression to
 produce an accessor procedure of arguments, an expression to produce a
 mutator procedure of one argument, and an identifier to use as the
 inferred name (if needed) for the right-hand value.

 The value of a binding using @rhombus(assign_meta.AssignParsed, ~stxclass)
 is an opaque syntax object that represents the @tech{parsed}
 assignment as an expression, while the @rhombus(group) field holds a
 syntax object for the original terms that were parsed. The result also
 has a @rhombus(tail) field that contains the remaining unparsed input.

@examples(
  ~eval: macro_eval
  ~defn:
    def mutable reflist = [0]
    expr.macro 'refcar
                  $(a :: assign_meta.AssignParsed('fun () : List.first(reflist)',
                                                  'fun (v): reflist := [v]',
                                                  'refelem'))
                  $()':
      values(a, '$a.tail ...')
  ~repl:
    refcar := 11
    reflist
    refcar := fun (x): x
    reflist
)

}


@«macro.close_eval»(macro_eval)
