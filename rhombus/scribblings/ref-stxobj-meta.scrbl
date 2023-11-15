#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open 
    "macro.rhm")

@title(~tag: "stxobj-macro"){Syntax Objects in Macros}


@doc(
  fun syntax_meta.error(at_stx :: Syntax)
  fun syntax_meta.error(message :: ReadableString, at_stx :: Syntax)
  fun syntax_meta.error(message :: ReadableString, in_stx :: Syntax, at_stx :: Syntax)
){

@provided_meta()

 Raises a syntax-error message concerning @rhombus(at_stx). If
 @rhombus(message) is not provided, the message is @rhombus("bad syntax").
 If @rhombus(in_stx) is provided, it should be something like an enclosing
 form of @rhombus(at_stx) to provide more context.

}


@doc(
  fun syntax_meta.value(name :: Name,
                        space :: SpaceMeta = expr_meta.space,
                        fail = ....)
){

@provided_meta()

 Returns the compile-time value of @rhombus(name), if available, in the
 space specified by @rhombus(space). If no compile-time value is
 available, then @rhombus(fail) is called if it is a procedure of 0
 arguments, otherwise @rhombus(fail) is returned.

 The default @rhombus(fail) is a procedure that raises an exception.

}


@doc(
  fun syntax_meta.equal_binding(stx1 :: Name,
                                stx2 :: Name,
                                space :: SpaceMeta = expr_meta.space,
                                phase1 :: SyntaxPhase = syntax_meta.expanding_phase(),
                                phase2 :: SyntaxPhase = phase1)
    :: Boolean
){

@provided_meta()

 Checks whether @rhombus(stx1) at phase @rhombus(phase1) refers to the
 same binding as @rhombus(stx2) at @rhombus(phase2) within the space
 reflected by @rhombus(space).

}

@doc(
  fun syntax_meta.equal_name_and_scopes(stx1 :: Name,
                                        stx2 :: Name,
                                        phase :: SyntaxPhase
                                          = syntax_meta.expanding_phase())
    :: Boolean
){

@provided_meta()

 Checks whether @rhombus(stx1) and @rhombus(stx) have the same name (as
 returned by @rhombus(Syntax.unwrap) on an identifier, for example) and
 the same scopes at @rhombus(phase).

 When @rhombus(stx1) or @rhombus(stx1) is a dotted name, then both must
 be dotted names where each dotted component has the same name and
 scopes.

}


@doc(
  fun syntax_meta.flip_introduce(stx :: Syntax) :: Syntax
){

@provided_meta()

 Returns a syntax object like @rhombus(stx), but where scopes indicating
 that the syntax object is macro-introduced are flipped. The result is
 @rhombus(stx) unmodified when not during the expansion of a macro.

 Macro-introduction is detected by flipping scopes to the input of a
 macro, then flipping scopes on the macro's result, so that the two flip
 operations cancel for any part of the macro's input that is used in the
 macro's output.

 A macro can flip introduction to implement a non-hygienic expansion for
 an introduced identifier. Flipping introduction may also be helpful when
 syntax is preserved through side channels, or in unusual cases when
 checking for the originaless of a term with
 @rhombus(Syntax.is_original).

}

@doc(
  fun syntax_meta.is_static(stx :: Operator || Identifier) :: Boolean
){

 Check whether the identifier @rhombus(#%dynamism) using the scopes of
 @rhombus(stx) is bound to indicate static mode. See @rhombus(use_static)
 for more information.

}


@doc(
  annot.macro 'SyntaxPhase'
){

@provided_meta()

 Matches an integer or @rhombus(#false).

 A phase-level integer corresponds to a phase of evaluation, especially
 relative to the main body of a module. Phase level @rhombus(0)
 corresponds to a module's run time, @rhombus(1) corresponds to expansion
 time for run-time forms, and so on. A phase level of @rhombus(#false)
 corresponds to the label phase, like @rhombus(meta_label, ~impo).

}


@doc(
  fun syntax_meta.expanding_phase() :: SyntaxPhase
){

@provided_meta()

 Returns the phase of expression forms currently being expanded, or
 @rhombus(0) if no expansion is in progress.

}

@doc(
  unquote_bind.macro '«bound_as $space_expr: '$id_or_op'»'
){

@provided_meta()

 Unquote binding operator for use with @rhombus($, ~bind). It matches a
 syntax object for an identifier or operator, where the identifier or
 operator's binding is the same as @rhombus(id_or_op) in
 the @rhombus(SpaceMeta, ~annot) reflected by @rhombus(space) (e.g.,
 @rhombus(expr_meta.space)).

@examples(
  ~defn:
    import:
      rhombus/meta open
      rhombus:
        rename: + as plus
        expose: plus
  ~defn:
    expr.macro 'simplify($stx)':
      def new_stx:
        match stx
        | '$(a :: Int) $(bound_as expr_meta.space: '+') $(b :: Int)':
            '$(a.unwrap() + b.unwrap())'
        | ~else:
            stx
      'Syntax.literal($new_stx)'
  ~repl:
    simplify(1 + 2)
    simplify(1 plus 2)
    simplify(1 * 2)
)

}
