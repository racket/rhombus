#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@title(~tag: "stxobj-macro"){Syntax Objects in Macros}


@doc(
  ~meta
  fun syntax_meta.error(~who: who :: maybe(error.Who) = #false,
                        in_stx :: Syntax)
    :: None
  fun syntax_meta.error(~who: who :: maybe(error.Who) = #false,
                        message :: ReadableString,
                        in_stx :: Syntax)
    :: None
  fun syntax_meta.error(~who: who :: maybe(error.Who) = #false,
                        message :: ReadableString,
                        in_stx :: Syntax,
                        at_stx :: Syntax || List.of(Syntax))
    :: None
){

 Throws a syntax-error message concerning @rhombus(in_stx). If
 @rhombus(who) is @rhombus(#false), it is inferred from @rhombus(in_stx). If
 @rhombus(message) is not provided, the message is @rhombus("bad syntax").
 If @rhombus(at_stx) is provided, it should be something like an enclosed
 form of @rhombus(in_stx) to provide extra context. Alternatively,
 @rhombus(at_stx) can be a list of syntax objects, in which case all
 syntax objects provide extra contexts, least-specific to
 most-specific.

}


@doc(
  ~meta
  fun syntax_meta.value(name :: Name,
                        in_space :: SpaceMeta = expr_meta.space,
                        fail :: Any:
                          fun (): throw Exn.Fail.Contract(....))
    :: Any
){

 Returns the compile-time value of @rhombus(name), if available, in the
 space specified by @rhombus(in_space). If no compile-time value is
 available, then @rhombus(fail) is called if it is a function of 0
 arguments, otherwise @rhombus(fail) is returned.

 The default @rhombus(fail) is a function that throws an exception.

}


@doc(
  ~meta
  fun syntax_meta.equal_binding(
    stx1 :: Name,
    stx2 :: Name,
    in_space :: SpaceMeta = expr_meta.space,
    phase1 :: SyntaxPhase = syntax_meta.expanding_phase(),
    phase2 :: SyntaxPhase = phase1
  ) :: Boolean
){

 Checks whether @rhombus(stx1) at phase @rhombus(phase1) refers to the
 same binding as @rhombus(stx2) at @rhombus(phase2) within the space
 reflected by @rhombus(in_space).

}

@doc(
  ~meta
  fun syntax_meta.equal_name_and_scopes(
    stx1 :: Name,
    stx2 :: Name,
    phase :: SyntaxPhase = syntax_meta.expanding_phase()
  ) :: Boolean
){

 Checks whether @rhombus(stx1) and @rhombus(stx2) have the same name (as
 returned by @rhombus(Syntax.unwrap) on an identifier, for example) and
 the same scopes at @rhombus(phase).

 When @rhombus(stx1) or @rhombus(stx2) is a dotted name, then both must
 be dotted names where each dotted component has the same name and
 scopes.

}


@doc(
  ~meta
  fun syntax_meta.binding_symbol(
    stx :: Name,
    in_space :: SpaceMeta = expr_meta.space,
    phase :: SyntaxPhase = syntax_meta.expanding_phase()
  ) :: Symbol
){

 Returns a symbol that corresponds to the binding of @rhombus(stx) in
 space @rhombus(in_space) and at phase @rhombus(phase). The result does
 not uniquely identify a binding, but two names that refer to the same
 binding will have the same symbol result.

}



@doc(
  ~meta
  fun syntax_meta.flip_introduce(stx :: Syntax) :: Syntax
){

 Returns a syntax object like @rhombus(stx), but where scopes indicating
 that the syntax object is macro-introduced are flipped. The result is
 @rhombus(stx) unmodified when not during the expansion of a macro.

 Macro-introduction is detected by flipping scopes to the input of a
 macro, then flipping scopes on the macro's result, so that the two flip
 operations cancel for any part of the macro's input that is used in the
 macro's output.

 A macro can flip introduction to implement a non-hygienic expansion
 for an introduced identifier, but generally @rhombus(Syntax.make_id)
 is more reliable for this purpose, because there can be extra scopes
 that differentiate macro-definition and use sites. Flipping
 introduction may also be helpful when a syntax object from the
 macro's input is preserved through side channels, so that the added
 scope is canceled, or in unusual cases when checking for the
 originalness of a term with @rhombus(Syntax.is_original).

}

@doc(
  ~meta
  fun syntax_meta.lift_expr_to_before(expr_stx :: Group)
    :: Identifier
  fun syntax_meta.can_lift_expr_to_before() :: Boolean
){

 The @rhombus(syntax_meta.lift_expr_to_before) function adjusts the current
 expansion so that @rhombus(expr_stx) is added to a top-level definition
 before the current expression. Expansion of the new expression is forced
 immediately, and the result is an identifier that is generated for the
 definition and that will be bound to the value produced by the
 expression.

 The @rhombus(syntax_meta.lift_expr_to_before) function can be called only
 during expansion. The @rhombus(syntax_meta.can_lift_expr_to_before) function
 reports whether an expansion is currently in process so that
 @rhombus(syntax_meta.lift_expr_to_before) can be used.

}

@doc(
  ~meta
  fun syntax_meta.lift_expr_to_module_end(expr_stx :: Group)
    :: Void
  fun syntax_meta.can_lift_expr_to_module_end() :: Boolean
){

 The @rhombus(syntax_meta.lift_expr_to_module_end) function adjusts the
 current expansion so that @rhombus(expr_stx) is added as an expression
 to the end of the module being expanded. The expression is @emph{not}
 immediately expanded, and it will be expanded when the expansion process
 reaches the lifted expression at the (current) end of the module.

 The @rhombus(syntax_meta.lift_expr_to_module_end) function can be called
 only during expansion of a module. The
 @rhombus(syntax_meta.can_lift_expr_to_module_end) function reports whether a
 module expansion is currently in process so that
 @rhombus(syntax_meta.lift_expr_to_module_end) can be used.

}

@doc(
  ~meta
  annot.macro 'syntax_meta.DefinitionContext'
  fun syntax_meta.make_definition_context(
    parent :: maybe(syntax_meta.DefinitionContext) = #false
  ) :: syntax_meta.DefinitionContext
  method (def_ctx :: syntax_meta.DefinitionContext).add_definitions(
    defns :: Syntax
  ) :: Void
  method (def_ctx :: syntax_meta.DefinitionContext).add_scopes(
    defns :: Syntax
  ) :: Syntax
  method (def_ctx :: syntax_meta.DefinitionContext).track_origin(
    stx :: Term
  ) :: Syntax
  method (def_ctx :: syntax_meta.DefinitionContext).call_using(
    thunk :: Function.of_arity(0)
  )
  method (def_ctx :: syntax_meta.DefinitionContext)
    .call_to_expand_using(
      stx :: Syntax,
      proc :: Syntax -> Syntax
  ) :: S7yntax
){

 The @rhombus(syntax_meta.make_definition_context) function creates a
 @deftech{definition context} to hold expansion-time definitions:

@itemlist(

 @item{The @rhombus(syntax_meta.DefinitionContext.add_definitions)
  method accepts a syntax object with definition forms, and it adds those
  definitions to the context.

  Only definitions that bind expansion-time values are allowed, such as
  @rhombus(macro), @rhombus(expr.macro), and @rhombus(meta.bridge).
  Definitions for run-time values, such as @rhombus(def) or @rhombus(let),
  are not allowed.}

 @item{The @rhombus(syntax_meta.DefinitionContext.add_scopes) method
  accepts a syntax object and adds the definition context's scopes to the
  object, returning the new syntax object with those scopes added.}

 @item{The @rhombus(syntax_meta.DefinitionContext.track_origin) method
  transfers gathered @rhombus(Syntax.track_origin)-like expansion
  information to the given syntax object, where information is gathered
  during each @rhombus(syntax_meta.DefinitionContext.add_definitions)
  call.}

 @item{The @rhombus(syntax_meta.DefinitionContext.call_using) method
  makes the definition context active for expansion while calling
  @rhombus(thunk). Making the definition context active includes changing
  the effect of @rhombus(syntax_meta.flip_introduce) to reflect a fresh
  expansion context. Consider using
  @rhombus(syntax_meta.DefinitionContext.call_to_expand_using), instead.}

 @item{The @rhombus(syntax_meta.DefinitionContext.call_to_expand_using)
  method is like @rhombus(syntax_meta.DefinitionContext.call_using), but
  it takes a syntax object to flip out of the current expansion context
  and into the fresh expansion context (in the sense of
  @rhombus(syntax_meta.flip_introduce)), and it also adds the internal
  definition's scopes like
  @rhombus(syntax_meta.DefinitionContext.add_scopes). The resulting syntax
  object is passed to @rhombus(proc), which must return a new syntax
  object; that result is flipped out of the fresh scope and back into the
  enclosing scope, and
  @rhombus(syntax_meta.DefinitionContext.track_origin) is also applied to
  it. The final result syntax object is the result of
  @rhombus(syntax_meta.DefinitionContext.call_to_expand_using).}

)

}

@doc(
  ~meta
  fun syntax_meta.is_static(stx :: Name) :: Boolean
){

 Check whether the identifier @rhombus(#%dynamism) using the scopes of
 @rhombus(stx) is bound to indicate static mode. See @rhombus(use_static)
 for more information.

}


@doc(
  ~meta
  fun syntax_meta.dynamic_name(
    name :: Name,
    ~space: space :: SpaceMeta = expr_meta.space,
    ~as_static: as_static = #false
  ) :: Identifier
){

 Returns an identifier that has the same binding as @rhombus(name) in
 @rhombus(space), and it also has a binding for @rhombus(#%dynamism) to
 indicate dynamic or static mode, depending on @rhombus(as_static).

 See @rhombus(use_static) for more information about dynamic versus
 static mode.

}


@doc(
  ~meta
  annot.macro 'SyntaxPhase'
){

 Matches an integer or @rhombus(#false).

 A phase-level integer corresponds to a phase of evaluation, especially
 relative to the main body of a module. Phase level @rhombus(0)
 corresponds to a module's run time, @rhombus(1) corresponds to expansion
 time for run-time forms, and so on. A phase level of @rhombus(#false)
 corresponds to the label phase, like @rhombus(meta_label, ~impo).

}


@doc(
  ~meta
  fun syntax_meta.expanding_phase() :: SyntaxPhase
){

 Returns the phase of expression forms currently being expanded, or
 @rhombus(0) if no expansion is in progress.

}

@doc(
  ~meta
  ~nonterminal:
    space_expr: block expr
  unquote_bind.macro '«bound_as $space_expr: '$op_or_id_name'»'
){

 Unquote binding operator for use with @rhombus($, ~bind). It matches a
 syntax object for an identifier or operator, where the identifier or
 operator's binding is the same as @rhombus(op_or_id_name) in
 the @rhombus(SpaceMeta, ~annot) reflected by @rhombus(space_expr) (e.g.,
 @rhombus(expr_meta.space)).

@examples(
  ~defn:
    import:
      rhombus/meta open
      rhombus:
        rename + as plus
        expose plus
  ~defn:
    expr.macro 'simplify($stx)':
      let new_stx:
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
