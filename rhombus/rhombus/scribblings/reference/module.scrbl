#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "submodules"){Modules and Submodules}

A @deftech{module} is normally written as its own file starting with
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus))) or similar. In an
interactive context, the @rhombus(module) form can declare a module; in
that case, the module declared as @nontermref(id) can be referenced using
@rhombus(self!, ~impo)@nontermref(id).

A @deftech{submodule} is a module that is textually nested in another
(sub)module using the @rhombus(module) form. Its lifetime might be
different than the enclosing (sub)module.

@doc(
  ~nonterminal:
    module_path: import ~defn

  decl.macro 'module $id:
                $body
                ...'
  decl.macro 'module ~splice $id:
                $body
                ...'
  decl.macro 'module $id ~lang $module_path:
                $body
                ...'
  decl.macro 'module ~early $id ~lang $module_path:
                $body
                ...'
  decl.macro 'module ~late $id ~lang $module_path:
                $body
                ...'
  decl.macro 'module ~splice $id ~lang $module_path:
                $body
                ...'
){

 Creates a @tech{submodule} within the enclosing module, or declares a
 module interactively in an interactive context (such as a
 read-eval-print loop). The module can be accessed with a module path
 that uses @rhombus(!, ~impo).

 A @rhombus(~lang) is required to declare a module interactively, and
 @rhombus(~late) or @rhombus(~splice) is not allowed interactively.

 When @rhombus(~lang) is not present, then the submodule's body can
 refer to bindings in the enclosing module, and the submodule implicitly
 imports the enclosing module. The enclosing module cannot directly
 import the submodule, in contrast, since that would create a import
 cycle. The same @rhombus(id) can be declared multiple times this
 way using @rhombus(module), and all of the @rhombus(body) forms for
 the same @rhombus(id) are combined (in the order as the appear)
 to create one submodule. The expansion of the @rhombus(body) forms is
 delayed until the enclosing module is fully expanded.

 When @rhombus(~lang) is present, the module named after @rhombus(~lang)
 supplies initial bindings for body of the module. In that case, a
 submodule body cannot refer directly to the bindings of the enclosing
 module, and no other @rhombus(module) in the enclosing module can use
 the name @rhombus(id).

 When @rhombus(~early) is present, or when @rhombus(~lang) is used
 without @rhombus(~late) or @rhombus(~splice),
 then the submodule is defined before the rest
 of the enclosing module is expanded. The rest of the module can import
 the submodule using @rhombus(self!, ~impo)@rhombus(id), for example, or
 the submodule might be used from outside the enclosing module. In the
 latter case, the enclosing module need not be instantiated to use the
 submodule.

 When @rhombus(~late) or @rhombus(~splice) is used with @rhombus(~lang)
 for a submodule, then the submodule
 is expanded only after the enclosing module, similar to use
 @rhombus(module) without @rhombus(~lang). The submodule can import
 from the enclosing module using @rhombus(parent, ~impo), or it can
 import from sibling submodules using a module path such as
 @rhombus(parent!, ~impo)@rhombus(id).

 Using @rhombus(~splice) without @rhombus(~lang) has no effect, but
 using it with @rhombus(~lang) means that multiple module definitions are
 combined, like when not using @rhombus(~lang). When both
 @rhombus(~splice) and @rhombus(~lang) are used for a submodule name,
 then every declaration for that name must use @rhombus(~splice) and
 @rhombus(~lang) with the same @rhombus(module_path) after
 @rhombus(~lang).

 The @rhombus(body) sequence in a module is implicitly wrapped as a
 @rhombus(#%module_block, ~datum) form, which allows a module that is
 used as another module's language to customize the treatment of the
 other module's content. The wrapper is on the outside of any
 @rhombus(import, ~defn) forms that appear in the module body, so
 @rhombus(#%module_block, ~datum) is useful only as exported by a module
 that is used as the language for another module. The
 @rhombus(#%module_block, ~decl) form exported by
 @rhombuslangname(rhombus) adds submodules.

}

@doc(
  ~nonterminal:
    module_path: import ~defn

  decl.macro 'pragma $decl'
  decl.macro 'pragma:
                $decl ...
                ...'

  grammar decl:
    ~unsafe
    ~empty_evaluator
){

 Controls properties of a module's compilation:

@itemlist(

 @item{@as_indexed{@rhombus(~unsafe)} compiles the module in unsafe mode,
  where annotation failures trigger unspecified behavior.}

 @item{@as_indexed{@rhombus(~empty_evaluator)} disables the use of the
  module's content for interactive evaluation, which can avoid overhead
  for the module.}

)

}

@doc(
  decl.macro '#%module_block:
                $body
                ...'
){

 A @rhombus(#%module_block, ~datum) form is implicitly added around the
 body of a module. A module that implements a language can export its own
 @rhombus(#%module_block, ~datum) to customize the treatment of a body
 sequence in any module that uses the exporting module as its language.

 The @rhombus(#%module_block, ~decl) form from @rhombuslangname(rhombus)
 mostly expands its @rhombus(body) sequence to serve as the body of the
 enclosing module, but it also performs additional adjustments to the
 module body:

@itemlist(

 @item{For each module-body form that is an expression (as opposed to a
  @tech{definition} or @tech{declaration}), @rhombus(#%module_block, ~decl)
  wraps the expression so that its result values are each printed using
  @rhombus(Evaluator.current_print). Expressions in places where a
  @tech{nestable declaration} is allowed, such
  as the body of a @rhombus(namespace), as similarly wrapped for
  printing---but not expressions in other definition contexts, such as in
  a @rhombus(fun) or @rhombus(block) body.}

 @item{The @rhombus(#%module_block, ~decl) form adds submodule declarations depending on
 ones that are already immediately declared in the @rhombus(body)
 sequence (see @secref(~doc: guide_doc, "configure") for more information):

@itemlist(

 @item{If a @rhombus(configure_runtime, ~datum) submodule is declared,
  it is instantiated when the enclosing module is used as the main
  module for a program.

  The @rhombus(#%module_block, ~decl) form always adds a
  @rhombus(#{configure-runtime}, ~datum) submodule, which is the
  configuration submodule name that is recognized at the Racket level.
  When @rhombus(configure_runtime, ~datum) is declared, then the added
  @rhombus(#{configure-runtime}, ~datum) submodule depends on the declared
  submodule as is otherwise empty. Otherwise, the added
  @rhombus(#{configure-runtime}, ~datum) submodule performs suitable
  configuration for Rhombus run-time behavior (e.g., a specific formatting
  for error messages).}

 @item{If a @rhombus(reader, ~datum) submodule is declared, it provides
  a parser and IDE configuration that applies when the enclosing module is
  used as a language via @hash_lang(). A @rhombus(reader, ~datum)
  submodule is never added automatically.

  If a @rhombus(configure_expand, ~datum) submodule is not also declared
  when a @rhombus(reader, ~datum) submodule is declared,
  then a @rhombus(#{configure-expand}, ~datum) submodule is added to
  configure expansion-time behavior (e.g., a specific formatting of syntax
  errors).

  If a @rhombus(configure_expand, ~datum) submodule is declared, then it
  is expected to export @rhombus(enter_parameterization, ~datum) and
  @rhombus(exit_parameterization, ~datum), and those bindings are
  reexported by an added @rhombus(#{configure-expand}, ~datum) submodule
  as @rhombus(#{enter-parameterization}, ~datum) and
  @rhombus(#{exit-parameterization}, ~datum).}

)}

)

 The @rhombus(module_block) form is ilke @rhombus(#%module_block), but
 it supports configuration of the extra behaviors by @rhombus(#%module_block).

}

@doc(
  decl.macro 'module_block:
                $option
                ...
                $body
                ...'

  grammar option:
    ~effect $effect_id
    ~effect: $effect_id
    ~no_added_submodules
){

 Like @rhombus(#%module_block), but with options that affect how the
 module body is treated.

 If an @rhombus(~effect) option provides an @rhombus(effect_id), then
 @rhombus(effect_id) is prefixed on every form where a @tech{nestable declaration}
 is allowed by the form is not a @tech{definition} or @tech{declaration}. The binding of
 @rhombus(effect_id) itself does not need to be an expression form. If no
 @rhombus(effect_id) is provided, the default causes expression results
 to be printed as by @rhombus(#%module_block).

 If the @rhombus(~no_added_modules) is provided, then no submodules are
 automatically declared. Otherwise, submodules are potentially added to
 the module body in the same way as by @rhombus(#%module_block).

}


@doc(
  meta.bridge #{#%module-begin}
){

 Not for direct use, but exported from @rhombuslangname(rhombus) as part
 of the protocol for a language. See
 @secref(~doc: guide_doc, "tilde-lang") for more information.

}
