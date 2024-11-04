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

 @item{@as_index{@rhombus(~unsafe)} compiles the module in unsafe mode,
  where annotation failures trigger unspecified behavior.}

 @item{@as_index{@rhombus(~empty_evaluator)} disables the use of the
  module's content for interactive evaluation, which can avoid overhead
  for the module.}

)

}
