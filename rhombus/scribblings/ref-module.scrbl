#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "submodules"){Modules and Submodules}

A @deftech{module} is normally written as its own file starting with
@rhombus(#,(@hash_lang()) #,(@rhombusmodname(rhombus))) or similar. In an
interactive context, the @rhombus(module) form can declare a module; in
that case, the module decared as @rhombus(id) can be referenced using
@rhombus(self!, ~impo)@rhombus(id).

A @deftech{submodule} is a module that is textually nested in another
(sub)module using the @rhombus(module) form. Its lifetime might be
different than the enclosing (sub)module.

@doc(
  ~nonterminal:
    module_path: import

  decl.macro 'module $id:
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
){

 Creates a @tech{submodule} within the enclosing module, or declares a
 module interactively in an interactive context (such as a
 read-eval-print loop). The module can be accessed with a module path
 that uses @rhombus(!, ~impo).

 A @rhombus(~lang) is required to declare a module interactively, and
 @rhombus(~late) is not allowed interactively.

 When @rhombus(~lang) is not present, then the submodule's body can
 refer to bindings in the enclosing module, and the submodule implicitly
 imports the enclosing module. The enclosing module cannot directly
 import the submodule, in contrast, since that would create a import
 cycle. The same @rhombus(id) can be declared multiple times this
 way using @rhombus(submodule), and all of the @rhombus(body) forms for
 the same @rhombus(id) are combined (in the order as the appear)
 to create one submodule. The expansion of the @rhombus(body) forms is
 delayed until the enclosing module is fully expanded.

 When @rhombus(~lang) is present, the module named after @rhombus(~lang)
 supplies initial bindings for body of the module. In that case, a
 submodule body cannot refer directly to the bindings of the enclosing
 module, and no other @rhombus(module) in the enclosing module can use
 the name @rhombus(id).

 When @rhombus(~early) is present, or when @rhombus(~lang) is used
 without @rhombus(~late), then the submodule is defined before the rest
 of the enclosing module is expanded. The rest of the module can import
 the submodule using @rhombus(self!, ~impo)@rhombus(id), for example, or
 the submodule might be used from outside the enclosing module. In the
 latter case, the enclosing module need not be instantiated to use the
 submodule.

 When @rhombus(~late) is used with @rhombus(~lang) for a submodule, then the submodule
 is expanded only after the enclosing module, similar to use
 @rhombus(module) without @rhombus(~lang). The submodule can import
 from the enclosing mosulde using @rhombus(parent, ~impo), or it can
 import from sibling submodules using a module path such as
 @rhombus(parent!, ~impo)@rhombus(id).

}
