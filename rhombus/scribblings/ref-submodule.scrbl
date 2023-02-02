#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title(~tag: "submodules"){Submodules}

A @deftech{submodule} is a module that is textually nested in another
(sub)module, but whose lifetime might be different than the enclosing
(sub)module.

@doc(
  decl.macro 'submodule $identifier:
                $body
                ...'
  decl.macro 'submodule $identifier ~lang $module_path:
                $body
                ...'
  decl.macro 'submodule ~early $identifier ~lang $module_path:
                $body
                ...'
  decl.macro 'submodule ~late $identifier ~lang $module_path:
                $body
                ...'
){

 Creates a submodule within the enclosing module. A submodule can be
 accessed with a module path that uses @rhombus(!, ~impo).

 When @rhombus(~lang) is not present, then the submodule's body can
 refer to bindings in the enclosing module, and the submodule implicitly
 imports the enclosing module. The enclosing module cannot directly
 import the submodule, in contrast, since that would create a import
 cycle. The same @rhombus(identifier) can be declared multiple times this
 way using @rhombus(submodule), and all of the @rhombus(body) forms for
 the same @rhombus(identifier) are combined (in the order as the appear)
 to create one submodule. The expansion of the @rhombus(body) forms is
 delayed until the enclosing module is fully expanded.

 When @rhombus(~lang) is present, the module named after @rhombus(~lang)
 supplies initial bindings for body of the submodule. In that case, the
 submodule body cannot refer directly to the bindings of the enclosing
 module, and no other @rhombus(submodule) in the enclosed module can use
 the name @rhombus(identifier).

 When @rhombus(~early) is present, or when @rhombus(~lang) is used
 without @rhombus(~late), then the submodule is defined before the rest
 of the enclosing module is expanded. The rest of the module can import
 the submodule using @rhombus(self!identifier, ~impo), for example, or
 the submodule might be used from outside the enclosing module. In the
 latter case, the enclosing module need not be instantiated to use the
 submodule.

 When @rhombus(~late) is used with @rhombus(~lang), then the submodule
 is expanded only after the enclosing module, similar to use
 @rhombus(submodule) without @rhombus(~lang). The submodule can import
 from the enclosing mosulde using @rhombus(parent, ~impo), or it can
 import from sibling submodules using a module path such as
 @rhombus(parent!identifier, ~impo).

}
