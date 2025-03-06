#lang rhombus/scribble/manual
@(import:
    "racket_names.rkt" open
    meta_label:
      rhombus open
      rhombus/meta:
        expose:
          expr_meta
          defn_meta
          decl_meta)

@title(~tag: "module"){Modules and Exports}

Rhombus and Racket modules exist in the same module system with the same
filesystem- and collection-based paths, so a Rhombus module can
@rhombus(import) a Racket module, and a Racket module can
@racket_require a Rhombus module.

@itemlist(

 @item{Relative-path module references and Rhombus @rhombus(file, ~impo)
 or Racket @racket_file paths work the same, where a @filepath{.rhm} or
 @filepath{.rkt} file suffix can be used in either language.}

 @item{A module name in Rhombus without quotes, such as
 @rhombusmodname(rhombus/random), implicitly gets a @filepath{.rhm}
 suffix or refers to a @filepath{main.rhm} file (when there is no
 @litchar{/}), while a similar Racket module name like
 @racketmod_racket_math implicitly gets a @filepath{.rkt} suffix or
 refers to a @filepath{main.rkt} file.

 To refer to a collection-based @filepath{.rkt} file from Rhombus, use
 the @rhombus(lib, ~impo) form, as in @rhombus(lib("racket/math.rkt")).
 To refer to a collection-based @filepath{.rhm} file from Racket, use the
 @racket_lib form as in @racketmod_rhombus_random.}

)

While importing bindings from one language into the other is relatively
straightforward, not all imported bindings can be used directly:

@itemlist(

 @item{A Racket binding can be used in a Rhombus expression if it
 corresponds to a non-macro definition or if it is an identifier macro
 that can be used alone as an expression.}

 @item{A Racket binding for a syntactic form (i.e., a macro) cannot be
 used directly in a Rhombus expression. Even though a use of the
 syntactic form in Racket looks like a function call, the Rhombus
 function call form does not (necessarily) expand to a Racket
 form with a function-call shape.

 Using a Racket syntactic form from Rhombus requires a Rhombus macro
 that expands to the Racket form. See @secref("racket-expr") for more
 information.}

 @item{A Rhombus binding can be used in a Racket expression if it
 corresponds to a @rhombus(fun, ~defn) definition or a
 @rhombus(def, ~defn) form where the binding is just an identifier or
 parenthesized identifiers @rhombus(values, ~bind). Other @rhombus(def)
 binding forms may work, but there is no guarantee, so do not rely on
 current behavior. Note that many Rhombus binding forms define
 identifiers that can be used as expressions, but they are implemented as
 syntactic forms; notably, the constructor bound by a
 @rhombus(class, ~defn) definition is not like a @rhombus(fun) binding.

 To use a Rhombus binding form Racket in general, use
 @racket_rhombus_expression from @racketmod_rhombus_parse.}


)
