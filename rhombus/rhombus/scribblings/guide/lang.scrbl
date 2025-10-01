#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "lang"){Defining Languages}

Rhombus not only supports macro extensions that add to the
@rhombuslangname(rhombus) language, it supports entirely new languages
that are smaller than @rhombuslangname(rhombus) or that have different
syntax and semantics. Languages are themselves implemented as Rhombus
modules that follow a particular protocol---exporting certain bindings
and implementing certain submodules.

The term @deftech{language} in Rhombus is used to refer to two different
kinds of languages for two different contexts:

@itemlist(

 @item{A language whose name is written after @hash_lang() for a module:
 This kind of language has full control over the parsing of the module's
 body at the level of characters and bytes.}

 @item{A language whose name is written after @rhombus(~lang) for a
 @rhombus(module, ~decl) form: This kind of language receives its
 module's body already parsed at the character level into a
 @tech(~doc: ref_doc){syntax object}. That parsing is performed by the
 enclosing module's language.}

)

These two kinds of languages are connected, because the result of a
@hash_lang()-triggered parser is a @rhombus(module, ~decl) form
(although at the Racket level), and so it includes a @rhombus(~lang)
reference (or the equivalent at the Racket level). Furthermore,
modules that implement a language are typically set up so that the
language name works in both contexts, and the @hash_lang() use of the name
generates a reference to the @rhombus(~lang) form of the name. For
example, @rhombuslangname(rhombus) works both after @hash_lang() and in
@rhombus(module, ~decl) after @rhombus(~lang), and both uses of the name
refer to the same set of bindings.

@// ------------------------------------------------------------
@section(~tag: "tilde-lang"){Module @rhombus(~lang) Protocol}

A module that is intended to be used as a language selected by
@rhombus(~lang) in @rhombus(module, ~decl) must export various bindings
to work:

@itemlist(

 @item{@rhombus(#{#%module-begin}, ~datum): A Racket-level bridge to
 handle the module body. In a Rhombus-implemented language, this should
 normally be @rhombus(#{#%module-begin}) from @rhombuslangname(rhombus).
 Using that @rhombus(#{#%module-begin}) obliges the module to also export
 @rhombus(#%module_body, ~datum).}

 @item{@rhombus(#%module_block, ~datum): A Rhombus declaration form that
 is implicitly wrapped around a module's body by
 @rhombus(#{#%module-begin}). The module body is received as a block.
 Exporting @rhombus(#%module_block, ~decl) from @rhombuslangname(rhombus)
 causes the module body to be treated the same as a sequence of
 declarations, definitions, and expressions in a
 @rhombuslangname(rhombus) module.}

 @item{@rhombus(#{#%top-interaction}, ~datum): A Racket-level bridge to
 handle forms evaluated in a read-eval-print loop (REPL). REPL evaluation
 is not mandatory, and if this binding is missing, then interactive
 evaluation is disabled. In a Rhombus-implemented language, this should
 normally be @rhombus(#{#%top-interaction}) from
 @rhombuslangname(rhombus). Using that @rhombus(#{#%top-interaction})
 obliges the module to also export @rhombus(#%interaction, ~datum) (or
 else interactive evaluation will still be disabled).}

 @item{@rhombus(#%interaction, ~datum): A Rhombus declaration form that
 is implicitly wrapped around interactive evaluation by
 @rhombus(#{#%top-interaction}). A form sequence o evaluate is received
 as a block. Exporting @rhombus(#%interaction, ~decl) from
 @rhombuslangname(rhombus) allows a REPL to work in the same way as for
 @rhombuslangname(rhombus) module.}

 @item{Other bindings as needed by the language, especially common forms
 like @rhombus(def, ~defn) and @rhombus(fun) and
 @seclink(~doc: ref_doc, "implicit"){implicit forms} like
 @rhombus(#%call), @rhombus(#%parens), and , @rhombus(#%literal). These
 bindings, too, are often reexported from @rhombuslangname(rhombus).}

)

For example, the following module defines a language that is like
@rhombuslangname(rhombus), but it replaces @rhombus(#%module_body) to
first print out the source of all forms in the module body. After
printing, the body forms are evaluated the same way as in
@rhombuslangname(rhombus).

@filebox(
  "noisy_rhombus.rhm",
  @rhombusblock(
    #,(@hash_lang())  #,(@rhombuslangname(rhombus))
    import:
      rhombus/meta open

    export:
      all_from(rhombus):
        except #%module_block
      rename:
        module_block as #%module_block

    decl.macro 'module_block: $form; ...':
      '#%module_block:
         println($(form.to_source_string()))
         ...
         $form
         ...'
  )
)

If that module is saved as @filepath{noisy_rhombus.rhm}, then a module
in the same directory can refer to it when declaring a
@rhombus(main, ~datum) submodule:

@filebox(
  "demo.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    module main ~lang "noisy_rhombus.rhm":
      1 + 2 // prints "1 + 2" and then "3"
  )
)

The name @filepath{noisy_rhombus.rhm} does not conform to the syntax of
languages that can be written after @hash_lang(), and the
@filepath{noisy_rhombus.rhm} module also doesn't supply a
character-level parser. One way to fill that gap, at least in the short
term, is to use the @rhombuslangname(shrubbery) language, which parses a
module body into shrubbery form and then uses the language module that
is named immediately after
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(shrubbery))):

@filebox(
  "demo2.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(shrubbery)) "noisy_rhombus.rhm"
    1 + 2 // prints "1 + 2" and then "3"
  )
)

@// ------------------------------------------------------------
@section(~tag: "configure"){Run-Time and Expand-Time Configuration}

Although bindings can capture most details of a language definition,
certain aspects of the compile-time and run-time environment span all
languages that are used to construct a program, and so they must be
configured in a different way. For example, the way that values should
print may differ for a programmer who is working in terms of Rhombus
versus one working in terms of Racket, even when printing is initiated
by a library that is meant to be used from either language. Racket
allows the main module for a program (e.g., the one provided on the
command line) to configure run-time behavior, and it allows the language
of a module being compiled to configure compile-time behavior. These
configurations take the form of submodules:

@itemlist(

 @item{A @rhombus(configure_runtime, ~datum) submodule is instantated
 before its enclosing module when then enclosing module is the main
 module of a program. Instantiating the submodule is intended to have
 side effects that configure the environment.

 More precisely, a @rhombus(#{configure-runtime}, ~datum) submodule is
 instantiated, because that is the Racket-level protocol, but the
 @rhombus(#%module_block, ~decl) form of @rhombuslangname(rhombus)
 arranges for a @rhombus(#{configure-runtime}, ~datum) submodule that
 depends on @rhombus(configure_runtime, ~datum).

 A @rhombus(configure_runtime, ~datum) submodule is relevant to any
 Rhombus module, not just a language module. The
 @rhombus(#%module_block, ~decl) form of @rhombuslangname(rhombus) not
 only adds @rhombus(#{configure-runtime}, ~datum) to trigger
 @rhombus(configure_runtime, ~datum), it adds a
 @rhombus(configure_runtime, ~datum) submodule if one is not explicitly
 declared in a module body. The automatic
 @rhombus(configure_runtime, ~datum) submodule depends on
 @rhombusmodname(rhombus/runtime_config), which configures the
 environment for working in Rhombus terms.

 A language's @rhombus(configure_runtime, ~datum) submodule is relevant
 when the language is selected for interactive evaluation in a REPL
 context, since the language module counts as the main module in that
 case.}

 @item{A @rhombus(configure_expand, ~datum) submodule provides
 @rhombus(enter_parameterization) and @rhombus(exit_parameterization)
 functions that are used to configure the expand-time environment while a
 module using the language is expanded. Instead of a direct side effect,
 @rhombus(enter_parameterization) returns a
 @tech(~doc: model_doc){parameterization} that is used while the module
 is being compiled, and @rhombus(exit_parameterization) is called to
 obtain a more nested parameterization to use when compilation is a
 dependency is triggered.

 More precisely, a @rhombus(#{configure-expand}, ~datum) submodule is
 instantiated, because that is the Racket-level protocol, but the
 @rhombus(#%module_block, ~decl) form of @rhombuslangname(rhombus)
 arranges for a @rhombus(#{configure-expand}, ~datum) submodule that
 uses @rhombus(configure_expand, ~datum) when the latter is present.

 If a @rhombus(configure_expand, ~datum) submodule is not explicitly
 declared in a module body, the @rhombus(#%module_block, ~decl) form of
 @rhombuslangname(rhombus) @emph{does not} add one automatically---unless
 a @rhombus(reader, ~datum) submodule (described in @secref("hash-lang"))
 is present. If a @rhombus(reader, ~datum) is present and not
 @rhombus(configure_expand, ~datum), then @rhombus(#%module_block, ~decl)
 adds a @rhombus(configure_expand, ~datum) submodule that uses
 @rhombusmodname(rhombus/expand_config).}


)

@// ------------------------------------------------------------
@section(~tag: "hash-lang"){@hash_lang() Language Protocol}

A language name that follows @hash_lang() must have only alphanumeric
ASCII, @litchar{+}, @litchar{-}, @litchar{_}, and/or @litchar{/}
characters terminated by whitespace or an end-of-file. Thus, a language
name cannot be a Rhombus string, but must instead be an unquoted module
path that refers to a module in a collection.

Furthermore, the unquoted path is turned into a module path in a way
that is different from a language name after @rhombus(~lang) in
@rhombus(module, ~decl) or in an @rhombus(import, ~defn) form: a
@filepath{.rkt} suffix is added instead of a @filepath{.rhm} suffix
(after @filepath{/main} is added in the case that @litchar{/} does not
appear in the path). Finally, a @rhombus(reader, ~datum) submodule is
found within that module.
@//
@margin_note{As a fallback, when a @rhombus(reader, ~datum) submodule is
 not found, a @filepath{.rkt} suffix is replaced with
 @filepath{/lang/reader.rkt} and tried as a module path in place of a
 @rhombus(reader, ~datum) submodule. This fallback is discouraged for new
 Rhombus and Racket languages.}

The @rhombus(reader, ~datum) submodule protocol, which is defined at the
Racket level, requires the submodule to export three functions:
@rhombus(#{read}, ~datum), @rhombus(#{read-syntax}, ~datum), and
@rhombus(#{get-info}, ~datum). The Rhombus-based language
@rhombuslangname(rhombus/reader) provides a streamlined interface that
is convenient for defining Rhombus-like languages.

The key clause in a @rhombuslangname(rhombus/reader) module is
@rhombus(~lang) followed by module path for the @rhombus(~lang)-protocol
module to use for the parsed module. The module can can be relative to
the enclosing @rhombus(reader, ~datum) submodule, so
@rhombus(parent, ~impo) serves as a reference to the enclosing module.
The following example is the same as @filepath("moisy_rhombus.rhm") in
@rhombus("tilde-lang"), but with a @rhombus(reader, ~datum) submodule
added, and saved as @filepath{main.rkt} in a @filepath{noisy_rhombus}
directory (note the @filepath{.rkt} extension instead of
@filepath{.rhm}).

@filebox(
  "noisy_rhombus/main.rkt",
  @rhombusblock(
    #,(@hash_lang())  #,(@rhombuslangname(rhombus))
    import:
      rhombus/meta open

    module reader ~lang rhombus/reader:
      ~lang parent

    export:
      all_from(rhombus):
        except #%module_block
      rename:
        module_block as #%module_block

    decl.macro 'module_block: $form; ...':
      '#%module_block:
         println($(form.to_source_string()))
         ...
         $form
         ...'
  )
)

Assuming that @filepath{noisy_rhombus} has been registered as a
collection (possibly by installing it as a package with @exec{raco pkg
 install noisy_rhombus/}), then @rhombus(noisy_rhombus, ~datum) works as
a language name immediately after @hash_lang():

@filebox(
  "demo3.rhm",
  @rhombusblock(
    #,(@hash_lang()) noisy_rhombus
    1 + 2 // prints "1 + 2" and then "3"
  )
)

A small problem remains here, created by the mismatch between
@hash_lang()'s interpretation of module names and the Rhombus
@rhombus(import) interpretation. The @hash_lang() interpretation of
@rhombus(noisy_rhombus, ~datum) is
@rhombus(lib("noisy_rhombus/main.rkt"), ~impo), while the
@rhombus(import) interpretation is
@rhombus(lib("noisy_rhombus/main.rhm"), ~impo). Consequently, these
following @rhombus(all_from, ~expo) does not work as would be expected:

@filebox(
  "demo4.rhm",
  @rhombusblock(
    #,(@hash_lang()) noisy_rhombus
    export:
      all_from(noisy_rhombus) // no `lib("noisy_rhombus/main.rhm")`
  )
)

In fact, the problem is not so much the @hash_lang() interpretation of
@rhombus(noisy_rhombus, ~datum) as the use of @rhombus(parent, ~impo)
in the @rhombus(reader, ~datum) module. Changing to

@rhombusblock(
  module reader ~lang rhombus/reader:
    ~lang "main.rhm"
)

causes as @rhombus(#,(@hash_lang()) noisy_rhombus) module to use
@rhombus(lib("noisy_rhombus/main.rhm"), ~impo) as the initially imported
module, and we can create @filepath("noisy_rhombus/main.rhm") to
reexport @filepath("noisy_rhombus/main.rkt"):

@filebox(
  "noisy_rhombus/main.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))
    import:
      "main.rkt"
    export:
      all_from(.main)
  )
)

Those changes allow @filepath{demo4.rhm} to work, but a syntax error in
@filepath{demo4.rhm} would be reported incorrectly, because
@filepath{noisy_rhombus/main.rhm} has no
@rhombus(configure_expand, ~datum) submodule. The
@rhombuslangname(rhombus/lang_bridge) module helps complete the picture
by reexporting and also propagating submodule definitions and exports.

@filebox(
  "noisy_rhombus/main.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus/lang_bridge))
    ~lang: "main.rkt"
  )
)

Note that @filepath{noisy_rhombus/main.rhm} depends on
@filepath{noisy_rhombus/main.rkt} while
@filepath{noisy_rhombus/main.rkt} indirectly depends on
@filepath{noisy_rhombus/main.rkt}. This kind of cycle is allowed,
because @rhombuslangname(rhombus/reader) delays its reference by quoting
the @rhombus(~lang) module name.

In short, a best practice for defining @hash_lang() languages with
Rhombus is

@itemlist(

 @item{Create or link a collection as a directly like
 @filepath{noisy_rhombus} (but with a more suitable name).}

 @item{Export the language's implementation from @filepath{main.rkt} in
 that directory.}

 @item{Use @rhombuslangname(rhombus/reader) to define a
 @rhombus(reader, ~datum) submodle in @filepath{main.rkt}.}

 @item{Supply @rhombus(~lang "main.rhm") in the
 @rhombus(reader, ~datum) submodle.}

 @item{Create @filepath{main.rhm} with
 @rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus/lang_bridge))) and
 use @rhombus(~lang: "main.rkt") as its body.}

)
