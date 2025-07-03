#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/expand_config open)

@title(~tag: "lang"){Languages and Modules}


@section(~tag: "rhombus-reader"){@hash_lang() Language Reader}

@docmodule(~lang, rhombus/reader)

The @rhombuslangname(rhombus/reader) language is useful for defining a
@rhombus(reader) submodule as described in
@secref(~doc: guide_doc, "hash-lang").

@doc(
  ~nonterminal:
    module_path: import ~defn
  non_target:
    grammar clause:
      ~lang $module_path
      ~lang: $module_path
){

 Each group in the body of a @rhombuslangname(rhombus/reader) module
 must match @rhombus(clause), and no two @rhombus(clause)s can start with
 the same keyword. At least one @rhombus(clause) must start with
 @rhombus(~lang).

 The resulting module exports @rhombus(#{read}, ~datum),
 @rhombus(#{read-syntax}, ~datum), and @rhombus(#{get-info}, ~datum)
 consistent with Racket's @hash_lang() protocol. A module using the
 language with this reader is parsed as shrubbery notation and then uses
 @rhombus(module_path) as the @rhombus(~lang) language (see
 @secref(~doc: guide_doc, "tilde-lang")).

}


@section(~tag: "rhombus-lang-bridge"){Bridge for @rhombus(module, ~decl) @rhombus(~lang)}

@docmodule(~lang, rhombus/lang_bridge)

The @rhombuslangname(rhombus/lang_bridge) language is useful for
creating a @filepath{.rhm} module that acts the same as a
@filepath{.rkt} module after @rhombus(~lang). See
@secref(~doc: guide_doc, "hash-lang") for more information.

The body of a @rhombuslangname(rhombus/lang_bridge) must contain a
single @rhombus(~lang) clause using the same syntax as in
@rhombuslangname(rhombus/reader).

@section(~tag: "runtime-config"){Run-Time Configuration}

@docmodule(rhombus/runtime_config)

The @rhombus(rhombus/runtime_config) library exports nothing.
Instantiating the module adjusts configuration parameters to make the
system behave in Rhombus terms, such as printing values in Rhombus
syntax. See @secref(~doc: guide_doc, "configure") for information about
how these functions are useful to define a
@rhombus(configure_runtime, ~datum) submodule.

@section(~tag: "expand-config"){Expand-Time Configuration}

@docmodule(rhombus/expand_config)

The @rhombus(rhombus/expand_config) library is for configuring an
expand-time time to behave in Rhombus terms, which as when a syntax
error is reported.

@doc(
  fun enter_parameterization() :: Parameterization
  fun exit_parameterization() :: Parameterization
){

 See @secref(~doc: guide_doc, "configure") for information about how
 these functions are useful to define a
 @rhombus(configure_expand, ~datum) submodule.

}
