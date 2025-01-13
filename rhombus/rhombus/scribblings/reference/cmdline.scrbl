#lang rhombus/scribble/manual
@(import:
    "common.rhm" open:
      except: def
    "nonterminal.rhm" open
    meta_label:
      rhombus/cmdline)

@title(~style: #'toc, ~tag: "cmdline"){Command Line Parsing}

@docmodule(rhombus/cmdline)

The @rhombusmodname(rhombus/cmdline) library provides access to
command-line arguments and forms for parsing those arguments in a
consistent way. For an overview, see @secref(~doc: guide_doc, "cmdline").

@local_table_of_contents()

@include_section("cmdline-make.scrbl")
@include_section("cmdline-help.scrbl")
@include_section("cmdline-class.scrbl")
