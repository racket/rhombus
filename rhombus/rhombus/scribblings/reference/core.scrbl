#lang rhombus/scribble/manual

@title(~style: #'toc){Core Language}

@docmodule(~lang,
           ~use_sources: lib("rhombus/private/amalgam.rkt")!core
                         lib("rhombus/private/amalgam.rkt")!#{core-macro}
                         lib("rhombus/private/amalgam.rkt")!#{core-meta},
           rhombus)

@local_table_of_contents()

@include_section("form.scrbl")

@include_section("def.scrbl")
@include_section("namespace.scrbl")
@include_section("import.scrbl")
@include_section("export.scrbl")
@include_section("function.scrbl")
@include_section("operator.scrbl")
@include_section("class.scrbl")
@include_section("veneer.scrbl")
@include_section("mutable.scrbl")

@include_section("repetition.scrbl")

@include_section("block.scrbl")
@include_section("cond.scrbl")
@include_section("match.scrbl")
@include_section("for.scrbl")
@include_section("recur.scrbl")
@include_section("parameter.scrbl")
@include_section("error.scrbl")
@include_section("exn.scrbl")
@include_section("control.scrbl")

@include_section("implicit.scrbl")
@include_section("dynamic-static.scrbl")

@include_section("module.scrbl")

@include_section("boolean.scrbl")
@include_section("number.scrbl")
@include_section("char.scrbl")
@include_section("string.scrbl")
@include_section("keyword.scrbl")
@include_section("symbol.scrbl")
@include_section("bytes.scrbl")
@include_section("list.scrbl")
@include_section("pair.scrbl")
@include_section("mutable-list.scrbl")
@include_section("array.scrbl")
@include_section("map.scrbl")
@include_section("set.scrbl")
@include_section("box.scrbl")
@include_section("range.scrbl")
@include_section("indexable.scrbl")
@include_section("listable.scrbl")
@include_section("sequence.scrbl")
@include_section("appendable.scrbl")
@include_section("comparable.scrbl")
@include_section("path.scrbl")
@include_section("srcloc.scrbl")
@include_section("void.scrbl")

@include_section("equal.scrbl")
@include_section("dot.scrbl")
@include_section("values.scrbl")
@include_section("annotation.scrbl")
@include_section("enum.scrbl")

@include_section("str.scrbl")
@include_section("io.scrbl")
@include_section("eval.scrbl")
@include_section("stxobj.scrbl")
@include_section("syntax-class.scrbl")
@include_section("macro.scrbl")

@include_section("check.scrbl")
