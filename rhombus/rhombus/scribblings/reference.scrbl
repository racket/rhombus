#lang rhombus/scribble/manual

@title(~style: #'toc){Core Reference}

@docmodule(~lang,
           ~use_sources: lib("rhombus/private/amalgam.rkt")!core
                         lib("rhombus/private/amalgam.rkt")!#{core-macro}
                         lib("rhombus/private/amalgam.rkt")!#{core-meta},
           rhombus)

@local_table_of_contents()

@include_section("ref-form.scrbl")

@include_section("ref-def.scrbl")
@include_section("ref-namespace.scrbl")
@include_section("ref-import.scrbl")
@include_section("ref-export.scrbl")
@include_section("ref-function.scrbl")
@include_section("ref-operator.scrbl")
@include_section("ref-class.scrbl")
@include_section("ref-veneer.scrbl")
@include_section("ref-mutable.scrbl")

@include_section("ref-repetition.scrbl")

@include_section("ref-block.scrbl")
@include_section("ref-cond.scrbl")
@include_section("ref-match.scrbl")
@include_section("ref-for.scrbl")
@include_section("ref-recur.scrbl")
@include_section("ref-parameter.scrbl")
@include_section("ref-exn.scrbl")
@include_section("ref-control.scrbl")

@include_section("ref-implicit.scrbl")
@include_section("ref-dynamic-static.scrbl")

@include_section("ref-module.scrbl")

@include_section("ref-boolean.scrbl")
@include_section("ref-number.scrbl")
@include_section("ref-char.scrbl")
@include_section("ref-string.scrbl")
@include_section("ref-keyword.scrbl")
@include_section("ref-symbol.scrbl")
@include_section("ref-bytes.scrbl")
@include_section("ref-list.scrbl")
@include_section("ref-pair.scrbl")
@include_section("ref-mutable-list.scrbl")
@include_section("ref-array.scrbl")
@include_section("ref-map.scrbl")
@include_section("ref-set.scrbl")
@include_section("ref-box.scrbl")
@include_section("ref-range.scrbl")
@include_section("ref-indexable.scrbl")
@include_section("ref-listable.scrbl")
@include_section("ref-sequence.scrbl")
@include_section("ref-appendable.scrbl")
@include_section("ref-comparable.scrbl")
@include_section("ref-path.scrbl")
@include_section("ref-srcloc.scrbl")
@include_section("ref-void.scrbl")

@include_section("ref-equal.scrbl")
@include_section("ref-dot.scrbl")
@include_section("ref-values.scrbl")
@include_section("ref-annotation.scrbl")
@include_section("ref-enum.scrbl")

@include_section("ref-str.scrbl")
@include_section("ref-io.scrbl")
@include_section("ref-eval.scrbl")
@include_section("ref-stxobj.scrbl")
@include_section("ref-syntax-class.scrbl")
@include_section("ref-macro.scrbl")

@include_section("ref-check.scrbl")
