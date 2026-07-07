#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/scribble/manual open
      rhombus/version.Version)

@title(~tag: "doc_history"){Documenting History}

@doc(
  expr.macro 'history:
                $history_clause
                ...'
  grammar history_clause
  | ~added $version_string
  | ~changed $version_string: $pre_content_expr
){

 Creates a version table to record when a change was introduced. The
 @rhombus(version_string) must be a well-formed
 @rhombus(Version, ~annot), and it should refer to the version of a
 package that provides a changed or added binding. For a
 @rhombus(~changed) clause, @rhombus(pre_content_expr) should produce
 @rhombus(PreContent, ~annot).

@(history:
    ~added "1.1")

}
