#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "toc"){Table of Contents}

@doc(~include rhombus/scribble/private/toc:
       table_of_contents){

 Creates a @tech{flow block} that renderes a table of contents for the
 enclosing document.

 For PDF/Latex output, the table of contents is for the whole document,
 but for other @tech{renderers}, the table of contents is only for the
 enclosing section.

}

@doc(~include rhombus/scribble/private/toc:
       local_table_of_contents){

 Similar to @rhombus(table_of_contents), but limited to sections within
 the enclosing section, and only for @tech{renderers} (like the one for
 HTML) that support it.

 The meaning of the @rhombus(style) argument depends on the
 @tech{renderer}, but @rhombus(#'#{immediate-only}) normally creates a
 table of contents that contains only immediate subsections of the
 enclosing section. See also the @rhombus(#'quiet) style property of
 @rhombus(part) (i.e., in a part structure, not supplied as the
 @rhombus(style) argument to @rhombus(local_table_of_contents)), which
 normally suppresses subpart entries in a table of contents.

}
