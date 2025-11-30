#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "manual-text"){Links and Literals}

@doc(~include lib("rhombus/scribble/private/manual-text.rhm"):
       litchar){

 Renders @rhombus(str) or the concatenated @rhombus(strs) as literal
 text. In HTML and PDF, at least, a shared background is shown behind
 colored fixed-width font to emphasize that the text is intended
 literally.

}

@doc(~include lib("rhombus/scribble/private/manual-text.rhm"):
       deftech
       tech){

 The @rhombus(deftech) form renders as @rhombus(pre_content), and it
 establishes an anchor so that @rhombus(tech) can refer to the technical
 term or phase with hyperlinking. As long as @rhombus(use_style) is true,
 @rhombus(pre_content) for @rhombus(deftech) renders in a style
 (typically italic) to indicate that it is a technical term or phase.

 When @rhombus(key) is @rhombus(#false), the @rhombus(Content.to_string)
 of @rhombus(pre_content) after @tech{decoding} is used as a key for
 reference. As long as @rhombus(normalize) is true, then the key string
 is normalized as follows:

@itemlist(

 @item{The string is case-folded.}

 @item{A trailing @litchar{ies} is replaced by @litchar{y}.}

 @item{A trailing @litchar{s} is removed.}

 @item{Consecutive hyphens and whitespaces are all replaced by a single space.}

)

 These normalization steps help support natural-language references that
 differ slightly from a defined form. For example, a definition of
 @rhombus(bananas) can be referenced with a use of @rhombus(banana).

 @rhombus(use_style) is true, then @rhombus(defterm) is used on
 pre-content.

 The @rhombus(index_extras) argument is used in the same was as by
 @rhombus(indexed).

}

@doc(~include lib("rhombus/scribble/private/manual-text.rhm"):
       defterm){

 Renders @rhombus(pre_content) in a style (typically italic) to indicate
 that it is a technical term or phase. Unlike @rhombus(deftech),
 @rhombus(defterm) does not establish a target for references via
 @rhombus(tech).

}

@doc(~include lib("rhombus/scribble/private/manual-text.rhm"):
       onscreen){

 Renders @rhombus(pre_content) in a style for labels in a GUI
 interface, such as menu names and button labels.

}

@doc(~include lib("rhombus/scribble/private/manual-text.rhm"):
       filepath){

 Renders @rhombus(pre_content) as a file name: adding straight quotes
 and using a fixed-width font.

}

@doc(~include lib("rhombus/scribble/private/manual-text.rhm"):
       pkg){

 Renders @rhombus(pre_content) as a package name: using a fixed-width
 font.

}

@doc(~include lib("rhombus/scribble/private/manual-text.rhm"):
       exec
       exec_flag){

 The @rhombus(exec) function renders @rhombus(content) without
 @tech{decoding}) as command-line input: using a fixed-width font.

 The @rhombus(exec_flag) function is similar, but it also disables line
 breaks, so that dashes for a command-line flag will not serve a points
 for breaking text across lines.

}

@doc(~include lib("rhombus/scribble/private/manual-text.rhm"):
       math){

 Renders @rhombus(pre_content) with further transformations after
 @tech{decoding}:

@itemlist(

  @item{Any immediate curly apostrophe @litchar{’} is converted to a
  prime character @litchar{′}.}

  @item{Parentheses and sequences of decimal digits in immediate strings
  are left as-is, but any other immediate string is italicized.}

  @item{ When @litchar{_} appears before a non-empty sequence of
  numbers, letters, and @litchar{-}, the sequence is typeset as a
  subscript.}

  @item{When @litchar{^} appears before a non-empty sequence of numbers,
  letters, and @litchar{-}, the sequence is typeset as a superscript.}

)

}

@doc(~include lib("rhombus/scribble/private/manual-text.rhm"):
       hash_lang){

 Renders as @hash_lang() with hyperlink to a description of the
 @hash_lang() syntax.

}
