#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "decoding"){Decoding Parts, Blocks, and Content}

Scribble @deftech{decoding} processes a sequence of string literals and
other values to produce a @tech{part}, @tech{flow}, or @tech{content}.

The body of a
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus/scribble)))
module, for example, contains a sequence of string literals and calls to
functions like @rhombus(section), @rhombus(tabular), or @rhombus(elem).
As explained in @secref("notation"), the module body starts in text
mode, so it is parsed as text literals except as escaped via
@litchar("@"). Any definitions or declarations in the module body---such
as @rhombus(import, ~defn), @rhombus(module, ~decl),
@rhombus(def, ~defn) forms---are lifted out of the sequence, so only
expressions in the module body count.

Decoding a module body starts in @tech{part mode}, then it decodes
portions with a part (and not in subparts) in @tech{flow mode}, and then
text literals within a flow block are decoded in @tech{content mode}.

@section{Part Decoding Mode}

In @deftech{part mode}, each @rhombus(PartDecl, ~annot) in a sequence of
values designates a part or subpart, while the result of @rhombus(title)
is used for the overall enclosing part created by the module body. The
result of @rhombus(section) designates an immediate subpart, the result
of @rhombus(subsection) designates a subsubpart within the subpart, and
so on.

All values between the start of a part and the start of its first
subpart are decoded in @tech{flow mode} to determine the part's flow.

@section{Flow Decoding Mode}

In @deftech{flow mode}, decoding recognizes a blank line as a paragraph
separator, and all non-block values between such separates are collected
into a @tech{paragraph}.

Blocks created by functions like @rhombus(nested) and @rhombus(tabular)
become immediate blocks in the flow, except that blocks (and decoded
paragraphs) without a blank line in between are collected into a
@tech{compound paragraph}.

A sequence of values to be combined into a paragraph are decoded in
@tech{content mode}.

Functions that accept @rhombus(PreFlow, ~annot) arguments, such as
@rhombus(para), use @tech{flow mode} on sequences of literal-string
arguments after flattening nested lists.

@section{Content Decoding Mode}

In @deftech{content mode}, decoding perform a handful of special text
conversions:

@itemlist(

 @item{@litchar{---}: converted to a em dash, @litchar{—} (U+2014).}

 @item{@litchar{---}: converted to a en dash, @litchar{–} (U+2013).}

 @item{@litchar{``}: converted to curly open double-quotes, @litchar{“} (U+201C).}

 @item{@litchar{''}: converted to curly close double-quotes, @litchar{”}
 (U+201D).}

 @item{@litchar{'}: converted to curly open single-quote, @litchar{‘} (U+2018).}

 @item{@litchar{'}: converted to curly close single-quote or apostrophe,
 @litchar{’} (U+2019).}

)

Functions that accept @rhombus(PreContent, ~annot) arguments, such as
@rhombus(italic) or @rhombus(elem), use @tech{content mode} on sequences
of literal-string arguments after flattening nested lists. Note that
@rhombus(literal) does not teat its argument that way. For example, in

@rhombusblock(
  @bold{``apple''}
)

the ``apple'' argument is decoded to use curly quotes, and then it is bolded.
