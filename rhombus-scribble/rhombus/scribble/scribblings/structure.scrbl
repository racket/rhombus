#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      pict.Pict)

@title(~tag: "structure"){Document Structure}

Scribble @tech{parts} (a generalization of ``sections'') contain
@tech{flow blocks} (a generalization of ``paragraphs'') that have
@tech{content} (a generalization of ``text'').

A document is represented as a @deftech{part}, as reflected by the
annotation @rhombus(Part, ~annot). A part has three main components:

@itemlist(

  @item{a title, which is is represented by @tech{content} (in a
 specific sense defined below);}

  @item{a sequence of @tech{flow blocks} that determine the part's text, also
 call a @deftech{flow} (i.e., a list of @tech{flow blocks} is a @tech{flow} in
 general); and}

  @item{a list of subparts, each represented as a @tech{part}, to be
 rendered after the part's immediate @tech{flow}.}

)

A part is normally not constructed directly. Instead, a @tech{decoding}
process turns a
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus/scribble))) module
body into a part. Functions like @rhombus(section) and
@rhombus(subsection) create produce values that guide the decoding
process to create subparts.

A @deftech{flow block} is a @deftech{paragraph}, a @deftech{nested flow}
(which itself contains a @tech{flow} sequence of @tech{flow blocks}), a
@deftech{compound paragraph} (which is like a @tech{nested flow}, but
rendered without per-block indentation, if any), an
@deftech{itemization} (where each item has a @tech{flow}), or a
@deftech{table} (where each cell has a @tech{flow}). A flow or an
individual paragraph is not normally created directly, and instead
created by the process of @tech{decoding}, such as for a
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus/scribble))) module
body. A paragraph can be constructed directly with @rhombus(para), while
@rhombus(nested), @rhombus(itemlist), and @rhombus(tabular) (among other
functions) explicitly create other kinds of flow blocks. Each flow block
also has a @tech{style}.

A @tech{paragraph} is the only kind of flow block whose content is not other
flow blocks, and the technical term @deftech{content} refers to values that
can be within a paragraph: strings, @tech{elements}, @tech{convertible}
values, and and lists of content that are flattened into a paragraph. A
@deftech{convertible} value is an object that follows a particular
protocol; most notably, a @rhombus(Pict, ~annot) instance is a
convertible value.

An @deftech{element} combines a @tech{style} with nested @tech{content}.
For example, @rhombus(bold), @rhombus(italic), and @rhombus(larger)
create elements. The @rhombus(elem) function creates an element with a
specific @tech{style}. An element can contain @tech{content}, but an
element cannot contain @tech{flow blocks} or @tech{parts}.

A @deftech{style} controls the rendering of an element or flow block. A
@tech{style} has an optional name that is a string or symbol, and it has
a list of @deftech{style properties}. The effect of a style via its name
and properties ultimately depends on the form that a Scribble document
is rendered into and depends on the @tech{renderer},
but various style names and properties are intended to
have a portable and consistent effect. For example, a style with the
name @rhombus(#'bold) on an element is intended to make the element
bold. A style property may be represented as just a symbol, or it may be
represented as a more structured object.
