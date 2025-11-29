#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "renderer"){Renderers}

A @deftech{renderer} is responsible for converting a processed document
into a rendered format, such as HTML, Latex, PDF (via Latex), or
Markdown.

After @tech{decoding} and before other processing, a Scribble document
is independent of its rendered format, and it simultaneously customizes
rendering for all possible formats through @tech{styles}. Some style
information applies to all or multiple renderers, while some is specific
to a renderer and ignore by other renderers. A render's output may be
subject to further customization, such as through CSS for HTML output or
through Latex style files and macros for Latex and PDF output; those
customizations might be attached to a document through @tech{styles}, or
they may be externally applied, depending on what the renderer supports.

Although a renderer is mostly involved with the last pass of document
processing (see @secref("passes")), a renderer is selected before any
documentation passes. The renderer can choose specifically how
cross-reference information is represented in a
@rhombus(CollectInfo, ~annot) or @rhombus(ResolveInfo, ~annot) table,
for example.
