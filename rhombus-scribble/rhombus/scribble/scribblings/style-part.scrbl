#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open
    "style.rhm" open)

@title(~tag: "style-part"){Part and Section Styles}

@doc(
  ~nonterminal_key: Part
  grammar part_style
){

@name_itemlist(
 @elem{a @tech{part}}

 @item{A @rhombus(String, ~annot): @html{Used as a CSS class name.}}

 @item{@symkey(#'index): @latex{The section rendered as an
   index, such as in a two-column format.}}

)

@property_itemlist(
 @elem{a @tech{part}}

 @item{@symkey(#'unnumbered): @all{A section number is not computed or
   rendered for the section.}}

 @item{@symkey(#'#{hidden-number}): @all{A section number is computed
   for the section, but it is not rendered as part of the section name.}}

 @item{@symkey(#'#{toc-hidden}): @all{The part title is not shown in
   tables of contents} @html{The part title is not included in ``on this
   page'' boxes.} @latex{The part title is omitted only if it is unnumbered
   or has a hidden number.}}

 @item{@symkey(#'hidden): @all{The part title is not shown. The
   @rhombus(#'#{toc-hidden}) @tech{style property} usually should be
   included with @rhombus(#'hidden) for consistency.} @latex{The part title
   is not shown only if its is empty, and in that case, it is also excluded
   from tables of contents.}}

 @item{@symkey(#'grouper): @all{The part is numbered with a Roman
   numeral, by default, and its subsections continue numbering as if they
   appeared in the preceeding part. In other words, the part acts like a
   ``part'' in a book where chapter numbering is continuous across parts.}}

 @item{A @rhombus(Style.Numberer, ~annot): @all{Determines a representation of
   the part's section number as an extension of it's parent's number. A
   @rhombus(Style.Numberer, ~annot) overrides the default representation, which
   is a natural number or (in the case of an accompanying
   @rhombus(#'grouper) property) a Roman numeral. If a
   @rhombus(#'unnumbered) property is also present, a
   @rhombus(Style.Numberer, ~annot) property is ignored.}}

 @item{@symkey(#'toc): @html{Subparts of the part are rendered on
   separate pages for multi-page mode.}}

 @item{@symkey(#'#{non-toc}): @html{Initial sub-parts of the part are
   @emph{not} rendered on separate pages for multi-page mode. This use of
   the @tech{style property} applies only to the main part.}}

 @item{@symkey(#'reveal): @html{Shows subparts when this part is
   displayed in a table-of-contents panel, which normally shows only the
   top-level sections.}}

 @item{@symkey(#'quiet): @all{Hides entries for sub-parts of this part in a
  @rhombus(table_of_contents) or @rhombus(local_table_of_contents)
  listing, except when those subparts are top-level entries in the
  listing.}}

 @item{@symkey(#'#{no-toc+aux}): @html{As a @tech{style property} for
   the main part of a rendered page, causes the output to not include a
   margin box for the main table of contents, ``on this page'', or tables
   with the @rhombus(#'aux) style property. The @rhombus(#'#{no-toc+aux})
   property effectively implies @rhombus(#'#{no-toc}) and
   @rhombus(#'#{no-sidebar}), but also suppresses @rhombus(#'aux) tables.}}

 @item{@symkey(#'#{no-toc}): @html{As a @tech{style property} for the
   main part of a rendered page, causes the output to not include a margin
   box for the main table of contents. The ``on this page'' box that
   contains page-local links (and that only includes an ``on this page''
   label for multi-page documents) takes on the location and color of the
   main table of contents, instead.}}

 @item{@symkey(#'#{no-sidebar}): @html{As a @tech{style property} for
   the main part of a document, causes the output to not include an ``on
   this page'' margin box.}}

 @item{@symkey(#'#{no-header-controls}): @html{Suppresses link and
   link-information icons (if any) as part of a section header.}}

 @item{@symkey(#'#{no-index}): @all{Has no effect as a @tech{style
    property} directly on a @tech{part}, but as a style property for
   @rhombus(title), @rhombus(section), and similar, the
   @rhombus(#'#{no-index}) @tech{style property} causes @tech{decoding} to
   skip the generation of an entry for the part's title in the document
   index.}}

 @item{A @rhombus(Style.DocumentVersion, ~annot): @all{A version number for
   this part and its sub-parts (except as overridden). When it is not
   @rhombus("") may be used when rendering a document; at a minimum, a
   non-@rhombus("") version is rendered when it is attached to a part
   representing the whole document. The default version for a document is
   @rhombus(system.version()). In rendered form, the version is normally
   prefixed with the word ``Version.''} @html{The prefix formatting can be
    controlled by overriding @tt{.version:before} and/or
    @tt{.versionNoNav:before} in CSS.} @latex{The prefix formatting can be
    controlled by redefining the @tt{\SVersionBefore} macro.}}

 @item{A @rhombus(Style.DocumentDate, ~annot): @latex{A date for the part,
   normally used on a document's main part. The default date for a document
   is @rhombus(#false), which avoids explicitly specifying a date at the
   Latex level, so that the current date is used as the document date. Set
   the date to @rhombus(#{""}) to suppress a date in an output document.}}

 @item{A @rhombus(Style.DocumentSource, ~annot): @html{Provides a module path
   for the part's source. Clicking on an HTML section title generated for
   the part or its subparts may show the module path plus a section-tag
   string, so that the user can create a reference to the section.}}

 @item{@rhombus(Style.HTML.BodyId, ~annot) structure: @html{Uses the
   value's string as an @tt{id} attribute of the @tt{<body>} tag. This
   @tech{style property} can be set separately for parts that start
   different HTML pages, otherwise it is effectively inherited by subparts.
   The default is @rhombus(#{"scribble-racket-lang.org"}), but @exec{raco
    setup} installs @rhombus(#{"doc-racket-lang.org"}) as the @tt{id} for
   any document that it builds.}}

 @item{An @rhombus(Style.HTML.Attributes, ~annot): @html{Provides
   additional attributes for the @tt{<html>} tag when the part corresponds
   to its own HTML page.}}

 @item{A @rhombus(Style.HTML.HeadExtra, ~annot): @html{Content for the
   @tt{<head>} tag when the part corresponds to its own HTML page.}}

 @item{A @rhombus(Style.HTML.HeadAddition, ~annot): @html{Like
   @rhombus(Style.HTML.HeadExtra, ~annot), but also propagated to enclosing
   and nested HTML pages.}}

 @item{A @rhombus(Style.Color, ~annot): @html{Applies a color to the part
   title.}}

 @item{A @rhombus(Style.BackgroundColor, ~annot): @html{Applies a color
   to the background of the part title.}}

 @item{A @rhombus(Style.HTML.Hover, ~annot): @html{Adds a text label to
   the title to be shown when the mouse hovers over it.}}

 @item{A @rhombus(Style.RenderConvertibleAs, ~annot): @html{Controls how
   objects that subscribe to the
   @racketmodname(lib("file/convertible.rkt")) protocol are rendered.}}

 @item{A @rhombus(Style.LinkRenderStyle, ~annot): @all{Determines the default
  rendering of links to sections or other destinations within the section.}}

 @item{A @rhombus(Style.HTML.PartTitleAndContentWrapper, ~annot): @html{Adds a tag
   with attributes around the part title and its content, including any
   content before the title from a @tech{paragraph} with the
   @rhombus(#'pretitle) style name. The wrapper is not used around a
   subpart that is rendered on a different HTML page.}}

 @item{A @rhombus(Style.HTML.PartLinkRedirect, ~annot): @html{Redirects
   hyperlinks that would otherwise go to the part so that they refer to a
   different URL.}}

 @item{@rhombus(#'#{enable-index-merge}): @latex{On an index part or one of its
  enclosing parts, causes index entries to be merged when
  they have the same content, with multiple references for the same entry
  combined with @ltx{Smanypageref}. The @ltx{Smanypageref} Latex macro
  must be redefined to accept multiple @litchar{,}-separated labels and
  generate a suitable set of references. See also
  @racketmodname(lib("scriblib/book-index.rkt")).}}

)

}
