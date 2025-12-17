#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "section"){Title and Sections}

@doc(~nonterminal: part_style: Part
     ~include rhombus/scribble/private/section: title){

 Declares a title for the document by producing a
 @rhombus(PartDecl, ~annot) that is recognized by the @tech{decoding}
 process in @tech{part mode} for a @rhombuslangname(rhombus/scribble)
 module body.

 In the main module of a document, this title is the overall title of
 the document. In a section included via @rhombus(include_section), it is
 the name of the section.

 See @rhombus(part_style) for information about @tech{styles} for parts.
 The @rhombus(vers) and @rhombus(date) argumentsa sre converted into
 style properties for the part.

 The @rhombus(category), @rhombus(foreign_category), and
 @rhombus(index_extras) arguments are merged into the @rhombus(tag_prefix)
 argument. If @rhombus(tag_prefix) is not a map that includes
 @rhombus(#'#{default-language-family}) already, then that key is added
 with the value @rhombus(PairList["Rhombus"]). Similarly,
 @rhombus(category) and @rhombus(foreign_category) arguments are merged
 into @rhombus(index_extras) if it does not already a have a
 @rhombus(#'#{doc-properties}) key.

}

@doc(~nonterminal: part_style: Part
     ~include rhombus/scribble/private/section: section){

 Declares a section at a level nested below @rhombus(title), recognized
 by the @tech{decoding} process in @tech{part mode}.

 See @rhombus(part_style) for information about @tech{styles} for parts.

}

@doc(~nonterminal: part_style: Part
     ~include rhombus/scribble/private/section:
       subsection
       subsubsection
       subsubsub_section){

 Like @rhombus(section), but declares sections as further nesting
 levels. At the @rhombus(subsubsub_section) level, the result is simply a
 @tech{paragraph} that renders as an unnumbered section header.

}

@doc(
  defn.macro 'include_section($module_path)'
){

 Makes the document at @rhombus(module_path) a section of the enclosing
 document, where the @rhombus(module_path) section hierarchy is
 effectively shifted to more nested my one layer. That is,
 @rhombus(title) within @rhombus(module_path) corresponds to a
 @rhombus(section) written in the document where
 @rhombus(include_section) is used, @rhombus(section) corresponds to
 @rhombus(subsection), and so on.

}
