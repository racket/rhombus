#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "rkt.rkt" open)

@title(~tag: "link"){Hyperlinks}

@doc(~include rhombus/scribble/private/link:
       hyperlink
       url){

 The @rhombus(hyperlink) function creates an element that typesets as
 the given @rhombus(pre_content), but that is hyperlinked to
 @rhombus(url_str) (if supported by the @tech{renderer}). The
 @rhombus(underline) argument is a shorthand way to select the default
 @rhombus(style) argument.

 The @rhombus(url) function is a shorthand to providing the same string
 to @rhombus(hyperlink) as both @rhombus(url_str) and
 @rhombus(pre_content).

}

@doc(~include rhombus/scribble/private/link:
       secref){

 Inserts a reference to the section tagged with @rhombus(tag), including
 a hyperlink if supported by the @tech{renderer}.

 If a module path is provided via @rhombus(~doc: module_path), the
 @rhombus(tag) refers to a tag with a prefix determined by
 @rhombus(module_path). When @exec{raco setup} renders documentation, it
 automatically adds a @tech{tag prefix} to the document based on the
 source module. So, for example, to refer to a section of the Rhombus
 guide, @rhombus(module_path) would be
 @rhombus(ModulePath'lib("rhombus/scribblings/guide/rhombus-guide.scrbl")').

 The @rhombus(~tag_prefixes: prefixes) argument similarly supports
 selecting a particular section as determined by a path of tag prefixes.
 When a @rhombus(~doc) argument is provided, then @rhombus(prefixes)
 should trace a path of tag-prefixed subsections to reach the
 @rhombus(tag) section. When @rhombus(~doc) is not provided, the
 @rhombus(prefixes) path is relative to any enclosing section (i.e., the
 youngest ancestor that produces a match).

 If @rhombus(ref_style) is not @rhombus(#false), then it is attached as
 a @tech{style property} and affects the rendering of the link.
 Alternatively, an enclosing part can have a @rhombus(LinkRenderStyle, ~annot)
 property that adjusts the rendering style for all links within the part.
 See @rhombus(LinkRenderStyle, ~annot) for more information about the rendering
 of section references.

 If @rhombus(underline) is @rhombus(#false), then a style is attached to
 the result so that the hyperlink is rendered in HTML without an
 underline.

 In Racket and Rhombus documentation that is rendered to HTML, clicking
 on a section title normally shows a Rhombus @rhombus(secref) or Racket
 @rkt_secref call that is needed to link to the section---and the module
 path will need conversion to use it with a @rhombus(secref) variant
 diferent from the one that is shown.

}

@doc(~include rhombus/scribble/private/link:
       Secref){

 Like @rhombus(secref), but if the rendered form of the reference starts
 with a word (e.g., ``section''), then the word is capitalized.

}

@doc(~include rhombus/scribble/private/link:
       seclink){

 Like @rhombus(secref), but the link is rendered as
 @rhombus(pre_content) instead of the target section's name.

 In addition to @rhombus(secref)'s arguments, @rhombus(seclink) supports
 a @rhombus(indirect) argument. When @rhombus(indirect) is true, then the
 section hyperlink's resolution in HTML is potentially delayed until the
 HTML is viewed, and it may link to a documentation server instead of
 requiring installed documentation.

}

@doc(~include rhombus/scribble/private/link:
       docref){

 Like @rhombus(secref) for a document's implicit @rhombus("top")
 @tech{tag}.

 If @rhombus(indirect) is @tech{content}, then the link's resolution in
 HTML can be delayed, like @rhombus(seclink) with
 @rhombus(~indirect: #true). The indirect content is prefixed with
 ``the'' and suffixed with “``documentation'' to generate the rendered
 text of the link. For example,

@rhombusblock(
  @docref(ModulePath'lib("parsack/parsack/parsack.scrbl")',
          ~indirect: "Parsec implementation in Racket")
)

renders as a hyperlink with the text

@nested(~style: #'inset){
  the Parsec implementation in Racket documentation
}

}

@doc(~include rhombus/scribble/private/link:
       elemtag
       elemref){

 The @rhombus(elemtag) function renders as @rhombus(pre_content), but
 establishes a target for hyperlinks using a @tech{tag} build with
 @rhombus(t).

 The @rhombus(elemref) function renders as @rhombus(pre_content), but
 hyperlinked to the target established by @rhombus(elemtag) with
 @rhombus(t).

}
