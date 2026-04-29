#lang scribble/rhombus/manual
@(import:
    "../common.rhm" open)


@(def doc_site:
    @hyperlink("https://docs.racket-lang.org/rhombus/index.html?fam=Rhombus&famroot=rhombus"){http://docs.racket-lang.org})

@title(
  ~category: #'#{getting-started},
){Getting Started with Rhombus}

@itemlist(

 @item{@secref(~doc: guide_doc, "Quick_Start")}

 @item{Choose your own adventure:

@itemlist(

  @item{@docref(guide_doc): Get a general introduction and tour of Rhombus.}

  @item{@docref(ModulePath'lib("rhombus/scribblings/tutorial/quick/rhombus-quick.scrbl")'):
  Get just a taste of the language---and then continue to the @seclink(~doc: guide_doc, "top"){guide}.}

  @item{@docref(ModulePath'lib("rhombus/scribblings/tutorial/metaprogramming/rhombus-meta-tutorial.scrbl")'):
  For programming-language enthusiasts, dive into the deep end of macros
  and language extensibility.}

)

 }

 @item{Explore the language definition:

@itemlist(

  @item{@docref(ref_doc): The main language reference.}

  @item{@docref(meta_doc): The main reference for macro support and
  language extensibility.}

  @item{@docref(model_doc): Conceptual prelude to the reference manuals,
  defines the language's core evaluation model and it's syntax concepts as
  layered on Shrubbery notation.}

  @item{@docref(shrub_doc): The language's underlying syntax substrate,
  which lifts a textual representation of code and data to syntax objects
  and other values.}

)

 }

 @item{@docref(ModulePath'lib("rhombus/scribblings/rhombus-racket/rhombus-racket.scrbl")'):
 Take advantage of more libraries in the Racket ecosystem.}

 @item{@seclink("top", ~doc: top_doc){Rhombus Documentation}: If you're
 reading this in your local installation, then it's all documentation
 that you have installed. If you're reading from @doc_site, it
 documentation for all registered Rhombus and Racket packages, including
 third-party packages.}

)
