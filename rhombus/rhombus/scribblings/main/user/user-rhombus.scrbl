#lang rhombus/scribble/manual
@(import:
    "../../../tests/version_guard.rhm")

@(version_guard.at_least "9.0.0.11"
  | import:
      lib("scribblings/main/contents.rkt")
    contents.#{build-contents}(~#{user?}: #true,
                               ~supplant: "rhombus",
                               ~#{main-language-family}: "Rhombus",
                               ~#{default-language-family}: PairList["Rhombus"],
                               ~#{default-category}: PairList[#'omit],
                               ~#{self-path}: "rhombus/index.html",
                               ~#{bug-url}: "https://github.com/racket/rhombus/issues",
                               ~version: default_document_version())
  | @title(~category: #'language){Rhombus}
    def start_doc = ModulePath'lib("rhombus/scribblings/getting-started/rhombus-getting-started.scrbl")'
    @para{Please see @secref(~doc: start_doc, "top").}
    @para{(In a newer version of Racket, this page becomes a useful listing)})
