#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "passes"){Document Passes}

A Scribble document is processed in four passes:

@itemlist(

 @item{The @deftech{traverse pass} traverses the document content in
 document order so that information from one part of a document can be
 communicated to other parts of the same document. The information is
 transmitted through a symbol-keyed mapping that can be inspected and
 extended by @rhombus(TraverseElement, ~annot)s and
 @rhombus(TraverseBlock, ~annot)s in the document. The @tech{traverse
  pass} iterates the traversal until it obtains a fixed point (i.e., the
 mapping from one iteration is unchanged from the previous iteration).}

 @item{The @deftech{collect pass} globally collects information in the
 document that can span documents that are built at separate times, such
 as targets for hyperlinking. A @rhombus(PartRelativeElement, ~annot) or
 @rhombus(CollectElement, ~annot) participates directly in this pass.}

 @item{The @deftech{resolve pass} matches hyperlink references with
 targets and expands delayed elements (where the expansion should not
 contribute new hyperlink targets). A @rhombus(DelayedElement, ~annot) or
 @rhombus(DelayedBlock, ~annot) participates directly in this pass.}

 @item{The @deftech{render pass} generates the result document. A
 @rhombus(RenderElement, ~annot) participates directly in this pass.}

)

None of the passes mutate the document representation. Instead, the
@tech{traverse pass}, @tech{collect pass}, and @tech{resolve pass}
accumulate information in a side @rhombus(CollectInfo, ~annot) map and
@rhombus(ResolveInfo, ~annot) map. The @tech{collect pass} and
@tech{resolve pass} are effectively specialized version of
@tech{traverse pass} that work across separately built documents.

@section(~tag: "tag"){Cross-Reference Tags}

Cross-reference information that is gathered during the @tech{collect
 pass} and used during the @rhombus{resolve pass} can make use of
@deftech{tags} as keys. A @tech{tag} is represented by a list (or pair
list) that starts with a symbol or as @rhombus(GeneratedTag, ~annot)
object. The symbol effectively identifies the type of the tag, such as
@rhombus(#'part) for a tag that links to a section, or @rhombus(#'def)
for a function definition. The symbol also effectively determines the
interpretation of the remainder of the tag.

A @tech{part} can have a @deftech{tag prefix}, which is effectively
added onto the second item within each tag whose first item is
@rhombus(#'part), @rhombus(#'tech), or @rhombus(#'cite), or whose second
item is a list that starts with @rhombus(#'prefixable):

@itemlist(

 @item{The prefix is added to a string second item by creating a list
 containing the prefix and string.}

 @item{The prefix is added to a list second item after @rhombus(#'part),
 @rhombus(#'tech), or @rhombus(#'cite) using @rhombus(List.cons).}

 @item{The prefix is added to a second item that starts
 @rhombus(#'prefixable) by adding it to the list after
 @rhombus(#'prefixable).}

 @item{A prefix is not added to a @rhombus(GeneratedTag, ~annot) item.}

)

A @tech{tag prefix} is used for reference outside its part, including
the use of tags in the @rhombus(Part.tags) property. Typically, a
document's main part has a tag prefix that applies to the whole
document; references to sections and defined terms within the document
from other documents must include the prefix, while references within
the same document omit the prefix. Part prefixes can be used within a
document as well, to help disambiguate references within the document.

Some procedures accept a ``tag'' that is just the string part of the
full tag, where the symbol part is supplied automatically. For example,
@rhombus(section) and @rhombus(secref) both accept a string ``tag'',
where @rhombus(#'part) is implicit.

@section(~tag: "part-context"){Part Context}

During the @tech{collect pass} and @tech{resolve pass}, @deftech{part
 context} information is accumulated from enclosing parts. The context
starts as an empty map. When @rhombus(Part.tag_prefix) reports a map,
then for each key in the table, the value in the table is merged with
the context from enclosing parts. A value is merged by adding the key
and value to the accumulation if the key is not yet present, or by
consing the new value to the context’s current value when the key is
present. Use @rhombus(Part.context_accumulation) during the
@tech{collect pass} or @tech{resolve pass} to retrieve the value that
has been accumulated from enclosing parts.
