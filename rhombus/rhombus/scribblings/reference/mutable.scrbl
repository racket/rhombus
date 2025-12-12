#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Mutable Variables and Assignment}

@doc(
  bind.macro 'mutable $id $maybe_annot'

  grammar maybe_annot
  | #,(@rhombus(::, ~bind)) $annot
  | #,(@rhombus(:~, ~bind)) $annot
  | #,(epsilon)
){

 Binds @rhombus(id) so that its value can be changed using an
 @tech(~doc: meta_doc){assignment operator} such as @rhombus(:=).

 If an @rhombus(annot) is present using @rhombus(::, ~bind), then the
 value of every assignment to @rhombus(id) must satisfy the annotation,
 and the value installed into @rhombus(id) is the converted value if
 @rhombus(annot) is a @tech(~doc: guide_doc){converter annotation}. Static information
 from @rhombus(annot) is associated with uses of @rhombus(id) whether
 attached by @rhombus(::, ~bind) or @rhombus(:~, ~bind).

@examples(
  ~repl:
    def mutable count = 0
    count := count + 1
    count
    count := "string"

  ~repl:
    def mutable count :: Int = 0
    count := count + 1
    ~error:
      count := "string"
)

}

@doc(
  expr.macro '$id := $expr'
){

 An @tech(~doc: meta_doc){assignment operator} that changes the value of @rhombus(id)
 to the result of @rhombus(expr) and returns @rhombus(#void). The
 @rhombus(id) must be bound with @rhombus(mutable, ~bind).

 The @rhombus(:=) operator is also recognized by other forms, such as
 @rhombus(.) and @rhombus(#%index), for changing mutable components of
 some values.

@examples(
  def mutable count = 0
  count := count + 1
  count
)

}
