#lang scribble/rhombus/manual
@(import: "util.rhm": no_prefix)

@title[~tag: "defn-macro"]{Definition and Declaration Macros}

The @rhombus[defn.macro] form defines a definition macro. It is similar
to @rhombus[expr.macro] in prefix form, except that the name must be an
identifier (never an operator), and the result syntax object should
represent a block, which is spliced into the definition context where
the macro is used.

Here’s the classic @rhombus[def_five] macro:


@(rhombusblock:
    import:
      rhombus/macro: no_prefix

    defn.macro '(def_five $id):
      '(:
          def $id: 5
      )

    def_five v
    v  // prints 5
  )

Declarations macros are written with @rhombus[decl.macro], and the
block produced by expansion can use forms like @rhombus[import] and
@rhombus[export].

By distinguishing between expression macros, definition macros, and
declaration macros, Rhombus can report errors for out-of-place uses
earlier and more clearly than Racket.
