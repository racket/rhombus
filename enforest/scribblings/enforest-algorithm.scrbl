#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Enforestation Algorithm}

The Rhombus parsing algorithm is similar to figure 1 of
@hyperlink("http://www.cs.utah.edu/plt/publications/gpce12-rf.pdf"){the
 Honu paper}, but with some key differences:

@itemlist(

 @item{When the inital term is an identifier followed by a name-path
  operator and @tt{lookup} produces a name root for the identifier, the
  name-root transformer is applied. Its result, a new term and tail, are
  used for the new state, without changing the current operator (if any).}

 @item{A prefix or infix operator has an associated transformer
  procedure to produce a Racket form, instead of always making a @tt{bin}
  or @tt{un} AST node. Whether the Racket form is an expression or some
  other form (such as pieces of a binding) depends on the context.}

 @item{Operators using the @tech{automatic protocol} are dispatched as in the
  figure. If a prefix operator's protocol is @tech(~key: "macro protocol"){macro}, it behaves
  the same as the figure's case of an identifier that is mapped to a
  transformer. A @tech(~key: "macro protocol"){macro} infix operator's treatment is analogous.}

 @item{Function calls, array references, and list construction are not
   built-in in the same way as Honu. Instead, those positions
   correspond to the use of implicit operators, such as @rhombus(#%call).}

 @item{The figure's prefix-operator case seems wrong; the old operator and
   combiner should be pushed onto the stack.}

 @item{Already-parsed forms that are encoded with @racket_q_parsed
  (which are ``term''s in the figure's terminology) are immediately
  converted to parsed form (i.e., ``tree term''s in the figure) by
  removing the @racket_q_parsed wrapper.}

)

The implementation represents pending operators during enforestation
through the Racket continuation, instead of using an explicit stack;
that is, instead of pushing on the stack, a recursive call is made to
the enforestation function. A transformer can call back into the
enforestation function, and in that case, it provides a current operator
for precedence purposes, and the enforestation function will stop when
it encounters either the end of the input or an operator at lower
precedence. When enforestation stops at an operator with lower
precedence, the enforestation function returns both the parsed form and
the remaining terms.

An identifier/operator is connected to a transformer using Racket's
mapping machinery (@racket_define_syntax, etc.). The enforestation
algorithm is parameterized over the space (in the sense of
@racket_provide_for_space) it should consult and accessor–predicate functions
that extract infix and prefix transformers from compile-time bindings.

When a context includes only prefix @tech(~key: "macro protocol"){macro} operators that are
constrained to consume all available terms, then enforestation is not
really relevant. In that case, the context just needs a way to find
and invoke operator transformers. The Rhombus expander provides a
shortcut that skips the full enforestation algorithm for that simpler
kind of context.
