#lang rhombus/scribble/manual
@(import:
    "racket_names.rkt" open
    meta_label:
      rhombus open
      rhombus/meta:
        expose:
          expr_meta
          defn_meta
          decl_meta
          expr)

@title(~tag: "racket-expr"){Racket Expressions from Rhombus}

Using a Racket syntactic form from Rhombus requires a Rhombus macro that
expands to the Racket form. A Rhombus macro can build a use of a Racket
syntactic form using @rhombus(expr_meta.pack_s_exp),
@rhombus(defn_meta.pack_s_exp), or @rhombus(decl_meta.pack_s_exp). Use
@rhombus(expr_meta.pack_expr) to pack a Rhombus expression for use
within a packed S-expression form.

For example, the following @rhombus(shared) macro expands to a use of the
Racket @racket_shared form, which specifically recognizes uses of the
Racket @racket_cons function.

@examples(
  ~defn:
    import:
      rhombus/meta open
      lib("racket/base.rkt").cons
      lib("racket/shared.rkt").shared as rkt_shared
  ~defn:
    expr.macro 'shared ($(id :: Identifier) = $rhs ...,
                        ...):
                  $body
                  ...':
      expr_meta.pack_s_exp(['rkt_shared',
                            [[id, expr_meta.pack_expr('$rhs ...')],
                             ...],
                            expr_meta.pack_expr('block:
                                                   $body
                                                   ...')])
  ~repl:
    shared (x = cons(1, y),
            y = cons(2, x)):
      x.rest.rest.rest.first
)