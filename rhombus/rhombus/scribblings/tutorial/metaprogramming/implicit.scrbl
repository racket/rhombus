#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/rx open
    meta_label:
      rhombus/rx open
    scribble/bnf)

@(def imp_eval = make_rhombus_eval())

@examples(
  ~eval: imp_eval
  ~hidden:
    import rhombus/meta open
)

@title(~tag: "implicit"){Implicit Forms}

A form such as

@rhombusblock(1 + f(2))

appears to be an addition expression to add the number @rhombus(1) to the
result of calling @rhombus(f) on the argument @rhombus(2). That is
almost certainly the right interpretation of the form as a Rhombus
expression. But if this form is in an enviornment where @rhombus(+) has
been shadowed by a macro definition, then it may mean something very
different.

Could the @rhombus(1) by itself mean anything different? If @rhombus(f)
is just a variable, might @rhombus(f(2)) be something other than a
function call? And in a space other than the expression space, how would
the meaning of a number or a function-call form get a meaning
appropriate to that space?

Rhombus answers these questions through @deftech{implicit forms} that
are used as prefix or infix forms when no identifier or operator is apparent as a
target to expand. When a literal number or string is found by itself,
for example, the @rhombus(#%literal) implicit prefix form is tried in the
appropriate space. When a form is followed by a parenthesized form,
the @rhombus(#%call) implicit infix form is tried.
@margin_note{Implicit form names starts with @litchar{#%}, which
is allowed in general at the start of Rhombus identifiers, but intended
for use by bindings that are not normally seen.}

Here's a contrived example that locally binds @rhombus(#%literal) to
treat each integer as @rhombus(10) times its normal value:

@examples(
  ~eval:
    imp_eval
  ~repl:
    block:
      import rhombus.#%literal as orig_literal
      expr.macro '#%literal $(n :: Int)':
                    'orig_literal $(n.unwrap() * 10)'
      2 + 5 - 1
)

The use of @rhombus(orig_literal) in the example's expansion is
important, otherwise the expansion would be a lone integer, which would
trigger the implicit @rhombus(#%literal) form again.

See
@secref("implicit", ~doc: ModulePath'lib("rhombus/scribblings/reference/rhombus-reference.scrbl")')
for a table of implicit forms and the patterns that trigger each form.
If an implicit form is tried but not bound, a syntax error is reported.

@section(~tag: "ex-implicit"){Exercise}

See the end of @local_file("interp_space.rhm"), which has the same
example @rhombus(interp) calls as @local_file("interp1.rhm"), but
instead of using constructors like @rhombus(Id) and @rhombus(Fun)
directly, a @rhombus(prog) form is used to write expressions that look
similar to Rhombus syntax. Those expressions could have been quoted and
parsed as in @local_file("interp3.rhm"), but they are instead parsed in
the definition of @rhombus(prog) by using the pattern
@rhombus('prog: $(lc :: lc_meta.Parsed)', ~bind).

The @rhombus(lc_meta) namespace is created by using
@rhombus(space.enforest) within @local_file("interp_space.rhm"). The use
of @rhombus(space.enforest) binds @rhombus(lc.macro) to enable binding
macros in the new space, and @rhombus(lc_meta.Parsed) to trigger parsing
via those macros.

Each use of @rhombus(lc_meta) in @local_file("interp_space.rhm") creates
a parsing point that translates a λ-calculus form to a use of a
constructor like @rhombus(Id) and @rhombus(Fun). In the end,
@rhombus(prog) directly uses a parsed result via
@rhombus(lc_meta.Parsed) as its result expression.

Add a substraction and conditional form this this interpreter and its
parser, the same as in an @seclink("ex-interp1"){earlier exercise} with
@local_file("interp1.rhm").

Solution: @local_file("interp_space_soln.rhm").

@// ==================================================

@close_eval(imp_eval)
