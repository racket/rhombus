#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(rhombus_typeset 'enforest': '@bold{@tt{enforest}}')
@(rhombus_typeset 'expand': '@bold{@tt{expand}}')
@(rhombus_typeset 'lookup': '@bold{@tt{lookup}}')

@(rhombus_typeset 'term': '@rhombus(term, ~var)')
@(rhombus_typeset 'group': '@rhombus(group, ~var)')
@(rhombus_typeset 'name': '@rhombus(name, ~var)')
@(rhombus_typeset 'tree': '@rhombus(tree, ~var)')
@(rhombus_typeset 'transform': '@rhombus(transform, ~var)')
@(rhombus_typeset 'op': '@rhombus(op, ~var)')

@(rhombus_typeset 'tree_var': '@elem{@rhombus(tree)@subscript{var}}')
@(rhombus_typeset 'tree_lhs': '@elem{@rhombus(tree)@subscript{lhs}}')
@(rhombus_typeset 'term_rest': '@elem{@rhombus(term, ~var)@subscript{rest}}')
@(rhombus_typeset 'term_other': '@elem{@rhombus(term, ~var)@subscript{other}}')
@(rhombus_typeset 'term_literal': '@elem{@rhombus(term, ~var)@subscript{lit}}')
@(rhombus_typeset 'op_prior': '@elem{@rhombus(op, ~var)@subscript{prior}}')
@(rhombus_typeset 'op_init': '@elem{@rhombus(op, ~var)@subscript{init}}')

@(rhombus_typeset '⇒': '@elem{⇒}')
@(rhombus_typeset 'where': '@elem{@hspace(1)where}')
@(rhombus_typeset 'and': '@elem{@hspace(1)and}')

@title(~tag: "parsing"){Parsing via Enforestation and Expansion}

@tech{Parsing} of a module body starts with a @tech{syntax object} that
represents a sequence of @seclink("top", ~doc: shrub_doc){shrubbery
 groups}.

Parsing procedes in
particular @tech{phase level}, starting with phase level 0. @tech{Bindings}
from the @tech{syntax object}'s @tech{lexical information} drive the
parsing process, and they cause new bindings to be introduced for the
lexical information of sub-expressions. In some cases, a
sub-form is expanded in a @tech{phase} deeper (having a
greater phase-level number) than the enclosing form.

Parsing also happens in a particular @tech{space}. Parsing of a module always
starts in the expression, definition, and declaration spaces for the
module body, and then certain forms will trigger parsing in other
spaces. For example, parsing a @rhombus(fun) expression form will
trigger parsing in the @tech{binding} space for the function's arguments
(if any) and in the @tech{annotation} space for the function's result
annotation (if declared).

In the parsing description that follows, we take the relevant phase
level and space of parsing to be known to a @rhombus(lookup) function.
We also first consider the parsing of an individual group, but the
process is analogous for dealing with a sequence of groups in a module
body or other definition context.

@section(~tag: "enforest-step"){Enforest Steps}

Parsing begins in @tech{enforestation} mode. This mode can be described by a
@rhombus(enforest) function that takes a group syntax object
@rhombus('term ...') and produces a parsed prefix @rhombus(tree)
followed by remaining terms. The @rhombus(enforest) function also
receives an operator @rhombus(op_prior) whose right-hand side is being
parsed; to start, assume a @rhombus(op_init) as @rhombus(op_prior) whose
precedence is lower than all other operators.

@rhombusblock(
  enforest('term ...', op_prior) = 'tree term_rest ...'
)

The syntax objects consumed and produced by @rhombus(enforest) can
contain parsed objects as terms. If @rhombus(enforest) receives a single
parsed term as its input, then parsing is done:

@rhombusblock(
  enforest('tree', op_prior) ⇒ 'tree'
)

Otherwise, parsing will proceed by calling @rhombus(enforest) again on
its result.

The simplest possibility for @rhombus(enforest) to make progress is that
the first @rhombus(term) in its input is a @rhombus(name) that is bound
as a variable. The @rhombus(lookup) function that resolves
bindings, and let @rhombus(tree_var) stand for the parsed representation
of a variable. Enforestation will continue with the parsed variable at
the start of the .

@rhombusblock(
  enforest('name term ...', op_prior) ⇒ enforest('tree_var term ...', op_prior)
  where lookup(name) = tree_var
)

Instead of a variable, the first term may be a @rhombus(name) that is
bound as a prefix macro. In that case, an @tech{expansion} step is
performed by calling the macro transformer, then continuing to enforest
with the result. The @rhombus(expand) function itself may need to recur
to arrive at a parsed @rhombus(tree), and we return to @rhombus(expand)
later. Meanwhile, the precedence of @rhombus(op_prior) is not relevant
when @rhombus(name) refers to a prefix operator.

@rhombusblock(
  enforest('name term ...', op_prior) ⇒ enforest('tree term_rest ...', op_prior)
  where lookup(name) = PrefixMacro(transform)
  and expand(transform, 'name term ...') = 'tree term_rest ...'
)

To detect an infix operator, enforestation must have an input seuqence
that starts with an already-parsed left-hand side. In that case, the
infix-macro binding must provide an operator whose precedence can be
compared to @rhombus(op_prior), and the infix operator's transformer is
applied only if it has higher precedence.

@rhombusblock(
  enforest('tree_lhs name term ...', op_prior) ⇒ enforest('tree term_rest ...', op_prior)
  where lookup(name) = InfixMacro(transform, op)
  and op > op_prior
  and expand(transform, 'tree_lhs name term ...') = 'tree term_rest ...'
)

If an infix operator is found with lower precedence, then
@rhombus(enforest) stops instead of calling itself recursively:

@rhombusblock(
  enforest('tree_lhs name term ...', op_prior) ⇒ 'tree_lhs name term ...'
  where lookup(name) = InfixMacro(transform, op)
  and op ≤ op_prior
)

The two infix possibilities for @rhombus(enforest) are the only place
where operator precedence needs to be compared. That's why precedence
can be pairwise between two operators, instead of a global order across
all operators.

Enforestation's last job is to introduce implicit operators where
needed. If it finds a non-name atomic @rhombus(term_literal) at the
start of the sequence, such as number or string, then it adds
@rhombus(#%literal) to the start of the stream:

@rhombusblock(
  enforest('term_literal term ...', op_prior) ⇒ enforest('#%literal term_literal term ...', op_prior)
)

Implicit operators like @rhombus(#%literal) are added only when they are
bound, otherwise, @rhombus(enforest) would recur forever adding the same
implicit name. The @tech{lexical information} of an added implicit name
is taken by @rhombus(enforest) from the first term in its input sequence.

Similarly, a parenthesized term triggers @rhombus(#%parens), and so on.

@rhombusblock(
  enforest('(group, ...) term ...', op_prior)
    ⇒ enforest('#%parens (group, ...) term ...', op_prior)
  enforest('[group, ...] term ...', op_prior)
    ⇒ enforest('#%brackets (group, ...) term ...', op_prior)
  enforest('{group, ...} term ...', op_prior)
    ⇒ enforest('#%braces (group, ...) term ...', op_prior)
  enforest('«'group; ...' term ...»', op_prior)
    ⇒ enforest('«#%quotes 'group; ...' term ...»', op_prior)
  enforest(': group; ...', op_prior)
    ⇒ enforest('#%block : group; ...', op_prior)
  enforest('| group; ... | ...', op_prior)
    ⇒ enforest('#%alts | group; ... | ...', op_prior)
)

Parentheses, brackets, and braces after a parsed term trigger a set of implicit infix names.

@rhombusblock(
  enforest('tree_lhs (group, ...) term ...', op_prior)
    ⇒ enforest('tree_lhs #%call (group, ...) term ...', op_prior)
  enforest('tree_lhs [group, ...] term ...', op_prior)
    ⇒ enforest('tree_lhs #%index (group, ...) term ...', op_prior)
  enforest('tree_lhs {group, ...} term ...', op_prior)
    ⇒ enforest('tree_lhs #%comp (group, ...) term ...', op_prior)
)

For a @rhombus(term_other) (after a parsed @rhombus(tree_lhs)) that is
not parentheses, brackets, or braces, @rhombus(#%juxtapose) is added.

@rhombusblock(
  enforest('tree_lhs term_other term ...', op_prior)
    ⇒ enforest('tree_lhs #%juxtapose term_other term ...', op_prior)
)

@section(~tag: "expand-step"){Expand Steps}

A macro-expansion transformer takes a syntax object and returns two
values: the expansion of the macro, and a sequence of terms that were
not consumed by the macro's application.

In the simple case, when @rhombus(expand) applies a macro transformer,
it gets an immediate parsed result base. For example, when a macro
defined by @rhombus(annot.macro) returns a result constructed with
@rhombus(annot_meta.pack_predicate) or
@rhombus(annot_meta.pack_converter), the result is a parsed form. In
that case, expansion returns to let @rhombus(enforest) proceed (as in
@secref("enforest-step")).

@rhombusblock(
  expand(transform, 'term ...') ⇒ 'tree term_rest ...'
  where transform('term ...') = values('tree', 'term_rest ...')
)

Otherwise, @rhombus(expand) will recur explicitly with
@rhombus(enforest) to cotinue expansion. The explicit use of
@rhombus(enforest) does not see the @rhombus('term_rest ...') tail
produces by a tranformer, which means that further expansion cannot
produce a semi-parsed form that is accidentally merged with that tail;
instead, it's parsing must complete as separate and intact.

@rhombusblock(
  expand(transform, 'term ...') ⇒ 'tree term_rest ...'
  where transform('term ...') = values('term ...', 'term_rest ...')
  and enforest('term ...', op_init) = 'tree'
)

Much of the heavy lifting of expansion is embedded in an individual
@rhombus(transform) function:

@itemlist(

 @item{When a syntax class like @rhombus(expr_meta.Parsed),
 @rhombus(bind_meta.Parsed), or a name bound by
 @rhombus(parse_syntax_class, ~space_meta_clause) is used to match
 syntax, the match applies @rhombus(enforest) with @rhombus(lookup)
 adapted to the appropriate @tech{space}. That process includes implicit
 uses of parsing syntax classes, such as an a macro bound by
 @rhombus(expr.macro) with an an escaped identifier as the pattern after
 the defined name. The syntax object associated with the match to a
 parsing syntax class is a parsed @rhombus(tree) that can be returned or
 incoporated into a larger syntax object.

 Nested uses of @rhombus(enforest) (via a syntax class) within a
 @rhombus(transform) class explain how parsing is trigered for spaces
 other than the initial expression space. For example, the @rhombus(fun)
 expression form parses bindings for ther function arguments and an
 optional annotation for the function result, which implies
 @rhombus(enforest) in each of those spaces.}

 @item{A @rhombus(transform) function generated for a macro defined by
 @rhombus(macro), @rhombus(expr.macro), @rhombus(annot.macro), and
 similar starts by allocating a fresh @deftech{macro scope} and
 @deftech{use-site scope}. Both scopes are added to the inputsynatx
 object because it is provided to the macro implementation. For the
 macro's results, the macro scope is flipped: added where it is not
 present and remove where it is present. As a consequence, the macro
 scope identifies components of a syntax object that are introduced by a
 macro invocation, while the use-site scope identifies components that
 were originally present in the macro use.

 Macro and use-site scopes influence how identifiers are used as
 bindings and a potential references to other bindings. Macro scopes on
 an introduced identifier cause it to being only instances of the
 identifier that are introduced by the same macro invocation, due to the
 rules on @tech{scope sets} and binding described in @secref("id-model").
 Use-site scopes, meanwhile, are stripped from an identifier before the
 identifier is used as binding in the immediate enclosing context of a
 macro expansion, which has the effect of preventing an identifier that
 is supplied to a macro from binding introduced identifiers in
 nested forms. See @secref("transformer-model") for examples.}

)
