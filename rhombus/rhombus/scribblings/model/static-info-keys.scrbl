#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(fun meta(s): italic(s))

@(nonterminal:
    bind: def bind ~defn
    expr: block expr
    test_expr: block expr
    then_expr: block expr
    else_expr: block expr)

@title(~tag: "static-info-key"){Representing Static Information}

Static information for an expression or binding is represented in
key--value form and encoded within @tech{syntax objects} at expansion time.
When static information is associated with a binding, it
is propagated to each use of the bound variable, so we can refer to an
expression @meta{E} that has static information without loss of
generality.

A macro defined with @rhombus(expr.macro) can associate arbitrary
key--value static information with its result using
@rhombus(statinfo_meta.wrap). More generally, any macro that produces or
manipulates an expression can add static information to the expression
using @rhombus(statinfo_meta.wrap). A macro can consult static
information for an expression using @rhombus(statinfo_meta.lookup) and
related functions. Predefined Rhombus forms use these operations
internally; for example, @rhombus(::) expands to an expression that has
static information provided by the annotation after @rhombus(::).

The @rhombus(statinfo.key) forms binds an identifier for use as a
static-information key. Rhombus built-in forms use several built-in keys
that can be accessed as identifier-valued variables like
@rhombus(statinfo_meta.dot_provider_key):

@itemlist(

  @item{@rhombus(statinfo_meta.dot_provider_key): Names a compile-time function
   that macro-expands any use of @rhombus(.) after @meta{E}. For example,
   assuming a @rhombus(class Posn(x, y)) declaration, @rhombus(p :: Posn)
   associates @rhombus(statinfo_meta.dot_provider_key) with uses of @rhombus(p) to
   expand @rhombus(p.x) and @rhombus(p.y) to field accesses. An expression
   is a @deftech{dot provider} when it has static information mapped by
   @rhombus(statinfo_meta.dot_provider_key).}

  @item{@rhombus(statinfo_meta.call_result_key): Provides static information
   to be attached to a function-call expression where the function position
   is @meta{E}. (The arguments in the call do not matter.) For example,
   @rhombus(class Posn(x, y)) associates @rhombus(statinfo_meta.call_result_key)
   with @rhombus(Posn) itself, and the associated value is static
   information with @rhombus(statinfo_meta.dot_provider_key). So, @rhombus(Posn(1, 2))
   gets @rhombus(statinfo_meta.dot_provider_key) that makes @rhombus(Posn(1, 2).x)
   select the @rhombus(1).}

  @item{@rhombus(statinfo_meta.index_result_key): Provides static information to be
   attached to a @brackets indexing reference where E is to the left
   of the @brackets. (The index expression inside @brackets does
   not matter.) For example @rhombus(ps :: List.of(Posn)) associates
   @rhombus(statinfo_meta.index_result_key) to @rhombus(ps), where the associated value
   is static information with @rhombus(statinfo_meta.dot_provider_key). So,
   @rhombus(ps[i].x) is allowed and selects an @rhombus(x) field from the @rhombus(Posn)
   instance produced by @rhombus(ps[i]).}

  @item{@rhombus(statinfo_meta.index_get_key) and @rhombus(statinfo_meta.index_set_key): Names a form to
   use for a @brackets indexing reference or assignment where E is to
   the left of the @brackets. (The index expression inside
   @brackets does not matter.) For example, @rhombus(p :: Array) associates
   @rhombus(statinfo_meta.index_get_key) to @rhombus(p) so that @rhombus(p[i]) uses an array-specific
   referencing operation, and it associates @rhombus(statinfo_meta.index_set_key) to
   @rhombus(p) so that @rhombus(p[i] = n) uses an array-specific assignment operation.}

  @item{@rhombus(statinfo_meta.append_key): Names a form to specialize the
   @rhombus(++) operator for appending maps, lists, and similar values. For example,
   @rhombus(p :: Array) associates @rhombus(statinfo_meta.append_key) to @rhombus(p)
   so that @rhombus(p ++ q) uses an array-specific appending operation.}

)

See @secref(~doc: meta_doc, "Static_Information") for additional predefined keys.

Static information from different sources sometimes needs to be merged,
and merging takes one of two forms: @emph{and} mode or @emph{or} mode.
Most obviously, @emph{and} mode is used when combining static
information from annotations via the @rhombus(&&, ~annot) operator, and
@emph{or} mode for combining with @rhombus(||, ~annot). For example,
satisfying the annotation
@rhombus(List.of(Int) || List.of(String), ~annot) implies the static
information of @rhombus(List, ~annot), at least. Another example of
@emph{and} mode is in
@rhombus(def #,(nontermref(bind)) = #,(nontermref(expr))), where both
@nontermref(bind) and @nontermref(expr) can supply static information
about the binding, and both sets of information will apply to uses of
the defined names. Another example of @emph{or} mode isin
@rhombus(if #,(nontermref(test_expr)) | #,(nontermref(then_expr)) | #,(nontermref(else_expr))),
where the overall @rhombus(if) form has static information thatis common
to both @nontermref(then_expr) and @nontermref(else_expr). When a new
static-information key is defined with @rhombus(statinfo.key), then
compile-time merging functions are provided in @rhombus(~and) and
@rhombus(~or) clauses.
