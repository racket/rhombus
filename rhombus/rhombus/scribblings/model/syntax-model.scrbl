#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "prog_step.rhm" open)

@// ------------------------------------------------------------------------
@title(~tag: "syntax-model", ~style: #'toc){Syntax Model}

The syntax of a Rhombus program is defined by

@itemlist(

 @item{A @deftech{read} pass converts characters to an intermediate
 abstract represented as a @tech{syntax object}. This surface syntax
 is @deftech{shrubbery notation} as defined in @docref(shrub_doc).}

 @item{A @deftech{parse} pass processes a @tech{syntax object} to
 produce one that is fully parsed and ready for evaluation. This @deftech{parsing},
 in turn, interleaves @deftech{expansion} to trigger macro
 rewrites and @deftech{enforestation} to detect and resolve precedence
 among operators. The expansion and enforestation processes
 are extensible within Rhombus itself, so that the syntax of a
 Rhombus program can be customized. @tech{Binding} information in a
 syntax object drives parsing, and when the
 expansion step encounters a binding form, it extends
 syntax objects for subexpressions with new binding information.}

)

@local_table_of_contents()

@// ------------------------------------------------------------------------
@section(~tag: "id-model"){Identifiers, Binding, and Scopes}

An @deftech{identifier} is a source-program entity. Parsing
a Rhombus program reveals that some identifiers
correspond to @tech{variables}, some refer to primitive syntactic forms
like @rhombus(fun), some refer to derived syntac forms as implemented by @deftech{macros}, and
some are quoted to produce @tech(~doc: ref_doc){symbols} or @tech{syntax objects}. An
identifier @deftech{binds} another (i.e., it is a @deftech{binding})
when the former is parsed as a variable or syntactic form and
the latter is parsed as a @deftech{reference} to the former; the
latter is @deftech{bound}.

For example, as a fragment of source, the text

@rhombusblock(
  def x = 5
  x
)

includes two identifiers: @rhombus(def) and @rhombus(x) (which
appears twice). When this source is parsed in a context where
@rhombus(def) has its usual meaning, the first @rhombus(x) @tech{binds}
the second @rhombus(x).

Bindings and references are determined through @tech{scope sets}. A
@deftech{scope} corresponds to a region of the program that is either
in part of the source or synthesized through elaboration of the
source. Nested binding contexts (such as nested functions) create
nested scopes, while macro expansion creates scopes that
overlap in more complex ways. Conceptually, each scope is
represented by a unique token, but the token is not directly
accessible. Instead, each scope is represented by a value that
is internal to the representation of a program.

A @deftech{form} is a fragment of a program, such as an identifier or
a function call. A form is represented as a @tech{syntax
object}, and each syntax object has an associated set of @tech{scopes}
(i.e., a @deftech{scope set}). In the above example,
the representations of the @rhombus(x)s include the scope that
corresponds to the @rhombus(def) form.

When a form parses as the binding of a particular identifier,
parsing updates a global table that maps a combination of an
identifier's @tech(~doc: ref_doc){symbol} and @tech{scope set} to its meaning: a
@tech{variable} or a @tech{macro}. An
identifier refers to a particular binding when the reference's symbol
and the identifier's symbol are the same, and when the reference's
scope set is a superset of the binding's
scope set. For a given identifier, multiple bindings may have
scope sets that are subsets of the identifier's; in that case,
the identifier refers to the binding whose set is a superset of all
others; if no such binding exists, the reference is ambiguous (and triggers a syntax
error if it is parsed as an expression). A binding @deftech{shadows}
any binding (i.e., it is @deftech{shadowing} any binding)
with the same symbol but a subset of scopes.

For example, in

@rhombusblock(
  fun (x):
    x
)

in a context where @rhombus(fun) corresponds to the usual
syntactic form, the parsing of @rhombus(fun) introduces a new
scope for the binding of @rhombus(x). Since the second @rhombus(x)
receives that scope as part of the @rhombus(fun) body, the first
@rhombus(x) binds the second @rhombus(x). In the more complex
case

@rhombusblock(
  fun (x):
    fun (x):
      x
)

the inner @rhombus(run) creates a second scope for the second
@rhombus(x), so its scope set is a superset of the first
@rhombus(x)'s scope set---which means that the binding for the
second @rhombus(x) shadows the one for the first @rhombus(x), and
the third @rhombus(x) refers to the binding created by the second one.

A @deftech{top-level binding} is a @tech{binding} from a definition at
the top-level; a @deftech{module binding} is a binding from a
definition in a module; all other bindings are @deftech{local
bindings}. Within a module, references to top-level bindings
are disallowed. An identifier without a binding is @deftech{unbound}.

Throughout the documentation, identifiers are typeset to
suggest the way that they are parsed. A hyperlinked identifier
like @rhombus(fun) indicates a reference to a syntactic form or
variable. A plain identifier like @rhombus(x, ~datum) is a
@tech{variable} or a reference to an unspecified @tech{top-level
variable}.

@subsection{Binding Spaces}

A @deftech{binding space}, or just @deftech{space}, represents a
distinct syntactic category that has its own set of bindings. A binding
space is implemented by a specific @tech{scope} for the space; an
identifier is bound in a space if its binding includes the space's scope
in its @tech{scope set}. As a special case, the @tech(~doc: ref_doc){expression} space
has no correspond scope; bindings in that space correspond to the
absence of other space's scopes.

Binding forms bind identifiers in specific spaces. The @rhombus(def),
@rhombus(let), and @rhombus(fun) forms, for example, bind in the
expression space. They may also bind in the static-information space to
record static information about the binding.

The @rhombus(import) and @rhombus(export) forms include support for
bindings spaces through subforms like @rhombus(only_space, ~impo) and
@rhombus(except_space, ~impo).

@subsection{Binding Phases}

Every binding has a @deftech{phase level} in which it can be
referenced, where a phase level normally corresponds to an
integer (but the special @tech{label phase level} does not
correspond to an integer).  Phase level 0 corresponds to the
run time of the enclosing module (or the run time of top-level
expressions). Bindings in phase level 0 constitute the
@deftech{base environment}.  Phase level 1 corresponds to the
time during which the enclosing module (or top-level expression) is
parsed; bindings in phase level 1 constitute the
@deftech{transformer environment}.  Phase level -1 corresponds to the
run time of a different module for which the enclosing module is
imported for use at phase level 1 (relative to the importing
module); bindings in phase level -1 constitute the
@deftech{template environment}. The @deftech{label phase level} does not
correspond to any execution time; it is used to track bindings (e.g.,
to identifiers within documentation) without implying an execution
dependency.

An identifier can have different bindings in different @tech{phase
levels}. More precisely, the @tech{scope set} associated with a
@tech{form} can be different at different phase levels; a top-level or
module context implies a distinct scope at every phase level, while
scopes from macro expansion or other syntactic forms are added to a
form's scope sets at all phases. The context of each binding
and reference determines the phase level whose scope set is
relevant.

@// ------------------------------------------------------------------------
@section(~tag: "stxobj-model"){Syntax Objects}

A @deftech{syntax object} combines a simpler Rhombus value, such as a symbol or list, with
@tech{lexical information}, source-location information, and @tech(~doc: ref_doc){syntax properties}.
The underlying value within a syntax object represents a shrubbery form
(see @secref(~doc: shrub_doc, "parsed-rep")), so it is symbols for identifiers,
lists to hold parenthesized group sequences, and so on.
The @deftech{lexical information} of a syntax object comprises a set of @tech{scope
sets}, one for each @tech{phase level}. In particular, an @tech{identifier} is represented as a syntax
object containing a @tech(~doc: ref_doc){symbol}, and its lexical information can be combined with the global
table of bindings to determine its @tech{binding} (if any) at each phase level.
Different components within a compound syntax object, such as one that represents
a block or parenthesized sequence, can have different lexical information.

For example, a @rhombus(List, ~datum) identifier might have
lexical information that designates it as the @rhombus(List) from
the @rhombuslangname(rhombus) language (i.e., the built-in
@rhombus(List)). Similarly, a @rhombus(fun, ~datum) identifier's
lexical information may indicate that it represents a function
form. Some other identifier's lexical information may
indicate that it references a @tech{top-level variable}.

When a syntax object represents a more complex expression than
an identifier or simple constant, its internal components can
be extracted. Even for extracted identifiers, detailed information
about binding is available mostly indirectly; two identifiers can be
compared to determine whether they refer to the same binding (i.e.,
@rhombus(syntax_meta.equal_binding)), or whether the identifiers have the same
scope set so that each identifier would bind the
other if one were in a binding position and the other in an expression
position (i.e., @rhombus(syntax_meta.equal_name_and_scopes)).

For example, when the program written as

@rhombusblock(
  fun (x):
    x + 6
)

is represented as a syntax object, then two syntax
objects can be extracted for the two @rhombus(x)s. Both the
@rhombus(syntax_meta.equal_binding) and @rhombus(syntax_meta.equal_name_and_scopes)
predicates will indicate that the @rhombus(x)s are the same. In contrast, the
@rhombus(fun) identifier is not @rhombus(syntax_meta.equal_binding)
or @rhombus(syntax_meta.equal_name_and_scopes) to either @rhombus(x).

The lexical information in a syntax object is
independent of the rest of the syntax object, and it can be copied to a new syntax
object in combination with an arbitrary other Rhombus value. Thus,
identifier-binding information in a syntax object is
predicated on the symbolic name of the identifier as well as
the identifier's lexical information; the same question with
the same lexical information but different base value can
produce a different answer.

For example, combining the lexical information from @rhombus(fun) in
the program above to @rhombus(#'x) would not produce an identifier that
is @rhombus(syntax_meta.equal_binding) to either @rhombus(x), since it does not
appear in the scope of the @rhombus(x) binding. Combining the lexical
context of the @rhombus(6) with @rhombus(#'x), in contrast, would produce
an identifier that is  @rhombus(syntax_meta.equal_name_and_scopes) to
both @rhombus(x)s.

The @rhombus(Syntax.literal_local) form bridges the evaluation of a
program and the representation of a program. Specifically,
@rhombus(Syntax.literal_local'#,(rhombus(datum, ~var))') produces a
syntax object that preserves all of the lexical information that
@rhombus(datum, ~var) had when it was parsed as part of the
@rhombus(Syntax.literal_local) form. Note that the
@rhombus(Syntax.literal) form is similar, but it removes certain
@tech{scopes} from the @rhombus(datum, ~var)'s @tech{scope sets}. Just
using quotes, as in @rhombus('#,(rhombus(datum, ~var))'), is similar to
using @rhombus(Syntax.literal), except that an escaping @rhombus($) is
recognized within @rhombus(datum, ~var).

@// ------------------------------------------------------------------------
@include_section("parsing.scrbl")

@// ------------------------------------------------------------------------
@section(~tag: "intro-binding"){Expansion Binding}

@tech{Bindings} are introduced during @tech{parsing} when certain
core syntactic forms are encountered:

@itemlist(

 @item{When an @rhombus(import) form is encountered at the top level or
 module level, each symbolic imported name is paired with the @tech{scope
  set} of the specification to introduce new bindings. If not otherwise
 indicated in the @rhombus(import) form, bindings are introduced at the
 @tech{phase level}s specified by the exporting modules: phase level 0
 for each normal @rhombus(export), phase level 1 for each
 @rhombus(export meta), and so on. The
 @rhombus(export meta #,(@rhombus(n, ~var))) form allows exports at an
 arbitrary phase level (as long as a binding exists within the module at
 the phase level).
 
 A @rhombus(meta, ~impo) clause within @rhombus(import) binds similarly,
 but the resulting bindings have a phase level that is one more than the
 exported phase levels---except that exports for the @tech{label phase
  level} are still imported at the label phase level. More generally, a
 @rhombus(meta #,(@rhombus(n, ~var)), ~impo) or
 @rhombus(meta_label, ~impo) clause within @rhombus(import) imports with
 the specified phase-level shift.}

 @item{When a form such a @rhombus(def), @rhombus(let), @rhombus(macro),
 @rhombus(expr.macro), @rhombus(annot.macro), or @rhombus(meta.bridge) is
 encountered at the top level or module level, bindings are added to
 phase level 0 (i.e., the @tech{base environment} is extended) for each
 defined identifier. In the case of @rhombus(def) or @rhombus(let), the
 specific bound names depend on the parsing of the binding form that
 immediately follows @rhombus(def) or @rhombus(let).}

 @item{When a @rhombus(meta) form is encountered at the top level or
 module level, bindings are introduced as for @rhombus(def) and similar,
 but at phase level 1 (i.e., the @tech{transformer environment} is
 extended). More generally, @rhombus(meta) forms can be nested, and each
 @rhombus(meta) shifts its body by one phase level.}

 @item{Various forms introduce nested bindings contexts or nested
 definition contexts. Such forms introduce at least one fresh scope to
 represent the nested context, and the scope is applied to all syntax
 objects repersenting the content of that context. As a result, bindings
 created for the nested context are not visble at the module level or top
 level. See @secref("intdef-body") for more information on how internal
 definition contexts are handled.

 For example, in

@rhombusblock(
  block:
    let x = 10
    x + y
)

 the binding introduced for @rhombus(x) applies to the @rhombus(x) in
 the body, because a fresh scope is created and added to both the binding
 @rhombus(x) and reference @rhombus(x). The same scope is added to the
 @rhombus(y), but since it has a different symbol than the binding
 @rhombus(x), it does not refer to the new binding. Any @rhombus(x)
 outside of this @rhombus(block) form does not receive the fresh scope
 and therefore does not refer to the new binding.}

)

@//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection(~tag: "transformer-model"){Transformer Bindings}

In a top-level or module context, when the
expander encounters a form like @rhombus(expr.macro) or
@rhombus(meta.bridge), the binding that it introduces for the defined
names is a @tech{macro} binding. The @tech{value} of the
@tech{binding} exists at expansion time, rather than run time (though
the two times can overlap), though the binding itself is introduced with
@tech{phase level} 0 (i.e., in the @tech{base environment}).

The value for the binding is obtained by evaluating the expression on
the right-hand side of a @rhombus(meta.bridge) form or a similar implied
function form for a macro definer like @rhombus(expr.macro). This
expression must be @tech{parse}d before it can be evaluated, and it is
parsed at phase level 1 (i.e., in the @tech{transformer environment})
instead of phase level 0. The result is further wrapped with scope
operations to produce a transformer function that can be recorded in a
binding and called as described in @secref("expand-step").

The @tech{scope}-introduction process for macro expansion (described in
@secref("expand-step")) helps keep binding in an expanded program
consistent with the lexical structure of the source program. For
example, the expanded form of the program

@rhombusblock(
  def x = 12
  macro 'm $id':
    'block:
      def x = 10
      $id'
  m x
)

is almost equivalent to the expansion of

@rhombusblock(
  def x = 12
  block:
    def x = 10
    x
)


However, the result of the expression produced by @rhombus(m) is
@rhombus(12), not @rhombus(10). The reason is that the transformer bound
to @rhombus(m) introduces the binding @rhombus(x), but the referencing
@rhombus(x) is present in the use of the macro. The introduced
@rhombus(x) is left with one fresh @tech{macro scope}, while the
reference @rhombus(x) has a different fresh scope, so the binding
@rhombus(x) is the one outside the @rhombus(block) form.

A @tech{use-site scope} on a binding identifier is ignored when the
definition is in the same context where the use-site scope was
introduced. This special treatment of use-site scopes allows a
macro to expand to a visible definition. For example, the expanded
form of the program

@rhombusblock(
  defn.macro 'm $id':
    'def $id = 5'
  m x
  x
)

is

@rhombusblock(
  def x = 5
  x
)

where the @rhombus(x) in the @rhombus(def) form had a @tech{use-site
 scope} that is not present on the final @rhombus(x). The final
@rhombus(x) nevertheless refers to the definition, because the use-site
scope is removed before installing the definition's binding. In
contrast, the expansion of

@rhombusblock(
  defn.macro 'm $id':
    'block:
       def x = 4
       def $id = 5
       x'
  m x
)

is

@rhombusblock(
  block:
    def x = 5
    def x = 5
    x
)

where the second @rhombus(def x) has a use-site scope that prevents it
from binding the final @rhombus(x). The use-site scope is not ignored in
this case, because the binding is not part of the definition context
where @rhombus(m x) was expanded.

In addition to using scopes to track introduced identifiers, the
expander tracks the expansion history of a form through @tech(~doc: ref_doc){syntax
 properties} such as @rhombus(#'origin). See
@secref(~doc: meta_doc, "stxobj-track") for more information.

@//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection(~tag: "local-binding-context"){Local Binding Context}

Although the @tech{binding} of an @tech{identifier} can be uniquely
determined from the combination of its @tech{lexical information} and
the global binding table, the expander also maintains a @deftech{local
 binding context} that records additional information about @tech{local
 bindings} to ensure they are not used outside of the lexical region in
which they are bound.

Due to the way local binding forms like @rhombus(block) add a fresh
@tech{scope} to both bound @tech{identifiers} and body forms, it isn't
ordinarily possible for an @tech{identifier} to reference a local
binding without appearing in the body of the @rhombus(block). However,
if macros use compile-time state to stash bound identifiers, they can
violate this constraint. For example, the following @rhombus(stash_id)
and @rhombus(unstash_id) macros cooperate to move a reference to a
locally-bound @rhombus(x) @tech{identifier} outside of the lexical
region in which it is bound:

@examples(
  ~hidden:
    import rhombus/meta open
  ~repl:
    meta:
      def mutable stashed_id = #false
    expr.macro 'stash_id $id':
      stashed_id := id
      '#void'
    expr.macro 'unstash_id':
      stashed_id
    block:
      def x = 42
      stash_id x
      unstash_id
    ~error:
      unstash_id
)

In general, an identifier's lexical information is not sufficient to
know whether or not its binding is available in the enclosing context,
since the @tech{scope set} for the identifier stored in
@rhombus(stashed_id) unambiguously refers to a binding in the global
binding table. However, the reference produced by @rhombus(unstash_id)
in the above program is still illegal, even if it isn't technically
unbound. To record the fact that @rhombus(x)'s binding is in scope only
within the body of its corresponding @rhombus(block) form, the expander
adds @rhombus(x)'s @tech{binding} to the @tech{local binding context}
while expanding the @rhombus(block) body. More generally, the expander
adds all local variable bindings to the local binding context while
expanding expressions in which a reference to the variable would be
legal. When the expander encounters an identifier bound to a local
variable, and the associated binding is not in the current local binding
context, it throws a syntax error.

The local binding context also tracks local @tech{macro} bindings
(i.e. bindings bound by forms like @rhombus(meta.bridge) or
@rhombus(expr.macro) in a similar way, except that the context also
stores the compile-time value associated with the transformer. When an
identifier that is locally bound as a transformer is parsed, the local
binding context is consulted to retrieve the value. If the binding is in
scope, its associated compile-time value is used; otherwise, the
expander throws a syntax error.

@//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection(~tag: "partial-expansion"){Partial Expansion}

In certain contexts, such as an @tech{internal-definition context} or
module context, @deftech{partial expansion} is used to determine
whether forms represent definitions, expressions, or other declaration
forms. Partial expansion works by cutting off the normal recursive
expansion when the relevant binding is for a primitive syntactic form.

@// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection(~tag: "intdef-body"){Internal Definitions}

An @deftech{internal-definition context} supports local definitions mixed
with expressions. Forms that allow internal definitions document such
positions using the @rhombus(body, ~var) meta-variable.

Expansion relies on @tech{partial expansion} of each @rhombus(body, ~var) in
an internal-definition sequence. Partial expansion of each
@rhombus(body, ~var) produces a form matching one of the following cases:

@itemlist(

 @item{A definition form like @rhombus(def): The binding table is immediately enriched
       with bindings for the definition form.  Further
       expansion of the definition is deferred, and partial expansion
       continues with the rest of the body.}

 @item{A transformer definition form like @rhombus(expr.macro): The right-hand side is
       expanded and evaluated, and a transformer
       binding is installed for the body sequence before partial
       expansion continues with the rest of the body.}

)

After all body forms are partially expanded, if no definitions were
encountered, then the expressions are collected into a sequence
as the internal-definition context's expansion.  Otherwise, at
least one expression must appear after the last definition.

Before partial expansion begins, expansion of an internal-definition
context begins with the introduction of a fresh @deftech{outside-edge
scope} on the content of the internal-definition context. This
outside-edge @tech{scope} effectively identifies syntax objects that are
present in the original form. An @deftech{inside-edge scope} is also
created and added to the original content; furthermore, the
inside-edge scope is added to the result of any partial expansion.
This inside-edge scope ensures that all bindings introduced by the
internal-definition context have a particular scope in common.

@// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection(~tag: "mod-parse"){Module Expansion, Phases, and Visits}

Expansion of a module form proceeds in a similar way to
@seclink("intdef-body"){expansion of an internal-definition context}:
an @tech{outside-edge scope} is created for the original module
content, and an @tech{inside-edge scope} is added to both the original
module and any form that appears during a partial expansion of the
module's top-level forms to uncover definitions and imports.

A @rhombus(import) form not only introduces @tech{bindings} at expansion
time, but also @deftech{visits} the referenced module when it is
encountered by the expander. That is, the expander instantiates any
variables defined in the module within @rhombus(meta), and it also
evaluates all expressions for @tech{macro} bindings via
@rhombus(meta.bridge), @rhombus(expr.macro) and similar.

Module @tech{visits} propagate through @rhombus(import)s in the same
way as module @tech{instantiation}. Moreover, when a module is
visited at @tech{phase} 0, any module that it imports with
@rhombus(import meta) is instantiated at phase 1, while
further @rhombus(import meta -1)s  leading back
to phase 0 causes the required module to be visited at
phase 0 (i.e., not instantiated).

During compilation, the top-level of module context is itself
implicitly visited. Thus, when the expander encounters
@rhombus(import meta), it immediately
instantiates the required module at phase 1, in addition
to adding bindings at @tech{phase level} 1 (i.e., the
@tech{transformer environment}). Similarly, the expander immediately
evaluates any form that it encounters within
@rhombus(meta).

Phases beyond 0 are visited on demand. For example,
when the right-hand side of a phase-0 @rhombus(expr.macro) is to
be expanded, then modules that are @tech{available} at phase 1
are visited. More generally, initiating expansion at phase
@math{n} visits modules at phase @math{n}, which in turn
instantiates modules at phase @math{n+1}. These
visits and instantiations apply to available
modules in the enclosing @tech{evaluator}'s @tech{module registry};
a per-registry lock prevents multiple threads from concurrently
instantiating and visiting available modules.

@//|--{On-demand instantiation
of available modules uses the same reentrant lock as
@racket[namespace-call-with-registry-lock].}--|

When the expander encounters @rhombus(import) and @rhombus(import meta)
within a module context, the resulting
visits and instantiations are specific to the expansion
of the enclosing module, and are kept separate from visits and
instantiations triggered from a top-level context or
from the expansion of a different module.

@//|--{Along the same lines, when a
module is attached to a namespace through
@racket[namespace-attach-module], modules that it @racket[require]s
are transitively attached, but instances are attached only at
phases at or below the namespace's @tech{base phase}.}--|

@//------------------------------------------------------------------------
@subsection(~tag: "macro-introduced-bindings"){Macro-Introduced Bindings in the Top Level}

When a top-level definition binds an identifier that originates from a
 macro expansion, the definition captures only uses of the identifier
 that are generated by the same expansion due to the fresh @tech{scope}
 that is generated for the expansion.

@examples(
  ~hidden:
    import rhombus/meta open
  ~defn:
    defn.macro 'def_and_use_of_x: $val':
      // x below originates from this macro:
      'def x = $val
       x'
  ~repl:
    def x = 1
    x
    def_and_use_of_x: 2
    x
  ~defn:
    defn.macro 'def_and_use $id: $val':
      // `id` below was provided by the macro use
      'def $id = $val
       $id'
  ~repl:
    def_and_use x: 3
    x
)

For a top-level definition (outside of a module), the order of
evaluation affects the binding of a generated definition for a generated
identifier use. If the use precedes the definition, then the use is
resolved with the bindings that are in place at that point, which will
not include the binding from the subsequently macro-generated
definition. (No such dependency on order occurs within a module, since a
module binding covers the entire module body.) To support the
declaration of an identifier before its use, the @rhombus(meta.bridge)
form avoids binding an identifier if the body of the definition produces
zero values.

@examples(
  ~hidden:
    import rhombus/meta open
  ~defn:
    def mutable bucket_1 = 0
    def mutable bucket_2 = 0
    defn.macro 'def_and_assign_from_x: $val':
      'bucket_1 := x
       def x = $val
       bucket_2 := x'
  ~repl:
    def x = 1
    def_and_assign_from_x: 2
    x
    bucket_1
    bucket_2
  ~defn:
    defn.macro 'def_and_uses_fail':
      // initial reference to `even` precedes definition
      'fun odd(x): if x == 0 | #false | even(x-1)
       fun even(x): if x == 0 | #true | odd(x-1)
       odd(17)'
  ~repl:
    ~error:
      def_and_uses_fail
  ~defn:
    defn.macro 'def_and_uses':
      // declare before definition via no-values `meta.bridge`
      'meta.bridge even: values()
       fun odd(x): if x == 0 | #false | even(x-1)
       fun even(x): if x == 0 | #true | odd(x-1)
       odd(17)'
  ~repl:
    def_and_uses
)

Macro-generated @rhombus(import) and @rhombus(export) forms also
introduce and reference generation-specific bindings (due to the added
@tech{scope}) with the same ordering effects as for definitions.

@//------------------------------------------------------------------------
@section(~tag: "evaluator-model"){Evaluators}

A @deftech{evaluator} is both a starting point for parsing and a
starting point for running @tech{compiled} code. An evaluator also has a
@tech{module registry} that maps module names to module declarations
(see @secref("module-eval-model")). An evaluator's module registry is
shared by all @tech{phase level}s, and it applies both to parsing and to
running compiled code. A function like @rhombus(Evaluate.make_rhombus())
creates a fresh evaluator with an empty set of top-level definitions and
a module registry that has only @rhombuslangname(rhombus) and its dependencies.

As a starting point for parsing, an evaluator provides @tech{scopes} (one
per phase level, plus one that spans all phase levels). Operations such
as @rhombus(Evaluator.import) create initial @tech{bindings} using the
evaluator's scopes, and the further parsing and evaluation in the
evaluator can create additional bindings. Evaluation of a form with an
evaluator always adds the evaluator's phase-specific scopes to the form
and to the result of expanding a top-level form; as a consequence, every
binding identifier has at least one scope. Every evaluator uses the same
scope as the one added to all phase levels, while the scopes specific to
a phase level are always distinct.

As a starting point for evaluating @tech{compiled} code, each evaluator
encapsulates a distinct set of top-level variables at various phases, as
well as a potentially distinct set of module instances in each phase.
That is, even though module declarations are shared for all phase
levels, module instances are distinct for each phase. Each evaluator has
a @deftech{base phase}, which corresponds to the phase used by
reflective operations such as @rhombus(eval) and
@rhombus(Evaluator.instantiate). In particular, using @rhombus(eval) on
a @rhombus(import) form @tech{instantiates} a module in the evaluator's
base phase.

After an evaluator is created, module instances from existing evaluators
can be attached to the new evaluator. In terms of the evaluation model,
top-level variables from different evaluators essentially correspond to
definitions with different prefixes, but attaching a module uses the
same prefix for the module's definitions in evaluators where it is
attached. The first step in evaluating any compiled expression is to
link its top-level variable and module-level variable references to
specific variables in the evaluator.

At all times during evaluation, some evaluator is designated as the
@deftech{current evaluator}. The current evaluator has no particular
relationship, however, with the evaluator that was used to expand the
code that is executing, or with the evaluator that was used to link
the compiled form of the currently evaluating code. In particular,
changing the current evaluator during evaluation does not change the
variables to which executing expressions refer. The current evaluator
only determines the behavior of reflective operations to expand code
and to start evaluating expanded/compiled code.

@examples(
  def x = #'orig // define in the original evaluator
  :
    // The following `block` expression is compiled in the original
    // evaluator, so direct references to `x` see `#'orig`
    block:
      def n = Evaluator.make_rhombus() // make new evaluator
      parameterize { Evaluator.current: n }:
        eval('def x = #'new'.strip_scopes()) // in the new evaluator
        println(x)         // prints `#'orig`
        println(eval('x'.strip_scopes())) // prints `#'new`
)

If an identifier is bound to a macro or to an import in an evaluator's
top level, then defining the identifier as a @tech{variable} shadows
the macro or import in future uses of the top level. Similarly, if an
identifier is bound to a @tech{top-level variable}, then binding
the identifier to a macro or an import shadows the variable; the
variable's value remains unchanged, however, and may be accessible
through previously evaluated expressions.

@examples(
  def x = 5
  fun f(): x
  x
  f()
  macro 'x': '10'
  x
  f()
  def x = 7
  x
  f()
  module m ~lang rhombus:
    export def x = 8
  import self!m.x
  x
  f()
)

Like an evaluator's top level, each @rhombus(module) form has an
associated scope to span all phase levels of the
module's content, plus a scope at each phase level. The
latter is added to every form, original or appearing through partial
macro expansion, within the module's immediate body. Those same scopes
are propagated to a namespace created by @rhombus(Evaluator.from_module)
for the module. Meanwhile, parsing of a @rhombus(module) form begins by
removing the all scopes that correspond to the enclosing top-level or
(in the case of @tech{submodules}) @rhombus(module) form.

@//------------------------------------------------------------------------
@section(~tag: "compilation-model"){Compilation}

Before parsed code is evaluated, it is first @deftech{compiled}. A
compiled form has essentially the same information as the corresponding
parsed form, though the internal representation naturally dispenses with
identifiers for syntactic forms and local bindings. A compiled form can
be marshaled to and from a byte string, so it is suitable for saving and
reloading code.

Although individual read, expand, compile, and evaluate operations are
available, the operations are often combined automatically. For example,
the @rhombus(eval) function takes a syntax object and parses it, compiles
it, and evaluates it.
