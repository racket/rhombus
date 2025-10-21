#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/rx open
    meta_label:
      rhombus/rx open
    scribble/bnf)

@(def space_eval = make_rhombus_eval())

@examples(
  ~eval: space_eval
  ~hidden:
    import rhombus/meta open
)

@title(~tag: "space"){Spaces}

So far, we have seen macros two different @tech{spaces}: definitions and
expressions, each with its own macro-definition form,
@rhombus(defn.macro, ~defn) or @rhombus(expr.macro, ~defn). Rhombus
includes many other such spaces.

In general, a @deftech{space} is a parsing context that has its own
bindings. For example, @rhombus(*) in an expression context is the
multiplication operator, but @rhombus(*, ~at rhombus/rx) in a regular-expression
context is a Kleene star. Getting from one space to another requires
a form in one space, such the @rhombus(rx) expression form, that
bridges to the other space. Here are some examples showing how
an asterisk interpretation is context-dependent.

@examples(
  ~defn:
    import rhombus/rx open
  ~repl:
    2 * 3
  ~repl:
    rx'"a" *'.match("aaa")
    rx'"a" *'.match("aba")
    rx'"1" *'.match(String.from_int(37037 * 3))
)

In the above example, @rhombus(rx) is an expression form that expects a
@bnf.nt{term} afterward using @quotes. The quoted form is not treated as
a syntax object template, but instead parsed as a regular expression.
The result of an @rhombus(rx) expression is an @rhombus(RX, ~class)
object that has an @rhombus(RX.match) method.

@section{Binding Macros}

Because each space has its own set of bindings, it naturally
supports its own, individualized set of macros. Even better, because a
separate space is used for bindings, Rhombus's binding positions
are macro extensible.

To demonstrate how such macros work and show the power of spaces other than the definition and
expression context spaces, here's an example using @rhombus(bind.macro), which
binds in the space used to parse Rhombus binding positions. Binding
positions include the left-hand side of @rhombus(def), the arguments for
a @rhombus(fun), the binding patterns of a @rhombus(match) form, and more.

@examples(
  ~eval: space_eval
  ~defn:
    expr.macro '$a ⊢ $b':
      '[$a, $b]'
    bind.macro '$a ⊢ $b':
      '[$a :: Map, $b]'
  ~defn:
    fun lookup(env ⊢ expr):
      env[expr]
  ~repl:
    {#'x: 1, #'y: 2} ⊢ #'y
    lookup({#'x: 1, #'y: 2} ⊢ #'y)
  ~repl:
    match {#'x: 1, #'y: 2} ⊢ #'z
    | env ⊢ var: var
)

The @rhombus(expr.macro) declaration ensures that, when @rhombus('$a ⊢ $b') appears
in an expression context,
the left-hand and right-hand arguments of @rhombus(⊢) will be parsed as
expressions. The @rhombus(bind.macro) declaration is similar ensuring that
the left-hand and right-hand components of @rhombus(⊢) will
be parsed as bindings in binding positions.

This combination of features explains how
@rhombus(class Id(name :: Symbol)) makes @rhombus(Id) work as a
constructor and also as a pattern form for @rhombus(match). The
@rhombus(class) form binds @rhombus(Id) as a function in the expression
space and also as a pattern-matching operator in the binding space.
Since @rhombus(Id) is a binding form, it can be used as a
@rhombus(match) or @rhombus(def) pattern. It can even be used with
@rhombus(⊢).

@examples(
  ~eval: space_eval
  ~defn:
    class Id(name :: Symbol)
  ~defn:
    fun interp(env ⊢ Id(name)):
      env[name]
  ~repl:
    interp({#'x: 1, #'y: 2} ⊢ Id(#'y))
)

@section{Parsing in Specific Spaces}

The @rhombus(rx) form itself is parsed in the expression space, but it
needs to parse the inside of subsequent @quotes in the regular-expression space.
Similarly, @rhombus(def), @rhombus(fun), and @rhombus(match) start out
in definition and expression spaces, but they need to parse portions of
their input as bindings. As one more example of a space, we will consider 
@deftech{annotations}, which appear after @rhombus(::, ~bind) in bindings.
For example, @rhombus(Int, ~annot) is an @tech{annotation}, and the
@rhombus(::, ~bind) form parses an annotation on its right-hand side to
impose a constraint on it's left-hand side:

@examples(
  ~repl:
    ~error:
      def n :: Int = "apple"
    def n :: Int = 5
)

@margin_note_block{The use of @rhombus(::, ~unquote_bind) in a
 syntax-pattern escape to specify a @tech{syntax class} is not quite the
 same as using @rhombus(::, ~bind) in a binding to specific a
 @tech{annotation}, but they're similar enough that Rhombus uses the same
 operator name in those different contexts. In other words,
 @deftech{unquote bindings} and @defterm{syntax classes} are two more
 examples of spaces.}

Note that a @rhombus(class) form like @rhombus(class Id(name :: Symbol))
binds @rhombus(Id) not only as a constructor for expressions and a
pattern-matching form for bindings, but also as an @tech{annotation} to
impose a constraint that an object is an instantiation of the class. All of these
spaces work together via macros that are bound in one space and bridge to
another space.

In order for a macro to trigger parsing in a specific space, it can use
a @tech{syntax class} associated with that space. The @rhombus(annot_meta.Parsed, ~stxclass)
syntax class triggers annotation parsing, for example.

@examples(
  ~eval: space_eval
  ~defn:
    expr.macro 'identity_at($(ann :: annot_meta.Parsed))':
      'fun (x :: $ann): x'
  ~repl:
    def add0 = identity_at(Int)
    add0(1)
    ~error:
      add0("apple")
    ~error:
      identity_at(0)
)

This @rhombus(identity_at) macro would work about as well if
@rhombus(ann) had no annotation and were simply spliced into the
generated @rhombus(fun) form as a group. Specifying the
@rhombus(annot_meta.Parsed, ~stxclass) syntax class ensures that the
annotation is parsed early and cannot be accidentally or unexpectedly
treated as anything other than an annotation in the expansion of
@rhombus(identity_at).

Once some syntax has been parsed via a @tech{syntax class}, the syntax
class offers operations that make sense for syntax in that specific class.
For example, the @rhombus(annot_meta.unpack_predicate) function
supports introspection on a parsed
annotation to extract a corresponding runtime predicate. The full
details of the mechanism are beyond the scope of this
tutorial, but those kinds of facilities are used by @rhombus(::) to
extract a run-time predicate associated with a parsed annotation or by
@rhombus(rx) to extract the meaning of a parsed regular expression.

@section(~tag: "macro-patterns"){Macro Patterns}

A syntax class like @rhombus(annot_meta.Parsed, ~stxclass) or
@rhombus(expr_meta.Parsed, ~stxclass) can be used only in places where
the @rhombus(Group, ~stxclass) @tech{syntax class} would be allowed. In
the @rhombus(identity_at) example above,
@rhombus(annot_meta.Parsed, ~stxclass) works because
@rhombus($(ann :: annot_meta.Parsed), ~unquote_bind) is alone within its
@bnf.nt{group}, so it can match the whole @bnf.nt{group}. The definition
without parentheses in the pattern around the use of the syntax class:


@rhombusblock(
  expr.macro 'identity_at $(ann :: annot_meta.Parsed)':
    'fun (x :: $ann): x'
)

would not be allowed, because @tech{enforestation} would not know where
to stop trying to parse an annotation and where to start parsing a
subsequent expression, which would be relevant for input such as

@rhombusblock(identity_at Int (1))

Instead, the macro pattern @rhombus('identity_at $(ann :: annot_meta.Parsed)', ~bind)
is statically rejected as having
@rhombus(annot_meta.Parsed, ~stxclass) in a disallowed position.

One way to resolve the ambiguity is to disallow an expression immediately
after @rhombus(identity_at). That choice can be implemented by making
the pattern unambiguously consume all remaining terms and, once that's done,
then requiring them to parse as an annotation:

@examples(
  ~eval: space_eval
  ~defn:
    expr.macro 'identity_at $term ...':
      match '$term ...'
      | '$(ann :: annot_meta.Parsed)':
          'fun (x :: $ann): x'
  ~repl:
    def double_reverse = identity_at List.of(Int)
    double_reverse([1, 2, 3])
    ~error:
      double_reverse(0)
)

These complexities illustrate why @rhombus(expr.macro, ~defn)
automatically treats a pattern @rhombus('$a op $b', ~bind) as demanding
@rhombus(expr_meta.Parsed, ~stxclass) parsing for @rhombus($a, ~bind)
and @rhombus($b, ~bind). It's complex, at best, to write that constraint
directly. Furthermore, automatic expression parsing for the left-hand
input to an infix macro is necessary for the infix operator to be
discovered at all. Automatic parsing is not always required for
right-hand side of a prefix or infix operator, however, and the additional flexibility
is needed for @rhombus(identity_at). Other syntax may use the flexibility
in a different way. For example, the @rhombus(.)
expression operator expects a field or method name on its right,
not another expression.

Overall, the macro-definition form @rhombus(expr.macro, ~defn) (and others like it)
interprets a macro pattern to define @rhombus(name, ~var) in the
following ways:

@tabular(
  ~sep: hspace(1),
  ~column_properties: [#'top],
  [[hspace(1), @rhombus('$#,(@rhombus(id, ~var)) #,(@rhombus(name, ~var)) $#,(@rhombus(id, ~var))'),
    @para{infix with parsed left and parsed right}],
   [hspace(1), @rhombus('$#,(@rhombus(id, ~var)) #,(@rhombus(name, ~var)) #,(@rhombus(any_pat, ~var)) $()'),
    @para{infix with parsed left and remainder of @bnf.nt{group} matched to @rhombus('#,(@rhombus(any_pat, ~var))')}],
   [hspace(1), @rhombus('$#,(@rhombus(id, ~var)) #,(@rhombus(name, ~var)) #,(@rhombus(tail_pat, ~var))'),
    @para{infix with parsed left and remainder of @bnf.nt{group} matched to @rhombus('#,(@rhombus(tail_pat, ~var))'),
          where a @rhombus(tail_pat, ~var) is one that ends with @rhombus(...), a @bnf.nt{block} pattern, or an @bnf.nt{alts} pattern}],
   [hspace(1), @rhombus('$#,(@rhombus(id, ~var)) #,(@rhombus(name, ~var)) #,(@rhombus(other_pat, ~var))'),
    @para{infix with parsed left and greedy match to @rhombus(other_pat, ~var)}],
   [@hspace(1), @tt{ }, @tt{ }],
   [hspace(1), @rhombus('#,(@rhombus(name, ~var)) $#,(@rhombus(id, ~var))'),
    @para{prefix with parsed right}],
   [hspace(1), @rhombus('#,(@rhombus(name, ~var)) #,(@rhombus(any_pat, ~var)) $()'),
    @para{prefix with remainder of @bnf.nt{group} matched to @rhombus('#,(@rhombus(any_pat, ~var))')}],
   [hspace(1), @rhombus('#,(@rhombus(name, ~var)) #,(@rhombus(tail_pat, ~var))'),
    @para{prefix with remainder of @bnf.nt{group} matched to @rhombus('#,(@rhombus(tail_pat, ~var))'),
          where a @rhombus(tail_pat, ~var) is one that ends with @rhombus(...), a @bnf.nt{block} pattern, or an @bnf.nt{alts} pattern}],
   [hspace(1), @rhombus('#,(@rhombus(name, ~var)) #,(@rhombus(other_pat, ~var))'),
    @para{prefix with greedy match to @rhombus(other_pat, ~var)}],
   ]   
)

@section(~tag: "ex-meta-parsed"){Exercise}

In @local_file("storage.rhm"), the @rhombus(storage) macro defines an
identifier as a function that accepts 0 or 1 arguments. When the
function receives 0 argument,  it returns the current value in storage.
When it receives 1 argument, it sets the storage content.

An annotation guards all accesses and updates of the storage, including
the initial value. But the @rhombus(storage) implementation is inefficient
in a way, because it copies terms to describe an annotation into three
places, which means that the annotation is parsed three times.

When @rhombus(storage) expands, it prints ``expand expr'', and when the
annotation @rhombus(NoisyInt) expands to just @rhombus(Int, ~annot), it
prints ``expand annot''. The way @rhombus(storage) copies an unparsed
annotation is reflected by ``expand annot'' printing more times than
``expand expr''.

Fix the macro so that the number of ``expand annot'' printouts matches
the number of ``expand expr'' printouts, instead of being three times as
many.

Solution: @local_file("storage_soln.rhm").

@section(~tag: "ex-macro-pat"){Exercise}

The interpreter in @local_file("interp_macro_pattern.rhm") defines a
@rhombus(=>) macro to make recursive calls in @rhombus(interp) a little
prettier.

Unfortunately, the expansion of @rhombus(=>) interferes with the tail
call that should be in the function returned by the
@rhombus(Fun(arg, body)) case. As a result, the Ω example at the end
(commented out) consumes memory without bound, instead of running in
constant space.@margin_note{DrRacket shows its heap size at the bottom
 right. Before trying Ω, make sure you have a memory limit in place as
 reported in DrRacket's REPL interaction pane!}

Adjust the @rhombus(=>) macro so that it recognizes when the right-hand
side of @rhombus(=>) is a single identifier, and in that case, ensure
that @rhombus(interp) is in tail position within the generated
expansion.

Solution: @local_file("interp_macro_pattern_soln.rhm").

@// ==================================================

@close_eval(space_eval)
