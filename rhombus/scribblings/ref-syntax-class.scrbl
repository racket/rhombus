#lang scribble/rhombus/manual
@(import: 
    "common.rhm" open    
    "nonterminal.rhm" open
    "macro.rhm")

@(def dots: @rhombus(..., ~bind))
@(fun list(x, ...): [x, ...])

@title{Syntax Classes}

@doc(
  ~nonterminal:
    id_bind: def bind
    rest_id: block id
    syntax_pattern: #%quotes pattern

  defn.macro 'syntax_class $id $maybe_args
              | $pattern_case
              | ...'
  defn.macro 'syntax_class $id $maybe_args:
                $class_clause
                ...'

  grammar maybe_args:
    ($id_bind, ...)
    ($id_bind, ..., & $rest_id)
    #,(epsilon)

  grammar class_clause:
    #,(@rhombus(pattern, ~syntax_class_clause)) $pattern_cases
    #,(@rhombus(description, ~syntax_class_clause)) $desc_rhs
    #,(@rhombus(error_mode, ~syntax_class_clause)) $error_mode_rhs
    #,(@rhombus(kind, ~syntax_class_clause)) $kind_rhs
    #,(@rhombus(fields, ~syntax_class_clause)): $id ...; ...
    #,(@rhombus(root_swap, ~syntax_class_clause)): $id $id

  grammar pattern_cases:
    $pattern_case
    Z| $pattern_case
     | ...

  grammar pattern_case:
    $syntax_pattern
    $syntax_pattern: $pattern_body; ...

  grammar pattern_body:
    #,(@rhombus(field, ~pattern_clause)) $field_decl
    #,(@rhombus(match_def, ~pattern_clause)) $also_decl
    #,(@rhombus(match_when, ~pattern_clause)) $when_rhs
    #,(@rhombus(match_unless, ~pattern_clause)) $unless_rhs
    $body
){

 Defines a @deftech{syntax class} that can be used in syntax patterns with
 @rhombus(::, ~unquote_bind). A syntax class can optionally have arguments, in which
 case every use of the syntax class with @rhombus(::, ~unquote_bind) must supply
 arguments; an @rhombus(id_bind) is like a @rhombus(kwopt_binding, ~var) for
 @rhombus(fun), but each binding must be a plain @rhombus(id) (i.e., annotations
 and general pattern matching are not supported). Identifiers bound as arguments
 are visible in @rhombus(clause) bodies.

 Syntax forms matched by the syntax class are described by
 @rhombus(pattern_case) alternatives. The
 @rhombus(pattern, ~syntax_class_clause) clause is optional in the sense
 that pattern alternatives can be inlined directly in the
 @rhombus(syntax_class) form, but the @rhombus(pattern, ~syntax_class_clause)
 clause form makes room for additional options as clauses. Each kind of
 @rhombus(class_clause) alternative can be supplied at most once, and
 @rhombus(pattern, ~syntax_class_clause) is required.

 An optional @rhombus(description,  ~syntax_class_clause) clause
 provides a description of the syntax class, which is used to produce
 clearer error messages when a term is rejected by the syntax class. The
 result of the @rhombus(block) block must be a string or
 @rhombus(#false), where @rhombus(#false) is equivalent to not specifying
 a @rhombus(description, ~syntax_class_clause). When
 @rhombus(error_mode, ~syntax_class_clause) is declared as
 @rhombus(~opaque), then parsing error messages will not refer to the
 interior details of the pattern cases; insteda, messages will use the
 decsription string.

 An optional @rhombus(kind, ~syntax_class_clause) declaration indicates
 the context within a
 pattern where a syntax class can be used, and it determines the kind
 of match that each pattern specifies. See @rhombus(kind, ~syntax_class_clause)
 for details. The default is inferred from the shapes for @rhombus(pattern_case)s as
 either @rhombus(~term), @rhombus(~sequence), or @rhombus(~multi).

 A @rhombus(fields, ~syntax_class_clause) declaration limits the set of pattern variables that
 are accessible from the class, where variables used in all
 @rhombus(pattern_case)s are otherwise available (as described next).
 Each identifier in @rhombus(fields, ~syntax_class_clause) must be a field name that would be
 made available. A @rhombus(root_swap, ~syntax_class_clause) class moves
 the value of one of the would-be fields to the root while moving the root
 to a fresh feild; see @rhombus(root_swap, ~syntax_class_clause) for more information.

 The @rhombus(pattern_case) alternatives are the main content
 of a syntax class.
 After the class @rhombus(name) is defined, then when a
 variable @rhombus(id, ~var) is bound through a
 @seclink("stxobj"){syntax pattern} with
 @rhombus($(#,(@rhombus(id, ~var)) :: #,(@rhombus(name, ~var)))),
 it matches a syntax object that matches any of the
 @rhombus(pattern_case)s in the definition of
 @rhombus(stx_class_id ,~var), where the @rhombus(pattern_case)s are tried
 first to last. A pattern variable that is included in all of the
 @rhombus(pattern_case)s is a field of the syntax class, which is
 accessed from a binding @rhombus(id, ~var) using dot notation. For
 example, if the pattern variable is @rhombus(var, ~var), its value is
 accessed from @rhombus(id, ~var) using
 @list(@rhombus(id, ~var), @rhombus(.), @rhombus(var, ~var)).

 A @rhombus(pattern_case) matches when

@itemlist(

 @item{the @rhombus(syntax_pattern) at the start of the
  @rhombus(pattern_case) matches;}

 @item{every @rhombus(match_def, ~pattern_clause) clause within the
  @rhombus(pattern_case) body also matches;}

 @item{every @rhombus(match_when, ~pattern_clause) clause within the
  @rhombus(pattern_case) body has a true value for its right-hand side;
  and}

 @item{every @rhombus(match_unless, ~pattern_clause) clause within
  the @rhombus(pattern_case) body has a false value for its right-hand
  side.}

)

 Every pattern variable in the initial @rhombus(syntax_pattern) of a
 @rhombus(pattern_case) as well as evey variable in every
 @rhombus(match_def, ~pattern_clause) is a candiate field name, as
 long as it is also a candiate in all other @rhombus(syntax_pattern)s
 within the syntax class. In addition, names declared with
 @rhombus(field, ~pattern_clause) are also candidates. A field must have
 the same repetition depth across all pattern cases, unless it is
 excluded from the syntax class's result through a
 @rhombus(fields, ~syntax_class_clause) declaration that does not list
 the field.

 The body of a @rhombus(pattern_case) can include other definitions and
 expressions. Those definitions and expressions can use pattern variables
 bound in the main @rhombus(syntax_pattern) of the case as well as any
 preceding @rhombus(match_def, ~pattern_clause) clause or a field
 declared by a preceding @rhombus(field, ~pattern_clause). Consecutive
 definitions and expressions within a @rhombus(pattern_case) form a
 definition context, but separated sets of definitions and expressions
 can refer only to definitions in earlier sets.

 A variable bound with a syntax class (within a syntax pattern) can be
 used without dot notation. The variable is bound to a syntax object
 corresponding to the entire match of a @rhombus(syntax_pattern).

@examples(
  ~eval: macro.make_for_meta_eval()
  meta:
    syntax_class Arithmetic
    | '$x + $y'
    | '$x - $y'
  expr.macro 'doubled_operands $(a :: Arithmetic)':
    '$a.x * 2 + $a.y * 2'
  doubled_operands 3 + 5
  expr.macro 'add_one_to_expression $(a :: Arithmetic)':
    '$a + 1'
  add_one_to_expression 2 + 2
  meta:
    syntax_class NTerms
    | '~one $a':
        field b = '0'
        field average = '$(Syntax.unwrap(a) / 2)'
    | '~two $a $b':
        def sum:
          Syntax.unwrap(a) + Syntax.unwrap(b)
        field average = '$(sum / 2)'
  expr.macro 'second_term $(e :: NTerms)':
    e.b
  second_term ~two 1 2
  second_term ~one 3
  expr.macro 'average $(e :: NTerms)':
    e.average
  average ~two 24 42
)

}


@doc(
  ~nonterminal:
    pattern_cases: syntax_class pattern_cases
    pattern_case: syntax_class pattern_case
                   
  syntax_class_clause.macro 'pattern $pattern_cases'
  bind.macro 'pattern $pattern_cases'
){

 The @rhombus(pattern, ~syntax_class_clause) clause form in
 @rhombus(syntax_class) describes the patterns that the class matches;
 see @rhombus(syntax_class) for more information. A
 @rhombus(pattern, ~syntax_class_clause) class with only a
 @rhombus(pattern_case) is a shorthand for writing the
 @rhombus(pattern_case) in a single @litchar{|} alternative.

 The binding variant of @rhombus(pattern , ~syntax_class_clause) (for
 direct use in a binding context) has the same syntax and matching rules
 as a @rhombus(pattern, ~syntax_class_clause) form in
 @rhombus(syntax_class), but with all fields exposed. In particular, a
 @rhombus(pattern_case) can have a block with
 @rhombus(match_def, ~pattern_clause),
 @rhombus(match_when, ~pattern_clause), and
 @rhombus(match_unless, ~pattern_clause) clauses that refine the match.

@examples(
  ~defn:
    fun simplify(e):
      match e
      | '($e)': simplify(e)
      | '0 + $e': simplify(e)
      | '$e + 0': simplify(e)
      | (pattern '$a + $b - $c':
           match_when same(simplify(b), simplify(c))):
          simplify(a)
      | (pattern '$a - $b + $c':
           match_when same(simplify(b), simplify(c))):
          simplify(a)
      | ~else: e
  ~defn:
    fun same(b, c):
      b.unwrap() == c.unwrap()
  ~repl:
    simplify('(1 + (2 + 0) - 2)')
    simplify('1 + 2 + 2')
)

}

@doc(
  syntax_class_clause.macro 'fields:
                               $id ...
                               ...'
){

 Limits the set of fields that are provided by a syntax class to the
 listed @rhombus(id)s. See @rhombus(syntax_class).

}


@doc(
  syntax_class_clause.macro 'root_swap: $id $id'
){

 Adjusts the match value and fields of a syntax class. The first
 @rhombus(id) names a field that te syntax class would otherwise
 provide, but the field is removed, and its value instead becomes the
 main value of an identifier that is bound with the syntax class.
 Meanwhile, the matching terms that would otherwise be the variable's
 value are associated instead with a fresh field named by the second
 @rhombus(id).

@examples(
  ~defn:
    syntax_class Parenthesized:
      root_swap: content group
      pattern
      | '($content)'
  ~repl:
    match '(1 2 3)'
    | '$(p :: Parenthesized)':
        [p, p.group]
)

}

@doc(
  syntax_class_clause.macro 'description:
                               $body;
                               ...'
  syntax_class_clause.macro 'description $expr'
){

 Configures a syntax class's description for error reporting. See
 @rhombus(syntax_class).

}


@doc(
  syntax_class_clause.macro 'error_mode: $error_mode_keyword'
  syntax_class_clause.macro 'error_mode $error_mode_keyword'
  grammar error_mode_keyword:
    ~opaque
    ~transparent
){

 Configures the way that failures to match a syntax class are reported.
 See @rhombus(syntax_class).

}


@doc(
  syntax_class_clause.macro 'kind: $kind_keyword'
  syntax_class_clause.macro 'kind $kind_keyword'

  grammar kind_keyword:
    ~term
    ~sequence
    ~group
    ~multi
    ~block
){

@provided_also_meta()

 Determines the contexts where a syntax class can be used and the kinds
 of matches that it produces:

@itemlist(

 @item{@rhombus(~term): each pattern case represents a single term, and
  the syntax class can be used in the same way at the
  @rhombus(Term, ~stxclass) syntax class.}

 @item{@rhombus(~sequence): each pattern case represents a sequence of
  terms that is spliced within a group. This is the default mode of a
  syntax class when no @rhombus(kind) is specified.}

 @item{@rhombus(~group): each pattern case represents a @tech{group},
  and the syntax class can be used in the same places as
  @rhombus(Group, ~stxclass) (i.e., at the end of an enclosing group).}

 @item{@rhombus(~multi): each pattern case represents multiple groups, and the
  syntax class can be used in the same way at the
   @rhombus(Multi, ~stxclass) syntax class.}

 @item{@rhombus(~block): each pattern case represents a block, and the
  syntax class can be used in the same way at the
  @rhombus(Block, ~stxclass) syntax class}
)

 With @rhombus(~term), each pattern case must match only a single term,
 and with @rhombus(~block), each pattern case must be a block pattern.

 See also @rhombus(syntax_class).

}


@doc(
  pattern_clause.macro 'field $id_maybe_rep:
                          $body
                          ...'
  pattern_clause.macro 'field $id_maybe_rep = $expr'
  
  grammar id_maybe_rep:
    $id
    [$id_maybe_rep, $ellipsis]

  grammar ellipsis:
    #,(dots)
){

 Similar to @rhombus(def), but restricted to defining a plain identifier
 or a simple list repetition within a @rhombus(syntax_class) pattern
 case, and adds a field (or, at least, a cadndidate field) to the pattern
 case.

 The result of the right-hand @rhombus(body) sequence or @rhombus(expr)
 is not required to be a syntax object or have syntax objects in nested
 lists. If the field is referenced so that it's value is included in a
 syntax template, a non-sytax value is converted to syntax at that point.
 Otherwise, the field can be used directly to access non-syntax values.

 See also @rhombus(syntax_class).

}

@doc(
  pattern_clause.macro '«match_def '$pattern':
                           $body
                           ...»'
  pattern_clause.macro '«match_def '$pattern' = $expr»'
){

 Constrains a pattern case to match only when the right-hand
 @rhombus(body) or @rhombus(expr) matches @rhombus(pattern), and adds the
 pattern variables of @rhombus(pattern) to the set of (candidate) syntax
 class fields.

 See @rhombus(syntax_class).

}

@doc(
  pattern_clause.macro 'match_when:
                          $body
                          ...'
  pattern_clause.macro 'match_when $expr'

  pattern_clause.macro 'match_unless:
                          $body
                          ...'
  pattern_clause.macro 'match_unless $expr'
){

 Constrains a pattern case to match only when a @rhombus(body) or
 @rhombus(expr) produces a true value in the case of
 @rhombus(match_when, ~pattern_clause) or a false value in the case of
 @rhombus(match_unless, ~pattern_clause).

 See @rhombus(syntax_class).

}

@doc(
  syntax_class Term: #,(@rhombus(kind, ~syntax_class_clause)): ~term
  syntax_class Identifier: #,(@rhombus(kind, ~syntax_class_clause)): ~term
  syntax_class Operator: #,(@rhombus(kind, ~syntax_class_clause)): ~term
  syntax_class Name: #,(@rhombus(kind, ~syntax_class_clause)): ~term
  syntax_class Keyword: #,(@rhombus(kind, ~syntax_class_clause)): ~term
  syntax_class String: #,(@rhombus(kind, ~syntax_class_clause)): ~term
  syntax_class Int: #,(@rhombus(kind, ~syntax_class_clause)): ~term
  syntax_class Group: #,(@rhombus(kind, ~syntax_class_clause)): ~group
  syntax_class Multi: #,(@rhombus(kind, ~syntax_class_clause)): ~multi
  syntax_class Block: #,(@rhombus(kind, ~syntax_class_clause)): ~block
){

@provided_also_meta()

 Syntax classes, all of which imply a single-term match except for
 @rhombus(Group, ~stxclass), @rhombus(Multi, ~stxclass), and
 @rhombus(Block, ~stxclass).

 The @rhombus(Group, ~stxclass) syntax class can be used only for a
 pattern identifier that is at the end of its group in a pattern. The
 identifier is bound to a match for the entire tail of the group as a
 group syntax object.

 The @rhombus(Multi, ~stxclass) syntax class can be used only for a
 pattern identifier that is the sole term where a sequence of groups is
 allowed, such as in the body of a block. The identifier is bound to a
 match for the entire sequence of groups.

 The @rhombus(Block, ~stxclass) syntax class can be used only for a
 pattern identifier that is the sole term of a block. The identifier is
 bound to a match for the entire block as a single term (i.e., as a
 single-term syntax object that has a block term, and not as a
 multi-group syntax object).

 The @rhombus(Name, ~stxclass) syntax class is the union of
 @rhombus(Identifier, ~stxclass) and @rhombus(Operator, ~stxclass).

}
