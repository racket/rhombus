#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@(def dollar: @rhombus($))

@title{Simple Expression Macros}

@doc(
  defn.macro 'macro $macro_patterns'
  expr.macro 'macro $macro_patterns'
  entry_point.macro 'macro $macro_patterns'

  grammar macro_patterns:
    $macro_pattern:
      $option; ...
      '$template'
    Z| $macro_pattern:
         $option; ...
         '$template'
     | ...

  grammar macro_pattern:
    '$defined_name $pattern ...'
    '$defined_name #,(dollar)(~parsed $right_identifier)'
    '$ $left_pattern $defined_name $pattern ...'
    '$ $left_pattern $defined_name #,(dollar)(~parsed right_identifier)'
  grammar defined_name:
    $identifier
    $operator
    #,(dollar)('#,(dollar)')
    ($identifier_path)
    ($operator_path)
    ()
  grammar left_pattern:
    identifier
    (~parsed $identifier)
  grammar option:
    ~stronger_than: $identifier_or_operator ...
    ~weaker_than: $identifier_or_operator ...
    ~same_as: $identifier_or_operator ...
    ~same_on_left_as: $identifier_or_operator ...
    ~same_on_right_as: $identifier_or_operator ...
    ~associativity: $assoc
    ~op_stx: $identifier
  grammar assoc:
    ~left
    ~right
    ~none
){

 As a definition form, @rhombus(macro) defines the @rhombus(defined_name)
 (which is an operator or identifier) within @rhombus(macro_pattern) as a
 pattern-based macro whose expansion is described by a
 @rhombus(template). When @rhombus(defined_name) is a plain
 @rhombus(operator), it cannot be @rhombus($), but the form
 @rhombus($('$')) can be used to define @rhombus($). A
 @rhombus(defined_name) cannot be @rhombus(()) for a @rhombus(macro)
 defintition. The @rhombus(defined_name) is bound in the
 @rhombus(expr, ~space) @tech{space}.

 As an expression or @tech{entry point}, @rhombus(macro) is a shorthand
 for a function that expects a syntax object to match, as explained
 @elemref("macro-entry-point"){further below}. The @rhombus(expr.macro)
 form is similar to @rhombus(macro); it allows more general compile-time
 code, but it also requires that @rhombusmodname(rhombus/meta) is
 imported.

 For a macro defined with @rhombus(macro), @rhombus(macro_pattern) is
 matched to a sequence witin a group. Within the @rhombus('') of each
 @rhombus(macro_pattern), either the first term is a
 @rhombus(defined_name) to be defined as a prefix macro, or the first
 term is a single @rhombus($, ~bind) escape followed by a
 @rhombus(defined_name) to be defined as an infix macro.

 In the case of a prefix macro, the left-hand @rhombus($, ~bind) escape
 can be an identifier or one that is wrapped with parentheses and
 @rhombus(~parsed). Declaring @rhombus(~parsed) is redundant, because the
 left-hand side must always be parsed to discover an infix operator, but
 it is allowed for cosistency with the right-hand side. The right-hand
 side of the macro (for either a prefix or infix form) can be an
 arbitrary pattern or a @rhombus(~parsed) escape:

@itemlist(

 @item{When the right-hand side is represented by an arbitrary pattern,
  the pattern can match any sequence of terms after the macro name in its
  enclosing group.}
  
 @item{When @rhombus(~parsed) is used for the the right-hand side, then
  the macro is applied with an already-parsed term after the macro name in
  a use of the macro. That parse heeds precedence and associativity
  declarations for other macros and for operators defined with
  @rhombus(operator).}

)

 In either case, a match need not extend to the end of the group, and
 the end of the right-hand side of the pattern does not count as the end
 of the group where, say, a @rhombus(Group, ~stxclass) pattern can
 appear. In a use of the pacro, the enclosing group might continue, such
 as with an infix operator.

 Using @litchar{|} alternatives, a single definition can have any number
 of @rhombus(macro_pattern)s. The patterns describe any number of prefix
 and infix variants that are (presumably) distinguished by patterns that
 are tried in order. The name to define must be the same across all
 @rhombus(macro_pattern)s. If @rhombus(~parsed) is used for the the
 right-hand side in a @rhombus(macro_pattern) of an infix or prefix form,
 then it must be the only infix or prefix @rhombus(macro_pattern) among
 the alternatives.

 The body after each @rhombus(macro_pattern) must be an immediate
 @rhombus('') template, and any @rhombus($) escape within the template
 can only refer to an input pattern variable or a literal syntax
 object, optionally parenthesized, for an operator (e.g.,
 @rhombus($('$')) to generate a literal @rhombus($)). More general
 compile-time expressions are not allowed; use @rhombus(expr.macro)
 or @rhombus(expr.macro), instead, to enable compile-time expressions.

 Before the @rhombus(template) body of a @rhombus(macro_pattern),
 @rhombus(option) keywords can appear. The options
 @rhombus(~weaker_than), @rhombus(~stronger_than), @rhombus(~same_as),
 @rhombus(~same_on_left_as), and @rhombus(~same_on_right_as) declare
 an name's precedence relative to other names. The
 @rhombus(~associativity) option is allowed only with an infix
 @rhombus(macro_pattern). The @rhombus(~op_stx) option binds an
 identifier or operator as it appears in a use of the macro (which cannot be
 matched directly in the @rhombus(macro_pattern), since that position
 is used for the name that @rhombus(expr.macro) binds).
 In a defined with @litchar{|} alternatives,
 most @rhombus(option)s are allowed only in the first case, but
 @rhombus(~op_stx) is allowed and separate in each case.

 See @secref("namespaces") for information on @rhombus(identifier_path)
 and @rhombus(operator_path).

@examples(
  ~defn:
    macro 'thunk: $body':
      'fun (): $body'
  ~repl:
    thunk: "ok"
    (thunk: "ok")()
)

 An @elemtag("macro-entry-point"){expression or entry-point}
 @rhombus(macro) describes a function that takes a syntax object and
 returns two syntax objects: the results produced by
 @rhombus(template) and the tail of an enclosing group that follows the
 match to @rhombus(pattern). The @rhombus(defined_name) position must be
 @rhombus(()), the pattern form must be a prefix (not infix) pattern,
 @rhombus(~parsed) is not allowed in a pattern, and the only allowed
 @rhombus(option) is @rhombus(~op_stx). For the purpose of matching a
 syntax object passed to the function produced by @rhombus(macro), the
 leading @rhombus(()) in each pattern is replaced by @rhombus(_, ~bind),
 and a @rhombus($tail ..., ~bind) pattern is added to the end; the result
 values are the syntax object produced by @rhombus(template) and
 @rhombus('$tail ...').

@examples(
  ~defn:
    def converter:
      macro '() [$x, ...]':
        'handle_list([$x, ...])'
  ~repl:
    converter('ListLike[1, 2, 3].more')
)

}


