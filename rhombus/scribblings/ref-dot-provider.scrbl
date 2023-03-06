#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@title{Dot Providers}

@doc(
  space.transform dot
){

 The @tech{space} for bindings of identifiers as @tech{dot providers}.

}

@doc(
  ~nonterminal:
    id_macro_patterns: defn.macro

  defn.macro 'dot.macro $id_macro_patterns'
){

 Similar to @rhombus(defn.macro, ~expr), but binds a @tech{dot provider} that
 is normally referenced indirectly via @tech{static information},
 instead of directly. The @rhombus(pattern, ~var) sequence after the leading
 @rhombus(defined_name, ~var) should match a sequence of three
 terms: a parsed left-hand expression, a @rhombus(.) term, and a
 right-hand identifier. The @rhombus(defined_name, ~var) is bound in the
 @rhombus(dot, ~space) @tech{space}.

 The result must be either @rhombus(#false) or a syntax object. A
 @rhombus(#false) result means that static resolution failed, in which
 case the @rhombus(.) operator will generate a fallback lookup that is
 dynamic and generic---unless the @rhombus(.) operator is in static
 mode, in which case it will report a syntax error. A syntax-object
 result provides an expression form (that normally includes the
 left-hand expression) to replace the @rhombus(.) expression.

}

@doc(
  ~nonterminal:
    id_macro_patterns: defn.macro

  defn.macro 'dot.macro_more_static $id_macro_patterns'
){

 Like @rhombus(dot.macro), but the @rhombus(pattern, ~var) sequence after
 the leading @rhombus(defined_name, ~var) should match a sequence
 of at least four terms: a boolean literal indicating whether
 the @rhombus(.) is in static mode (see @rhombus(use_static)), a
 parsed left-hand expression, a @rhombus(.) term, and a right-hand
 identifier. Furthermore, the pattern input may have additional terms
 that appear after the right-hand identifier. Like
 @rhombus(expr.macro), the body result should be two values: an
 expression as the expansion of the @rhombus(.) form plus a sequence
 of remaining terms that were not consumed by the expansion.

 The expansion result can be @rhombus(#false) to indicate that static
 resolution failed. Two @rhombus(#false) result values are treated the
 same as a single false result, while a single non-@rhombus(#false)
 result is not allowed. When @rhombus(#false) is returned, a dynamic
 fallback is used only if the @rhombus(.) operator is not in static
 mode. At the same time, the implementation of the macro can use the
 literal boolean at the start of the input to parse input differently
 in static and dynamic modes (e.g., to require parentheses after the
 right-hand name to form a method-like call).
 
}
