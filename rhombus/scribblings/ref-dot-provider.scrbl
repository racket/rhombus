#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Dot Providers}

@doc(
  space.transform dot
){

 The @tech{space} for bindings of identifiers as @tech{dot providers}.

}

@doc(
  ~nonterminal:
    prefix_macro_patterns: defn.macro ~defn
    defined_name: defn.macro ~defn

  defn.macro 'dot.macro $prefix_macro_patterns'

  grammar option:
    ~op_stx: $id
    ~op_stx $id
    ~is_static: $id
    ~is_static $id
    ~tail: '$pattern'
    ~tail '$pattern'
){

 Similar to @rhombus(defn.macro), but binds a @tech{dot provider} that
 is normally referenced indirectly via @tech{static information},
 instead of directly. The @rhombus(pattern) sequence after the leading
 @rhombus(defined_name) should match a sequence of three
 terms: a parsed left-hand expression, a @rhombus(.) term, and a
 right-hand identifier. The @rhombus(defined_name) is bound in the
 @rhombus(dot, ~space) @tech{space}.

 Two extra @rhombus(option)s are supported: @rhombus(~is_static) and
 @rhombus(~tail). The identifier for @rhombus(~is_static) is bound to
 @rhombus(#true) or @rhombus(#false), depending on whether the use of
 @rhombus(.) to reach the provider was a static or dynamic dot; see
 @rhombus(use_static). The pattern for @rhombus(~tail) is matched to the
 tail of the enclosing group after the @rhombus(.) and subsequent
 identifier. If the @rhombus(~tail) pattern doesn't match, then the case
 containing the @rhombus(~tail) pattern does not match, which is useful
 in a multi-case @rhombus(dot.macro) form.

 The result must be either @rhombus(#false), a syntax object, or two
 syntax-object values. A @rhombus(#false) result means that static
 resolution failed, in which case the @rhombus(.) operator will generate
 a fallback lookup that is dynamic and generic---unless the @rhombus(.)
 operator is in static mode, in which case it will report a syntax error.
 A (first) syntax-object result provides an expression form (that normally
 includes the left-hand expression) to replace the @rhombus(.)
 expression. When a second syntax-object result is provided, it is used
 as the remainder of the enclosing group for further processing, and the
 default result is the same tail that would be provided for an identifier
 declared with the @rhombus(~tail) option.

}


@doc(
  ~nonterminal:
    defined_id: block id
    left_id: block id
    obj_expr: block expr

  class_clause.macro '«dot '$ $left_id . $defined_id':
                         $option; ...
                         $body
                         ...»'
  interface_clause.macro '«dot '$ $left_id . $defined_id':
                             $option; ...
                             $body
                             ...»'
  veneer_clause.macro '«dot '$ $left_id . $defined_id':
                          $option; ...
                          $body
                          ...»'
  grammar option:
    ~op_stx: $id
    ~op_stx $id
    ~head_stx: $id
    ~head_stx $id
    ~is_static: $id
    ~is_static $id
    ~tail: '$pattern'
    ~tail '$pattern'
){

 A form for @rhombus(class), @rhombus(interface), or @rhombus(veneer) to bind a macro that
 is normally triggered by using the @rhombus(defined_id) after @rhombus(.) on an
 expression that has the class's or interface's annotation. The macro can also be
 triggered by @rhombus(#,(@rhombus(name, ~var)).defined_id(obj_expr)) for a class,
 interface, or veneer @rhombus(name, ~var) (more specifically, for the
 defined @tech{namespace}), in which case the @rhombus(obj_expr) is
 checked to be a correct instance.

 The pattern for @rhombus(dot, ~class_clause) is constrained to have an
 escape for @rhombus(left_id) as the left-hand expression (which has the
 class or interface annotation), a literal @rhombus(.), and then an
 identifier for @rhombus(defined_id).

 The @rhombus(option)s and result @rhombus(body) are as for
 @rhombus(dot.macro), except that @rhombus(~head_stx) is also allowed as
 an option. An identifier after @rhombus(~head_stx) is bound to the
 called form, either @rhombus(obj_expr.defined_id) or
 @rhombus(defined_id(obj_expr)).

}
