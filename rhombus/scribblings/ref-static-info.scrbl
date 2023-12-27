#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@title{Static Information}

@doc(
  space.transform statinfo
){

 The @tech{space} for bindings of identifiers that provide static
 information.

}


@doc(
  defn.macro '«statinfo.macro '$id':
                 $body
                 ...»'
){

 Binds @rhombus(id) in the static-information space to
 associate the static information produced by the @rhombus(body) block.
 This static information applies to a use of @rhombus(id) in the
 expression space. The static information produced by the
 @rhombus(body) block must be in unpacked form (i.e.,
 @rhombus(statinfo_meta.pack) is applied automatically). The
 @rhombus(id) is bound in the @rhombus(statinfo, ~space)
 @tech{space}.

 See @secref("annotation-macro") for an example.

}

@doc(
  fun statinfo_meta.wrap(expr_stx:: Syntax,
                         statinfo_stx :: Syntax)
    :: Syntax
){

 @provided_meta()

 Returns a syntax object for an expression equivalent to
 @rhombus(expr_stx), but with the static information
 @rhombus(statinfo_stx) attached. The @rhombus(statinfo_stx)
 information must be in unpacked form (i.e.,
 @rhombus(statinfo_meta.pack) is applied automatically).

 See @secref("annotation-macro") for an example.

}

@doc(
  fun statinfo_meta.pack(statinfo_stx :: Syntax) :: Syntax
){

 @provided_meta()

 Converts static information described by @rhombus(statinfo_stx) into
 an opaque internal format. The given @rhombus(statinfo_stx) must
 match the form

@rhombusblock(
   ((#,(@rhombus(key_id, ~var)), #,(@rhombus(val, ~var))), ...))

 Keys for static information are compared based on binding, not merely
 the key's symbolic form.

 See @secref("annotation-macro") for an example.

}

@doc(
  fun statinfo_meta.unpack(statinfo_stx :: Syntax) :: Syntax
){

 @provided_meta()

 The inverse of @rhombus(statinfo_meta.pack). This function is
 potentially useful to parse a result from
 @rhombus(statinfo_meta.lookup) for a key whose value is packed
 static information.

}

@doc(
  fun statinfo_meta.pack_group(statinfo_stx :: Syntax)
    :: Syntax
  fun statinfo_meta.unpack_group(statinfo_stx :: Syntax)
    :: Syntax
){

 @provided_meta()

 Analogous to @rhombus(statinfo_meta.pack) and
 @rhombus(statinfo_meta.unpack), but for a sequence of static information
 sets, such as one set of static information per value produced by a
 multiple-value expression (see @rhombus(statinfo_meta.values_key)).

 An unpacked sequence is represented as a group syntax object, where
 each term in the group is unpacked static information in the sense of
 @rhombus(statinfo_meta.pack).

}


@doc(
  fun statinfo_meta.lookup(expr_stx :: Syntax,
                           key :: Identifier)
    :: maybe(Syntax)
){

@provided_meta()

 Finds static information for @rhombus(expr_stx), which might be a
 name with static information associated to its binding, or it might be an
 expression with static information associated directly. The result is
 a syntax object for the value associated to @rhombus(key) in that
 static information, or @rhombus(#false) if no information or value
 for @rhombus(key) is available.

 When @rhombus(expr_stx) is an expression, consider using
 @rhombus(expr_meta.parse_more) to reveal additional static information
 by forcing an earlier expansion of the expression. Use the result in
 place of @rhombus(expr_stx), instead of continuing to use
 @rhombus(expr_stx) (which could cause parsing and expansion to happen a
 second time for the same expression). A potential drawback of forcing
 expansion is that it might constrain the order of definitions in a
 recursive definition context; as a convention to limit such problems,
 avoid expansion of terms that are nested within a block.

 Keys for static information are compared based on binding, not merely
 the key's symbolic form.

}

@doc(
  def statinfo_meta.call_result_key :: Identifier
  def statinfo_meta.index_result_key :: Identifier
  def statinfo_meta.index_get_key :: Identifier
  def statinfo_meta.index_set_key :: Identifier
  def statinfo_meta.append_key :: Identifier
  def statinfo_meta.dot_provider_key :: Identifier
  def statinfo_meta.sequence_constructor_key :: Identifier
  def statinfo_meta.sequence_element_key :: Identifier
  def statinfo_meta.values_key :: Identifier
){

 @provided_meta()

 Values that can be used to associate static information with an
 expression:

 @itemlist(

  @item{@rhombus(statinfo_meta.call_result_key) --- packed static
        information for the result value if the expression is used as
        a function to call}

  @item{@rhombus(statinfo_meta.index_result_key) --- packed static information
        for the result value if the expression is used with
        @rhombus([]) to access an element}

  @item{@rhombus(statinfo_meta.index_get_key) --- an identifier bound to a
        function to call (instead of falling back to a generic dynamic
        dispatch) when the expression is used with @rhombus([]) to
        access an element}

  @item{@rhombus(statinfo_meta.index_set_key) --- an identifier bound to a
        function to call (instead of falling back to a generic dynamic
        dispatch) when the expression is used with @rhombus([]) to
        update an element}

  @item{@rhombus(statinfo_meta.append_key) --- an identifier bound to a
        function to call (instead of falling back to a generic dynamic
        dispatch) when the expression is used with @rhombus(++) to
        append the result of another expression}

  @item{@rhombus(statinfo_meta.dot_provider_key) --- an identifier
        bound to a @rhombus(dot.macro) or
        @rhombus(dot.macro_more_static) to implement the expression's
        behavior as a @tech{dot provider}}

  @item{@rhombus(statinfo_meta.sequence_constructor_key) --- an identifier
        bound as a variable or a macro that is wrapped around an expression
        to create or specialize a sequence for @rhombus(for), or @rhombus(#true) to
        indicate that no wrapper is needed}

  @item{@rhombus(statinfo_meta.sequence_element_key) --- packed static information
        for the elements of the expression as a sequence used with
        @rhombus(each, ~for_clause); for a sequence with
        multiple values in each element, the static information can
        map @rhombus(statinfo_meta.values_key) to a group of per-value
        static information; the number of static information must be
        consistent with the bindings, otherwise no static information
        will be propageted at all; when @rhombus(statinfo_meta.sequence_element_key)
        is not specified, @rhombus(each, ~for_clause) uses
        @rhombus(statinfo_meta.index_result_key)}

  @item{@rhombus(statinfo_meta.values_key) --- a packed group of
        static information (see @rhombus(statinfo_meta.pack_group)),
        one for each value produced by a multiple-value expression}

)

 See @secref("annotation-macro") for examples using some of these keys.

}


@doc(
  class_clause.macro 'static_info: $body; ...'
  interface_clause.macro 'static_info: $body; ...'
  veneer_clause.macro 'static_info: $body; ...'
){

 A clause form for @rhombus(class), @rhombus(interface), or @rhombus(veneer) that adds
 static information associated with the class or interface name as an
 annotation (i.e., static information for instances of the class or
 interface). The @rhombus(body) sequence should produce static
 information in unpacked form, like the form accepted by
 @rhombus(statinfo_meta.pack).

}
