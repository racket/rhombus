#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def statinfo_key_defn = @rhombus(statinfo.key, ~defn))

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
 the key's symbolic form. Key identifiers should be defined with
 @rhombus(statinfo.key, ~defn).

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
 the key's symbolic form.  Key identifiers should be defined with
 @statinfo_key_defn.

}

@doc(
  fun statinfo_meta.gather(expr_stx :: Syntax) :: Syntax
){

 Returns all of the static information of @rhombus(expr_stx) in unpacked
 form. The returned static information corresponds to all the possible
 keys for which @rhombus(statinfo_meta.lookup) would return a value.

}

@doc(
  fun statinfo_meta.union(statinfo_stx :: Syntax, ...) :: Syntax
  fun statinfo_meta.intersect(statinfo_stx :: Syntax, ...) :: Syntax
){

 Takes static information in unpacked form and combines it into one set
 of static information in unpacked form, where the returned information
 is the union or intersection of all given information.

}


@doc(
  ~nonterminal:
    union_proc_expr: block expr
    intersect_proc_expr: block expr

  defn.macro '«statinfo.key $id:
                 $clause
                 ...»'
  grammar clause:
    ~union: union_proc_expr
    ~intersect: intersect_proc_expr
){

 Binds @rhombus(id) for use as a static info key identifier. Both
 @rhombus(union_proc_expr) and @rhombus(intersect_proc_expr) are
 required, and they should each produce a function that accepts two
 syntax objects as values for static information keyed by @rhombus('id').
 The union operation is used, for example, on static information from
 annotations combined with @rhombus(&&, ~annot), and interaction is used
 on static information from annotations combined with
 @rhombus(||, ~annot).

 Typically, a definition @rhombus(statinfo.key id) should be paired with
 a definition @rhombus(meta #,(rhombus(def)) #,(rhombus(id_key, ~var)) = 'id') so
 that @rhombus(id_key, ~var) can be used directly as a key, instead of
 @rhombus('id').

 When a static information key is not an identifier bound via
 @rhombus(statinfo.key, ~defn), then default union and intersection operations
 are used for the key's values. The default functions use the same merge
 operations as @rhombus(statinfo_meta.call_result_key), which means that
 they merge nested static information.

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
  def statinfo_meta.indirect_key :: Identifier
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

  @item{@rhombus(statinfo_meta.append_key) --- an identifier or a boxed identifier bound to a
        function to call (instead of falling back to a generic dynamic
        dispatch) when the expression is used with @rhombus(++) to
        append the result of another expression; for a boxed identifier,
        the application will be guarded by a check to ensure that both
        arguments share the same @rhombus(append, ~datum) implementation}

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

  @item{@rhombus(statinfo_meta.indirect_key) --- an identifier whose
        static information is lazily spliced in place of this key
        and identifier}

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
