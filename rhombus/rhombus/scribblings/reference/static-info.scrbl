#lang rhombus/scribble/manual
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

 See @secref(~doc: guide_doc, "annotation-macro") for an example.

}

@doc(
  ~meta
  fun statinfo_meta.wrap(expr_stx:: Syntax,
                         statinfo_stx :: Syntax)
    :: Syntax
){

 Returns a syntax object for an expression equivalent to
 @rhombus(expr_stx), but with the static information
 @rhombus(statinfo_stx) attached. The @rhombus(statinfo_stx)
 information must be in unpacked form (i.e.,
 @rhombus(statinfo_meta.pack) is applied automatically).

 See @secref(~doc: guide_doc, "annotation-macro") for an example.

}

@doc(
  ~meta
  fun statinfo_meta.pack(statinfo_stx :: Syntax) :: Syntax
){

 Converts static information described by @rhombus(statinfo_stx) into
 an opaque internal format. The given @rhombus(statinfo_stx) must
 match the form

@rhombusblock(
   ((#,(@rhombus(key_id, ~var)), #,(@rhombus(val, ~var))), ...))

 Keys for static information are compared based on binding, not merely
 the key's symbolic form. Key identifiers should be defined with
 @rhombus(statinfo.key, ~defn).

 See @secref(~doc: guide_doc, "annotation-macro") for an example.

}

@doc(
  ~meta
  fun statinfo_meta.unpack(statinfo_stx :: Syntax) :: Syntax
){

 The inverse of @rhombus(statinfo_meta.pack). This function is
 potentially useful to parse a result from
 @rhombus(statinfo_meta.lookup) for a key whose value is packed
 static information.

}

@doc(
  ~meta
  fun statinfo_meta.pack_group(statinfo_stx :: Syntax)
    :: Syntax
  fun statinfo_meta.unpack_group(statinfo_stx :: Syntax)
    :: Syntax
){

 Analogous to @rhombus(statinfo_meta.pack) and
 @rhombus(statinfo_meta.unpack), but for a sequence of static information
 sets, such as one set of static information per value produced by a
 multiple-value expression (see @rhombus(statinfo_meta.values_key)).

 An unpacked sequence is represented as a group syntax object, where
 each term in the group is unpacked static information in the sense of
 @rhombus(statinfo_meta.pack).

}

@doc(
  ~meta
  fun statinfo_meta.check_function_arity(arity :: Syntax,
                                         num_args :: NonnegInt,
                                         arg_kws :: List.of(Keyword))
    :: Boolean
){

 Takes packed arity information associated with
 @rhombus(statinfo_meta.function_arity_key), the number of non-keyword
 arguments in a function call, and a list of keywords for keyword
 arguments in a function call, and reports whether the arguments are
 consist with the packed arity information---that is, that the number and
 keyword arguments will be accepted.

}

@doc(
  ~meta
  fun statinfo_meta.pack_call_result([[arity_mask :: Int,
                                       statinfo_stx :: Syntax],
                                      ...])
    :: Syntax
  fun statinfo_meta.unpack_call_result(statinfo_stx :: Syntax)
    :: matching([[_ :: Int, _ :: Syntax], ...])
){

 Analogous to @rhombus(statinfo_meta.pack) and
 @rhombus(statinfo_meta.unpack), but for information that represents
 function-call results, where a result may be specific to different
 numbers of arguments. Information of this shape is used with
 @rhombus(statinfo_meta.call_result_key).

 Each @rhombus(arity_mask) has a bit set for each number of arguments
 where the associated @rhombus(statinfo_stx) describes the function
 result. For example, a @tech(~doc: guide_doc){property} or @tech{context parameter}
 function may have a result that depends on the number of argument it
 receives. A mask of @rhombus(-1) indicates a result that applies for any
 number of arguments. Each @rhombus(statinfo_stx) represents static
 information for the result in unpacked form.

}

@doc(
  ~meta
  fun statinfo_meta.lookup(expr_stx :: Syntax,
                           key :: Identifier)
    :: maybe(Syntax)
){

 Finds static information for @rhombus(expr_stx), which might be a
 name with static information associated to its binding, or it might be an
 expression with static information associated directly. The result is
 a syntax object for the value associated to @rhombus(key) in that
 static information, or @rhombus(#false) if no information or value
 for @rhombus(key) is available.

 Keys for static information are compared based on binding, not merely
 the key's symbolic form.  Key identifiers should be defined with
 @statinfo_key_defn.

}

@doc(
  fun statinfo_meta.gather(expr_stx :: Syntax) :: Syntax
  fun statinfo_meta.replace(expr_stx :: Syntax, statinfo_stx :: Syntax) :: Syntax
){

 Returns or replaces all of the static information of @rhombus(expr_stx)
 in unpacked form. The result of @rhombus(statinfo_meta.gather) can be
 used with @rhombus(statinfo_meta.find) to get the same values as using
 @rhombus(statinfo_meta.lookup) on @rhombus(expr_str). The result of
 @rhombus(fun statinfo_meta.replace) is the same expression as
 @rhombus(expr_str), but removing any static information that is directly
 attached to @rhombus(expr_str) and replacing it with
 @rhombus(statinfo_stx).

}

@doc(
  fun statinfo_meta.find(statinfo_stx :: Syntax,
                         key :: Identifier)
    :: maybe(Syntax)
){

 Searches for @rhombus(key) in @rhombus(statinfo_stx), returning a
 syntax-object value if the key is found, @rhombus(#false) otherwise.

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
    union_func_expr: block expr
    intersect_func_expr: block expr

  defn.macro '«statinfo.key $id:
                 $clause
                 ...»'
  grammar clause:
    ~union: $union_func_expr
    ~intersect: $intersect_func_expr
){

 Binds @rhombus(id) for use as a static info key identifier. Both
 @rhombus(union_func_expr) and @rhombus(intersect_func_expr) are
 required, and they should each produce a function that accepts two
 syntax objects as values for static information keyed by @rhombus('id').
 The union operation is used, for example, on static information from
 annotations combined with @rhombus(&&, ~annot), and intersection is used
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
  ~meta
  def statinfo_meta.function_arity_key :: Identifier
  def statinfo_meta.call_result_key :: Identifier
  def statinfo_meta.index_result_key :: Identifier
  def statinfo_meta.index_get_key :: Identifier
  def statinfo_meta.index_set_key :: Identifier
  def statinfo_meta.append_key :: Identifier
  def statinfo_meta.dot_provider_key :: Identifier
  def statinfo_meta.sequence_constructor_key :: Identifier
  def statinfo_meta.sequence_element_key :: Identifier
  def statinfo_meta.list_bounds_key :: Identifier
  def statinfo_meta.pairlist_bounds_key :: Identifier
  def statinfo_meta.maybe_key :: Identifier
  def statinfo_meta.flonum_key :: Identifier
  def statinfo_meta.fixnum_key :: Identifier
  def statinfo_meta.values_key :: Identifier
  def statinfo_meta.indirect_key :: Identifier
){

 Values that can be used to associate static information with an
 expression:

 @itemlist(

  @item{@rhombus(statinfo_meta.function_arity_key): Packed information
        about the number of arguments and keywords that are accepted if
        the result value if the expression is called as a function;
        see @rhombus(statinfo_meta.check_function_arity).}

  @item{@rhombus(statinfo_meta.call_result_key): Packed, per-arity static
        information for the result value if the expression is used as
        a function to call; see @rhombus(statinfo_meta.unpack_call_result).}

  @item{@rhombus(statinfo_meta.index_result_key): Packed static information
        for the result value if the expression is used with
        @rhombus([]) to access an element.}

  @item{@rhombus(statinfo_meta.index_get_key): An identifier bound to a
        function to call (instead of falling back to a generic dynamic
        dispatch) when the expression is used with @rhombus([]) to
        access an element.}

  @item{@rhombus(statinfo_meta.index_set_key): An identifier bound to a
        function to call (instead of falling back to a generic dynamic
        dispatch) when the expression is used with @rhombus([]) to
        update an element.}

  @item{@rhombus(statinfo_meta.append_key): An identifier or a boxed identifier bound to a
        function to call (instead of falling back to a generic dynamic
        dispatch) when the expression is used with @rhombus(++) to
        append the result of another expression. For a boxed identifier,
        the application will be guarded by a check to ensure that both
        arguments share the same @rhombus(append, ~datum) implementation.}

  @item{@rhombus(statinfo_meta.dot_provider_key): An identifier
        bound by @rhombus(dot.macro) @rhombus(dot.macro_more_static) to
        implement the expression's behavior as a @tech(~doc: guide_doc){dot provider},
        a packed sequence of such identifiers, or a packed sequence mixing
        identifiers and packed sequences of identifiers. In the case of an overall
        sequence, the first element is used to find a dot provider, and if that
        element is itself a sequence, the dot providers are tried in order.
        The rest of an overall sequence records progressively less-specific
        dot providers, such as the dot providers for superclasses of a class
        or superinterfaces of an interface. Intersection of overall sequences finds
        a shared tail, while union of overall sequences combines elements pairwise.}

  @item{@rhombus(statinfo_meta.sequence_constructor_key): An identifier
        bound as a variable or a macro that is wrapped around an expression
        to create or specialize a sequence for @rhombus(for), or @rhombus(#true) to
        indicate that no wrapper is needed.}

  @item{@rhombus(statinfo_meta.sequence_element_key): Packed static information
        for the elements of the expression as a sequence used with
        @rhombus(each, ~for_clause). For a sequence with
        multiple values in each element, the static information can
        map @rhombus(statinfo_meta.values_key) to a group of per-value
        static information. The number of static information must be
        consistent with the bindings, otherwise no static information
        will be propagated at all. When @rhombus(statinfo_meta.sequence_element_key)
        is not specified, @rhombus(each, ~for_clause) uses
        @rhombus(statinfo_meta.index_result_key).}

  @item{@rhombus(statinfo_meta.list_bounds_key): A group containing
        two elements, indicating a list with a minimum size given by the first
        element, and a maximum size for the second element or @rhombus(#false)
        if no maximum size is known.}

  @item{@rhombus(statinfo_meta.pairlist_bounds_key): Like
        @rhombus(statinfo_meta.list_bounds_key), but for a @tech{pair list}.}

  @item{@rhombus(statinfo_meta.maybe_key): Packed static
        information that applies to a non-@rhombus(#false) value. This
        information is exposed by @rhombus(!!), for example.}

  @item{@rhombus(statinfo_meta.flonum_key): A boolean indicating whether
        the value is a @tech{flonum}. Normally, this key is only present if it
        has a @rhombus(#true) value.}

  @item{@rhombus(statinfo_meta.fixnum_key): A boolean indicating whether
        the value is a @tech{fixnum}. Normally, this key is only present if it
        has a @rhombus(#true) value.}

  @item{@rhombus(statinfo_meta.values_key): A packed group of
        static information (see @rhombus(statinfo_meta.pack_group)),
        one for each value produced by a multiple-value expression.}

  @item{@rhombus(statinfo_meta.indirect_key): An identifier whose
        static information is lazily spliced in place of this key
        and identifier.}

)

 See @secref(~doc: guide_doc, "annotation-macro") for examples using some of these keys.

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
