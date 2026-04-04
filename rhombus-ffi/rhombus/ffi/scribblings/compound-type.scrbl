#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def ffi_eval = make_rhombus_eval())
@examples(
  ~eval: ffi_eval
  ~hidden:
    import ffi open
)

@title(~tag: "ffi-compound-ffi-type"){Compound Foreign Types}

@doc(
  ~literal:
    ::
  ~nonterminal:
    id: block id
    field_id: block id
    field_type: * type ~at rhombus/ffi/type
  type.macro 'struct $maybe_tag (
                $field_id #,(@rhombus(::, ~bind)) $field_type,
                ...
              )'
  defn.macro 'foreign.struct $id (
                $field_id #,(@rhombus(::, ~bind)) $field_type,
                ...
              )'
  grammar maybe_tag
  | $id
  | ϵ
){

 The @rhombus_t(struct) type form describes a type that is represented
 by a @tt{struct} declaration on the C side and a @tech{pointer} object
 in the Rhombus side. If @rhombus(maybe_tag) is an identifier, the name
 of the identifier is suffixed with @litchar{*} used as a @tech{tag} for
 pointers that represent instances of the @rhombus_t(struct) type,
 otherwise a generic @rhombus_t(ptr_t) pointer is used.

 Each @rhombus(field_id) must be distinct, and the corresponding
 @rhombus(field_type) describes the field's representation on the C side
 and the representation used on the Rhombus side if the field's value is
 extracted from a representation of the @rhombus_t(struct) type.

 The @rhombus(foreign.struct) form defines @rhombus(id) as an alias for
 the corresponding @rhombus_t(struct) type using @rhombus(id) like
 @rhombus(maybe_tag) for a pointer @tech{tag}. The @rhombus(id) is also
 defined for additional roles:

@itemlist(

 @item{The @rhombus(id) is defined as an annotation that recognizes
  pointers tagged with @rhombus(id).}

 @item{The @rhombus(id) is defined as a veneer, and @rhombus(id) as a
  type implies the static information of @rhombus(id) as a veneer. Each
  @rhombus(field_id) is bound as a field of the veneer that can be used to
  access or update the corresponding field in an instance of the
  @rhombus_t(struct).

  @itemlist(

   @item{When a field is accessed, the result is a representation of the
   corresponding C field value based on the conversion implied by the
   associated @rhombus(field_type).}

   @item{When the field is set using @rhombus(:=) to a Rhombus value, a
   value converted to C based on the associated @rhombus(field_type), and
   that C value is installed into the @rhombus_t(struct) instance.}

  )}

 @item{The @rhombus(id) is defined for use with @rhombus(new) allocate
  to an instance of the @rhombus(struct) with field values provided as
  arguments (in addition to the possibility of using @rhombus(id) by
  itself as a type to create an uninitialized instance).}

)


 Note that the Racket-side representation is the same for @rhombus_t(id)
 and @rhombus_t(id*), even though the C-side representation differs.

@examples(
  ~eval: ffi_eval
  ~defn:
    foreign.struct point_t(x :: double_t,
                           y :: double_t)
  ~repl:
    sizeof(point_t)
    sizeof(point_t*)
    def p1 = new point_t(1.0, 2.0)
    p1
    p1 is_a point_t
    p1 is_a foreign.type point_t*
    p1.x
    p1.y
    p1.x := 5.0
    p1.x
    point_t.x(p1)
    ~error:
      point_t.x(malloc(16))
)

 With this example's definition of @rhombus_t(point_t), a field in
 another @rhombus_t(struct) type would take up 16 bytes, while a
 @rhombus_t(point_t*) field would take up 8 bytes. Accessing the field in
 either case would produce a Racket representation that is a
 @tech{pointer} tagged as @litchar{point_t*}. In the case of a
 @rhombus_t(point_t) field, the returned pointer would refer to memory
 within the accessed @rhombus_t(struct) instance.

 Along similar lines, a pointer tagged with @litchar{point_t*} is
 suitable as an argument to a C function that has either a
 @rhombus_t(point_t) or @rhombus_t(point_t*) argument. In the case of a
 @rhombus_t(point_t) argument, the C function receives a copy of the
 content of the pointer. In the case of a @rhombus_t(point_t*) argument,
 the C function receives the same address as encapsulated by the pointer.

}

@doc(
  ~nonterminal:
    maybe_tag: struct ~at rhombus/ffi/type
    id: block id
    field_id: block id
    field_type: * type ~at rhombus/ffi/type
  type.macro 'union $maybe_tag (
                $field_id :: $field_type,
                ...
              )'
  defn.macro 'foreign.union $id (
                $field_id :: $field_type,
                ...
              )'
){

 Similar to @rhombus_t(struct) and @rhombus(foreign.struct), but for a
 type that uses @tt{union} on the C side.

 The use of a defined @rhombus(id) with @rhombus(new) plus arguments is
 different than for a type defined with @rhombus(foreign.struct), since a
 @rhombus(foreign.union) instance is a variant for only one of the
 fields, not a composite value with all of the fields. To specify the
 relevant variant, there must be only one argument, and it must be
 in a block that is prefixed with the relevant @rhombus($field_id).

@examples(
  ~eval: ffi_eval
  ~defn:
    foreign.union grade_t (score :: double_t,
                           pass_fail :: bool_t)
  ~repl:
    sizeof(grade_t)
    def g1 = new grade_t(score: 93.0)
    g1.score
    def g2 = new grade_t(pass_fail: #true)
    g2.pass_fail
    def g3  = new grade_t(score: 0.0)
    g3.pass_fail
)

}


@doc(
  ~nonterminal:
    type: * type ~at rhombus/ffi/type
    arg: -> ~at rhombus/ffi/type
  type.macro '#%parens ($type)'
  type.macro '#%parens ($arg, ...)'
  type.macro '#%parens ($arg, ..., ~varargs, $arg, ...)'
){

 The @rhombus_t(#%parens) prefix operator is an implicit form and not
 usually written explicitly: @rhombus_t((#,(@rhombus(group, ~var)), ...))
 is equivalent to @rhombus_t(#%parens (#,(@rhombus(group, ~var)), ...)).

 A type form @rhombus((type)) is equivalent to @rhombus(type).

 A @rhombus((arg, ...)) or @rhombus((arg, ..., ~varargs, arg, ...)) form
 is allowed as a ``type'' only on the left-hand side of the
 @rhombus_t(->) operator.

}

@close_eval(ffi_eval)
