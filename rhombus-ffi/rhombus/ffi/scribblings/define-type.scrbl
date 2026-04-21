#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def ffi_eval = make_rhombus_eval())
@examples(
  ~eval: ffi_eval
  ~hidden:
    import ffi open
)

@title(~tag: "ffi-define-type"){Defining Foreign Types}

@doc(
  ~literal:
    =
  ~nonterminal:
    id: block
    arg_id: block id
    tag_id: block id
    parent_type: * type ~at rhombus/ffi/type
    rhombus_to_c_body: block body
    predicate_body: block body
    release_body: block body
    c_to_rhombus_body: block body
  defn.macro 'foreign.type $id'
  defn.macro 'foreign.type $id = $parent_type'
  defn.macro 'foreign.type $id:
                ~extends: $parent_type'
  defn.macro 'foreign.type $id:
                $option
                ...'
  defn.macro 'foreign.type $id($arg_id, ...):
                $option
                ...'
  grammar option
  | ~extends: $parent_type
  | ~extends $parent_type
  | ~rhombus_to_c: $rhombus_to_c_body; ...
  | ~predicate: $predicate_body; ...
  | ~release: $release_body; ...
  | ~c_to_rhombus: $c_to_rhombus_body; ...
  | ~tag: $tag_id
  | ~tag $tag_id
){

 Defines a type @rhombus_t(id) or a type constructor @rhombus_t(id).
 When not defining a type constructor, @rhombus_t(id) is also defined as
 an annotation that recognizes Rhombus representations of the type.

 When @rhombus(id) is provided by itself, then @rhombus(id) represents
 an @deftech{opaque type} whose C representations is unspecified. An
 opaque type is useful only with the @rhombus_t(*) type operator to
 create a pointer type that is @tech{tag}ged using the name @rhombus(id)
 with a @litchar{*} suffix.

 When @rhombus(id = parent_type) is provided, then @rhombus(id) is
 defined as an alias of @rhombus(parent_type). The tags as name of the
 type for pointer-tagging purposes remains the same as for
 @rhombus(parent_type).

 When an @rhombus(options) block is provided, then @rhombus(~extends) is
 required. If only @rhombus(~extends) is provided, then @rhombus(id)
 represents an @tech{opaque type} that creates a @tech{pointer subtype}
 relative to @rhombus(parent_type): a pointer representing a value of
 @rhombus_t(id*) has the tags of @rhombus_t(parent_type*) with an
 additional tag at the end formed by @rhombus(tag_id) suffixed with
 @litchar{*}. If @rhombus(tag_id) is not specified with a @rhombus(~tag)
 option, then @rhombus(id) is used for @rhombus(tag_id). The
 @rhombus(~tag) option is allowed only in this case.

 When an @rhombus(options) block has multiple options, then it defines a
 new type that has the same C-side representation as
 @rhombus(parent_type), but its Rhombus-side representation can be
 different as determined by other options as described below.

 If @rhombus(foreign.type) is followed by @rhombus_t(id(arg_id, ...)),
 then @rhombus_t(id) is defined as a type constructor that receives
 expression arguments when applied in a type position, and the remainder
 of the @rhombus(foreign.type) form can refer to the @rhombus(arg_id)s to
 parameterize the definition over supplied values. Each use of
 @rhombus(id) must be applied to any many expression arguments as
 @rhombus(arg_id)s in the definition.

 In all cases, the new type @rhombus(id) (or the type that it
 constructs) has the same C representation as @rhombus(parent_type). The
 Rhombus representation can be adjusted via @rhombus(~rhombus_to_c) and
 @rhombus(~c_to_racket) options, which may need accompanying
 @rhombus(~predicate) and @rhombus(~release) functions:

@itemlist(

 @item{@rhombus(~predicate): Provides a predicate function as the result
  of the @rhombus(predicate_body) sequence. This predicate is used when
  checks are enabled for Rhombus values to be converted to C for the type
  @rhombus(id), but the predicate can be skipped on request. The predicate
  determines whether a value is suitable as an argument to a function
  provided by @rhombus(~rhombus_to_c). If @rhombus(predicate) is not
  provided, then the predicate associated with @rhombus(parent_type) is
  used.}

 @item{@rhombus(~racket_to_c): Provides a conversion function toward C
  as the result of @rhombus(racket_to_c_body) sequence. This converter is
  applied to a Rhombus value that is supplied for a @rhombus(id) type. The
  result of conversion should be a Rhombus representation for
  @rhombus(parent_type). If @rhombus(~racket_to_c) is not provided,
  conversion is the identity function.}

 @item{@rhombus(~release): Provides a function that finalizes conversion
  from Rhombus to C as the result of @rhombus(release_body) sequence. The
  function is applied to the result of @rhombus(parent_type)'s release
  function after the C value is delivered (e.g., passed in a foreign
  call that has returned). For base pointer types, the release
  function is @rhombus(Function.black_box), which is useful because it
  keeps a pointer live if it is subject to garbage collection. A release
  function could explicitly deallocate a pointer that was allocated by
  the @rhombus(~racket_to_c) function, but a release function is not
  called if control somehow escapes or the current thread is forcibly
  terminated.}

 @item{@rhombus(~c_to_racket): Provides a conversion function toward
  Rhombus as the result of the @rhombus(c_to_racket_body) sequence. This
  converter is applied to a Rhombus representation of
  @rhombus(parent_type) as extracted from a C representation. The result
  of conversion should be a Rhombus representation for @rhombus(id). If
  @rhombus(~c_to_racket) is not provided, conversion is the identity
  function.}

)

@examples(
 ~eval: ffi_eval
 ~defn:
   foreign.type percentage_t:
     ~extends: double_t
     ~predicate: fun (v): v is_a Real.in(0.0, 100.0)
     ~rhombus_to_c: fun (v): v / 100.0
     ~c_to_rhombus: fun (v): v * 100.0
 ~repl:
   def p = new double_t
   mem *(double_t *)p := 0.5
   mem *(percentage_t *)p
   mem *(percentage_t *)p := 0.25
   mem *(double_t *)p
 ~defn:
   foreign.type percentage_box_t:
     ~extends: ptr_t
     ~predicate: fun (bx): bx is_a Box && Box.value(bx) is_a percentage_t
     ~rhombus_to_c: fun (bx :~ Box):
                      let p = new ~immobile percentage_t
                      mem p[0] := bx.value
                      p
     ~c_to_rhombus: fun (ptr):
                      Box(mem *(percentage_t *)ptr)
 ~repl:
   def p = new ~traced ptr_t
   mem *(percentage_box_t *)p := Box(50.5)
   mem *(percentage_box_t *)p
   mem *(double_t *)(mem *(ptr_t *)p)
 ~defn:
   foreign.type offset_double_t(delta):
     ~extends: double_t
     ~c_to_rhombus: fun (v): v + delta
     ~rhombus_to_c: fun (v): v - delta
   foreign.struct posn_t (x :: offset_double_t(1.0),
                          y :: offset_double_t(2.0))
 ~repl:
   def p = new posn_t(10.0, 20.0)
   mem (cast (double_t *)p)[0]
   mem (cast (double_t *)p)[1]
   p.x := 100.0
   p.x
   mem (cast (double_t *)p)[0]
)

}

@doc(
  ~nonterminal:
    macro_patterns: expr.macro ~defn
  defn.macro 'type.macro $macro_patterns'
){

 Like @rhombus(expr.macro), but binds a macro that is expanded in type
 positions, instead of expression positions.

}

@doc(
  ~meta
  ~nonterminal:
    macro_patterns: expr.macro ~defn
    op_name: namespace ~defn
  syntax_class type_meta.Parsed
  syntax_class type_meta.AfterPrefixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
  syntax_class type_meta.AfterInfixParsed(op_name):
    kind: ~group
    fields:
      group
      [tail, ...]
){

 Like @rhombus(expr.Parsed), @rhombus(expr.AfterPrefixParsed), and
 @rhombus(expr.AfterInfixParsed), but for type positions.

}

@doc(
  ~nonterminal:
    type: * type ~at rhombus/ffi/type
  expr.macro 'sizeof($type)'
){

 Returns the number of bytes used for the C representation of
 @rhombus(type).

@examples(
  ~eval: ffi_eval
  ~repl:
    sizeof(int32_t)
)

}

@doc(
  ~nonterminal:
    type: * type ~at rhombus/ffi/type
    field_id: block id
  expr.macro 'offsetof($type, $field_id)'
){

 Returns the number of bytes in the C representation of @rhombus(type)
 that precede the field named @rhombus(field_id). The @rhombus(type) must
 be have the C representation of a @rhombus(struct) or @rhombus(union)
 type; the result is always @rhombus(0) in the case of a @rhombus(union)
 type.

@examples(
  ~eval: ffi_eval
  ~defn:
    foreign.struct Point_t(x :: int_t,
                           y :: int_t)
  ~repl:
    offsetof(Point_t, x)
    offsetof(Point_t, y)
)

}

@doc(
  ~nonterminal:
    at_type: * type ~at rhombus/ffi/type
  annot.macro 'foreign.type $at_type'
){

 Satisfied by values that are valid Racket representations of @rhombus(at_type).

 A type name typically doubles as an annotation itself, but
 @rhombus(foreign.type, ~annot) can be used with more complex type forms,
 such as @rhombus(ptr_t*).

@examples(
  ~eval: ffi_eval
  ~repl:
    def p = new double_t
    p is_a double_t
    p is_a foreign.type double_t*
)

}

@doc(
  ~nonterminal:
    id: block
    parent_type: * type ~at rhombus/ffi/type
  ~literal:
    =
  defn.macro 'foreign.enum $name $parent_type
              | $enum_clause
              | ...'
  grammar enum_clause
  | $id
  | $id = $literal_int
){

 Like @rhombus(enum) restricted to @rhombus(id) cases, but also defines
 @rhombus(name) as a type that extends @rhombus(parent_type), which must
 be an integer type. The C representation of the new type is the same as
 @rhombus(parent_type). The Rhombus representation is a symbol---the
 symbol form of one of the listed @rhombus(id)s---except that conversion
 from C can produce an integer if it does not match the numeric value
 associated with one of the @rhombus(id)s.

 The numeric value of a @rhombus(id) can be provided as a
 @rhombus(literal_int). If @rhombus(literal_int) is not provided for an
 @rhombus(id), then the integer value is @rhombus(0) if it is the first
 @rhombus(id), otherwise it is one more than the value for the preceding
 @rhombus(id).

 When converting from C to Rhombus, if multiple @rhombus(id)s have the
 same numeric value, the symbol form of the last listed @rhombus(id) is
 used.

@examples(
  ~eval: ffi_eval
  ~repl:
    foreign.enum shape_t int_t
    | circle
    | triangle = 3
    | square
    cast ~from (shape_t) ~to (int_t) #'circle
    cast ~from (shape_t) ~to (int_t) #'triangle
    cast ~from (shape_t) ~to (int_t) #'square
    cast ~from (int_t) ~to (shape_t) 3    
)

}

@doc(
  ~nonterminal:
    then_type: * type ~at rhombus/ffi/type
    else_type: * type ~at rhombus/ffi/type
  type.macro 'system_case $key
              | $vals: $then_type
              | ~else: $else_type'
  grammar key
  | #,(@rhombus(type, ~datum))
  | os
  | arch
  | word
  grammar vals
  | $id
  | 32
  | 64
  | $vals || $vals
){

 Describes a type with a platform-specific representation or a
 platform-specific choice of function @tech{ABI}, enabling a
 compile-time (later than expand-time) choice. The symbol form of
 @rhombus(key) corresponds to a method of
 @rhombus(system, ~at rhombus/namespace), and each @rhombus(val) must be
 a potential result: an identifier for @rhombus(key)s other than
 @rhombus(word, ~datum), or either @rhombus(32) or @rhombus(64) in the
 case of @rhombus(word, ~datum).

 Each of @rhombus(then_type) amd @rhombus(else_type) must be a
 @deftech{scalar} type, such as @rhombus_t(int_t) or @rhombus_t(float_t).
 A @rhombus_t(system_case) type is also scalar, since it selects
 among scalar types.

}

@close_eval(ffi_eval)
