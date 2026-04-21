#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def ffi_eval = make_rhombus_eval())
@examples(
  ~eval: ffi_eval
  ~hidden:
    import ffi open
)

@(def foreign_type = @rhombus(foreign.type))

@title(~tag: "pointer"){Foreign Pointers}

A @deftech{pointer} object encapsulates a memory address and a
potentially empty sequence of symbolic @tech{tags}. A pointer object
may refer to an address that under the control of Racket's memory
manager and garbage collection, or it may refer to an address is
managed externally.

The base type for pointers is @rhombus_t(ptr_t) (equivalent to
@rhombus_t(void_t*)), and new pointer types can be created via
@rhombus(foreign.type) with either a name for an opaque content type, an
existing pointer type, a @rhombus_t(struct) type or a @rhombus_t(union)
type.

A pointer's @deftech{tags} enable sanity checking that the right kind of
pointer is provided to an operation. If an operation expects a pointer
with a certain sequence of tags, it accepts a pointer with additional
tags added to the end, so additional tags form a @deftech{pointer
 subtype}. A @rhombus_t(ptr_t) representation has no tags, which means
that a C conversion to Rhombus via @rhombus_t(ptr_t) is not accepted by
any context that expects some tag, whereas a pointer with any tags is
accepted as a @rhombus_t(ptr_t) representation to translate to C.
Whether a pointer object represents an address managed by Rhombus's
garbage collector is independent of its tags.

@doc(
  ~nonterminal:
    type: * type ~at rhombus/ffi/type
  ~literal:
    gcable
  type.macro '$type *'
  type.macro '$type /gcable'
  annot.macro 'GCable_ptr_t'
){

 The @rhombus_t(*) postfix type operator describes a pointer type that
 is tagged with the name of @rhombus_t(type) with a @litchar{*} suffix.
 If @rhombus_t(type) is a pointer type, then its ``name'' for this
 purpose is the tag used for its pointers. Otherwise, it is the name as
 defined via @foreign_type. If @rhombus(type) is a @rhombus_t(struct) or
 @rhombus_t(union) type, the new type @rhombus_t(type*) gets the same
 static information as @rhombus(type).

 The @as_indexed{@rhombus_t(/gcable)} type operator (more precisely, a
 @rhombus_t(/) operator that expects a subsequent literal
 @rhombus_t(gcable) always) requires that the argument @rhombus(type) is
 a pointer type, and it describes a type that is the same, but that
 represents an address within memory that is managed by Rhombus's garbage
 collector. The Rhombus-to-C conversion of a @rhombus_t(/gcable) pointer
 is no different that for the original pointer type (i.e., it is not
 required to refer to a garbage-collectable address), but it affects the
 handling of a C address representation to a Rhombus representation.

 The @rhombus(GCable_ptr_t, ~annot) annotation is satisfied only by
 pointer objects that are allowed to reference memory that is managed
 Racket's garbage collector. In contrast,
 @rhombus(foreign.type ptr_t/gcable, ~annot) as an annotation is
 satisified by any pointer object, since @rhombus_t(ptr_t/gcable) accepts any
 pointer for conversion to C.

 When an expression has the static information of the pointer type, then
 indexing via @rhombus([]) (i.e., @rhombus(#%index)) or @rhombus(mem)
 extracts a Rhombus representation for @rhombus(type), and indexing with
 @rhombus([]) or @rhombus(mem) plus @rhombus(:=) assigns to the pointer
 by converting a Rhombus representation of @rhombus(type) to a C
 representation to install. A pointer expression works with
 @rhombus([]) only via static information.

}

@doc(
  ~nonterminal:
    size_expr: block expr
    elem_type: * type ~at rhombus/ffi/type
  type.macro '$elem_type #%index [$size_expr]'
){

 The @rhombus_t(#%index) infix operator is an implicit form and not
 usually written explicitly: @rhombus_t(elem_type[size_expr]) is
 equivalent to @rhombus_t(elem_type #%index [size_expr]),

 Describes a type that is represented by an array on the C side and a
 @tech{pointer} object in the Rhombus side. In most type contexts, the
 array's @rhombus(size_expr) must be a literal nonnegative integer. When a
 type using @litchar{[]} is used with @rhombus(new), however, then
 @rhombus(size_expr) can be any expression that produces a nonnegative
 integer.

 The Rhombus-side pointer representation uses a @tech{tag} formed by
 adding a @litchar{*} suffix on the name of @rhombus_t(elem_type), as
 long as it has a name. If @rhombus_t(elem_type) is an immediate
 @rhombus_t(struct), @rhombus_t(union), or @rhombus_t(->) form, then it
 has no name, and the Rhombus-side representation is a generic pointer.

 When an expression has the static information of an array type, then
 indexing via @rhombus([]) (i.e., @rhombus(#%index)) or
 @rhombus(mem) extracts a Rhombus representation for an
 element of the array, and indexing with @rhombus([]) or @rhombus(mem) plus @rhombus(:=)
 assigns to the array by converting a Rhombus value to a C representation
 to install into the array. If @rhombus(size_expr) is a literal integer,
 then the bounds checking prevents indexing with positions that are
 negative or not less than the size. An array-pointer expression works with
 @rhombus([]) only via static information.

@examples(
  ~eval: ffi_eval
  ~repl:
    sizeof(double_t[3])
    def p = new double_t[3]
    p
    p[0] := 0.0
    p[1] := 10.0
    p[2] := 20.0
    p[1]
    ~error:
      mem p[3]
)

}

@doc(
  ~nonterminal:
    field_expr: block expr
    variant_id: block id
    type: * type ~at rhombus/ffi/type
  expr.macro 'new $maybe_mode $type'
  expr.macro 'new $maybe_mode $type($field_expr, ...)'
  expr.macro 'new $maybe_mode $type($variant_id: $field_expr)'
  grammar maybe_mode
  | ~manual
  | ~gcable
  | ~immobile
  | ~traced
  | ~traced_immobile
  | ϵ
){

 Allocates memory. A @rhombus(type(field_expr, ...)) form is allowed
 only when @rhombus(type) is a @rhombus_t(struct) type, and a
 @rhombus(type(variant_id: field_expr)) form is allowed only when
 @rhombus(type) is a @rhombus_t(union) type.

 The amount of allocated memory depends on @rhombus(type), where the
 allocated memory spans as many bytes as the C representation of
 @rhombus(type). A type of the form
 @rhombus(#,(@rhombus(elem_type, ~var))[#,(@rhombus(expr, ~var))]) is
 allowed with a non-literal @rhombus(expr, ~var) to allocate the
 indicated multiple of the C size of @rhombus(elem_type, ~var) in bytes.

 By default, allocation uses @rhombus(~gcable) mode, but a
 @rhombus(maybe_mode) specification can pick any of the supported modes:

@itemlist(

 @item{@rhombus(~gcable): Allocates in Rhombus's garbage-collected
  space. The allocated memory becomes eligible for garbage collection when
  it is not referenced by any reachable pointer object or @tech{traced}
  allocated memory. Even before collection, the memory manager may relocate
  the object, but garbage collection or relocation cannot happen with a
  foreign-procedure call is active.}

 @item{@rhombus(~immobile): Like @rhombus(~gcable), but the allocated
  memory will not be relocated by a different address by the memory
  manager as long as it is not collected.}

 @item{@rhombus(~traced): Like @rhombus(~gcable), but the allocated
  memory is @deftech{traced}, meaning that it can itself contain
  references to other allocated memory. The references are updated by the
  memory manager if it moved the referenced objects.}

 @item{@rhombus(~traced_immobile): Like @rhombus(~traced), but the
  allocated memory will not be relocated by a different address by the
  memory manager as long as it is not collected.}

 @item{@rhombus(~manual): Allocates outside of Rhombus's
  garbage-collected space. The allocated memory is never relocated by the
  garbage collection, and it must be freed explicitly with
  @rhombus(free).}

)

@examples(
  ~eval: ffi_eval,
  ~repl:
    def p1 = new int_t
    mem *p1 := 3
    mem *p1
  ~repl:
    def pm = new ~manual int_t
    mem *pm := 4
    free(pm)
  ~repl:
    def p3 = new int_t[1+2]
    p3[2] := 20
    p3[2]
  ~repl:
    def p1_imm = new ~immobile int_t
    def i1_imm = ptr_to_uintptr(p1_imm)
    ~fake:
      :
        memory.GC() // probably moves p1, does not move p1_imm
      #void
    i1_imm == ptr_to_uintptr(p1_imm)
  ~repl:
    def pp = new ~traced int_t*
    pp[0]
    pp[0] := new int_t
    pp[0][0] := 5
    ~fake:
      :
        memory.GC() // probably moves pp[0]
      #void
    pp[0][0]
  ~repl:
    def mutable pm = new ~manual int_t
    pm[0] := 6
    def pm_i = ptr_to_uintptr(pm)
    pm := #false
    ~fake:
      :
        memory.GC() // does not affect manual allocation
      #void
    pm := uintptr_to_ptr(pm_i)
    pm[0]
    free(pm)
)

}

@doc(
  ~nonterminal:
    size_expr: block expr
    maybe_mode: new
    to_type: * type ~at rhombus/ffi/type
  expr.macro 'malloc $maybe_mode $maybe_as ($size_expr)'
  grammar maybe_as
  | ~as $to_type
  | ϵ
){

 Allocates memory in the same way as @rhombus(new), but where
 @rhombus(size_expr) specifies a size in bytes.

 If @rhombus(~as to_type) is specified, the result pointer is
 @tech{tag}ged as @rhombus(to_type), where @rhombus(to_type) must be a
 pointer type, and the @rhombus(malloc) expression has the static
 information of @rhombus(to_type).

@examples(
  ~eval: ffi_eval,
  ~repl:
    def gp = malloc(64)
    gp
    gp is_a foreign.type int_t*
  ~repl:
    def p = malloc ~as int_t* (64)
    p is_a foreign.type int_t*
    mem *p := 5
    mem *p
  ~defn:
    foreign.struct Point_t(x :: int_t,
                           y :: int_t)
  ~repl:
    def pt0 = malloc(sizeof(Point_t))
    pt0
    pt0 is_a Point_t
    def pt = malloc ~as Point_t* (sizeof(Point_t))
    pt is_a Point_t
    pt.x := 1
    pt.y := 2
    pt.x + pt.y
)

}

@doc(
  fun free(p :: ptr_t) :: Void
){

 Deallocates memory that was allocated with @rhombus(new) or
 @rhombus(malloc) in @rhombus(~manual) mode.

}

@doc(
  ~literal:
    *
    :=
  ~nonterminal:
    ptr_expr: block expr
    val_expr: block expr
    index_expr: block expr
    delta_expr: block expr
    type: * ~at rhombus/ffi/type
    ptr_type: * type ~at rhombus/ffi/type
  expr.macro 'mem #,(@rhombus_t(*))$ptr_expr'
  expr.macro 'mem #,(@rhombus_t(*))$ptr_expr := $val_expr'
  expr.macro 'mem #,(@rhombus_t(*))($ptr_type)$ptr_expr'
  expr.macro 'mem #,(@rhombus_t(*))($ptr_type)$ptr_expr := $val_expr'
  expr.macro 'mem $ptr_expr[$index_expr]'
  expr.macro 'mem $ptr_expr[$index_expr] := $val_expr'
  expr.macro 'mem & $ptr_expr[$delta_expr]'
){

 Extracts a Rhombus representation for an inferred @rhombus(type) of the C
 representation stored at the address represented by the @tech{pointer}
 result of @rhombus(ptr_expr); updates the C representation stored at the
 address represented by @rhombus(ptr_expr) with a value converted from a
 Rhombus representation produced by @rhombus(val_expr); or, in the case
 of @rhombus(mem &), shifts the address from @rhombus(ptr_expr) by
 @rhombus(delta_expr) times the size of @rhombus(type).

 When the @rhombus(*(ptr_type)ptr_expr) form is used, the tags on the
 result of @rhombus(ptr_expr) are ignored. The pointer is effectively
 @rhombus(cast) to @rhombus(ptr_type), and @rhombus(ptr_type) must be a
 pointer type referring to @rhombus(type) elements.

 When the @rhombus(* ptr_expr), @rhombus(ptr_expr[index_expr]), or
 @rhombus(& ptr_expr[delta_expr]) form is used, then @rhombus(ptr_expr)
 must have static information to indicate an element type or
 pointer-referenced type to be used as @rhombus(type). The
 @rhombus(mem ptr_expr[index_expr]) form is equivalent to
 @rhombus(ptr_expr[index_expr]) without @rhombus(mem), since
 @rhombus(ptr_expr) must have static information that would also allow it
 to work with @rhombus([]) via @rhombus(#%index).

@examples(
  ~eval: ffi_eval
  ~repl:
    def p = new double_t
    p
    mem p[0] := 0.0
    mem p[0]
    mem *(int64_t*)p
    mem *(int64_t*)p := -1
    mem p[0]
    mem (mem &p[-3])[3]
)

}

@doc(
  ~nonterminal:
    offset_expr: block expr
    from_type: * type ~at rhombus/ffi/type
    to_type: * type ~at rhombus/ffi/type
    expr: block
  expr.macro 'cast $maybe_from_type $maybe_to ($to_type) $maybe_offset $expr'
  grammar maybe_from_type
  | ~from $from_type
  | ϵ
  grammar maybe_to
  | ~to
  | ϵ
  grammar maybe_offset
  | ~offset ($offset_expr)
  | ϵ
){

 Converts the Rhombus representation produced by @rhombus(expr) from one
 type's representation to another. If @rhombus(from_type) or is not
 specified, it defaults to @rhombus_t(ptr_t). The C representations for
 both @rhombus(from_type) and @rhombus(to_type) must be both addresses or
 both @tech{scalars}, and conversions may apply to the Rhombus
 representation produced by @rhombus(expr) (based on @rhombus(from_type))
 or the converted result (based on @rhombus(to_type)) of the cast
 address.

 If @rhombus(~offset (offset_expr)) is specified, then the
 (pre-conversion) address produced by the cast is @rhombus(offset_expr)
 bytes after the address represented by the converted result of
 @rhombus(expr). A @rhombus(~offset (offset_expr)) is allowed only when
 @rhombus(to_type) has an address representation.


@examples(
  ~eval: ffi_eval
  ~repl:
    def p_i = new int16_t
    def p_b = cast (byte_t*)p_i
    mem p_b[0] := 1
    mem p_b[1] := 1
    mem *p_i
    mem *(cast (byte_t*) ~offset(1) p_i) := 2
    mem p_b[0] := mem p_b[1]
    mem *p_i
  ~repl:
    ~error:
      cast ~from (int32_t*) ~to (byte_t*) p_i
    cast ~from (int16_t*) ~to (byte_t*) p_i
)

}

@doc(
  fun memcpy(
    dest :: ptr_t,
    src :: ptr_t,
    len :: NonnegInt,
    ~dest_offset: dest_offset :: Int = 0,
    ~src_offset: src_offset :: Int = 0
  ) :: Void
  fun memmove(
    dest :: ptr_t,
    src :: ptr_t,
    len :: NonnegInt,
    ~dest_offset: dest_offset :: Int = 0,
    ~src_offset: src_offset :: Int = 0
  ) :: Void
  fun memset(
    dest :: ptr_t,
    byte :: Byte,
    len :: NonnegInt,
    ~dest_offset: dest_offset :: Int = 0
  ) :: Void
){

 The @rhombus(memcpy) and @rhombus(memmove) functions copy @rhombus(len)
 bytes from the address represented by @rhombus(src) plus
 @rhombus(src_offset) to the address represented by @rhombus(dest) plus
 @rhombus(dest_pffset). In the case of @rhombus(memcpy), the source and
 destination regions must not overlap.

 The @rhombus(memcpy) function sets @rhombus(len) bytes at the address
 represented by @rhombus(dest) plus @rhombus(dest_pffset) so that each
 byte's value is @rhombus(byte).

@examples(
  ~eval:
    ffi_eval,
  ~repl:
    def bstr1 = #"Hello".copy()
    def bstr2 = #"Goodbye".copy()
    memcpy(cast ~from (bytes_ptr_t) ~to (ptr_t) bstr1,
           cast ~from (bytes_ptr_t) ~to (ptr_t) bstr2,
           2)
    bstr1
    memmove(mem &(cast ~from (bytes_ptr_t) ~to (byte_t*) bstr1)[2],
            cast ~from (bytes_ptr_t) ~to (ptr_t) bstr1,
            3)
    bstr1
    memset(cast ~from (bytes_ptr_t) ~to (ptr_t) bstr1,
           Char"o".to_int(),
           3,
           ~dest_offset: 2)
    bstr1
)

}

@doc(
  fun ptr_to_uintptr(ptr :: ptr_t) :: uintptr_t
  fun uintptr_to_ptr(addr :: uintptr_t) :: ptr_t
){

 Conversions between addresses represented as pointers and addresses
 represented as integers.

 Beware that the integer form of an address managed by Rhombus's garbage
 collector can become immediately invalid, unless an object at the
 address was allocated as immobile.

}

@close_eval(ffi_eval)
