#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "class"){Classes}

@doc(
  ~literal: :: extends binding field
  ~nonterminal:
    default_expr: block expr
    default_body: block body
    method_impl: method ~class_clause
    property_impl: method ~class_clause
    method_decl: abstract ~class_clause
    property_decl: abstract ~class_clause
    field_impl: field ~class_clause

  defn.macro 'class $id_name($field_spec, ...)'
  defn.macro 'class $id_name($field_spec, ...):
                $class_clause_or_body_or_export
                ...'

  grammar field_spec:
    $modifiers $id $maybe_annot $maybe_default
    $keyword: $modifiers $id $maybe_annot $maybe_default
    $keyword $maybe_default

  grammar modifiers:
    #,(@rhombus(private, ~class_clause))
    #,(@rhombus(mutable, ~bind))
    #,(@rhombus(private, ~class_clause)) #,(@rhombus(mutable, ~bind))
    #,(@rhombus(protected, ~class_clause))
    #,(@rhombus(protected, ~class_clause)) #,(@rhombus(mutable, ~bind))
    #,(epsilon)

  grammar maybe_annot:
    #,(@rhombus(::, ~bind)) $annot
    #,(@rhombus(:~, ~bind)) $annot
    #,(epsilon)

  grammar maybe_default:
    = $default_expr
    : $default_body; ...
    #,(epsilon)

  grammar class_clause_or_body_or_export:
    $class_clause
    $body
    $export

  grammar class_clause:
    #,(@rhombus(field, ~class_clause)) $field_impl
    #,(@rhombus(immutable, ~class_clause)) $field_impl
    #,(@rhombus(immutable, ~class_clause)) #,(@rhombus(field, ~class_clause)) $field_impl
    #,(@rhombus(private, ~class_clause)) #,(@rhombus(field, ~class_clause)) $field_impl
    #,(@rhombus(private, ~class_clause)) #,(@rhombus(immutable, ~class_clause)) #,(@rhombus(field, ~class_clause)) $field_impl
    #,(@rhombus(protected, ~class_clause)) #,(@rhombus(field, ~class_clause)) $field_impl
    #,(@rhombus(protected, ~class_clause)) #,(@rhombus(immutable, ~class_clause)) #,(@rhombus(field, ~class_clause)) $field_impl
    #,(@rhombus(method, ~class_clause)) $method_impl
    #,(@rhombus(override, ~class_clause)) $method_impl
    #,(@rhombus(final, ~class_clause)) $method_impl
    #,(@rhombus(private, ~class_clause)) $method_impl
    #,(@rhombus(protected, ~class_clause)) $method_impl
    #,(@rhombus(abstract, ~class_clause)) $method_decl
    #,(@rhombus(property, ~class_clause)) $property_impl
    #,(@rhombus(extends, ~class_clause)) $id_name
    #,(@rhombus(implements, ~class_clause)) $implements_decl
    #,(@rhombus(private, ~class_clause)) #,(@rhombus(implements, ~class_clause)) $implements_decl
    #,(@rhombus(protected, ~class_clause)) #,(@rhombus(implements, ~class_clause)) $implements_decl
    #,(@rhombus(final, ~class_clause))
    #,(@rhombus(nonfinal, ~class_clause))
    #,(@rhombus(internal, ~class_clause)) $id
    #,(@rhombus(constructor, ~class_clause)) $constructor_decl
    #,(@rhombus(expression, ~class_clause)) $expression_decl
    #,(@rhombus(binding, ~class_clause)) $binding_decl
    #,(@rhombus(annotation, ~class_clause)) $annotation_decl
    #,(@rhombus(reconstructor, ~class_clause)) $reconstructor_impl
    #,(@rhombus(reconstructor_fields, ~class_clause)) $reconstructor_fields_decl
    #,(@rhombus(dot, ~class_clause)) $dot_decl
    #,(@rhombus(static_info, ~class_clause)) $static_info_decl
    #,(@rhombus(opaque, ~class_clause))
    #,(@rhombus(prefab, ~class_clause))
    #,(@rhombus(serializable, ~class_clause)) $serializable_decl
    #,(@rhombus(primitive_property, ~class_clause)) $primitive_property_decl
    $other_class_clause
){

 Binds @rhombus(id_name) as a @deftech{class} name in several
 @tech{spaces} (except as suppressed with @rhombus(class_clause)s such as
 @rhombus(expression ~none, ~class_clause)):

@itemlist(

 @item{In the @top_rhombus(expr, ~space) space, @rhombus(id_name)
  a constructor function or form, which by default is a function that
  takes as many arguments
  as the supplied non-@rhombus(private, ~class_clause)/@rhombus(protected, ~class_clause) @rhombus(field_spec)s
  in parentheses, and it returns an instance of the class;},

 @item{in the @top_rhombus(annot, ~space) space,
  an annotation, which is satisfied by any instance of the class,
  and by default an annotation constructor @rhombus(id_name.of) or
  @rhombus(id_name.now_of), which
  default takes as many annotation arguments as supplied
  non-@rhombus(private, ~class_clause)/@rhombus(protected, ~class_clause) @rhombus(field_spec)s in
  parentheses; the name is @rhombus(id_name.of) if all such
  @rhombus(field_spec)s are for immutable fields;},

 @item{in the @rhombus(bind, ~space) space,
  a binding-pattern constructor, which by default takes as many
  patterns as the supplied non-@rhombus(private, ~class_clause)/@rhombus(protected, ~class_clause)
  @rhombus(field_spec)s in parentheses and matches an instance of the
  class where the fields match the corresponding patterns;},

 @item{in the @rhombus(namespace, ~space) space,
  a @tech{namespace} to access exported bindings as well as (if not replaced by an export) a
  function
  @rhombus(id_name#,(rhombus(.))#,(@rhombus(method,~var))),
  a function
  @rhombus(id_name#,(rhombus(.))#,(@rhombus(property,~var))),
  a syntactic form
  @rhombus(id_name#,(rhombus(.))#,(@rhombus(dot,~var))),
  and a field accessor
  @rhombus(id_name#,(rhombus(.))#,(@rhombus(field,~var))) for each
  non-@rhombus(private, ~class_clause)/@rhombus(protected, ~class_clause) method, property, dot syntax, and field in the class
  (including inherited methods, properties, dot syntax, and fields), respectively; and}

 @item{in the @rhombus(class, ~space) space, a representation of the
  class for reference as a superclass.}

)

 Fields, methods, properties, and dot syntax declared in a class can be accessed
 from an object (as opposed to just a class) using @rhombus(.), but fields,
 methods, and properties
 declared as @rhombus(private, ~class_clause) or @rhombus(protected, ~class_clause) can only be accessed by
 @rhombus(.) within methods and properties of the class or through an identifier bound
 by an @rhombus(internal, ~class_clause) form. Fields, methods, and properties
 declared as @rhombus(protected, ~class_clause) can be accessed in subclass methods,
 in addition. In static mode (see
 @rhombus(use_static)), a non-@rhombus(property, ~class_clause) method
 must be called like a function; in dynamic mode, a
 method accessed from an object
 closes over the object. Private fields, methods, and properties can be
 accessed with @rhombus(.) only statically. Syntactic forms bound via
 @rhombus(dot, ~class_clause) can be accessed with @rhombus(.) only statically,
 and functional update via @rhombus(with) also relies on static access.

 A @rhombus(field_spec) has an identifier, keyword, or both. A keyword
 implies that the default constructor expects the corresponding argument
 as a keyword argument instead of a by-position argument. The default
 annotation and binding pattern similarly expect a keyword-tagged subform
 instead of a by-position form for the corresponding fields. The name of
 the field for access with @rhombus(.) is the identifier, if present,
 otherwise the name is the symbolic form of the keyword. When a
 @rhombus(field_spec) has the @rhombus(private, ~class_clause)
 or @rhombus(protected, ~class_clause) modifier,
 however, then it is not included as an argument for the default
 constructor, binding form, or annotation form.

 When a default-value expression or block is provided for a field after
 @rhombus(=) or @litchar{:}, then the default constructor evaluates the
 @rhombus(default_expr) or @rhombus(default_body)s to obtain a value for the argument when it is not
 supplied. If a by-position field has a default-value expression or block, then
 all later by-position fields must have a default. If the class extends a
 superclass that has a non-@rhombus(private, ~class_clause)/@rhombus(protected, ~class_clause) by-position
 argument with a default, then all by-position arguments of the subclass
 must have a default. A @rhombus(default_expr) or @rhombus(default_body) can refer to earlier field
 names in the same @rhombus(class) to produce a default value. If a
 @rhombus(private) @rhombus(field_spec) lacks a @rhombus(=) and
 default-value expression, then a custom constructor must be declared
 with @rhombus(constructor, ~class_clause).

 If a block follows a @rhombus(class) form's @rhombus(field_spec) sequence,
 the block contains a mixture of definitions, expressions, exports, and class clauses. A
 @deftech{class clause} adjusts the class and bindings created by the
 @rhombus(class) form; it be one of the predefined clause forms,
 or it can be a macro that ultimately expands to a predefined form.
 Definitions and expressions in a @rhombus(class) block are evaluated at
 when the @rhombus(class) form is evaluated, and not when an
 instance is created. Definitions are scoped to the block for
 potential use by class clauses, but a @rhombus(class) form is analogous
 to @rhombus(namespace) in that local definitions can be exported.
 Exported names can replace non-private field, method, and property
 names, which are otherwise exported automatically, but exported names
 must be distinct from dot-syntax names. Since the definitions and expressions of a @rhombus(class)
 body must be processed to find @tech{class clauses} in the body, the
 class is not available for use until after the definitions and
 expressions, as if the definitions and expressions appeared before the
 @rhombus(class) form.

 When a @rhombus(class_clause) is a @rhombus(field, ~class_clause) or @rhombus(immutable, ~class_clause) form,
 then an additional field is added to the class, but the additional field
 is not represented by an arguments to the default constructor, annotation form,
 or binding-pattern form. Instead, the @rhombus(expr) or
 @rhombus(body) block the @rhombus(field, ~class_clause) gives the added
 field its initial value; that expression or block is evaluated each time an instance
 of the class is created, and it can refer to @rhombus(field_spec) names
 and earlier @rhombus(field, ~class_clause) names, but it cannot refer to @rhombus(this),
 later fields of the class, methods of the class, or properties of the class. All fields
 added through a @rhombus(field, ~class_clause) clause without @rhombus(immutable, ~class_clause) are mutable, and they
 can be updated in a custom constructor (form example) using
 @tech{assignment operators} such as @rhombus(:=). The
 @rhombus(field, ~class_clause) or @rhombus(immutable, ~class_clause) form can appear any number of times as a
 @rhombus(class_clause), with or without a
 @rhombus(private, ~class_clause) or @rhombus(protected, ~class_clause) prefix.

 When a @rhombus(class_clause) is a @rhombus(method, ~class_clause)
 form, @rhombus(override, ~class_clause) form,
 @rhombus(abstract, ~class_clause) form, @rhombus(property, ~class_clause) form,
 or method- or property-shaped
 @rhombus(final, ~class_clause), @rhombus(private, ~class_clause), or @rhombus(protected, ~class_clause) form,
 then the clause declares a method or property for the class. These clauses can
 appear any number of times as a @rhombus(class_clause) to add or
 override any number of methods or properties. See @rhombus(method, ~class_clause)
 for more information on methods and properties.

 When a @rhombus(class_clause) is an @rhombus(extends, ~class_clause)
 form, the new class is created as a subclass of the extended class. The
 extended class must not be @tech{final}. At most one
 @rhombus(class_clause) can have @rhombus(extends, ~class_clause).

 When a @rhombus(class_clause) is an @rhombus(implements, ~class_clause)
 form, the new class is created as an implementation of the named
 interfaces. Like a superclass, an interface can supply method and property
 implementations (that can be overridden) and have abstract methods and properties,
 but an interface does not have fields; see @rhombus(interface) for more
 information. Prefixing @rhombus(implements, ~class_clause) with
 @rhombus(private, ~class_clause) or @rhombus(protected, ~class_clause) makes the interface privately or protectedly
 implemented, and the interface's methods are private or protected; see @rhombus(interface) for information on privately
 or protectedly implementing an interface. A @rhombus(class_clause) can have any number
 of @rhombus(implements, ~class_clause) clauses (with or without
 @rhombus(private, ~class_clause) and @rhombus(protected, ~class_clause)). Any rule that applies to the
 superinterface of an interface also applies to the implemented
 interfaces of class, as well as any superinterface of those interfaces.

 Unless some @rhombus(class_clause) is @rhombus(nonfinal, ~class_clause),
 then the new class is @deftech{final}, which means that it cannot have
 subclasses. When a @rhombus(class_clause) is
 @rhombus(nonfinal, ~class_clause), then the new class is not final. At
 most one @rhombus(class_clause) can have
 @rhombus(nonfinal, ~class_clause).

 When a @rhombus(class_clause) is an @rhombus(internal, ~class_clause)
 form, then the clause's @rhombus(id) is bound in similar ways as
 the main class @rhombus(id_name): as a constructor, annotation
 form, binding pattern form, and namespace. A use of the internal
 @rhombus(id) as a constructor creates an instance of the same
 class, but the constructor expects arguments for all fields declared
 with @rhombus(field_spec)s, including private and protected fields. For more
 information on internal names, see @rhombus(constructor, ~class_clause),
 since the details of internal names are closely related to constructor,
 annotation, and binding pattern customization. Any number of
 @rhombus(internal, ~class_clause) declarations can appear among the
 @rhombus(class_clause)s, which means that multiple internal aliases may
 be defined.

 The @rhombus(class_clause) forms @rhombus(constructor, ~class_clause)
 or @rhombus(expression, ~class_clause),
 @rhombus(binding, ~class_clause), and
 @rhombus(annotation, ~class_clause) replace default meanings of the
 defined @rhombus(id_name) for an expression context, binding
 context, and annotation context, respectively. The
 @rhombus(reconstructor, ~class_clause) form with optional
 @rhombus(reconstructor_fields, ~class_clause) replaces the way that
 @rhombus(with) functional update is implemented. The
 @rhombus(dot, ~class_clause) form (which must be imported
 through @rhombusmodname(rhombus/meta)) replaces the way that
 @rhombus(.) accesses are resolved for expressions that have the class's
 annotation. The
 @rhombus(static_info, ~class_clause) form (which must be imported
 through @rhombusmodname(rhombus/meta)) adds static information for
 the class's instances. See
 @rhombus(constructor, ~class_clause),
 @rhombus(expression, ~class_clause),
 @rhombus(binding, ~class_clause),
 @rhombus(annotation, ~class_clause),
 @rhombus(reconstructor, ~class_clause),
 @rhombus(dot, ~class_clause), and
 @rhombus(static_info, ~class_clause) for more information on those forms.

 When a method function is accessed from a class (as a namespace) via
 @rhombus(.), the function expects an extra by-position argument that
 must be an instance of the class, and the extra argument is supplied before
 all other arguments. A field accessor from a class (as a
 namespace) via @rhombus(.) similarly takes an instance of the class,
 and it accepts a second argument to act as a mutator if the field is mutable.
 A property accessor from a class (as a
 namespace) via @rhombus(.) takes an instance of the class, and it accepts
 an additional value to assign to the property (if the property supports assignment).
 Even when a method is accessed via its class instead of an object, if
 the method and class are not @tech{final}, the called method is
 determined by the object and may be from a subclass that overrides the
 method; the same is true for properties.

 Each field, method, property, and dot-syntax name must be distinct from all other field,
 method, property, and dot-syntax names, whether from a parenthesized @rhombus(field_spec), from a
 @rhombus(field, ~class_clause) clause, or from a method, property, or dot-syntax clause. If an
 @rhombus(extends, ~class_clause) or @rhombus(implements, ~class_clause) clause is present, then each name
 must also be distinct from any name in the superclass or interface, except that
 a @rhombus(override, ~class_clause) clause must name a method or property that is
 already declared in the superclass. Private superclass fields,
 methods, and properties are not visible to the subclass, so their names are not required
 to be distinct from subclass field, method, and property names. When a method or property is
 overridden via @rhombus(override, ~class_clause), the original and
 overriding versions must be both methods or both properties.

 See @secref(~doc: guide_doc, "static-info-rules") for information about static
 information associated with classes.

@examples(
  class Posn(x, y)
  Posn(1, 2)
  Posn.x
  Posn.x(Posn(1, 2))
  Posn(1, 2).x
  ~error:
    class Posn3(z):
      extends Posn
  class Posn2D(x, y):
    nonfinal
  class Posn3D(z):
    extends Posn2D
  Posn3D(1, 2, 3)
  class Rectangle(w, h):
    nonfinal
    constructor (~width: w, ~height: h):
      super(w, h)
  class Square():
    extends Rectangle
    constructor (~side: s):
      super(~width: s, ~height: s)()
  Square(~side: 10)
)

}

@doc(
  ~literal: :: class interface
  defn.macro 'class.together:
                $class_or_interface
                ...'
  grammar class_or_interface:
    class $class_decl
    interface $interface_decl
){

 Defines the same bindings as the @rhombus(class_or_interface)s, but
 with an indirection on annotations so that the defined class and
 interface names can be used as field, method, and property-result
 annotations and in all of the other class and interface declarations.

 The @rhombus(class.together) form expands to a combination of
 @rhombus(namespace), @rhombus(annot.delayed_declare), and
 @rhombus(annot.delayed_complete) declarations.

@examples(
  class.together:
    class Tree(x :: List.of(Node))
    class Node(val, children :: Tree)
  class.together:
    class Even():
      nonfinal
      abstract method get_next() :: Odd
    class Odd():
      nonfinal
      abstract method get_next() :: Even
)

}


@doc(
  class_clause.macro 'extends $id_name'
  class_clause.macro 'extends: $id_name'
){

 A @tech{class clause} recognized by @rhombus(class) to define a class
 that is a subclass of the one named by @rhombus(id_name).

}

@doc(
  class_clause.macro 'implements $id_name ...'
  class_clause.macro 'implements: $id_name ...; ...'
){

 A @tech{class clause} recognized by @rhombus(class) to define a class
 that implements subclasses named by @rhombus(id_name)s. See
 @rhombus(class) and @rhombus(interface).

}

@doc(
  class_clause.macro 'nonfinal'
){

 As a @tech{class clause}, @rhombus(nonfinal, ~class_clause) is
 recognized by @rhombus(class) so that the new class is not @tech{final}
 (as it would be by default).

}

@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause

  class_clause.macro 'final $method_impl'
  class_clause.macro 'final #,(@rhombus(method, ~class_clause)) $method_impl'
  class_clause.macro 'final #,(@rhombus(override, ~class_clause)) $method_impl'
  class_clause.macro 'final #,(@rhombus(override, ~class_clause)) #,(@rhombus(method, ~class_clause)) $method_impl'
  class_clause.macro 'final #,(@rhombus(property, ~class_clause)) $property_impl'
  class_clause.macro 'final #,(@rhombus(override, ~class_clause)) #,(@rhombus(property, ~class_clause)) $property_impl'
  class_clause.macro 'final #,(@rhombus(protected, ~class_clause)) $method_impl'
  class_clause.macro 'final #,(@rhombus(protected, ~class_clause)) #,(@rhombus(method, ~class_clause)) $method_impl'
  class_clause.macro 'final #,(@rhombus(protected, ~class_clause)) #,(@rhombus(property, ~class_clause)) $property_impl'
){

 The @rhombus(final, ~class_clause) form as a @tech{class clause}
 is followed by a method or property declaration.
 In that case, the method or property is final, even
 if the enclosing class is not (and an interface is never final). A final
 method or property cannot be overridden in subclaseses or subinterfaces. Using
 @rhombus(final, ~class_clause) with an immediate declaration is the same
 as @rhombus(final, ~class_clause) followed by
 @rhombus(method, ~class_clause). Including
 @rhombus(override, ~class_clause) means that the method or property must be defined
 in the superclass or a superinterface, while it must not be defined in
 the superclass or a superinterface if @rhombus(override, ~class_clause)
 is not used.

}

@doc(
  ~nonterminal:
    maybe_annot: class ~defn
  class_clause.macro 'field $field_impl'
  class_clause.macro 'immutable $field_impl'
  class_clause.macro 'immutable field $field_impl'

  grammar field_impl:
    $id $maybe_annot = $expr
    $id $maybe_annot: $body; ...
){

 A @tech{class clause} recognized by @rhombus(class) to add fields to
 the class. The @rhombus(expr) or @rhombus(body) block is evaluated each time the class is instantiated,
 but it cannot refer to @rhombus(this) or fields, methods, or properties of an object.
 See @rhombus(class) for more information.

}

@doc(
  ~nonterminal:
    maybe_res_annot: fun ~defn
    case_maybe_kw_opt: fun ~defn
    case_maybe_kw: fun ~defn
    bind_maybe_kw_opt: fun ~defn
    name_option: fun ~defn
    who_option: fun ~defn
    rest: fun ~defn

  class_clause.macro 'method $method_impl'
  class_clause.macro 'property $property_impl'
  class_clause.macro 'override $method_impl'
  class_clause.macro 'override #,(@rhombus(method, ~class_clause)) $method_impl'
  class_clause.macro 'override #,(@rhombus(property, ~class_clause)) $property_impl'

  grammar method_impl:
    $id $maybe_res_annot:
      $entry_point
    $id $case_maybe_kw_opt:
      $name_option; ...
      $body
      ...
    Z| $id $case_maybe_kw:
         $name_option; ...
         $body
         ...
     | ...

  grammar property_impl:
    $id $maybe_res_annot:
      $who_option; ...
      $body
        ...
    Z| $id $maybe_res_annot:
         $who_option; ...
         $body
         ...
    Z| $id $maybe_res_annot:
         $who_option; ...
         $body
         ...
     | $id := $binding:
         $who_option; ...
         $body
         ...
){

 @margin_note_block{The @rhombus(method, ~class_clause) form is used in
  documentation with a different shape than in implementation. See
  @secref("doc_method") for more information.}

 These @tech{class clauses} are recognized
 by @rhombus(class) to declare methods and properties, along
 with the method and property forms of @rhombus(final, ~class_clause),
 @rhombus(private, ~class_clause), and @rhombus(protected, ~class_clause). The combination
 @rhombus(override, ~class_clause) followed by
 @rhombus(method, ~class_clause) is the same as just
 @rhombus(override, ~class_clause).

 A @rhombus(method_impl) is either an @rhombus(id) followed by
 an optional result annotation and a block containing an @tech{entry
  point}, or it has the same form as a @rhombus(fun) definition with a
 form name like @rhombus(method, ~class_clause) in place of
 @rhombus(fun). A @rhombus(maybe_res_annot) applies to the immediate method
 implementation as well as overriding implementations in subclasses; a
 result annotation within an @tech{entry point}, in contrast, does not
 apply to subclasses. A @rhombus(maybe_res_annot) can specify a
 @tech(~doc: guide_doc){converter annotation} only if the method is @rhombus(final)
 or the enclosing class is @tech{final}; the conversion applies
 before inherited result annotations for the method are checked.

 A @rhombus(property, ~class_clause) clause declares or overrides a
 @tech(~doc: guide_doc){property}, which is like a method in that using the property evaluates a
 block. However, the property is used either as an expression, which is
 analogous to calling a method with no arguments, or as the left-hand
 side of an @tech{assignment operator} like @rhombus(:=) form, which is
 analogous to calling a method with
 the right-hand side of @rhombus(:=) as the method argument. A property
 always supports a reference form, but it supports assignment only when
 the @rhombus(property, ~class_clause) form includes a @rhombus(:=) case.
 In a @rhombus(property, ~class_clause)'s @rhombus(:=) case, the part
 after @rhombus(:=) is a binding analogous to a function-argument binding,
 and the subsequent @rhombus(body) will normally refer to that binding.
 Using @rhombus(:=) with a property always produces @rhombus(#void),
 but more generally, any value returned by the @rhombus(body) of a property definition's
 @rhombus(:=) case is ignored by assignment operators. A @rhombus(maybe_res_annot) in a
 @rhombus(property, ~class_clause) clause applies to overriding implementations
 in subclasses, but it imposes no constraints on the right-hand part of
 @rhombus(:=) or other assignment operators when assigning to a property.

 In the body of a method or property, the special expression form @rhombus(this)
 refers to the object whose method was called. Fields (in the case of a
 class) and methods can be accessed using @rhombus(this) and @rhombus(.),
 but they can also be used directly.
 Using a field, method, or property name
 directly is the same as using @rhombus(this) and @rhombus(.) in static
 mode (which implies that a direct reference to a method name must be a
 call of the method). An argument that has the same name as a field,
 method, or property shadows the field, method, or property.

}

@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause
    field_impl: field ~class_clause

  class_clause.macro 'private #,(@rhombus(implements, ~class_clause)) $id_name ...'
  class_clause.macro 'private #,(@rhombus(implements, ~class_clause)): $id_name ...; ...'
  class_clause.macro 'private #,(@rhombus(field, ~class_clause)) $field_impl'
  class_clause.macro 'private #,(@rhombus(immutable, ~class_clause)) $field_impl'
  class_clause.macro 'private #,(@rhombus(immutable, ~class_clause)) #,(@rhombus(field, ~class_clause)) $field_impl'
  class_clause.macro 'private $method_impl'
  class_clause.macro 'private #,(@rhombus(method, ~class_clause)) $method_impl'
  class_clause.macro 'private #,(@rhombus(property, ~class_clause)) $property_impl'
  class_clause.macro 'private #,(@rhombus(override, ~class_clause)) $method_impl'
  class_clause.macro 'private #,(@rhombus(override, ~class_clause)) #,(@rhombus(method, ~class_clause)) $method_impl'
  class_clause.macro 'private #,(@rhombus(override, ~class_clause)) #,(@rhombus(property, ~class_clause)) $property_impl'
){

 A @tech{class clause} that declares interfaces that are privately
 implemented (see @rhombus(interface)), a @tech{class clause} that
 declares a private field, or a @tech{class clause}
 that declares a private method or property. See @rhombus(class)
 and @rhombus(method, ~class_clause) for more
 information on field, method, and property declarations. A
 @rhombus(private, ~class_clause) without
 @rhombus(implements, ~class_clause), @rhombus(field, ~class_clause), @rhombus(immutable, ~class_clause),
 @rhombus(method, ~class_clause), @rhombus(override, ~class_clause),
 or @rhombus(property, ~class_clause) is
 equivalent to @rhombus(private, ~class_clause) followed by
 @rhombus(method, ~class_clause).

 Private fields, methods, and properties can be accessed only within the body of the enclosing
 @rhombus(class) or through an identifier declared with
 @rhombus(internal, ~class_clause). When referenced via the
 @rhombus(.) operator, only static references are allowed
 through the enclosing class's annotation (not a subclass annotation) or
 through an @rhombus(internal, ~class_clause) identifier's annotation.

}

@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause
    field_impl: field ~class_clause

  class_clause.macro 'protected #,(@rhombus(implements, ~class_clause)) $id_name ...'
  class_clause.macro 'protected #,(@rhombus(implements, ~class_clause)): $id_name ...; ...'
  class_clause.macro 'protected #,(@rhombus(field, ~class_clause)) $field_impl'
  class_clause.macro 'protected #,(@rhombus(immutable, ~class_clause)) $field_impl'
  class_clause.macro 'protected #,(@rhombus(immutable, ~class_clause)) #,(@rhombus(field, ~class_clause)) $field_impl'
  class_clause.macro 'protected $method_impl'
  class_clause.macro 'protected #,(@rhombus(method, ~class_clause)) $method_impl'
  class_clause.macro 'protected #,(@rhombus(property, ~class_clause)) $property_impl'
){

 Like @rhombus(private, ~class_clause), but protected fields, methods,
 and properties, can be referenced within subclasses, and they can be overridden when not
 @rhombus(final, ~class_clause). An overriding declaration does not use
 @rhombus(protected, ~class_clause) again.

}

@doc(
  ~nonterminal:
    maybe_res_annot: fun ~defn
    case_maybe_kw_opt: fun ~defn
    case_maybe_kw: fun ~defn
    bind_maybe_kw_opt: fun ~defn
    rest: fun ~defn

  class_clause.macro 'abstract $method_decl'
  class_clause.macro 'abstract #,(@rhombus(method, ~class_clause)) $method_decl'
  class_clause.macro 'abstract #,(@rhombus(override, ~class_clause)) $method_decl'
  class_clause.macro 'abstract #,(@rhombus(property, ~class_clause)) $property_decl'
  class_clause.macro 'abstract #,(@rhombus(override, ~class_clause)) #,(@rhombus(property, ~class_clause)) $property_decl'
  class_clause.macro 'abstract #,(@rhombus(protected, ~class_clause)) $method_decl'
  class_clause.macro 'abstract #,(@rhombus(protected, ~class_clause)) #,(@rhombus(method, ~class_clause)) $method_decl'
  class_clause.macro 'abstract #,(@rhombus(protected, ~class_clause)) #,(@rhombus(property, ~class_clause)) $property_decl'

  grammar method_decl:
    $id $maybe_res_annot
    $id ($bind_maybe_kw_opt, ..., $rest, ...) $maybe_res_annot
    Z| $id ($bind_maybe_kw_opt, ..., $rest, ...) $maybe_res_annot
     | ...

  grammar property_decl:
    $id $maybe_res_annot
    Z| $id $maybe_res_annot

){

 A @tech{class clause} that declares a method
 or property without an implementation.

 When a class has an abstract method or property,
 either declared directly or inherited, the underlying constructor for the class
 throws an exception. The method or property must be overridden with a
 @rhombus(override, ~class_clause) class in a subclass, and then the
 subclass can be instantiated (as long as it has no other abstract
 methods). A @tech{final} class cannot have an abstract method or property.

 A method or property can be both @rhombus(abstract, ~class_clause) and
 @rhombus(override, ~class_clause). In that case, if the overridden method or property
 is not abstract, then the method or property becomes abstract and most be overridden
 in a subclass before instantiation. Even if the overridden method or property is
 already abstract, an @rhombus(abstract, ~class_clause)
 @rhombus(override, ~class_clause) can be useful to impose an additional
 result annotation.

}

@doc(
  expr.macro 'this'
){

 The @rhombus(this) form can only be used within a method or property. See
 @rhombus(method, ~class_clause) for more information.

}

@doc(
  ~nonterminal:
    arg_expr: block expr

  expr.macro 'super . $id($arg_expr, ...)'
  expr.macro 'super'
){

 The @rhombus(super) form can only be used in two places: within a
 method property call to invoke another method or property @rhombus(id) that is
 statically known to be implemented in a superclass or superinterface of
 the enclosing class; or within a custom constructor to as a reference to
 an underlying constructor. In the case of a method call or property use, the
 @rhombus(super) call invokes the superclass's or superinterface's
 implementation, even if a method or property named @rhombus(id) is
 overridden in a class, interface, or a subclass.

}

@doc(
  class_clause.macro 'internal $id'
  class_clause.macro 'internal: $id'
){

 A @tech{class clause} recognized by
 @rhombus(class) and @rhombus(interface) to bind @rhombus(id) to
 the class's representation. See @rhombus(class)
 and @rhombus(constructor, ~class_clause) for more
 information.

 When used as a @tech{namespace}, @rhombus(id) can access the
 immediate private and protected fields, methods, and properties of the class or interface
 containing the @rhombus(internal, ~class_clause) declaration. Along
 similar lines, @rhombus(id) as an annotation associates static
 information with an expression or binding so that @rhombus(.) can be
 used to access private and protected fields, methods and properties, but only with @rhombus(.) as
 statically resolved.

}

@doc(
  ~nonterminal:
    id_name: namespace ~defn
    case_maybe_kw_opt: fun ~defn
    case_maybe_kw: fun ~defn

  class_clause.macro 'constructor $maybe_name: $entry_point'
  class_clause.macro 'constructor $maybe_name $case_maybe_kw_opt'
  class_clause.macro 'constructor
                      | $maybe_name $case_maybe_kw
                      | ...'
  class_clause.macro 'constructor $disable_form'
  class_clause.macro 'expression: $entry_point'
  class_clause.macro '«expression '$id $pattern ...': '$template ...'»'
  class_clause.macro '«expression
                       | '$id $pattern ...': '$template ...'
                       | ...»'
  class_clause.macro 'expression $disable_form'
  class_clause.macro 'binding: $entry_point'
  class_clause.macro '«binding '$id $pattern ...': '$template ...'»'
  class_clause.macro '«binding
                       | '$id $pattern ...': '$template ...'
                       | ...»'
  class_clause.macro 'binding $disable_form'
  class_clause.macro 'annotation: $entry_point'
  class_clause.macro '«annotation '$id $pattern ...': '$template ...'»',
  class_clause.macro '«annotation
                       | '$id $pattern ...': '$template ...'
                       | ...»'
  class_clause.macro 'annotation $disable_form'

  grammar maybe_name:
    $id
    #,("ϵ")

  grammar disable_form:
    ~error
    : ~error
    ~none
    : ~none
){

 These @tech{class clauses} are recognized by @rhombus(class) to replace or suppress
 the default constructor, expression form, binding form, or annotation form. For
 @rhombus(constructor, ~class_clause), the second two forms are shorthand
 for using a @rhombus(fun) @tech{entry point}. For each of
 @rhombus(expression, ~class_clause),
 @rhombus(binding, ~class_clause), and @rhombus(annotation, ~class_clause),
 the @rhombus(pattern) and @rhombus(template) shorthands are similar to
 using a @rhombus(macro, ~entry_point) entry point, but an @rhombus(id) must be
 presented instead of @rhombus(()) for the pattern, and @rhombus(id)
 must match the name of the class being defined. When @rhombus(annotation, ~class_clause)
 is not present, in addition to the class name being bound as a default annotation,
 an @rhombus(of, ~datum) annotation constructor is exported as a field
 of the class.

 When a @rhombus(constructor, ~class_clause),
 @rhombus(expression, ~class_clause), @rhombus(binding, ~class_clause),
 or @rhombus(annotation, ~class_clause) clause has a
 @rhombus(disable_form) containing @rhombus(~error), then the binding of
 the class name in the corresponding space reports an error for any use.
 When a @rhombus(disable_form) constains @rhombus(~none), then the class
 name is not bound in the corresponding space by the enclosing
 @rhombus(class) form. The @rhombus(constructor, ~class_clause) and
 @rhombus(expression, ~class_clause) forms are equivalent when used with a
 @rhombus(disable_form).

 When a @rhombus(class) has a @rhombus(constructor, ~class_clause)
 form with an empty @rhombus(maybe_name), then a use of new class's
 @rhombus(id_name) as a
 constructor function invokes a function the @tech{entry point} (typically a
 @rhombus(fun, ~entry_point) form) in the block after
 @rhombus(constructor, ~class_clause). That function must return an
 instance of the new class, typically by calling
 @rhombus(super):

@itemlist(

 @item{If the new class does not have a superclass, then
  @rhombus(super) accesses the default constructor, which returns an
  instance of the class. Note that this instance might be an instance of a
  subclass if the new class is not @tech{final}.},

 @item{If the new class has a superclass, then @rhombus(super) accesses
  a curried function. The function accepts the same arguments as the
  superclass constructor. Instead of returning an instance of the class,
  it returns a function that accepts arguments as declared by
  @rhombus(field_spec, ~var)s in the new subclass, including
  @rhombus(private, ~class_clause)/@rhombus(protected, ~class_clause) clause fields; the result of that function is
  an instance of the new class. Again, the result instance might be an
  instance of a subclass if the new class is not @tech{final}.}

)

 If a @rhombus(constructor, ~class_clause) form has an
 @rhombus(id) for @rhombus(maybe_name) that is not the same as
 the enclosing class's @rhombus(id_name), then the constructor is bound to
 @rhombus(id) instead of @rhombus(id_name).
 Typically, naming a constructor is paired with an
 @rhombus(expression, ~class_clause) declaration that refers to that
 constructor.

 If a class has an @rhombus(internal, ~class_clause) clause, then the
 bound name acts as a constructor like @rhombus(super), except that it
 always instantiates the class that contains the
 @rhombus(internal, ~class_clause) clause (so, the internal name is not a
 substitute for using @rhombus(super) in a custom constructor). If a
 superclass has a custom constructor, the default constructor of a
 subclass assumes that the superclass constructor accepts the same
 argument as the default superclass constructor.

 When a @rhombus(class) has a @rhombus(expression, ~class_clause) form,
 then a use of the new class's @rhombus(id_name) as an
 expression invokes the @tech{entry point} (typically a
 @rhombus(macro, ~entry_point) form) in the block after
 @rhombus(expression, ~class_clause). The @rhombus(entry_point) is a
 meta-time expression. This macro replaces the default meaning of the
 @rhombus(id_name) as a reference to the constructor. When
 @rhombus(expression, ~class_clause), then the default
 @rhombus(id_name.of) annotation constructor accepts only
 @tech(~doc: guide_doc){predicate annotations}.

 When a @rhombus(class) has a @rhombus(binding, ~class_clause) form,
 then a use of the new class's @rhombus(id_name) as a
 binding-pattern constructor invokes the @tech{entry point} (typically a
 @rhombus(macro, ~entry_point) form) in the block after
 @rhombus(binding, ~class_clause). The @rhombus(entry_point) is a
 meta-time expression. There is no @rhombus(super) for custom binding
 patterns; instead, use @rhombus(internal, ~class_clause) to bind an
 internal name that acts similar to the class's default binding form, but
 with two differences: it does not expect bindings for superclass fields,
 but it does expect bindings for private and protected fields declared with a
 @rhombus(field_spec, ~var). When a class has a superclass, then a custom
 binding form is typically implemented using an internal binding form,
 the superclass's binding form, and the @rhombus(&&, ~bind) binding
 operator. When a superclass has a custom binding form, then a class must
 have a custom binding form, too (unlike the case with constructors,
 where a default constructor still can be generated to call a custom
 superclass constructor).

 When a @rhombus(class) has an @rhombus(annotation, ~class_clause) form,
 then a use of new class's @rhombus(id_name) in a
 annotation invokes the @tech{entry point} (typically a
 @rhombus(macro, ~entry_point) form) in the block after
 @rhombus(annotation, ~class_clause). The @rhombus(entry_point) is a
 meta-time expression. Similar to custom binding forms, a custom
 annotation form normally needs an internal annotation name bound with
 @rhombus(internal, ~class_clause); the @rhombus(of) form of that
 annotation expects annotations for only immediate fields of the class,
 but including private and protected ones declared with @rhombus(field_spec, ~var)s.
 Use the @rhombus(&&, ~annot) annotation operator to combine the internal
 annotation with a superclass annotation. When a superclass has a custom
 annotation form, then a class must have a custom annotation form, too.
 Typically, an @rhombus(of, ~datum) annotation is also defined and
 exported from the class's namespace to go along with the customization
 of the class name as an annotation.

}


@doc(
  class_clause.macro 'opaque'
){

 When a @tech{class clause} is @rhombus(opaque, ~class_clause), then the
 default printed form of a class instance does not show fields, and instead prints
 @litchar{...} in parentheses after the class name.

}


@doc(
  ~nonterminal:
    field_spec: class ~defn
  class_clause.macro 'prefab'
){

 When a @tech{class clause} is @rhombus(prefab, ~class_clause), then the
 representation of an instance depends only on the class name, the number
 of fields in the class, and the mutability of each field. When two
 @rhombus(class) declarations have @rhombus(prefab, ~class_clause), the
 same field count, and the same mutability of each field, then instances
 from one @rhombus(class) declaration count as instances of the other
 @rhombus(class) declaration, even if they have different annotations on
 the fields, different methods, and so on. That sharing implies a number
 of constraints:

@itemlist(

 @item{A @rhombus(prefab, ~class_clause) class's fields and methods are
  accessible from an instance expression followed by @rhombus(.) only when
  the reference is statically resolved. That is, dynamic @rhombus(.)
  lookup will fail to find a field or a method.}

 @item{A @rhombus(prefab, ~class_clause) class's methods and properties
  must all be @rhombus(final, ~class_clause).}

 @item{If a @rhombus(prefab, ~class_clause) class has a superclass, the
  superclass must also be @rhombus(prefab, ~class_clause).}

 @item{A @rhombus(prefab, ~class_clause) class is implicitly
  @rhombus(nonfinal, ~class_clause), and it cannot be
  @rhombus(opaque, ~class_clause) or @rhombus(authentic, ~class_clause).}

 @item{When a field is declared with a @rhombus(field_spec) that has a
  @rhombus(::, ~annot) annotation, a field value is checked against the
  annotation only when an instance is created through a constructor from
  the @rhombus(class) declaration, and not when the field is accessed. The
  field access is effectively treated as having a @rhombus(:~, ~annot)
  annotation.}

)

}


@doc(
  class_clause.macro 'authentic'
){

 When a @tech{class clause} is @rhombus(authentic, ~class_clause), then
 the new class cannot be chaperoned or impersonated. At most one class
 clause in a @rhombus(class) form can be
 @rhombus(authentic, ~class_clause).

}


@doc(
  expr.macro '$obj with ($id = $expr, ...)'
){

 Performs a functional update of the object produced by @rhombus(obj) by
 creating a new instance of the same class, using the values of
 @rhombus(obj)'s fields for the new object except as replaced by
 @rhombus(fields).

 The listed @rhombus(id)s must all correspond to fields of the object
 that would be represented by a default constructor, independent of
 whether the field is optional or would be supplied with a keyword. The
 fields are checked dynamically, unless @rhombus(with) is static (see
 @rhombus(use_static)), in which case the set of fields must syntactically
 match the ones expected for the class indicated by static information.

 An object is updated by calling its class's @deftech{reconstructor}. A
 default reconstructor for a non-abstract class without a custom
 constructor is implemented by calling the class's constructor. A
 @rhombus(class) declaration can contain an
 @rhombus(reconstructor, ~class_clause) clause to replace the default
 implementation or supply an implementation when a default is not
 available.

@examples(
  ~defn:
    class Posn(x, y)
  ~repl:
    def p = Posn(1, 2)
    p with (x = 10)
    p
  ~defn:
    class Posn(x, y):
      nonfinal
    class Posn3D(z):
      extends Posn
      // same as the default reconstructor:
      reconstructor (x, y, z = this.z):
        Posn3D(x, y, z)
  ~repl:
    def p = Posn3D(1, 2, 3)
    (p :: Posn) with (x = 10)
  ~hidden:
    import rhombus/meta open
  ~defn:
    class PosnX(x, y):
      internal _PosnX
      expression 'PosnX< $x ... || $y ... >':
        '_PosnX($x ..., $y ...) :~ PosnX'
      reconstructor (x, y):
        PosnX< x || y >
  ~repl:
    def px = PosnX< 1 || 2 >
    px with (y = 20)
    px
  ~defn:
    class PosnD(x, y):
      reconstructor_fields:
        x: this.x
        y: this.y
        delta: 0
      reconstructor (x, y, delta):
        PosnD(x+delta, y+delta)
  ~repl:
    PosnD(1, 2) with (delta = 10)
)

}


@doc(
  ~nonterminal:
    case_maybe_kw_opt: fun ~defn
    case_maybe_kw: fun ~defn

  class_clause.macro 'reconstructor: $entry_point'
  class_clause.macro 'reconstructor case_maybe_kw_opt'
  class_clause.macro 'reconstructor
                      | case_maybe_kw
                      | ...'
){

 A form for @rhombus(class) to provide an implementation of a
 functional-update @tech{reconstructor} via @rhombus(with). The
 implementation is similar to a method, in that @rhombus(this) is bound
 to the object being updated (i.e., the object whose fields are being
 used to create a new instance of the class).

 By default, a class's reconstructor should expect as many arguments as
 the class has fields, and it should expect them in the declared order.
 If the class has a @rhombus(reconstructor_fields, ~class_clause)
 declaration, the reconstructor should instead expect those fields in addition to
 the ones that the superclass (if any) expects for its reconstuctor.

 A reconstructor should not expect keyword arguments; all fields are
 supplied by-position. If the class has a superclass, then any fields
 added by the class should be made optional, because those arguments will
 not be supplied when an instance of the class is updated based on static
 information corresponding to the superclass. The optional arguments
 typically have a default value that is drawn from the corresponding
 field of @rhombus(this).

 See @rhombus(with) for examples.

}

@doc(
  ~nonterminal:
    field_id: block id

  class_clause.macro 'reconstructor_fields:
                        field_id: $body; ...
                        ...'
){

 Declares fields available to be supplied to a @rhombus(with)
 functional-update operation for instances of a class, instead of fields
 declared in the enclosing class, but in addition to the
 fields that are allowed for the class's superclass (if any).

 The @rhombus(body) sequence for a field is evaluated for a use of
 @rhombus(with) that does not supply the field. The @rhombus(body)
 sequence can refer to @rhombus(this) or other bindings that would be
 available in a 0-argument method of the class.

 When a class has a @rhombus(reconstructor_fields, ~class_clause)
 declaration, then the class and any subclass that extends it must have a
 @rhombus(reconstructor, ~class_clause) declaration, since there is not
 necessarily any connection between the declared reconstructor fields and
 the constructor's arguments.

 See @rhombus(with) for an example.

}


@doc(
  class_clause.macro 'primitive_property $expr: $body; ...'
){

 A @tech{class clause} that bridges Rhombus
 and Racket protocols. The enclosing class
 implements the primitive, Racket-level property produced by
 @rhombus(expr) with the value produced by @rhombus(body).

 If the class is not @tech{final}, then any subclass of the class by
 default implements the property with the same value, but it can specify
 the same property to produce a different value that replaces the one
 produced by @rhombus(body). Implementing an interface with
 @rhombus(implements, ~class_clause), meanwhile, corresponds to writing
 @rhombus(primitive_property, ~class_clause) for each primitive property
 in the interface and its superinterfaces, which means that those
 properties cannot be immediately overridden with
 @rhombus(primitive_property, ~class_clause).

 The @rhombus(expr) and @rhombus(body) are evaluated in order relative
 to surrounding @rhombus(expr) and @rhombus(defn) forms.

@examples(
  ~defn:
    import lib("racket/base.rkt")
    class NamedPosn(name, x, y):
      primitive_property base.#{prop:object-name}:
        fun (self): self.name
  ~repl:
    base.#{object-name}(NamedPosn("Rumpelstiltskin", 1, 2))
)

}
