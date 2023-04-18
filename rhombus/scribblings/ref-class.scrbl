#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Classes and Interfaces}

@doc(
  ~literal: :: extends binding field
  ~nonterminal:
    default_expr: block expr
    default_body: block body
    method_impl: method ~class_clause
    property_impl: method ~class_clause
    method_decl: method ~class_clause
    property_decl: method ~class_clause

  defn.macro 'class $id_path($field_spec, ...)'
  defn.macro 'class $id_path($field_spec, ...):
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
    ε

  grammar maybe_annot:
    :: annot
    ε

  grammar maybe_default:
    = $default_expr
    : : $default_body; ...
    ε

  grammar class_clause_or_body_or_export:
    $class_clause
    $body
    $export

  grammar class_clause:
    #,(@rhombus(field, ~class_clause)) $id $maybe_annot = $expr
    #,(@rhombus(field, ~class_clause)) $id $maybe_annot: $body; ...
    #,(@rhombus(private, ~class_clause)) #,(@rhombus(field, ~class_clause)) $id $maybe_annot = $expr
    #,(@rhombus(private, ~class_clause)) #,(@rhombus(field, ~class_clause)) $id $maybe_annot: $body; ...
    #,(@rhombus(method, ~class_clause)) $method_impl
    #,(@rhombus(override, ~class_clause)) $method_impl
    #,(@rhombus(final, ~class_clause)) $method_impl
    #,(@rhombus(private, ~class_clause)) $method_impl
    #,(@rhombus(abstract, ~class_clause)) $method_decl
    #,(@rhombus(property, ~class_clause)) $property_impl
    #,(@rhombus(extends, ~class_clause)) $id_path
    #,(@rhombus(implements, ~class_clause)) $implements_decl
    #,(@rhombus(private, ~class_clause)) #,(@rhombus(implements, ~class_clause)) $implements_decl
    #,(@rhombus(final, ~class_clause))
    #,(@rhombus(nonfinal, ~class_clause))
    #,(@rhombus(internal, ~class_clause)) $id
    #,(@rhombus(constructor, ~class_clause)) $constructor_decl
    #,(@rhombus(expression, ~class_clause)) $expression_decl
    #,(@rhombus(binding, ~class_clause)) $binding_decl
    #,(@rhombus(annotation, ~class_clause)) $annotation_decl
    #,(@rhombus(opaque, ~class_clause))
    $other_class_clause
){

 Binds @rhombus(id_path) as a class name in several @tech{spaces}:

@itemlist(

 @item{in the @rhombus(expr, ~space) space,
  a constructor function or form, which by default is a function that
  takes as many arguments
  as the supplied non-@rhombus(private, ~class_clause) @rhombus(field_spec)s
  in parentheses, and it returns an instance of the class;},

 @item{in the @rhombus(annot, ~space) space,
  an annotation, which is satisfied by any instance of the class,
  and an annotation constructor @rhombus(id_path.of), which by
  default takes as many annotation arguments as supplied
  non-@rhombus(private, ~class_clause) @rhombus(field_spec)s in
  parentheses;},

 @item{in the @rhombus(bind, ~space) space,
  a binding-pattern constructor, which by default takes as many
  patterns as the supplied non-@rhombus(private, ~class_clause)
  @rhombus(field_spec)s in parentheses and matches an instance of the
  class where the fields match the corresponding patterns;},

 @item{in the @rhombus(namespace, ~space) space,
  a @tech{namespace} to access exported bindings as well as a
  function
  @rhombus(id_path#,(rhombus(.))#,(@rhombus(method,~var))),
  a function
  @rhombus(id_path#,(rhombus(.))#,(@rhombus(property,~var))),
  and a field accessor
  @rhombus(id_path#,(rhombus(.))#,(@rhombus(field,~var))) for each
  non-@rhombus(private, ~class_clause) method, property, and field in the class
  (including inherited methods, properties, and fields); and}

 @item{in the @rhombus(class, ~space) space, a representation of the
  class for reference as a superclass.}
 
)

 Fields, methods, and properties declared in a class can be accessed
 from an object (as opposed to just a class) using @rhombus(.), but fields,
 methods, and properties
 declared as @rhombus(private, ~class_clause) can only be accessed by
 @rhombus(.) within methods and properties of the class or through an identifier bound
 by an @rhombus(internal, ~class_clause) form. In static mode (see
 @rhombus(use_static)), a non-@rhombus(property, ~class_clause) method
 must be called like a function; in dynamic mode, a
 method accessed from an object
 closes over the object. Private fields, methods, and properties can be
 accessed with @rhombus(.) only statically.

 A @rhombus(field_spec) has an identifier, keyword, or both. A keyword
 implies that the default constructor expects the corresponding argument
 as a keyword argument instead of a by-position argument. The default
 annotation and binding pattern similarly expect a keyword-tagged subform
 instead of a by-position form for the corresponding fields. The name of
 the field for access with @rhombus(.) is the identifier, if present,
 otherwise the name is the symbolic form of the keyword. When a
 @rhombus(field_spec) has the @rhombus(private, ~class_clause) modifier,
 however, then it is not included as an argument for the default
 constructor, binding form, or annotation form.

 When a default-value expression or block is provided for a field after
 @rhombus(=) or @litchar{:}, then the default constructor evaluates the
 @rhombus(default_expr) or @rhombus(default_body)s to obtain a value for the argument when it is not
 supplied. If a by-position field has a default-value expression or block, then
 all later by-position fields must have a default. If the class extends a
 superclass that has a non- @rhombus(private, ~class_clause) by-position
 argument with a default, then all by-position arguments of the subclass
 must have a default. A @rhombus(default_expr) or @rhombus(default_body) can refer to earlier field
 names in the same @rhombus(class) to produce a default value. If a
 @rhombus(priviate) @rhombus(field_spec) lacks a @rhombus(=) and
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
 exported names must be distinct from all non-private field, method, and property
 names (which are automatically exported from the class in its role as a
 namespace). Since the definitions and expressions of a @rhombus(class)
 body must be processed to find @tech{class clauses} in the body, the
 class is not available for use until after the definitions and
 expressions, as if the definitions and expressions appeared before the
 @rhombus(class) form.

 When a @rhombus(class_clause) is a @rhombus(field, ~class_clause) form,
 then an additional field is added to the class, but the additional field
 is not represented by an arguments to the default constructor, annotation form,
 or binding-pattern form. Instead, the @rhombus(expr) or
 @rhombus(body) block the  @rhombus(field, ~class_clause) gives the added
 field its initial value; that expression or block is evaluated each time an instance
 of the class is created, but it cannot refer to @rhombus(this),
 fields of the class, methods of the class, or properties of the class. All fields
 added through a @rhombus(field, ~class_clause) clause are mutable, and they
 can be updated in a custom constructor (form example) using @rhombus(:=). The
 @rhombus(field, ~class_clause) can appear any number of times as a
 @rhombus(class_clause), with or without a
 @rhombus(private, ~class_clause) prefix.

 When a @rhombus(class_clause) is a @rhombus(method, ~class_clause)
 form, @rhombus(override, ~class_clause) form,
 @rhombus(abstract, ~class_clause) form, method-shaped
 @rhombus(final, ~class_clause) or @rhombus(private, ~class_clause) form,
 or @rhombus(property, ~class_clause) form,
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
 @rhombus(private, ~class_clause) makes the interface privately
 implemented; see @rhombus(interface) for information on privately
 implementing an interface. A @rhombus(class_clause) can have any number
 of @rhombus(implements, ~class_clause) clauses (with or without
 @rhombus(private, ~class_clause)). Any rule that applies to the
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
 the main class @rhombus(id_path): as a constructor, annotation
 form, binding pattern form, and namespace. A use of the internal
 @rhombus(id) as a constructor creates an instance of the same
 class, but the constructor expects arguments for all fields declared
 with @rhombus(field_spec)s, including private fields. For more
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
 defined @rhombus(id_path) for an expression context, binding
 context, and annotation context, respectively. See
 @rhombus(constructor, ~class_clause),
 @rhombus(expression, ~class_clause),
 @rhombus(binding, ~class_clause), and
 @rhombus(annotation, ~class_clause) for more information on those forms.

 When a method procedure is accessed from a class (as a namespace) via
 @rhombus(.), the procedure expects an extra by-position argument that
 must be an instance of the class, and the extra argument is supplied before
 all other arguments. A field accessor from a class (as a
 namespace) via @rhombus(.) similarly takes an instance of the class.
 A property accessor from a class (as a
 namespace) via @rhombus(.) takes an instance of the class, and it accepts
 an additional value to assign to the property (if the property supports assignment).
 Even when a method is accessed via its class instead of an object, if
 the method and class are not @tech{final}, the called method is
 determined by the object and may be from a subclass that overrides the
 method; the same is true for properties.
 
 Each field, method, and property name must be distinct from all other field,
 method, and property names, whether from a parenthesized @rhombus(field_spec), from a
 @rhombus(field, ~class_clause) clause, or from a method or property clause. If an
 @rhombus(extends, ~class_clause) clause is present, then each field name
 must also be distinct from any field name in the superclass, except that
 a @rhombus(override, ~class_clause) clause must name a method or property that is
 already declared in the superclass. Private superclass fields,
 methods, and properties are not visible to the subclass, so their names are not required
 to be distinct from subclass field, method, and property names. When a method or property is
 overridden via @rhombus(override, ~class_clause), the original and
 overriding versions must be both methods or both properties.

 See @secref("static-info-rules") for information about static
 information associated with classes.

@examples(
  class Posn(x, y)
  Posn(1, 2)
  Posn.x
  Posn.x(Posn(1, 2))
  Posn(1, 2).x
  ~error: class Posn3(z):
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
  ~literal: :: extends binding field
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause
    method_decl: method ~class_clause
    property_decl: method ~class_clause

  defn.macro 'interface $id_path'
  defn.macro 'interface $id_path:
                $interface_clause_or_body_or_export
                ...'

  grammar interface_clause_or_body_or_export:
    $interface_clause
    $body
    $export

  grammar interface_clause:
    #,(@rhombus(method, ~interface_clause)) $method_impl
    #,(@rhombus(override, ~interface_clause)) $method_impl
    #,(@rhombus(final, ~interface_clause)) $method_impl
    #,(@rhombus(private, ~interface_clause)) $method_impl
    #,(@rhombus(abstract, ~interface_clause)) $method_decl
    #,(@rhombus(property, ~interface_clause)) $property_impl
    #,(@rhombus(extends, ~interface_clause)) $extends_decl
    #,(@rhombus(internal, ~interface_clause)) $internal_decl
    #,(@rhombus(expression, ~interface_clause)) $expression_decl
    #,(@rhombus(annotation, ~interface_clause)) $annotation_decl
    $other_interface_clause

){

 Similar to @rhombus(class) for defining classes, but defines an
 interface, which has no fields but can have multiple superinterfaces. A
 @rhombus(method, ~interface_clause),
 @rhombus(property, ~interface_clause), or @rhombus(override, ~interface_clause)
 clause is allowed to have just a
 name or omit the body, in which case @rhombus(abstract, ~interface_clause)
 implicitly prefixes the declaration.

 The body of an @rhombus(interface) form has @deftech{interface clauses}
 that are similar to @tech{class clauses}, but declared separately and
 sometimes with different syntax. For example,
 @rhombus(extends, ~interface_clause) as an interface clause supports multiple
 superinterface names instead of just one, and
 @rhombus(extends, ~interface_clause) can appear multiple times in an
 @rhombus(interface) body.

 Interfaces cannot be instantiated. They are implemented by classes via
 the @rhombus(implements, ~class_clause) form. When a class implements an
 interface (not privately), it has all methods and properties of the interface, and its
 instances satisfy the interface as an annotation.

 Typically, an interface declares methods and properties with
 @rhombus(abstract, ~interface_clause) to be implemented by classes that
 implement the interface. However, an interface can define method and property
 implementations; those implementations are inherited by classes that implement the
 interface or any subinterface that extends the interface. An interface can
 also have private helper methods and properties, but they are useful only when an
 interface also has implemented public methods or properties that refer to them.

 When a class implements an interface privately using
 @rhombus(#,(@rhombus(private, ~class_clause)) #,(@rhombus(implements, ~class_clause))),
 its instances do not satisfy the interface as an annotation. If the
 privately implemented interface has an internal name declared with
 @rhombus(internal, ~interface_clause), however, instances satisfy the
 internal name as an annotation. Methods and properties of a privately implemented
 instance can be accessed only with static @rhombus(.) via the
 internal-name annotation. As long as a method or property belongs to only privately
 implemented interfaces, it can be overridden with
 @rhombus(#,(@rhombus(private, ~class_clause)) #,(@rhombus(override, ~class_clause))),
 otherwise it is overidden normally. If a class declares the
 implementation of a interface both normally and privately, then the
 interface is implemented normally. Abstract private methods and properties must be
 implemented immediately in the class that privately implements the
 associated interface.

 When a class or interface extends or implements multiple interfaces
 that provide a method or property with the same name, the method or property implementation must
 be the same for all interfaces. That is, the method or property must be
 abstract, the implementation must reside in a shared superinterface
 of the interfaces, or the method or property must be overridden in the implementing
 class. Overriding applies to same-named methods or properties of all interfaces.

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
 reorders the expansion so that the defined class and interface names can
 be used as field, method, and property-result annotations and in all of the other
 class and interface declarations.

 Definitions and expressions within each @rhombus(class) and
 @rhombus(interface) form (i.e., forms that are not @tech{class clauses}
 or @tech{interface clauses}) are ordered before all of the class and
 interface definitions, so using @rhombus(class.together) does not make
 the class and interface names more available in those terms.

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
  class_clause.macro 'extends $id_path'
  class_clause.macro 'extends: $id_path'
  interface_clause.macro 'extends $id_path'
  interface_clause.macro 'extends: $id_path ...; ...'
){

 A @tech{class clause} recognized by @rhombus(class) to define a class
 that is a subclass of the one named by @rhombus(id_path), and an
 @tech{interface clause} recognized by @rhombus(interface) to define an
 interface that is a subinterface of the ones named by the
 @rhombus(id_path)s.

}

@doc(
  class_clause.macro 'implements $id_path ...'
  class_clause.macro 'implements: $id_path ...; ...'
){

 A @tech{class clause} recognized by @rhombus(class) to define a class
 that implements subclasses named by @rhombus(id_path)s. See
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
  interface_clause.macro 'final $method_impl'
  interface_clause.macro 'final #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'final #,(@rhombus(override, ~interface_clause)) $method_impl'
  interface_clause.macro 'final #,(@rhombus(override, ~interface_clause)) #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'final #,(@rhombus(override, ~interface_clause)) #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'final #,(@rhombus(property, ~interface_clause)) $property_impl'
  interface_clause.macro 'final #,(@rhombus(override, ~interface_clause)) #,(@rhombus(property, ~interface_clause)) $property_impl'
){

 The @rhombus(final, ~class_clause) form as a @tech{class clause} or
 @tech{interface clause} is followed by a method or property declaration.
 In that case, the method or property is final, even
 if the enclosing class is not (and an interface is never final). A final
 method or property cannot be overridden in subclaseses or subinterfaces. Using
 @rhombus(final, ~class_clause) with an immediate declaration is the same
 as @rhombus(final, ~class_clause) followed by
 @rhombus(method, ~class_clause). Including
 @rhombus(override, ~class_clause) means that the method or proerty must be defined
 in the superclass or a superinterface, while it must not be defined in
 the superclass or a superinterface if @rhombus(override, ~class_clause)
 is not used.
}

@doc(
  ~nonterminal:
    maybe_annot: class
  class_clause.macro 'field $id $maybe_annot = $expr'
  class_clause.macro 'field $id $maybe_annot: $body; ...'
){

 A @tech{class clause} recognized by @rhombus(class) to add fields to
 the class. The @rhombus(expr) or @rhombus(body) block is evaluated each time the class is instantiated,
 but it cannot refer to @rhombus(this) or fields, methods, or properties of an object.
 See @rhombus(class) for more information.

}

@doc(
  ~nonterminal:
    maybe_res_annot: fun

  class_clause.macro 'method $method_impl'
  class_clause.macro 'property $property_impl'
  class_clause.macro 'override $method_impl'
  class_clause.macro 'override #,(@rhombus(method, ~class_clause)) $method_impl'
  class_clause.macro 'override #,(@rhombus(property, ~class_clause)) $property_impl'
  interface_clause.macro 'method $method_decl'
  interface_clause.macro 'method $method_impl'
  interface_clause.macro 'property $property_decl'
  interface_clause.macro 'property $property_impl'
  interface_clause.macro 'override $method_impl'
  interface_clause.macro 'override $method_decl'
  interface_clause.macro 'override #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'override #,(@rhombus(method, ~interface_clause)) $method_decl'
  interface_clause.macro 'override #,(@rhombus(property, ~interface_clause)) $property_impl'
  interface_clause.macro 'override #,(@rhombus(property, ~interface_clause)) $property_decl'

  grammar method_impl:
    $id $maybe_res_annot: $entry_point
    $id ($kwopt_bind, ..., $rest, ...) $maybe_res_annot: $body; ...
    Z| $id($bind, ..., $rest, ...) $maybe_res_annot:
         $body
         ...
     | ...

  grammar method_decl:
    $id $maybe_res_annot
    $id ($kwopt_binding, ..., $rest, ...) $maybe_res_annot
    
  grammar property_impl:
    $id $maybe_res_annot: $body
    Z| $id $maybe_res_annot: $body
    Z| $id $maybe_res_annot: $body
     | $id := $binding: $body

  grammar property_decl:
    $id $maybe_res_annot
    Z| $id $maybe_res_annot
){

 These @tech{class clauses} and @tech{interface clauses} are recognized
 by @rhombus(class) and @rhombus(interface) to declare methods and properties, along
 with the method and property forms of @rhombus(final, ~class_clause) and
 @rhombus(private, ~class_clause). The combination
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
 apply to subclasses.

 A @rhombus(property, ~class_clause) clause declares or overrides a
 @tech{property}, which is like a method in that using the property evaluates a
 block. However, the property is used either as an expression, which is
 analogous to calling a method with no arguments, or as the left-hand
 side of a @rhombus(:=) form, which is analogous to calling a method with
 the right-hand side of @rhombus(:=) as the method argument. A property
 always supports a reference form, but it supports assignment only when
 the @rhombus(property, ~class_clause) form includes a @rhombus(:=) case.
 In a @rhombus(property, ~class_clause)'s @rhombus(:=) case, the part
 after @rhombus(:=) is a binding analogous to a function-argument binding,
 and the subsequent @rhombus(body) will normally refer to that binding.
 Using @rhombus(:=) with a property always produces @rhombus(#void);
 any value returned by the @rhombus(body) of a property definition's
 @rhombus(:=) case is ignored. A @rhombus(maybe_res_annot) in a
 @rhombus(property, ~class_clause) clause applies to overriding implementations
 in subclasses, but it imposes no constraints on the right-hand part of
 @rhombus(:=) when assigning to a property.

 In the body of a method or property, the special expression form @rhombus(this)
 refers to the object whose method was called. Fields (in the case of a
 class) and methods can be accessed using @rhombus(this) and @rhombus(.),
 but they can also be used directly. 
 Using a field, method, or property name
 directly is the same as using @rhombus(this) and @rhombus(.) in static
 mode (which implies that a direct reference to a method name must be a
 call of the method). An argument that has the same name as a field,
 method, or property shadows the field, method, or property.

 In an interface, a @rhombus(method, ~interface_clause), @rhombus(override, ~interface_clause), or
 @rhombus(property, ~interface_clause) declation can be just an identifier, or
 it can omit a body block. In that case, @rhombus(method, ~interface_clause), @rhombus(override, ~interface_clause),
 or @rhombus(property, ~interface_clause) is treated as if
 @rhombus(abstract, ~interface_clause) is added before. If arguments are declared for
 an abstract method, they determine the method's expectations
 for static argument-count checking (see @rhombus(use_static)), but they
 do not impose constraints on overriding implementations.
 When a @rhombus(property_decl) uses the single-case @litchar{|} form,
 it declares the property as not supporting assignment; that declaration
 is not enforced on implementations of the property, but it affects
 static resolution of a property assignment.

}

@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause

  class_clause.macro 'private #,(@rhombus(implements, ~class_clause)) $id_path ...'
  class_clause.macro 'private #,(@rhombus(implements, ~class_clause)): $id_path ...; ...'
  class_clause.macro 'private #,(@rhombus(field, ~class_clause)) $field_decl'
  class_clause.macro 'private $method_impl'
  class_clause.macro 'private #,(@rhombus(method, ~class_clause)) $method_impl'
  class_clause.macro 'private #,(@rhombus(property, ~class_clause)) $property_impl'
  class_clause.macro 'private #,(@rhombus(override, ~class_clause)) $method_impl'
  class_clause.macro 'private #,(@rhombus(override, ~class_clause)) #,(@rhombus(method, ~class_clause)) $method_impl'
  class_clause.macro 'private #,(@rhombus(override, ~class_clause)) #,(@rhombus(property, ~class_clause)) $property_impl'
  interface_clause.macro 'private $method_impl'
  interface_clause.macro 'private #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'private #,(@rhombus(property, ~interface_clause)) $property_impl'
  interface_clause.macro 'private #,(@rhombus(override, ~interface_clause)) $method_impl'
  interface_clause.macro 'private #,(@rhombus(override, ~interface_clause)) #,(@rhombus(method, ~interface_clause))  $method_impl'
  interface_clause.macro 'private #,(@rhombus(override, ~interface_clause)) #,(@rhombus(property, ~interface_clause))  $property_impl'
){

 A @tech{class clause} that declares interfaces that are privately
 implemented (see @rhombus(interface)), a @tech{class clause} that
 declares a private field, or a @tech{class clause} or @tech{interface
  clause} that declares a private method or property. See @rhombus(class),
 @rhombus(interface), and @rhombus(method, ~class_clause) for more
 information on field, method, and property declarations. A
 @rhombus(private, ~class_clause) without
 @rhombus(implements, ~class_clause), @rhombus(field, ~class_clause),
 @rhombus(method, ~class_clause), @rhombus(override, ~class_clause),
 or @rhombus(property, ~class_clause) is
 equivalent to @rhombus(private, ~class_clause) followed by
 @rhombus(method, ~class_clause).

 Private fields, methods, and properties can be accessed only within the body of the enclosing
 @rhombus(class) or through an identifier declared with
 @rhombus(internal, ~class_clause). When referenced via the
 @rhombus(.) operator, only static references are allowed
 through the enclosing class's annotation (not a subclass annotation) or
 through an @rhombus(internal, ~class_clause) identfier's annotation.

}

@doc(
  ~nonterminal:
    method_decl: method ~class_clause
    property_decl: method ~class_clause

  class_clause.macro 'abstract $method_decl'
  class_clause.macro 'abstract #,(@rhombus(method, ~class_clause)) $method_decl'
  class_clause.macro 'abstract #,(@rhombus(override, ~class_clause)) $method_decl'
  class_clause.macro 'abstract #,(@rhombus(property, ~class_clause)) $property_decl'
  class_clause.macro 'abstract #,(@rhombus(override, ~class_clause)) #,(@rhombus(property, ~class_clause)) $property_decl'
  interface_clause.macro 'abstract $method_decl'
  interface_clause.macro 'abstract #,(@rhombus(method, ~interface_clause)) $method_decl'
  interface_clause.macro 'abstract #,(@rhombus(override, ~interface_clause)) $method_decl'
  interface_clause.macro 'abstract #,(@rhombus(property, ~interface_clause)) $property_decl'
  interface_clause.macro 'abstract #,(@rhombus(override, ~interface_clause)) #,(@rhombus(property, ~interface_clause))  $property_decl'
){

 A @tech{class clause} or @tech{interface clause} that declares a method
 or property without an implementation. See @rhombus(method, ~interface_clause) for the
 shape of @rhombus(method_decl), and see @rhombus(property, ~interface_clause) for the
 shape of @rhombus(property_decl).

 When a class has an abstract method or property,
 either declared directly or inherited, the underlying constructor for the class
 raises an exception. The method or property must be overridden with a
 @rhombus(override, ~class_clause) class in a subclass, and then the
 subclass can be instantiated (as long as it has no other abstract
 methods). A @tech{final} class cannot have an abstract method or property.

 A method or property can be both @rhombus(abstract, ~class_clause) and
 @rhombus(override, ~class_clause). In that case, if the overriden method or property
 is not abstract, then the method or property becomes abstract and most be overridden
 in a subclass before instantiation. Even if the overidden method or property is
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
  interface_clause.macro 'internal $id'
  interface_clause.macro 'internal: $id'
){

 A @tech{class clause} or @tech{interface clause} recognized by
 @rhombus(class) and @rhombus(interface) to bind @rhombus(id) to
 the class or interface's representation. See @rhombus(class),
 @rhombus(interface), and @rhombus(constructor, ~class_clause) for more
 information.

 When used as a @tech{namespace}, @rhombus(id) can access the
 immediate private fields, methods, and properties of the class or interface
 containing the @rhombus(internal, ~class_clause) declaration. Along
 similar lines, @rhombus(id) as an annotation associates static
 information with an expression or binding so that @rhombus(.) can be
 used to access private fields, methods and properties, but only with @rhombus(.) as
 statically resolved.

}

@doc(
  ~nonterminal:
    maybe_res_annot: fun
  class_clause.macro 'constructor $maybe_name: $entry_point'
  class_clause.macro 'constructor $maybe_name($kwopt_binding, ...,
                                              $rest, ...) $maybe_res_annot:
                        $body; ...'
  class_clause.macro 'constructor
                      | $maybe_name($binding, ..., $rest, ...) $maybe_res_annot:
                          $body; ...
                      | ...'
  class_clause.macro 'expression: $entry_point'
  class_clause.macro '«expression '$id $pattern ...': '$template'»'
  class_clause.macro '«expression | '$id $pattern ...': '$template'
                                  | ...»'
  class_clause.macro 'binding: $entry_point'
  class_clause.macro '«binding '$id $pattern ...': '$template'»'
  class_clause.macro '«binding | '$id $pattern ...': '$template'
                               | ...»'
  class_clause.macro 'annotation: $entry_point'
  class_clause.macro '«annotation '$id $pattern ...': '$template'»',
  class_clause.macro '«annotation | '$id $pattern ...': '$template'
                                  | ...»'

  grammar maybe_name:
    $id
    #,("ϵ")
){

 These @tech{class clauses} are recognized by @rhombus(class) to replace
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
 
 When a @rhombus(class) has a @rhombus(constructor, ~class_clause)
 form with an empty @rhombus(maybe_name), then a use of new class's
 @rhombus(id_path) as a
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
  @rhombus(private, ~class_clause) clause fields; the result of that function is
  an instance of the new class. Again, the result instance might be an
  instance of a subclass if the new class is not @tech{final}.}

)

 If a @rhombus(constructor, ~class_clause) form has an
 @rhombus(id) for @rhombus(maybe_name) that is not the same as
 the enclosing class's @rhombus(id_path), then the constructor is bound to
 @rhombus(id) instead of @rhombus(id_path).
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
 then a use of the new class's @rhombus(id_path) as an
 expression invokes the @tech{entry point} (typically a
 @rhombus(macro, ~entry_point) form) in the block after
 @rhombus(expression, ~class_clause). The @rhombus(entry_point) is a
 meta-time expression. This macro replaces the default meaning of the
 @rhombus(id_path) as a reference to the constructor.
 
 When a @rhombus(class) has a @rhombus(binding, ~class_clause) form,
 then a use of the new class's @rhombus(id_path) as a
 binding-pattern constructor invokes the @tech{entry point} (typically a
 @rhombus(macro, ~entry_point) form) in the block after
 @rhombus(binding, ~class_clause). The @rhombus(entry_point) is a
 meta-time expression. There is no @rhombus(super) for custom binding
 patterns; instead, use @rhombus(internal, ~class_clause) to bind an
 internal name that acts similar to the class's default binding form, but
 with two differences: it does not expect bindings for superclass fields,
 but it does expect bindings for private fields declared with a
 @rhombus(field_spec, ~var). When a class has a superclass, then a custom
 binding form is typically implemented using an internal binding form,
 the superclass's binding form, and the @rhombus(&&, ~bind) binding
 operator. When a superclass has a custom binding form, then a class must
 have a custom binding form, too (unlike the case with constructors,
 where a default constructor still can be generated to call a custom
 superclass constructor).

 When a @rhombus(class) has an @rhombus(annotation, ~class_clause) form,
 then a use of new class's @rhombus(id_path) in a
 annotation invokes the @tech{entry point} (typically a
 @rhombus(macro, ~entry_point) form) in the block after
 @rhombus(annotation, ~class_clause). The @rhombus(entry_point) is a
 meta-time expression. Similar to custom binding forms, a custom
 annotation form normally needs an internal annotation name bound with
 @rhombus(internal, ~class_clause); the @rhombus(of) form of that
 annotation expects annotations for only immediate fields of the class,
 but including private ones declared with @rhombus(field_spec, ~var)s.
 Use the @rhombus(&&, ~annot) annotation operator to combine the internal
 annotation with a superclass annotation. When a superclass has a custom
 annotation form, then a class must have a custom annotation form, too.
 Typically, an @rhombus(of, ~datum) annotation is also defined and
 exported from the class's namespace to go along with the customization
 of the class name as an annotation.

}


@doc(  
  interface_clause.macro 'expression: $expresssion_decl'
  interface_clause.macro 'annotation: $annotation_point'
){

 These @tech{interface clause} forms have the same syntax and analogous
 meaning as the @rhombus(expression, ~class_clause) and
 @rhombus(annotation, ~class_clause) @tech{class clauses}.

 There is no @rhombus(constructor, ~class_clause) for interfaces, since
 interfaces cannot be instantiated directly, but an
 @rhombus(expression, ~interface_clause) clause can make an interface
 identifier behave like a constructor, perhaps instantiating some default
 class. There is no @rhombus(binding, ~class_clause) for interfaces,
 because @rhombus(interface) does not otherwise define an interfeace name
 for binding, and so @rhombus(bind.macro) can be
 used alongside @rhombus(interface) with the same interface name.

}


@doc(
  class_clause.macro 'opaque'
){

 When a @tech{class clause} is @rhombus(opaque, ~class_clause), then the
 default printed form does not show fields, and instead prints
 @litchar{...} in parentheses after the class name.

}


@doc(
  class_clause.macro 'authentic'
){

 When a @tech{class clause} is @rhombus(authentic, ~class_clause), then
 the new class cannot be chaperoned or impersonated. At most one class
 clause in a @rhombus(class) form can be
 @rhombus(authentic, ~class_clause).

}
