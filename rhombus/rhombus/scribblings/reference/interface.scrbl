#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title(~tag: "interface"){Interfaces}

@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause
    method_decl: abstract ~class_clause
    property_decl: abstract ~class_clause

  defn.macro 'interface $id_name'
  defn.macro 'interface $id_name:
                $interface_clause_or_body_or_export
                ...'

  grammar interface_clause_or_body_or_export
  | $interface_clause
  | $body
  | $export

  grammar interface_clause
  | #,(@rhombus(method, ~interface_clause)) $method_impl
  | #,(@rhombus(override, ~interface_clause)) $method_impl
  | #,(@rhombus(final, ~interface_clause)) $method_impl
  | #,(@rhombus(private, ~interface_clause)) $method_impl
  | #,(@rhombus(protected, ~interface_clause)) $method_impl
  | #,(@rhombus(abstract, ~interface_clause)) $method_decl
  | #,(@rhombus(property, ~interface_clause)) $property_impl
  | #,(@rhombus(extends, ~interface_clause)) $extends_decl
  | #,(@rhombus(internal, ~interface_clause)) $internal_decl
  | #,(@rhombus(expression, ~interface_clause)) $expression_decl
  | #,(@rhombus(annotation, ~interface_clause)) $annotation_decl
  | #,(@rhombus(dot, ~interface_clause)) $dot_decl
  | #,(@rhombus(static_info, ~interface_clause)) $static_info_decl
  | #,(@rhombus(primitive_property, ~interface_clause)) $primitive_property_decl
  | $other_interface_clause

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
 interface (not privately or protectedly), it has all methods and properties of the interface, and its
 instances satisfy the interface as an annotation.

 Typically, an interface declares methods and properties with
 @rhombus(abstract, ~interface_clause) to be implemented by classes that
 implement the interface. However, an interface can define method and property
 implementations; those implementations are inherited by classes that implement the
 interface or any subinterface that extends the interface. An interface can
 also have private helper methods and properties, but they are useful only when an
 interface also has implemented public or protected methods or properties that refer to them.

 When a class implements an interface privately using
 @rhombus(#,(@rhombus(private, ~class_clause)) #,(@rhombus(implements, ~class_clause)))
or protectedly using
 or @rhombus(#,(@rhombus(protected, ~class_clause)) #,(@rhombus(implements, ~class_clause))),
 its instances do not satisfy the interface as an annotation. If the
 privately or protectedly implemented interface has an internal name declared with
 @rhombus(internal, ~interface_clause), however, instances satisfy the
 internal name as an annotation. Methods and properties of a privately or protectedly implemented
 instance can be accessed only with static @rhombus(.) via the
 internal-name annotation. As long as a method or property belongs to only
 to privately implemented interfaces, it can be overridden with
 @rhombus(#,(@rhombus(private, ~class_clause)) #,(@rhombus(override, ~class_clause))),
 otherwise it is overridden normally. Methods of a protectedly implemented interface
 are treated as protected in the implementing class, even when the
 methods are declared as non-protected in the interfaces. If a class declares
 the implementation of a interface both normally and privately or protectedly, then the
 interface is implemented normally; if an interface is declared as implemented both privately
 and protectedly, then it is protectedly implemented.
 Abstract private methods and properties must be
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
  interface_clause.macro 'extends $id_name'
  interface_clause.macro 'extends: $id_name ...; ...'
){

 An @tech{interface clause} recognized by @rhombus(interface) to define
 an interface that is a subinterface of the ones named by the
 @rhombus(id_name)s.

}


@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause

  interface_clause.macro 'final $method_impl'
  interface_clause.macro 'final #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'final #,(@rhombus(override, ~interface_clause)) $method_impl'
  interface_clause.macro 'final #,(@rhombus(override, ~interface_clause)) #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'final #,(@rhombus(override, ~interface_clause)) #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'final #,(@rhombus(property, ~interface_clause)) $property_impl'
  interface_clause.macro 'final #,(@rhombus(override, ~interface_clause)) #,(@rhombus(property, ~interface_clause)) $property_impl'
  interface_clause.macro 'final #,(@rhombus(protected, ~interface_clause)) $method_impl'
  interface_clause.macro 'final #,(@rhombus(protected, ~interface_clause)) #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'final #,(@rhombus(protected, ~interface_clause)) #,(@rhombus(property, ~interface_clause)) $method_impl'
){

 Like the @rhombus(final, ~class_clause) @tech{class clause} form, but
 as an @tech{interface clause}.

}

@doc(
  ~nonterminal:
    maybe_res_annot: fun ~defn
    case_maybe_kw_opt: fun ~defn
    case_maybe_kw: fun ~defn
    bind_maybe_kw_opt: fun ~defn
    rest: fun ~defn
    method_impl: method ~class_clause
    method_decl: abstract ~class_clause
    property_impl: method ~class_clause
    property_decl: abstract ~class_clause

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

){

 These @tech{interface clauses} are recognized by @rhombus(interface) to
 declare methods and properties, and they are analogous to the
 @rhombus(method, ~class_clause), @rhombus(property, ~class_clause), and @rhombus(override, ~class_clause)

 A @rhombus(method, ~interface_clause), @rhombus(override, ~interface_clause), or
 @rhombus(property, ~interface_clause) declaration can be just an identifier, or
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
    field_impl: field ~class_clause

  interface_clause.macro 'private $method_impl'
  interface_clause.macro 'private #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'private #,(@rhombus(property, ~interface_clause)) $property_impl'
  interface_clause.macro 'private #,(@rhombus(override, ~interface_clause)) $method_impl'
  interface_clause.macro 'private #,(@rhombus(override, ~interface_clause)) #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'private #,(@rhombus(override, ~interface_clause)) #,(@rhombus(property, ~interface_clause)) $property_impl'
){

 An @tech{interface clause} that declares a private method or property.
 See @rhombus(interface) and
 @rhombus(method, ~class_clause) for more information on method
 and property declarations. A @rhombus(private, ~interface_clause) without
 @rhombus(method, ~interface_clause),
 @rhombus(override, ~interface_clause), or @rhombus(property, ~interface_clause)
 is equivalent to @rhombus(private, ~interface_clause) followed by
 @rhombus(method, ~interface_clause).

}

@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause
    field_impl: field ~class_clause

  interface_clause.macro 'protected $method_impl'
  interface_clause.macro 'protected #,(@rhombus(method, ~interface_clause)) $method_impl'
  interface_clause.macro 'protected #,(@rhombus(property, ~interface_clause)) $property_impl'
){

 Like @rhombus(private, ~interface_clause), but protected methods and
 and properties, can be referenced and overridden within subclasses, subinterfaces, and
 subveneers. An overriding declaration does not use
 @rhombus(protected, ~interface_clause) again.

}

@doc(
  ~nonterminal:
    method_decl: abstract ~class_clause
    property_decl: abstract ~class_clause

  interface_clause.macro 'abstract $method_decl'
  interface_clause.macro 'abstract #,(@rhombus(method, ~interface_clause)) $method_decl'
  interface_clause.macro 'abstract #,(@rhombus(override, ~interface_clause)) $method_decl'
  interface_clause.macro 'abstract #,(@rhombus(property, ~interface_clause)) $property_decl'
  interface_clause.macro 'abstract #,(@rhombus(override, ~interface_clause)) #,(@rhombus(property, ~interface_clause)) $property_decl'
  interface_clause.macro 'abstract #,(@rhombus(protected, ~interface_clause)) $method_decl'
  interface_clause.macro 'abstract #,(@rhombus(protected, ~interface_clause)) #,(@rhombus(method, ~interface_clause)) $method_decl'
  interface_clause.macro 'abstract #,(@rhombus(protected, ~interface_clause)) #,(@rhombus(property, ~interface_clause)) $property_decl'
){

 An @tech{interface clause} that declares a method or property without
 an implementation, analogous to @rhombus(abstract, ~class_clause) for
 classes. Methods and properties of an interface can be defined as
 abstract without the @rhombus(abstract, ~interface_clause) form by
 declaring the method or property without an implementation.

}

@doc(
  interface_clause.macro 'internal $id'
  interface_clause.macro 'internal: $id'
){

 An @tech{interface clause} recognized by @rhombus(interface) to bind
 @rhombus(id) to the interface's representation. See @rhombus(interface)
 and the @rhombus(internal, ~class_clause) @tech{class clause} for more
 information.

}

@doc(
  interface_clause.macro 'constructor $constructor_decl'
  interface_clause.macro 'expression: $expression_decl'
  interface_clause.macro 'annotation: $annotation_point'
){

 These @tech{interface clause} forms have the same syntax and analogous
 meaning as the @rhombus(constructor, ~class_clause),
 @rhombus(expression, ~class_clause), and
 @rhombus(annotation, ~class_clause) @tech{class clauses}.

 There is no default constructor for interfaces, since
 interfaces cannot be instantiated directly, but a
 @rhombus(constructor, ~class_clause) or
 @rhombus(expression, ~interface_clause) clause can make an interface
 identifier behave like a constructor, perhaps instantiating some default
 class. When @rhombus(constructor, ~class_clause) is declared, its
 result is checked to ensure that it is an instance of the interface.

 There is no @rhombus(binding, ~class_clause) for interfaces,
 because @rhombus(interface) does not otherwise define an interface name
 for binding, and so @rhombus(bind.macro) can be
 used alongside @rhombus(interface) with the same interface name.

}


@doc(
  interface_clause.macro 'primitive_property $expr: $body; ...'
){

 An @tech{interface clause} that bridges Rhombus
 and Racket protocols. Any class that implements the interface will
 implement the primitive property with the @rhombus(body) value.

 The @rhombus(expr) and @rhombus(body) are evaluated in order relative
 to surrounding @rhombus(expr) and @rhombus(defn) forms.

 See also the @rhombus(primitive_property, ~class_clause) @tech{class
  clause} form.

}
