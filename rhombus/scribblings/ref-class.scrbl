#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Classes}

@doc(
  ~literal: :: extends binding field,
  defn.macro 'class $identifier_path($field_spec, ...)',
  defn.macro 'class $identifier_path($field_spec, ...):
                $class_clause_or_body
                ...',

  grammar identifier_path:
    $identifier
    $identifier_path . $identifier,

  grammar field_spec:
    $maybe_mutable $identifier $maybe_annot $maybe_default
    $keyword: $maybe_mutable $identifier $maybe_annot $maybe_default
    $keyword $maybe_default,

  grammar maybe_mutable:
    $$(@rhombus(mutable, ~bind))
    ε,

  grammar maybe_annot:
    :: $$(@rhombus(annotation, ~var))
    ε,

  grammar maybe_default:
    = $default_expr
    ε,

  grammar class_clause_or_body:
    $class_clause
    $body,

  grammar class_clause:
    $$(@rhombus(field, ~class_clause)) $identifier $maybe_annotation: $body; ...
    $$(@rhombus(private, ~class_clause)) $$(@rhombus(field, ~class_clause)) $identifier $maybe_annotation: $body; ...
    $$(@rhombus(method, ~class_clause)) $method_decl
    $$(@rhombus(override, ~class_clause)) $method_decl
    $$(@rhombus(final, ~class_clause)) $method_decl
    $$(@rhombus(private, ~class_clause)) $method_decl
    $$(@rhombus(unimplemented, ~class_clause)) $method_decl
    $$(@rhombus(extends, ~class_clause)) $identifier_path
    $$(@rhombus(final, ~class_clause))
    $$(@rhombus(nonfinal, ~class_clause))
    $$(@rhombus(internal, ~class_clause)) $identifier
    $$(@rhombus(constructor, ~class_clause)) $constructor_decl
    $$(@rhombus(binding, ~class_clause)) $binding_decl
    $$(@rhombus(annotation, ~class_clause)) $annotation_decl
    $other_class_clause

){

 Binds @rhombus(identifier_path) as a class name, which serves several roles:

@itemlist(

 @item{a constructor function, which by default takes as many arguments
  as the supplied @rhombus(field_spec)s in parentheses, and it returns an
  instance of the class;},

 @item{an annotation, which is satisfied by any instance of the class,
  and an annotation constructor @rhombus(identifier_path.of), which by
  default takes as many annotation arguments as supplied
  @rhombus(field_spec)s in parentheses;},

 @item{a binding-pattern constructor, which by default takes as many
  patterns as the supplied @rhombus(field_spec)s in parentheses and
  matches an instance of the class where the fields match the
  corresponding patterns;},

 @item{a dot povider to access accessor functions
  @rhombus(identifier_path$$(rhombus(.))$$(@rhombus(field,~var))), which
  by default includes a @rhombus(field,~var) for every field in the class,
  whether listed as a @rhombus(field_spec) in parentheses or added by a
  @rhombus(field, ~class_clause) clause.}

)

 Fields and methods of a class can be accessed from an object using
 @rhombus(.), but fields and methods declared as
 @rhombus(private, ~class_clause) can only be accessed by @rhombus(.)
 within methods of the class. In static mode (see @rhombus(use_static)),
 a method must be called like a function; in dynamic mode, a method
 accessed from an object closes over the object. Private fields and
 methods can be accessed with @rhombus(.) only statically.

 A @rhombus(field_spec) has an identifier, keyword, or both. A keyword
 implies that the default constructor expects the corresponding argument
 as a keyword argument instwad of a by-position argument. The default
 annotation and binding pattern similarly expect a keyword-tagged subform
 instead of a by-position form for the corresponding fields. The name of
 the field for access with @rhombus(.) is the identifier, if present,
 otherwise the name is the symbolic form of the keyword.

 When a default-value expression is provided for a field after
 @rhombus(=), then the default constructor evaluates the
 @rhombus(default_expr) to obstain a value for the argument when it is
 not supplied. If a by-position field has a default-value expression,
 then all later by-position fields must have a default. If the class
 extends a superclass that has a by-position argument with a default,
 then all by-position arguments of the subclass must have a default. A
 @rhombus(default_expr) can refer to earlier field names in the same
 @rhombus(class) to produce a default value.

 If a block follows a @rhombus(class) form's @rhombus(field_spec) sequence,
 it contains a mixture of definitions, expressions, and class clauses. A
 @deftech{class clause} adjusts the class and bindings created by the
 @rhombus(class) form; it be one of the predefined clause forms,
 or it can be a macro that ultimately expands to a predefined form.
 Definitions and expressions in a @rhombus(class) block are evaluated at
 the same time as the @rhombus(class) form is evaluated (not when an
 instance is created), and definitions are scoped to the block for
 potential use by class clauses.

 When a @rhombus(class_clause) is a @rhombus(field, ~class_clause) form,
 then an additional field is added to the class, but the additional field
 is not represented by an arguments to the constructor. Instead, the
 @rhombus(body) block in @rhombus(field, ~class_clause) gives the added
 field its initial value; that block is evaluated at the time the
 @rhombus(class) form is evaluated, not when an object is instantiated,
 and the same values are used for every instance of the class. All fields
 added through a @rhombus(field, ~class_clause) clause are mutable. The
 @rhombus(field, ~class_clause) can appear any number of times as a
 @rhombus(class_clause), with or without a
 @rhombus(private, ~class_clause) prefix.

 When a @rhombus(class_clause) is a @rhombus(method, ~class_clause)
 form, @rhombus(override, ~class_clause) form,
 @rhombus(unimplemented, ~class_clause) form, or method-shaped
 @rhombus(final, ~class_clause) or @rhombus(private, ~class_clause) form,
 then the clause declares a method for the class. These clauses can
 appear any number of times as a @rhombus(class_clause) to add or
 override any number of methods. See @rhombus(method, ~class_clause) for
 more information on methods.
 
 When a @rhombus(class_clause) is an @rhombus(extends, ~class_clause)
 form, the new class is created as a subclass of the extended class. The
 extended class must not be @tech{final}. At most one
 @rhombus(class_clause) can have @rhombus(extends, ~class_clause).

 When a @rhombus(class_clause) is @rhombus(final, ~class_clause), then
 the new class is @deftech{final}, which means that it cannot have
 subclasses. When a @rhombus(class_clause) is
 @rhombus(nonfinal, ~class_clause), then the new class is not final. A
 class is final by default, unless it has a superclass, in which case it
 is not final by default. At most one @rhombus(class_clause) can have
 @rhombus(final, ~class_clause) or @rhombus(nonfinal, ~class_clause).

 When a @rhombus(class_clause) is an @rhombus(internal, ~class_clause)
 form, then the clause's @rhombus(identifier) is bound to the default
 constructor, annotation, binding-pattern constructor, or dot provider as
 would be bound to the @rhombus(class) form's main
 @rhombus(identifier_path) if not replaced by clauses like
 @rhombus(constructor, ~class_clause) and
 @rhombus(binding, ~class_clause). In other words, @rhombus(identifier)
 accesses the primitive class representation, instead of the customized
 view. If the internal @rhombus(identifier) is not exported, then that
 primitive view stays private to the scope of its definition. At most one
 @rhombus(class_clause) can have @rhombus(internal, ~class_clause).

 The @rhombus(class_clause) forms @rhombus(constructor, ~class_clause),
 @rhombus(binding, ~class_clause), and
 @rhombus(annotation, ~class_clause) replace default meanings of the
 defined @rhombus(identifier_path) for an expression context, binding
 context, and annotation context, respectively. In each case, an
 identifier can be provided, such as @rhombus(make_identifier, ~var) or
 @rhombus(bind_identifier, ~var); within the clause, that identifier is bound
 to refer to the default implementation, roughly. When an identifier like
 @rhombus(make_identifier, ~var) is not provided in a clause, then the
 @rhombus(class) form must include a @rhombus(internal, ~class_clause)
 clause, and the @rhombus(identifier) from
 @rhombus(internal, ~class_clause) is used (shadowing the binding for
 @rhombus(identifier) outside of the specific clause). When a superclass
 specified by @rhombus(extends, ~class_clause) has a custom constructor,
 binding, or annotation form, then the new subclass must also specify a
 custom constructor, binding, or annotation form. See
 @rhombus(constructor, ~class_clause), @rhombus(binding, ~class_clause),
 and @rhombus(annotation, ~class_clause) for more information on those
 forms.
 
 Each field and method name must be distinct from all other field and
 method names, whether from a parenthesized @rhombus(field_spec), from a
 @rhombus(field, ~class_clause) clause, or from a method clause. If an
 @rhombus(extends) clause is present, then each field name must also be
 distinct from any field name in the superclass, except that a
 @rhombus(override) clause must name a method that is already declared in
 the superclass. Private superclass fields and methods are not visible to
 the subclass, so their names are not required to be distinct from
 subclass field and method names.

 See @secref("static-info-rules") for information about static
 information associated with classes.

 See @secref("namespaces") for information on @rhombus(identifier_path).

@examples(
  class Posn(x, y),
  Posn(1, 2),
  Posn.x,
  Posn.x(Posn(1, 2)),
  Posn(1, 2).x,
  ~error: class Posn3(z):
            extends Posn,
  class Posn2D(x, y):
    nonfinal,
  class Posn3D(z):
    extends Posn2D,
  Posn3D(1, 2, 3),
  class Rectangle(w, h):
    nonfinal
    constructor (make):
      fun (~width: w, ~height: h):
        make(w, h),
  class Square():
    extends Rectangle
    constructor (make):
      fun (~side: s):
        make(~width: s, ~height: s)(),
  Square(~side: 10)
)

}

@doc(
  class_clause.macro 'extends $identifier_path',
){

 A @tech{class clause} recognized by @rhombus(class) to define a class
 that is a subclass of the one named by @rhombus(identifier_path).

}

@doc(  
  class_clause.macro 'nonfinal',
  class_clause.macro 'final',
  class_clause.macro 'final $method_decl',
  class_clause.macro 'final $$(@rhombus(method, ~class_clause)) $method_decl',
  class_clause.macro 'final $$(@rhombus(override, ~class_clause)) $method_decl',
  class_clause.macro 'final $$(@rhombus(override, ~class_clause)) $$(@rhombus(method, ~class_clause)) $method_decl',
){

 As a @tech{class clause} by itself, @rhombus(nonfinal, ~class_clause)
 and @rhombus(final, ~class_clause) are recognized by @rhombus(class) to
 determine whether the defined class is @tech{final}.

 The @rhombus(final, ~class_clause) form can instead be followed by a
 method declaration, where @rhombus(method_decl) is the same as for
 @rhombus(method, ~class_clause). In that case the method is final, even
 if the class if not. A final method cannot be overridden in subclaseses.
 Using @rhombus(final, ~class_clause) with an immediate declaration is
 the same as @rhombus(final, ~class_clause) followed by ,
 @rhombus(method, ~class_clause). Including
 @rhombus(override, ~class_clause) means that the method must be defined
 in the superclass, while it must not be defined in the superclass if
 @rhombus(override, ~class_clause) is not used.

}

@doc(  
  class_clause.macro 'field $identifier $maybe_annotation: $body; ...',
){

 A @tech{class clause} recognized by @rhombus(class) to add fields to
 the class. See @rhombus(class) for more information.

}

@doc(
  class_clause.macro 'method $method_decl',
  class_clause.macro 'override $method_decl',
  class_clause.macro 'override $$(@rhombus(method, ~class_clause)) $method_decl',

  grammar method_decl:
    $identifier $fun_form
    $identifier: $entry_point,
  
  grammar fun_form:
    ($kwopt_binding, ..., $rest, ...) $maybe_res_ann: $body; ...
    ($binding, ..., $rest, ...) $maybe_res_ann: $body; ...
){

 These @tech{class clauses} are recognized by @rhombus(class) to declare
 methods, along with the method forms of @rhombus(final, ~class_clause)
 and @rhombus(private, ~class_clause). The combination
 @rhombus(override, ~class_clause) followed by
 @rhombus(method, ~class_clause) is the same as just
 @rhombus(override, ~class_clause).

 A @rhombus(method_decl) either has the same form as after a
 @rhombus(fun) definition, or it is an @rhombus(identifier) followed by a
 block containing an @tech{entry point}.

 In the body of a method, the special expression form @rhombus(this)
 refers to the object whose method was called. Fields and methods can be
 accessed using @rhombus(this) and @rhombus(.), but they can also be used
 directly. Using a field or method name directly is the same as using
 @rhombus(this) and @rhombus(.) in static mode (which implies that a
 direct reference to a method name must be a call of the method). An
 argument that has the same name as a field or method shadow the field or
 method.

}

@doc(
  class_clause.macro 'private $$(@rhombus(field, ~class_clause)) $field_decl',
  class_clause.macro 'private $method_decl',
  class_clause.macro 'private $$(@rhombus(method, ~class_clause)) $method_decl',
){

 A @tech{class clause} that declares a private field or method. See
 @rhombus(class) and @rhombus(method, ~class_clause) for more
 information on field and method declarations. A
 @rhombus(private, ~class_clause) without @rhombus(field, ~class_clause)
 or @rhombus(method, ~class_clause) is equivalent to
 @rhombus(private, ~class_clause) followed by @rhombus(method, ~class_clause).

 Private fields can be accessed only statically, and only through the
 enclosing class's annotation (not a subclass annotation).

}

@doc(
  class_clause.macro 'unimplemented $identifier',
  class_clause.macro 'unimplemented $$(@rhombus(method, ~class_clause)) $identifier',
){

 A @tech{class clause} that declares a method without an implementation.
 When a class has an unimplemented method, the constructor for the class
 raises an exception. The method must be overridden with a
 @rhombus(override, ~class_clause) class in a subclass, and then the
 subclass can be instantiated (as long as it has no other unimplemented
 methods). A @tech{final} class cannot have an unimplemented method.

}

@doc(  
  expr.macro 'this'
){

 The @rhombus(this) form can only be used with a method. See
 @rhombus(method, ~class_clause) for more information.

}

@doc(  
  class_clause.macro 'internal $identifier'
){

 A @tech{class clause} recognized by @rhombus(class) to bind
 @rhombus(identifier) to the class's representation. See @rhombus(class)
 for more information.

}

@doc(  
  class_clause.macro 'constructor ($make_identifier): $entry_point',
  class_clause.macro 'constructor: $entry_point',
  class_clause.macro 'binding ($bind_identifier): $entry_point',
  class_clause.macro 'binding: $entry_point',
  class_clause.macro 'annotation ($annot_identifier): $entry_point',
  class_clause.macro 'annotation: $entry_point',
){

 These @tech{class clauses} are recognized by @rhombus(class) to
 replace the default constructor, binding form, or annotation form. See
 @rhombus(class) for in formation about a default
 @rhombus(make_identifier), @rhombus(bind_identifier), or
 @rhombus(annot_identifier) based on an @rhombus(internal, ~class_clause)
 form.
 
 When a @rhombus(class) has a @rhombus(constructor, ~class_clause)
 form, then a use of new class's @rhombus(identifier_path, ~var) as a
 constructor function invokes the @tech{entry point} (typically a
 @rhombus(fun, ~entry_point) form) in the block after
 @rhombus(constructor, ~class_clause). That function must return an
 instance of the new class, typically by calling
 @rhombus(make_identifier):

@itemlist(

 @item{If the new class does not have a superclass, then
  @rhombus(make_identifier) is bound to the default constructor, which
  returns an instance of the class. Note that this instance might be an
  instance of a subclass if the new class is not @tech{final}.},

 @item{If the new class has a superclass, then @rhombus(make_identifier)
  is bound to a curried function. The function accepts the same arguments
  as the superclass constructor. Instead of returning an instance of the
  class, it returns a function that accepts arguments as declared by
  @rhombus(field_spec, ~var)s in the new subclass, and the result of that function
  is an instance of the new class. Again, the result instance might be an
  instance of a subclass if the new class is not @tech{final}.}

)
 
 When a @rhombus(class) has a @rhombus(binding, ~class_clause)
 form, then a use of new class's @rhombus(identifier_path, ~var) as a
 binding-pattern constructor invokes the @tech{entry point} (typically a
 @rhombus(rule, ~entry_point) form) in the block after
 @rhombus(binding, ~class_clause). The @rhombus(entry_point) is a
 meta-time expression. The specified @rhombus(bind_identifier) refers to
 a default binding constructor in the case that the new class has no
 superclass, or it is bound to a ``curried'' constructor that expects two
 bindings terms (that are typically each parenthesized sequences): the
 first term is combined with superclass's binding constructor, and the
 second term must be a parenthesized sequence of bindings that correspond
 to the fields declared by @rhombus(field_spec, ~var)s. Note that a binding
 constructor is not required to expect parentheses, but it must expect a
 single shrubbery term to work with this protocol for a subclass binding
 constructor.

 When a @rhombus(class) has a @rhombus(annotation, ~class_clause)
 form, then a use of new class's @rhombus(identifier_path, ~var) in a
 annotation invokes the @tech{entry point} (typically a
 @rhombus(rule, ~entry_point) form) in the block after
 @rhombus(annotation, ~class_clause). The @rhombus(entry_point) is a
 meta-time expression. The specificed @rhombus(annot_identifier) is bound
 so that it refers to the default annotation binding, but when the new
 class has a superclass, the annotation's @rhombus(of) form is
 ``curried'' in the sense that it expects be followed by two terms (that
 are typically each parenthesized sequences): one the first term is
 combined with the superclass annotation's @rhombus(of) form, and the
 second must be a parenthesized sequence of annotations corresponding to
 the fields declared by @rhombus(field_spec, ~var)s. Note that an annotation
 @rhombus(of) constructor is not required to expect parentheses, but it
 must expect a single shrubbery term to work with this protocol for a
 subclass annotation constructor.

}

@doc(
  class_clause.macro 'authentic'
){

 When a @rhombus(class_clause) is @rhombus(authentic, ~class_clause),
 then the new class cannot be chaperoned or impersonated. At most one
 @rhombus(class_clause) can have @rhombus(authentic, ~class_clause).

}