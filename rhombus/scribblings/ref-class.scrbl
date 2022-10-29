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

  class_clause.macro 'extends $identifier_path',
  class_clause.macro 'final',
  class_clause.macro 'nonfinal',
  class_clause.macro 'field $identifier $maybe_annotation: $body; ...',
  class_clause.macro 'internal $identifier',
  class_clause.macro 'constructor ($make_identifier): $entry_point',
  class_clause.macro 'constructor: $entry_point',
  class_clause.macro 'binding ($bind_identifier): $entry_point',
  class_clause.macro 'binding: $entry_point',
  class_clause.macro 'annotation ($annot_identifier): $entry_point',
  class_clause.macro 'annotation: $entry_point',
  class_clause.macro 'authentic',
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

 When a @rhombus(class_clause) is a @rhombus(field, ~class_clause) form,
 then an additional field is added to the class, but the additional field
 is not represented by an arguments to the constructor. Instead, the
 @rhombus(body) block in @rhombus(field, ~class_clause) gives the added
 field its initial value; that block is evaluated at the time the
 @rhombus(class) form is evaluated, not when an object is instantiated,
 and the same values are used for every instance of the class. All fields
 added through a @rhombus(field, ~class_clause) clause are mutable. The
 @rhombus(field, ~class_clause) can appear any number of times as a
 @rhombus(class_clause).

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
 identifier can be provided, such as @rhombus(make_identifier) or
 @rhombus(bind_identifier); within the clause, that identifier is bound
 to refer to the default implementation (roughly, in the case of
 constructors). When an identifier like @rhombus(make_identifier) is not
 provided in a clause, then the @rhombus(class) form must include a
 @rhombus(internal, ~class_clause) clause, and the @rhombus(identifier)
 from @rhombus(internal, ~class_clause) is used (shadowing the binding
 for @rhombus(identifier) outside of the specific clause).
 
 When a @rhombus(class_clause) is a @rhombus(constructor, ~class_clause)
 form, then a use of new class's @rhombus(identifier_path) as a
 constructor function invokes the @tech{entry point} (typically a
 @rhombus(fun, ~entry_point) form) in the block after
 @rhombus(constructor, ~class_clause). That function must return an
 instance of the new class, typically by calling
 @rhombus(make_identifier):

@itemlist(

 @item{If the new class does not have a superclass, then
  @rhombus(make_identifier) is bound to a function that accepts as many
  arguments as declared @rhombus(field_spec)s (nout counting
  @rhombus(field, ~class_clause) clauses, if any), and it returns an
  instance of the class. Note that this instance might be an instance of a
  subclass if the new class is not @tech{final}.},

 @item{If the new class has a superclass, then @rhombus(make_identifier)
  is bound to a function that accepts the same arguments as the superclass
  constructor. Instead of returning an instance of the class, it returns a
  function that accepts as many arguments as declared
  @rhombus(field_spec)s in the new subclass (again, not counting
  @rhombus(field, ~class_clause) clauses), and the result of that function
  is an instance of the new class. Again, the result instance might be an
  instance of a subclass if the new class is not @tech{final}.}

)
 
 When a @rhombus(class_clause) is a @rhombus(binding, ~class_clause)
 form, then a use of new class's @rhombus(identifier_path) as a
 binding-pattern constructor invokes the @tech{entry point} (typically
 a @rhombus(rule, ~entry_point) form) in the block after
 @rhombus(binding, ~class_clause). The specified
 @rhombus(bind_identifier) is bound so that it refers to the default
 binding-pattern constructor.

 When a @rhombus(class_clause) is a @rhombus(annotation, ~class_clause)
 form, then a use of new class's @rhombus(identifier_path) in a
 annotation invokes the @tech{entry point} (typically
 a @rhombus(rule, ~entry_point) form) in the block after
 @rhombus(annotation, ~class_clause). The specificed
 @rhombus(ann_identifier) is bound so that it refers to the default
 annotation binding.

 When a @rhombus(class_clause) is @rhombus(authentic, ~class_clause),
 then the new class cannot be chaperoned or impersonated. At most one
 @rhombus(class_clause) can have @rhombus(authentic, ~class_clause).
 
 Each field name must be distinct from all other field names, whether
 from a parenthesized @rhombus(field_spec) or from a
 @rhombus(field, ~class_clause) clause. If an @rhombus(extends) clause is
 present, then each field name must also be distinct from any field name
 in the superclass.

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
