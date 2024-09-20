#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Veneers}

@doc(
  ~literal: :: extends binding
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause

  defn.macro 'veneer $id_name($this_decl)'
  defn.macro 'veneer $id_name($this_decl):
                $veneer_clause_or_body_or_export
                ...'

  grammar this_decl:
    this #,(@rhombus(::, ~bind)) $annot
    this #,(@rhombus(:~, ~bind)) $annot

  grammar veneer_clause_or_body_or_export:
    $veneer_clause
    $body
    $export

  grammar veneer_clause:
    #,(@rhombus(method, ~veneer_clause)) $method_impl
    #,(@rhombus(override, ~veneer_clause)) $method_impl
    #,(@rhombus(final, ~veneer_clause)) $method_impl
    #,(@rhombus(private, ~veneer_clause)) $method_impl
    #,(@rhombus(property, ~veneer_clause)) $property_impl
    #,(@rhombus(extends, ~veneer_clause)) $id_name
    #,(@rhombus(implements, ~veneer_clause)) $implements_decl
    #,(@rhombus(private, ~veneer_clause)) #,(@rhombus(implements, ~class_clause)) $implements_decl
    #,(@rhombus(protected, ~veneer_clause)) #,(@rhombus(implements, ~class_clause)) $implements_decl
    #,(@rhombus(expression, ~veneer_clause)) $expression_decl
    #,(@rhombus(dot, ~veneer_clause)) $dot_decl
    #,(@rhombus(static_info, ~veneer_clause)) $static_info_decl
    #,(@rhombus(converter, ~veneer_clause))
    $other_veneer_clause
){

 Similar to @rhombus(class), but binds @rhombus(id_name) as a static
 class @deftech{veneer} over an existing representation, instead of creating a new
 representation like @rhombus(class) does. The existing representation is
 indicated by the @rhombus(annot) written after the @rhombus(this)
 pseudo-field (in parentheses after @rhombus(id_name)); this
 representation is checked only when using @rhombus(::, ~bind), and not
 when using @rhombus(:~, ~bind).

 When a value has a veneer's static information---typically because the
 veneer name is used as an @tech(~doc: guide_doc){annotation}---then static lookup of
 methods, indexing operations, etc., use the veneer instead of the
 underlying representation. Within the @rhombus(veneer) body, however,
 @rhombus(this) has the static information of @rhombus(annot).

 A veneer affects only static resolution of operations. To help avoid
 confusing behavior when dynamic resolution is chosen in a dynamic-mode
 context, a veneer can be used as an expression or annotation only in a
 static-mode context (see @rhombus(use_static)). Using a veneer
 annotation in a dynamic-mode context is a syntax error.

 The veneer's @rhombus(id_name) is bound in several @tech{spaces}:

@itemlist(

 @item{in the @top_rhombus(expr, ~space) space, a constructor function
  or form, which by default is a function that takes one argument and
  returns it, but with the static information of @rhombus(id_name);}

 @item{in the @top_rhombus(annot, ~space) space, an annotation that is
  satisfied by any value that satisfies the @rhombus(annot) specified for
  @rhombus(this);}

 @item{in the @rhombus(namespace, ~space) space,
  a @tech{namespace} to access exported bindings as well as a
  function
  @rhombus(id_name#,(rhombus(.))#,(@rhombus(method,~var))),
  a function
  @rhombus(id_name#,(rhombus(.))#,(@rhombus(property,~var))),
  and a syntactic form
  @rhombus(id_name#,(rhombus(.))#,(@rhombus(dot,~var))) for each
  non-@rhombus(private, ~class_clause)/@rhombus(protected, ~class_clause) method, property, and dot syntax in the veneer
  (including inherited methods, properties, and dot syntax), respectively; and}

 @item{in the @rhombus(class, ~space) space, a representation of the
  veneer for reference as a superveneer.}

)

 When the @rhombus(annot) declared for @rhombus(this) is checked, the
 check occurs whenever the veneer is used as an annotation, by the
 default veneer constructor, when a method or property of the veneer is
 called. In the case of a method or property, the check applies when
 using @rhombus(.) after an expression that has the veneer's static
 information or when using a function like
 @rhombus(id_name#,(rhombus(.))#,(@rhombus(method,~var))). An
 @rhombus(annot) can be a @tech(~doc: guide_doc){converter annotation} only if the
 @rhombus(converter, ~veneer_clause) veneer clause is specified, in which
 case the veneer is also a converter annotation.

 Analogous to @rhombus(class), the body of a @rhombus(veneer) form
 consists of definitions, exports, and @deftech{veneer clauses}.

 When a @rhombus(veneer_clause) is an @rhombus(extends, ~veneer_clause)
 form, the new veneer is created as a subveneer of the extended veneer.
 If both the superveneer and subveneer have checked @rhombus(annot)s,
 then both checks apply for any use of the subveneer. If a superveneer's
 @rhombus(annot) is a converter annotation, then the
 @rhombus(converter, ~veneer_clause) veneer clause is implicit. In the
 case of a converter subveneer, the subveneer's conversion applies before
 the superveneer's conversion (or predicate). In the case of a
 @tech(~doc: guide_doc){predicate annotation} subveneer, the superveneer's predicate is
 tried first.

 A veneer can implement only specific interfaces that serve as bridges
 to static dispatch: @rhombus(Indexable, ~class),
 @rhombus(MutableIndexable, ~class), @rhombus(Appendable, ~class),
 and @rhombus(Comparable, ~class).
 Note that @rhombus(is_a) is a dynamic operation, not a static operation.
 So, for example, a value with a veneer that implements
 @rhombus(Indexable, ~class) is not an instance in the sense of
 @rhombus(is_a) (unless the underlying representation is an instance).
 Instead, having a veneer implement @rhombus(Indexable, ~class) only
 makes static @rhombus(#%index) references (usually written with
 @brackets) use the veneer's @rhombus(get, ~datum) method. A veneer
 cannot implement @rhombus(Sequenceable, ~class), but it can implement
 static sequence conversion with @rhombus(sequence, ~veneer_clause).

@examples(
  ~defn:
    use_static
    veneer Posn(this :: Pair):
      property x:
        this.first
      property y:
        this.rest
      private implements Indexable
      private override get(i):
        match i
        | 0: x
        | 1: y
      expression 'Posn($x, $y)':
        'Pair($x, $y) :~ Posn'
  ~repl:
    def p = Posn(10, 20)
    p.x
    p[1]
    ~error:
      p.first
    block:
      use_dynamic
      p.first
    ~error:
      dynamic(p).x
)

}


@doc(
  veneer_clause.macro 'extends $id_name'
  veneer_clause.macro 'extends: $id_name'
){

 A @tech{veneer clause} recognized by @rhombus(veneer) to define a
 veneer that is a subveneer of the one named by @rhombus(id_name). See
 @rhombus(veneer).

}

@doc(
  veneer_clause.macro 'implements $id_name ...'
  veneer_clause.macro 'implements: $id_name ...; ...'
){

 A @tech{veneer clause} recognized by @rhombus(veneer) to define a class
 that implements subclasses named by @rhombus(id_name)s. See
 @rhombus(veneer).

}

@doc(
  veneer_clause.macro 'converter'
){

 A @tech{veneer clause} that is recognized by @rhombus(veneer) so that
 the new veneer is a @tech(~doc: guide_doc){converter annotation}. See @rhombus(veneer).

}

@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause

  veneer_clause.macro 'final $method_impl'
  veneer_clause.macro 'final #,(@rhombus(method, ~class_clause)) $method_impl'
  veneer_clause.macro 'final #,(@rhombus(override, ~class_clause)) $method_impl'
  veneer_clause.macro 'final #,(@rhombus(override, ~class_clause)) #,(@rhombus(method, ~class_clause)) $method_impl'
  veneer_clause.macro 'final #,(@rhombus(property, ~class_clause)) $property_impl'
  veneer_clause.macro 'final #,(@rhombus(override, ~class_clause)) #,(@rhombus(property, ~class_clause)) $property_impl'
){

 Like @rhombus(final, ~class_clause), but as a @tech{veneer clause} to
 be followed by a method or property declaration. In that case, the
 method or property is final, and a final method or property cannot be
 overridden in subveneers.

}


@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause

  veneer_clause.macro 'method $method_impl'
  veneer_clause.macro 'property $property_impl'
  veneer_clause.macro 'override $method_impl'
  veneer_clause.macro 'override #,(@rhombus(method, ~class_clause)) $method_impl'
  veneer_clause.macro 'override #,(@rhombus(property, ~class_clause)) $property_impl'
){

 Like @rhombus(method, ~class_clause) and other class clauses, but as
 @tech{veneer clauses}. See @rhombus(veneer).

}

@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause

  veneer_clause.macro 'private #,(@rhombus(implements, ~class_clause)) $id_name ...'
  veneer_clause.macro 'private #,(@rhombus(implements, ~class_clause)): $id_name ...; ...'
  veneer_clause.macro 'private $method_impl'
  veneer_clause.macro 'private #,(@rhombus(method, ~class_clause)) $method_impl'
  veneer_clause.macro 'private #,(@rhombus(property, ~class_clause)) $property_impl'
  veneer_clause.macro 'private #,(@rhombus(override, ~class_clause)) $method_impl'
  veneer_clause.macro 'private #,(@rhombus(override, ~class_clause)) #,(@rhombus(method, ~class_clause)) $method_impl'
  veneer_clause.macro 'private #,(@rhombus(override, ~class_clause)) #,(@rhombus(property, ~class_clause)) $property_impl'
){

 Like @rhombus(private, ~class_clause) as a class clause, but as a
 @tech{veneer clause}. See @rhombus(veneer).

}


@doc(
  ~nonterminal:
    method_impl: method ~class_clause
    property_impl: method ~class_clause

  veneer_clause.macro 'protected #,(@rhombus(implements, ~class_clause)) $id_name ...'
  veneer_clause.macro 'protected #,(@rhombus(implements, ~class_clause)): $id_name ...; ...'
  veneer_clause.macro 'protected $method_impl'
  veneer_clause.macro 'protected #,(@rhombus(method, ~class_clause)) $method_impl'
  veneer_clause.macro 'protected #,(@rhombus(property, ~class_clause)) $property_impl'
){

 Like @rhombus(protected, ~class_clause) as a class clause, but as a
 @tech{veneer clause}. See @rhombus(veneer).

}


@doc(
  veneer_clause.macro 'expression: $entry_point'
  veneer_clause.macro '«expression '$id $pattern ...': '$template ...'»'
  veneer_clause.macro '«expression
                        | '$id $pattern ...': '$template ...'
                        | ...»'
){

 A @tech{veneer clauses} as recognized by @rhombus(veneer) to replace
 the default expression form, analogous to
 @rhombus(expression, ~class_clause) for @rhombus(class).

}
