#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    "macro.rhm")

@(def macro_eval: macro.make_macro_eval())

@(def dollar: @rhombus($))

@title{Class and Interface Clause Macros}

@doc(
  space.transform class
){

 The @tech{space} for class and interface names as bound by
 @rhombus(class) and @rhombus(interface).

}

@doc(
  space.transform class_clause
  space.transform interface_clause
){

 The @tech{spaces} for bindings of identifiers that implement
 @rhombus(class) and @rhombus(interface) clauses, respectively.

}

@doc(
  ~nonterminal:
    defined_name: defn.macro

  defn.macro '«class_clause.macro '$defined_name $pattern ...':
                $option; ...
                $body
                ...»'
  defn.macro '«class_clause.macro
               | '$defined_name $pattern ...':
                   $option; ...
                   $body
                   ...
               | ...»'
  grammar option:
    ~op_stx: $id
    ~info: $id
){

 Like @rhombus(defn.macro, ~expr), but defines a name in the
 @rhombus(class_clause, ~space) @tech{space} as a clause form for use
 within a @rhombus(class) body.

 The compile-time @rhombus(body) block returns the expansion result. The
 result must be a sequence of groups to be spliced in place of the macro
 use, where each group can be either a another @rhombus(class) clause, an
 expression, a defintion, or an export.

 In addition to an @rhombus(~op_stx) option like @rhombus(defn.macro, ~expr),
 the optional @rhombus(~info) body form binds an @rhombus(id) to
 a @rhombus(class_meta.Info) value. Use the
 @rhombus(class_meta.Info.lookup) function to access information about
 the class declaration that appears before the use of the class-clause
 macro.

@examples(
  ~eval: macro_eval
  class_clause.macro 'lazy_method $id(): $body':
    'private field result: #false
     method $id():
       result || (block:
                    def v: $body
                    result := v
                    v)'
  class Person(name):
    lazy_method greeting(): "Hello, " +& name
  def ming: Person("Ming")
  ming.greeting()
  ming.greeting() === ming.greeting()
)

}

@doc(
  ~nonterminal:
    defined_name: defn.macro

  defn.macro '«interface_clause.macro '$defined_name $pattern ...':
                 $option; ...
                 $body
                 ...»'
  defn.macro '«interface_clause.macro
               | '$defined_name $pattern ...':
                   $option; ...
                   $body
                   ...
               | ...»'
){

Like @rhombus(class_clause.macro), but for @rhombus(interface) clauses and binding
in the @rhombus(interface_clause, ~space) @tech{space}.

}

@doc(
  ~nonterminal:
    defined_name: defn.macro

  defn.macro '«class_and_interface_clause.macro '$defined_name $pattern ...':
                 $option; ...
                 $body
                 ...»'
  defn.macro '«class_and_interface_clause.macro
               | '$defined_name $pattern ...':
                   $option; ...
                   $body
                   ...
               | ...»'
){

 Like @rhombus(class_clause.macro), but defines for use both in
 @rhombus(class) clauses and @rhombus(interface) clauses by
 binding in both the the @rhombus(class_clause, ~space) and
 @rhombus(interface_clause, ~space) @tech{spaces}.

}

@doc(
  fun class_meta.describe(name :: Identifier) :: class_meta.Info
  fun interface_meta.describe(name :: Identifier) :: interface_meta.Info
){

@provided_meta()

 Returns an object containing information about @rhombus(name), which
 must be bound as a class name or internal class name for
 @rhombus(class_meta.describe) or as an interface name or internal
 interface name for @rhombus(interface_meta.describe). Use
 @rhombus(class_meta.Info.lookup) or @rhombus(interface_meta.Info.lookup)
 to extract details from the information object.

}

@doc(
  annot.macro 'class_meta.Info'
  fun class_meta.Info.lookup(info :: class_meta.Info,
                             key :: Symbol)
  annot.macro 'interface_meta.Info'
  fun interface_meta.Info.lookup(info :: interface_meta.Info,
                                 key :: Symbol)
){

 @provided_meta()

 A value satisfying @rhombus(class_meta.Info, ~annot) can be obtained
 from @rhombus(class_meta.describe) or recieved by a class-clause macro
 through an @rhombus(~info) declaration. Similarly, a value satisfying
 @rhombus(interface_meta.Info, ~annot) can be obtained by
 @rhombus(interface_meta.describe) or recieved by a interface-clause
 macro through an @rhombus(~info) declaration. In a macro declared with
 @rhombus(class_and_interface_clause.macro), a value received by
 @rhombus(~info) is an instance of either
 @rhombus(class_meta.Info, ~annot) or
 @rhombus(interface_meta.Info, ~annot), which indicates the context where
 the macro is used.

@dispatch_table(
  "class information"
  @rhombus(class_meta.Info)
  [info.lookup(key), class_meta.Info.lookup(info, key)]
)

@dispatch_table(
  "interface information"
  @rhombus(interface_meta.Info)
  [info.lookup(key), interface_meta.Info.lookup(info, key)]
)

 The @rhombus(class_meta.Info.lookup) and
 @rhombus(interface_meta.Info.lookup) functions access information via a
 @rhombus(key) symbol. The currently recognized keys are described below,
 but more may be added in the future.

 When receieved within a class-clause or interface-clause macro, the
 information reported for a key covers only class and interface clauses
 before the macro use, so a use of the same key in the same class but in
 a later clause may report different information. Furthermore, the
 information has not been fully checked; for example, the name of a class
 or interface being extended is reported as it appears, and has not yet
 been checked to be a valid class or interface name, and multiple names
 may be reported if a class contains multiple @rhombus(extends) clauses.
 Similarly, field- or method-name lists may include duplicates.

 When obtained via @rhombus(class_meta.describe) or
 @rhombus(interface_meta.describe), the information covers the whole body
 of the referenced @rhombus(class) or @rhombus(interface) form. When a
 class or interface name is used, then information about private fields
 and methods is omitted from queries; using an internal name for a class
 or interface includes information about private fields and methods. A
 class- or inteface-clause macro, which directly receives only partial
 information, can expand to a use of an expression macro that uses
 @rhombus(class_meta.describe) or @rhombus(interface_meta.describe) to
 get more information based on a retained or newly declared internal
 name.
 
 Reocgnized keys for classes:

@itemlist(

 @item{@rhombus(#'name): An identifier for the defined class, including
  any dotted-name prefix in the @rhombus(class) declaration.}

 @item{@rhombus(#'extends): A list of indentifiers for classes declared
  as superclasses. Normally, the list will be empty or have one element,
  but a multi-identifier list can be reported for a clause macro if a
  @rhombus(class) form has multiple @rhombus(extends, ~class_clause)
  clauses (even though an error will be reported later).}

 @item{@rhombus(#'implements): A list of indentifiers for interfaces
  declared to be implemented by the class. The list does not include
  interfaces that are implied by superclasses and implemented interfaces.
  For a clause macro or starting from @rhombus(class_meta.describe) with a
  class internal name, the list includes privately implemented
  interfaces.}

 @item{@rhombus(#'implements_visibilities): A list of symbols in parallel to
  a @rhombus(#'implements) list that categorizes each implementation's visibility:
  @rhombus(#'public) or @rhombus(#'private).}

 @item{@rhombus(#'internal_names): A list of identifiers declared as internal
  names for this class. This list will be non-empty only in the
  information provided to a clause macro.}
 
 @item{@rhombus(#'field_names): A list of identifiers for fields
  declared for the class in the order of the field declarations. For a
  clause macro, the list does not include fields that will be inherited
  from a superclass, while inherited public fields are included in information
  obtained via @rhombus(class_meta.describe).}

 @item{@rhombus(#'field_mutabilities): A list of symbols in parallel to
  a @rhombus(#'field_names) list that categorizes each field's mutability:
  @rhombus(#'immutable) or @rhombus(#'immutable).}

 @item{@rhombus(#'field_visibilities): A list of symbols in parallel to
  a @rhombus(#'field_names) list that categorizes each field's visibility:
  @rhombus(#'public) or @rhombus(#'private).}

 @item{@rhombus(#'field_constructives): A list of symbols in parallel to
  a @rhombus(#'field_names) list that categorizes each field's use in the
  default constructor: @rhombus(#'required), @rhombus(#'optional), or
  @rhombus(#'absent). All private fields are classified as @rhombus(#'absent).}

 @item{@rhombus(#'field_keywords): A list of keywords and
  @rhombus(#false)s in parallel to a @rhombus(#'field_names), where a
  keyword indicates that the field uses that keyword as an argument for
  the default constructor.}

 @item{@rhombus(#'method_names): A list of identifiers for methods
  declared for the class in the order of the method declarations. The list
  does not include methods that will be inherited from a superclass or
  implemented interface, while inherited public methods are included in
  information obtained via @rhombus(class_meta.describe).}

 @item{@rhombus(#'method_visibilities): A list of symbols in parallel to
  a @rhombus(#'methods_names) list that categorizes each methods's visibility:
  @rhombus(#'public) or @rhombus(#'private).}

 @item{@rhombus(#'method_arities): A list of arity representations in
  parallel to a @rhombus(#'methods_names) list. An arity represenation can
  be an integer, in which case each bit set in the integer represents a
  number of arguments accepted by the method, and no keyword argument are
  accepted. An arity represenation can be a list, in which case the first
  element is an integer (like an integer by itself), the second element is
  a list of required keywords for arguments, and the last element is either
  a list of allowed keywords or @rhombus(#false) if all keywords are
  accepted.  An arity representation can be @rhombus(#false) if no information
  about accepted arguments is available. Keep in mind that when a subclass
  overrides a method, it is not limited to the existing method's arity.}

 @item{@rhombus(#'property_names): A list of identifiers for properties
  declared for the class in the order of the property declarations.  The list
  does not include properties that will be inherited from a superclass or
  implemented interface, while inherited public properties are included in
  information obtained via @rhombus(class_meta.describe).}

 @item{@rhombus(#'property_visibilities): A list of symbols in parallel to
  a @rhombus(#'property_names) list that categorizes each property's visibility:
  @rhombus(#'public) or @rhombus(#'private).}

 @item{@rhombus(#'property_arities): A list of arity representations in
  parallel to a @rhombus(#'property_names) list. An arity representation
  is @rhombus(1) if the property does not support assignent, @rhombus(3)
  if it does support assignment, or @rhombus(#false) if the property's
  support for assignments is unknown.}

 @item{@rhombus(#'uses_default_constructor): A boolean indicating
  whether the class name in an expression position refers to a default
  constructor function.}

 @item{@rhombus(#'uses_default_binding): A boolean indicating
  whether the class name in a binding position refers to a default
  binding form.}

 @item{@rhombus(#'uses_default_annotation): A boolean indicating
  whether the class name in an annotation position refers to a default
  annotation form.}

)

Reocgnized keys for interfaces:

@itemlist(

 @item{@rhombus(#'name): An identifier for the interface being defined, including
  any dotted-name prefix in the @rhombus(interface) declaration.}

 @item{@rhombus(#'extends): A list of indentifiers for interfaces
  declared as superinterfaces. The list does not include interfaces that
  are implied by superinterfaces.}

 @item{@rhombus(#'internal_names): The same as for classes.}

 @item{@rhombus(#'method_names): The same as for classes.}

 @item{@rhombus(#'method_arities): The same as for classes.}

 @item{@rhombus(#'method_visibilities): The same as for classes.}

 @item{@rhombus(#'property_names): The same as for classes.}

 @item{@rhombus(#'property_arities): The same as for classes.}

 @item{@rhombus(#'property_visibilities): The same as for classes.}

 @item{@rhombus(#'uses_default_annotation): A boolean indicating
  whether the interface name in an annotation position refers to a default
  annotation form.}

)

}

@«macro.close_eval»(macro_eval)
