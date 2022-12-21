#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def method_eval: make_rhombus_eval())

@examples(~eval: method_eval,
          ~hidden: #true,
          fun lookup_specs(make, model): [10, 30])

@title(~tag: "private-method"){Private Fields and Methods}

Sometimes, a class needs extra fields in its objects that are not
visible outside the class's implementation. The @rhombus(class) form
supports two ways of declaring private fields. One way is to use
@rhombus(private, ~class_clause) @rhombus(field, ~class_clause)
in the @rhombus(class) body:

@(demo:
    ~defn:
      class Car(mpg):
        private field gas: 10
        method go(dist):
          gas := gas - dist/mpg
    ~repl:
      def c : Car(30)
      c.go(240)
      c
  )

Another way is to use the @rhombus(private, ~class_clause) modifier for
a field listed in parentheses after the class name. Also including a
default-value expression with @rhombus(=) avoids the need for a custom
constructor, since it is automatically omitted from the default
constructor.

@(demo:
    ~defn:
      class Car(mpg, private mutable gas = 10):
        method go(dist):
          gas := gas - dist/mpg
    ~repl:
      def c : Car(30)
      c.go(240)
      c
  )

Both approaches work in this example, because the field is mutable. If a
private field is immutable, then it needs to be written with other
fields in parentheses, because a @rhombus(field, ~class_clause)
declaration in a class body always creates a mutable field. If the
initial value of a private depends on values supplied when an object is
created, then a custom constructor may be needed. When a private field
is declared within parentheses after a class name, then the underlying
constructor accessed with @rhombus(super)---as used in a custom
constructor---accepts values for private fields as well as public ones.
  
@(demo:
    ~eval: method_eval
    ~defn:
      class Car(make, model, private mpg):
        private field gas: 0
        constructor(make, model):
          def [tank_size, mpg]: lookup_specs(make, model)
          def car: super(make, model, mpg)
          car.gas := tank_size
          car
        method go(dist):
          gas := gas - dist/mpg
    ~repl:
      def c : Car("Mazda", "Miata")
      c.go(240)
      c
)

Private fields are visible only within methods of the same class when
accessed directly or through the class's annotation, within a
constructor through the class's annotation, or through an
@rhombus(internal, ~class_clause) name's static annotation. For example,
declaring an internal name @rhombus(_Car) makes a priviate @rhombus(gas)
field accessible outside the class's implementation:

@(demo:
    ~defn:
      class Car(mpg):
        internal _Car
        private field gas: 10
        method go(dist):
          gas := gas - dist/mpg
    ~repl:
      def c : Car(30)
      c.go(240)
      ~error:
        c.gas
      (c -: _Car).gas
  )

Methods can be private too, following essentially the same rules as
private fields. Private methods can be useful as helpers within a
class's implementation, or via an @rhombus(internal, ~class_clause) name
that is selectively exported, they can be used to limit access to
some methods.

@close_eval(method_eval)
