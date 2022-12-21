#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def method_eval: make_rhombus_eval())

@title(~tag: "properties"){Fields and Properties}

A @rhombus(mutable) declaration before a field enables assignment to the
field using @rhombus(:=). An assignment can appear within methods using
the field name directly, or it can appear on a @rhombus(.) expression
that accesses a field in an object.

@(demo:
    ~eval: method_eval
    ~defn:
      class Posn(mutable x, mutable y):
        method go_home():
          x := 0
          y := 0
    ~repl:
      def p = Posn(3, 4)
      p.x := 9
      p
      p.go_home()
      p
  )

Extra fields can be added to a class with @rhombus(field, ~class_clause)
clauses. These fields are not represented the class's constructor, and
so a @rhombus(field, ~class_clause) has an expression to provide the
field's initial value. A field added with @rhombus(field, ~class_clause)
is always mutatble, and @rhombus(:=) can be used to assign to the field.

@(demo:
    ~eval: method_eval
    ~defn:
      class Posn(x, y):
        field name = "Jane Doe"
    ~repl:
      def p = Posn(3, 4)
      p.name := "Dotty"
      p.name
  )

Sometimes, you want a value associated to an object that is not stored
in a field, but is still accessed and assigned with field-like notation.
A @deftech{property} is like a field in that it is accessed without
method-call parentheses, and it can also support assignment via
@rhombus(:=). A property is different from a field, however, in that
access and assignment can trigger arbitrary method-like comptation, and
a property implementation can be overridden in a subclass.

A read-only @rhombus(property, ~class_clause) is written similar to a
@rhombus(method, ~class_clause), possibly with a result annotation, but
without parentheses for arguments.

@(demo:
    ~eval: method_eval
    ~defn:
      class Posn(x, y):
        property angle :: Real:
          atan(y, x)
        property magnitude :: Real:
          sqrt(x*x + y*y)
    ~repl:
      Posn(3, 4).magnitude
      Posn(4, 4).angle
      ~error:
        Posn(4, 4).angle := 0.0
  )

To define a property that supports assignment, use @litchar{|} similar
to defining a function with @rhombus(fun) and multiple cases. The first
case should look like a simple @rhombus(property, ~class_clause) clause
to implement property access, while the second case should resemble an
assignment form. In the assignment-like form to define a property, the
part after @rhombus(:=) is a binding position, just like a function or
method argument.

@(demo:
    ~eval: method_eval
    ~defn:
      class Posn(mutable x, mutable y):
        property
        | angle :: Real:
            atan(y, x)
        | angle := new_angle :: Real:
            def m = magnitude
            x := m * cos(new_angle)
            y := m * sin(new_angle)
        property magnitude :: Real:
          sqrt(x*x + y*y)
          
    ~repl:
      def p = Posn(4, 4)
      p.magnitude
      p.angle
      p.angle := 0.0
      p
  )

A property can be a good choice for derived values like
@rhombus(magnitude) and @rhombus(angle) in a @rhombus(Posn), because
they require relatively little computation and are deterministically
derived from the object's fields. A property is probably not a good
choice for a lookup action that involves visible side effects or that
involves a lot of computation, because those features defeat the
expectation of someone reading the code, because they see a field-like
access.

@close_eval(method_eval)
