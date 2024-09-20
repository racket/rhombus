#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def subclass_eval = make_rhombus_eval())

@title(~tag: "interfaces"){Interfaces}

The @rhombus(interface) definition form for an @deftech{interface}
is similar to @rhombus(class),
but without fields. A @rhombus(class) form can declare that the class
implements an interface through an @rhombus(implements, ~class_clause)
clause. When a class implements an interface, then instances of the
class satify the interface annotation, similar to the way that a
subclass instances satifies the superclass annotation. A class can have
at most one superclass. but it can implement any number of interfaces.
An interface is never final, so @rhombus(nonfinal, ~class_clause) is
not needed in an interface.

@examples(
  ~defn:
    interface Shape
    interface Dance
    class Square(side):
      implements:
        Shape
        Dance
  ~repl:
    def s = Square(10)
    s is_a Shape
    s is_a Dance
)

Interfaces can extend other interfaces. Unlike
classes extending at one most superclass, interfaces can extend any
number of superinterfaces.

@examples(
  ~defn:
    interface MailingAddress
    interface Residence
    interface HomeAddress:
      extends:
        MailingAddress
        Residence
  ~defn:
    interface LawncareClient
  ~defn:
    class SingleFamilyHome(street, city, state, zip):
      implements:
        HomeAddress
        LawncareClient
)


@(close_eval(subclass_eval))
