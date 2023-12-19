#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@(def method_eval = make_rhombus_eval())

@title(~tag: "class-together"){Mutual Dependencies}

The @rhombus(class) and @rhombus(interface) forms define names that can
be used as annotations, but the rules about defining annotations before
using them can be subtle. For example, this works:

@examples(
  block:
    def make:
      fun () :: Posn:
        Posn(1, 2)
    class Posn(x, y)
    make()
)

But this does not:

@examples(
  ~error:
    block:
      fun make() :: Posn:
        Posn(1, 2)
      class Posn(x, y)
)

The reason the first example above works, however, is the same as the
reason this one does not:

@examples(
  ~error:
    block:
      def make:
        fun () :: Posn:
          Posn(1, 2)
      class Posn(x, y)
      use_static  // causes the next line to fail
      make().x
)

When you use @rhombus(fun) as a definition form to bind a function
@nontermref(id_name) with a result annotation, the static
information associated with that result annotation needs to be
determined to bind @nontermref(id_name). That's why the middle
example fails. Using a @rhombus(fun) expression on the right-hand side
of @rhombus(def) to bind @nontermref(id_name) doesn't need to
inspect the result annotation to create the binding, but it also does
not propagate that static information.

For this small example, the solution is straightforward, and similar to
the one in @secref("class-namespace"): define @rhombus(Posn) before
trying to use it:

@examples(
  block:
    class Posn(x, y)
    fun make() :: Posn:
      Posn(1, 2)
    use_static
    make().x
)

This ``just define it before'' strategy does not work, however, when you
have two classes that need to refer to each other.

@examples(
  ~error:
    block:
      class Posn2D(x, y):
        method inject() :: Posn3D:
          Posn3D(x, y, 0)
      class Posn3D(x, y, z):
        method project() :: Posn2D:
          Posn2D(x, y)
)

The problem here is that the annotation facet of @rhombus(class) is
bundled together with the method-declaration facet of @rhombus(class), so
they canot be ordered differently. To enable mutual references, use the
@rhombus(class.together) form to combine the definitions.

@examples(
  block:
    class.together:
      class Posn2D(x, y):
        method inject() :: Posn3D:
          Posn3D(x, y, 0)
      class Posn3D(x, y, z):
        method project() :: Posn2D:
          Posn2D(x, y)
    use_static
    Posn2D(1, 2).inject().project()
)

The @rhombus(class.together) form can also be used when references among
classes only go in one way, but you'd prefer to define the referencing
class before the referenced class, and it can resolve the problem of
referencing a class from definitions within the @rhombus(class) form as
discussed in @secref("class-namespace").

The @rhombus(class.together) form works by using
@rhombus(annot.delayed_declare) and @rhombus(annot.delayed_complete).
You can use @rhombus(annot.delayed_declare) and
@rhombus(annot.delayed_complete) directly instead of
@rhombus(class.together), and that approach allows declarations to span
different modules. Within a module, prefer @rhombus(class.together),
because it avoids potential pitfalls of using
@rhombus(annot.delayed_declare) and @rhombus(annot.delayed_complete)
directly.


@(close_eval(method_eval))
