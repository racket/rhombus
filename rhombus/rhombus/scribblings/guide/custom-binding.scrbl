#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "../macro.rhm")

@(def method_eval = macro.make_macro_eval())

@title(~tag: "custom-binding"){Binding and Annotation}

When a @rhombus(constructor, ~class_clause) clause creates a constructor
that is different enough from the default one, then the binding and
annotation forms associated with the class name also should be updated.
The @rhombus(binding, ~class_clause) and
@rhombus(annotation, ~class_clause) clause forms support that
customization. Binding and annotation are customized by macros, and
similar to constructor customization, they work by relying on an
underlying binding or annotation form. But instead of having a
@rhombus(super) that is mapped to the underlying forms, binding and
annotation customization rely on an @rhombus(internal, ~class_clause)
clause to give a name to the more primitive form, and then that name can
be used in an example.

For example, suppose we customize the constructor for a
@rhombus(Sandwich) class to accept any number of arguments, instead of
having the user put those arguments into a list. That is, we want users
to write @rhombus(Sandwich("pb", "j"), ~bind) instead of
@rhombus(Sandwich(["pb", "j"]), ~bind). To make pattern-matching binding
consistent with that choice, we should also customize the binding, so
that @rhombus(Sandwich(top, bottom), ~bind) would match instead of
@rhombus(Sandwich([top, bottom]), ~bind). Similarly,
@rhombus(Sandwich.of(String), ~annot) would be preferable to
@rhombus(Sandwich.of(List.of(String)), ~annot), which requires adding an
@rhombus(of, ~datum) annotation form that is exported from
@rhombus(Sandwich) as a namespace.

The customization shown below defines @rhombus(_Sandwich) with
@rhombus(internal, ~class_clause) so that it can be used in the binding
and annotation expansion:

@examples(
  ~eval: method_eval
  ~defn:
    class Sandwich(ingredients):
      nonfinal
      internal _Sandwich
      constructor (ingredient, ...):
        super([ingredient, ...])
      binding 'Sandwich($ingredient, ...)':
        '_Sandwich([$ingredient, ...])'
      annotation 'Sandwich':
        '_Sandwich'
      annot.macro 'of($ingredient)':
        '_Sandwich.of(List.of($ingredient))'
      export:
        of
  ~repl:
    def blt = Sandwich("bacon", "lettuce", "tomato")
    def Sandwich(top, _, _) = blt
    top
    blt is_a Sandwich
    blt is_a Sandwich.of(Number)
)

When a class has a superclass, the @rhombus(super) constructor function
is curried for customizing a subclass constructor. The name bound by
@rhombus(internal, ~class_clause) can be used as a constructor, and it is
curried in the same way as @rhombus(super)---but don't use a internal
name instead of @rhombus(super), because @rhombus(super) adapts to
subclass construction, while an internal name always constructs an
immediate instance of its class.

An internal name for a subclass is not curried as a binding or
annotation form. Instead, it always refers to just the immediate fields
of the class. A superclass binding or annotation can be combined with an
internal binding or annotation using the @rhombus(&&, ~bind) binding
operator or @rhombus(&&, ~annot) annotation operator.

@examples(
  ~eval: method_eval
  ~defn:
    class Sub(inches):
      extends Sandwich
      internal _Sub
      constructor (~inches: inches, ingredient, ...):
        super(ingredient, ...)(inches)
      binding 'Sub($pre, ..., ~inches: $inches, $post, ...)':
        'Sandwich($pre, ..., $post, ...) && _Sub($inches)'
      annotation 'Sub':
        '_Sub'
      annot.macro 'of($ingredient)':
        '_Sub && Sandwich.of($ingredient)'
      export:
        of
  ~repl:
    def blt = Sub("bacon", "lettuce", "tomato", ~inches: 6)
    def Sub(~inches: len, stuff, ...) = blt
    len
    [stuff, ...]
    blt is_a Sub.of(String)
    Sandwich("pb", "j") is_a Sub.of(String)
)


@(close_eval(method_eval))
