#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def macro_eval = make_rhombus_eval())

@title(~tag: "bind-macro"){Binding and Annotation Macros}

Macros can extend binding-position syntax, too, via
@rhombus(bind.macro). In the simplest case, a binding operator is implemented
by expanding to other binding operators, like this definition of @rhombus($$$)
as a prefix operator to constrain a pattern to number inputs:

@examples(
  ~eval: macro_eval
  ~defn:
    import:
      rhombus/meta open

    bind.macro '$$$ $n':
      '$n :: Number'
  ~repl:
    def $$$salary = 100.0

    salary
)

More expressive binding operators can use a lower-level protocol where a
binding is represented by transformers that generate checking and
binding code. It gets complicated, and it's tied up with the propagation
of static information, so the details are in @secref("bind-macro-protocol").
After an expressive set of binding forms are implemented with the
low-level interface, however, many others can be implemented though
simple expansion.

The @rhombus(annot.macro) form is similar to @rhombus(bind.macro), but for
annotations.

@rhombusblock(
  use_static

  annot.macro 'PosnList': 'List.of(Posn)'

  fun nth_x(ps :~ PosnList, n):
    ps[n].x
)

Annotations and binding patterns serve similar and interacting purposes.
The @rhombus(:~, ~bind) and @rhombus(::, ~bind) binding operators put annotations to
work in a binding. For the other direction, the @rhombus(matching, ~annot)
annotation operator puts a binding form to work in a annotation.

For example, suppose you want a annotation @rhombus(PersonList), which
is a list of maps, and each map must at least relate @rhombus("name") to
a @rhombus(String, ~bind) and @rhombus("location") to a @rhombus(Posn). The
@rhombus(Map.of, ~annot) annotation combination cannot express a per-key
specialization, but the @rhombus(Map) binding pattern can.

@examples(
  ~eval: macro_eval
  ~hidden:
    class Posn(x, y)
  ~defn:
    annot.macro 'PersonList':
      'List.of(matching({"name": (_ :: String),
                         "location": (_ :: Posn)}))'

    def players :: PersonList:
      [{"name": "alice", "location": Posn(1, 2)},
       {"name": "bob", "location": Posn(3, 4)}]
)

As another example, here's how a @rhombus(ListOf) annotation constructor
could be implemented if @rhombus(List.of, ~annot) did not exist already:

@examples(
  ~eval: macro_eval
  ~defn:
    annot.macro 'ListOf ($ann ...)':
      'matching([_ :: ($ann ...), $('...')])'
)

At a lower level, the bridge between binding patterns and annotations is
based on their shared use of @seclink("static-info"){static information}
as described in the @seclink("bind-macro-protocol"){binding API} and the
@seclink("annotation-macro"){annotation API}.

@(close_eval(macro_eval))
