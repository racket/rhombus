#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def ann_eval = make_rhombus_eval())
@examples(
  ~eval: ann_eval
  ~hidden:
    import:
      rhombus/meta open
    class Posn(x, y)
)

@title(~tag: "annotation-vs-bind"){Annotations versus Binding Patterns}

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
  ~eval: ann_eval
  ~defn:
    annot.macro 'PersonList':
      'List.of(matching({"name": (_ :: String),
                         "location": (_ :: Posn)}))'

    def players :: PersonList:
      [{"name": "alice", "location": Posn(1, 2)},
       {"name": "bob", "location": Posn(3, 4)}]
)

As another example, here's how a @rhombus(ListOf) annotation constructor
could be implemented if @rhombus(List.of, ~bind) did not exist already:

@examples(
  ~eval: ann_eval
  ~defn:
    annot.macro 'ListOf ($ann ...)':
      'matching([_ :: ($ann ...), $('...')])'
)

At a lower level, the bridge between binding patterns and annotations is
based on their shared use of @seclink("static-info"){static information}
as described in the @seclink("bind-macro-protocol"){binding API} and the
@seclink("annotation-macro"){annotation API}. @tech{Converter
 annotations}, as opposed to @tech{predicate annotations}, also connect
bindings and annotations as described in
@secref("annotation-macro-protocol").


@(close_eval(ann_eval))
