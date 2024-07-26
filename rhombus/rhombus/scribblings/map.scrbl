#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def map_eval = make_rhombus_eval())
@examples(
  ~eval: map_eval
  ~hidden:
    class Posn(x, y)
)

@title(~tag: "map"){Maps}

The @rhombus(Map) constructor creates an immutable mapping of arbitrary
keys to values. A map is @tech{indexable} using @brackets with a key,
and the result is the corresponding value.

The @rhombus(Map) constructor can be used like a function, in which case
it accepts keys paired with values in two-item lists to create a map:

@examples(
  ~eval: map_eval
  ~defn:
    def neighborhood = Map(
      ["alice", Posn(4, 5)],
      ["bob", Posn(7, 9)],
    )
  ~repl:
    neighborhood["alice"]
    ~error:
      neighborhood["clara"]
)

Curly braces @braces can be used as a shorthand
for writing @rhombus(Map(#,(@elem{...}))). Within curly braces, the key and value
are joined by @litchar{:}. (If a key expression needs to use @litchar{:}
itself, the expression will have to be in parentheses.)

@examples(
  ~eval: map_eval
  ~defn:
    def neighborhood = {
      "alice": Posn(4, 5),
      "bob": Posn(7, 9),
    }
  ~repl:
    neighborhood["alice"]
)

You can also put @rhombus(Map) in front of @braces, but that makes
more sense with map constructors other than the @rhombus(Map) default,
such as @rhombus(MutableMap).

To functionally extend a map, use the @rhombus(++) append operator:

@examples(
  ~eval: map_eval
  ~defn:
    def new_neighborhood = neighborhood ++ {"alice": Posn(40, 50)}
  ~repl:
    new_neighborhood["alice"]
    neighborhood["alice"]
)

When @rhombus(++) is used with a left-hand side that is statically known
to be a map, and when the right-hand
argument is an immediate map construction with a single element, then
the use of @rhombus(++) is compiled as an efficient single-key update of
the map. Whether optimized or general, the @rhombus(++) operator will
only combine certain compatible kinds of values. For example, @rhombus(++)
will append two lists or combine two maps, but it will not combine a
list and a map.

@rhombus(Map) or its curly-braces shorthand is also an annotation and a
binding constructor. As an annotation or binding constructor,
@rhombus(Map) refers to immutable maps.

In a binding use of @rhombus(Map), the key positions are @emph{expressions},
not @emph{bindings}. The binding matches an input that includes the keys, and
each corresponding value is matched to the value binding pattern.

@examples(
  ~eval: map_eval
  ~defn:
    fun alice_home({"alice": p}):
      p
  ~repl:
    alice_home(neighborhood)
)

The @rhombus(Map.of, ~annot) annotation constructor takes two annotations, one
for keys and one for values:

@examples(
  ~eval: map_eval
  ~defn:
    fun locale(who, neighborhood :~ Map.of(String, Posn)):
      let p = neighborhood[who]
      p.x +& ", " +& p.y
  ~repl:
    locale("alice", neighborhood)
)

Note that @rhombus(buckets[0]) above statically resolves to the use of
map lookup, instead of going through a generic lookup at run time.

The @rhombus(MutableMap) constructor works similarly to the @rhombus(Map)
constructor, but it creates a mutable map. A mutable map can be updated
using @litchar{[}...@litchar{]} with @rhombus(:=) just like an array.

@examples(
  ~eval: map_eval
  ~defn:
    def locations = MutableMap{
      "alice": Posn(4, 5),
      "bob": Posn(7, 9),
    }
  ~repl:
    locations["alice"] := Posn(40, 50)
    locations["alice"]
)

In a map @braces pattern, a @rhombus(&) form binds
to map for the ``rest'' of the map, analogous to the way @rhombus(&)
binds with lists. In a map @braces expression,
@rhombus(&) splices in the content of another map, similar to the way
@rhombus(&) works for list construction.

@examples(
  ~eval: map_eval
  ~defn:
    def {"bob": bob_home, & others} = neighborhood
  ~repl:
    others
    {& others, "clara": Posn(8, 2)}
)

Map patterns can also bind repetitions, and map constructions can use
repetitions. These repetition constructions tend to go through
intermediate lists, and so they tend to be less efficient than using
@rhombus(&) to work with maps, but they are especially useful when the
intent is to convert between lists and maps.

Before @rhombus(...) in a map construction, supply one repetition for
keys before @rhombus(:), and supply another repetition for values. The
repetitions must have the same length.

@examples(
  ~defn:
    def [key, ...] = ["a", "b", "c"]
    def [val, ...] = [1, 2, 3]
  ~repl:
    {key: val, ...}
)

In a map pattern, @(colon)-separated key and value bindings should
appear before @rhombus(...). Unlike key expressions for individual keys,
the key part of a repetition binding is a binding. There is no
guarantee about the order of the keys and values, except that those two
repetitions use the same order (i.e., keys with associated values in
parallel).

@examples(
  ~defn:
    def {key: val, ...} = {"b": 2, "a": 1, "c": 3}
  ~repl:
    [key, ...]
    [val, ...]
)


@(close_eval(map_eval))
