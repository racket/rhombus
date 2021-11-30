#lang scribble/rhombus/manual
@(import:
    "util.rhm": no_prefix
    "common.rhm": no_prefix)

@title[~tag: "map"]{Arrays and Maps}

The @rhombus[Array] constructor is similar to @rhombus[List], but it
creates an array, which has a fixed length at the time that it’s created
and offers constant-time access to any element of the array. Like a
list, and array is a map. Unlike a list, an array is mutable, so
@litchar{[}...@litchar{]} for indexing can be combined with @rhombus[:=]
for assignment.

@(rhombusblock:
    val buckets: Array(1, 2, 3, 4)

    buckets[0]       // prints 1
    buckets[1] := 5  // prints 5
    buckets          // prints Array1, 5, 3, 4)
  )

@rhombus[Array] is also an annotation and a binding contructor,
analogous to @rhombus[List], and @rhombus[Array.of] is an annotation
constructor. The @rhombus[Array] binding and expression constructors do
not support @rhombus[..., ~bind].

The @rhombus[Map] constructor creates an immutable mapping of arbitrary
keys to values. The term @deftech{map} is meant to be generic, and
@rhombus[Map] as a constructor just uses a default implementation of
maps. The @rhombus[Map] constructor can be used like a function, in
which case it accepts keys alternating with values:

@(rhombusblock:«
    val neighborhood: Map("alice", Posn(4, 5),
                          "bob", Posn(7, 9))

    neighborhood["alice"]     // prints Posn(4, 5)
    // neighborhood["clara"]  // would be a run-time error
  »)

Curly braces @litchar["{"]...@litchar["}"] can be used as a shorthand
for writing @rhombus[Map(...)]. Within curly braces, the key and value
are joined by @rhombus[:]. (If a key expression needs to use @rhombus[:]
itself, the expression will have to be in parentheses.)

@(rhombusblock:
    val neighborhood: {"alice": Posn(4, 5),
                       "bob": Posn(7, 9)}
    neighborhood["alice"]       // prints Posn(4, 5)
  )

To functionally extend a map, use the @rhombus[++] append operator:

@(rhombusblock:
    val new_neighborhood: neighborhood ++ {"alice": Posn(40, 50)}
    new_neighborhood["alice"] // prints Posn(40, 50)
    neighborhood["alice"]     // prints Posn(4, 5)
  )

When @rhombus[++] is used with a left-hand side that is statically known
to be the default implementation of maps, and when the right-hand
argument is an immediate map construction with a single element, then
the use of @rhombus[++] is compiled as an efficient single-key update of
the map. Whether optimized or general, the @rhombus[++] operator will
only combine certain compatible kinds of maps. For example, @rhombus[++]
will append lists and combine default-implementation maps, but it will
not combine two vectors or combine a list and a default-implementation
map with keys and values.

@rhombus[Map] or its curly-braces shorthand is also an annotation and a
binding constructor. As an annotation or binding constructor,
@rhombus[Map] refers to map values genercially, and not to a specific
implementation. For example, a list can be passed to a function that
expects a @rhombus[Map] argument.

In a binding use of @rhombus[Map], the key positions are @emph{expressions},
not @emph{bindings}. The binding matches an input that includes the keys, and
each corresponding value is matched to the value binding pattern.

@(rhombusblock:
    fun alice_home({"alice": p}): p

    alice_home(neighborhood)  // prints Posn(4, 5)
  )

The @rhombus[Map.of] annotation constructor takes two annotations, one
for keys and one for values:

@(rhombusblock:
    fun locale(who, neighborhood -: Map.of(String, Posn)):
      val p: neighborhood[who]
      p.x & ", " & p.y

    locale("alice", neighborhood)  // prints "4, 5"
  )

Unlike @rhombus[.], indexed access via @litchar{[}...@litchar{]} works
even without static information to say that the access will succeed.
Still, static information can select a more specific and potentially
fast indexing operator. For example, @rhombus[buckets[0]] above
statically resolves to the use of array lookup, instead of going through
a generic function for maps at run time.

The @rhombus[make_map] function works similarly to the @rhombus[Map]
constructor, but it creates a mutable map. A mutable map can be updated
using @litchar{[}...@litchar{]} with @rhombus[:=] just like an array.

@(rhombusblock:
    val locations: make_map("alice", Posn(4, 5),
                            "bob", Posn(7, 9))
    locations["alice"] := Posn(40, 50)
    locations["alice"]  // prints Posn(40, 50)
  )

