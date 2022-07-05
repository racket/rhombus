#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Maps}

Immutable maps can be constructed using the syntax
@rhombus[{$$(@rhombus[key_expr, ~var]): $$(@rhombus[value_expr, ~var]), ...}],
which creates a map from the values of the @rhombus[key_expr, ~var]s to
the corresponding values of the @rhombus[value_expr, ~var]s. Note
that using @litchar{,} in place of @litchar{:} creates a set with
separate values, instead of a key--value mapping.

To access a mapping, use square brackets after a map expression with an
expression for the key within square brackets. Mutable maps can be
updated with a combination of square brackets and the @rhombus[:=]
operator.

@doc[
  fun Map(key :: Any, value:: Any, ...) :: Map
]{

 Constructs an immutable map containing given keys mapped to the given
 values, equivalent to using @rhombus[{key: value, ...}]. The number of
 arguments to @rhombus[Map] must be even with keys and values
 interleaved.

@examples[
  val m: Map("x", 1, "y", 2),
  m,
  m["x"]
]

}

@doc[
  bind.macro 'Map($key_expr, $binding, ...)'
]{

 Matches a map the keys computed by @rhombus[key_expr] to values that
 match the corresponding @rhombus[binding]s.

@examples[
  val Map("x", x, "y", y): {"x": 1, "y": 2},
  y
]

}

@doc[
  annotation.macro 'Map',
  annotation.macro 'Map.of($key_annotation, $value_annotation)',
]{

 Matches any map in the form without @rhombus[of]. The @rhombus[of]
 variant matches a map whose keys satisfy @rhombus[key_annotation]
 and whose values satisfy @rhombus[value_annotation].

}

@doc[
  folder.macro 'Map'
]{

 A @tech{folder} used with @rhombus[for], expects two results from a
 @rhombus[for] body, and accumulates them into a map using the first
 result as a key and the second result as a value.

}

@doc[
  fun make_map(key :: Any, value:: Any, ...) :: Map
]{

 Similar to @rhombus[Map] as a constructor, but creates a mutable map
 that can be updated using @rhombus[=].

@examples[
  val m: make_map("x", 1, "y", 2),
  m,
  m["x"],
  m["x"] := 0,
  m
]

}


@doc[
  operator ((v1 :: Map) ++ (v2 :: Map)) :: Map,
  operator ((v1 :: Set) ++ (v2 :: Set)) :: Set,
  operator ((v1 :: List) ++ (v2 :: List)) :: List,
  operator ((v1 :: String) ++ (v2 :: String)) :: String,
]{

 Appends @rhombus[v1] and @rhombus[v2] to create a new map, set, list, or
 string. In the case of maps, mappings for keys in @rhombus[v2] replace
 ones that exist already in @rhombus[v1]. In the case of sets, the new
 set has all of the elements of @rhombus[v1] and @rhombus[v2].
 In the case of lists and strings, the elements of @rhombus[v1] appear
 first in the result followed by the elements of @rhombus[v2].

 The combination
 @rhombus[$$(@rhombus(map_expr, ~var)) ++ {$$(@rhombus(key_expr, ~var)): $$(@rhombus(value_expr, ~var))}]
 is recognized by the compiler and turned into an efficient functional update of the
 map produced by @rhombus[map_expr], as opposed to creating an intermediate map.
 Set update is handled similarly.

@examples[
  val m: {"x": 1, "y": 2},
  m ++ {"x": 0},
  m,
  {1, 2, 3} ++ {"four", "five"},
  [1, 2, 3] ++ [4, 5],
  "hello" ++ " " ++ "world"
]

}
