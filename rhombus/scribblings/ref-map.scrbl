#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Maps}

Immutable maps can be constructed using the syntax
@rhombus({$$(@rhombus(key_expr, ~var)): $$(@rhombus(value_expr, ~var)), ...}),
which creates a map from the values of the @rhombus(key_expr, ~var)s to
the corresponding values of the @rhombus(value_expr, ~var)s. Note
that using @litchar{,} in place of @litchar{:} creates a set with
separate values, instead of a key--value mapping. More precisely, a
use of curly braces with no preceding expression is parsed as an
implicit use of the @rhombus(#{#%braces}) form.

To access a mapping, use square brackets after a map expression with an
expression for the key within square brackets. Mutable maps can be
updated with a combination of square brackets and the @rhombus(:=)
operator. These uses of square brackets are implemented by
@rhombus(#{#%ref}).

@dispatch_table(
  "map",
  @rhombus(Map),
  [map.length(), Map.length(map)],
  [map.keys(), Map.keys(map)],
  [map.values(), Map.values(map)],
  [map.has_key(k), Map.has_key(map, k)]
)

@doc(
  expr.macro '#{#%braces} {$key_val_or_splice, ...}',
  grammar key_val_or_splice:
    $key_expr: $val_expr
    & $map_expr,
  expr.macro '#{#%braces} {$expr_or_splice, ...+}',
  grammar expr_or_splice:
    $elem_expr
    & $set_expr,
  repet.macro '#{#%braces} {$key_val_or_splice_repet, ...}',
  repet.macro '#{#%braces} {$expr_or_splice_repet, ...+}',
){

 Constructs either a map or a set, depending on whether
 @rhombus(key_expr) and @rhombus(val_expr) are provided or
 @rhombus(elem_expr) is provided. If no elements are provided directly,
 the result is a map (not a set). Map/set constructions can also serve as
 repetitions, where @rhombus(key_val_or_splice_repet) and
 @rhombus(expr_or_splice_repet) are like
 @rhombus(key_val_or_splice) and @rhombus(expr_or_splice),
 but with repetitions in place of expressions.

 When @rhombus(& map_expr) or @rhombus(& set_expr) appears among the
 content, the map or set produced by @rhombus(map_expr)
 @rhombus(set_expr) is included in the result map or set.

 Mappings or elements are added to the result map or set left-to-right,
 which means that a later @rhombus(key_expr) or @rhombus(elem_expr) may
 replace one earlier in the sequence. This ordering applies to mappings
 or elements spliced via @rhombus(&), too.
 
 @see_implicit(@rhombus(#{#%braces}), @rhombus({}), "expression")

@examples(
  {1, 2, 3},
  {"a": 1, "b": 2},
  #{#%braces} {1, 2, 3},
  #{#%braces} {"a": 1, "b": 2},
  {1, 2, & {3, 4}},
  {"a": 1, "b": 2, & {"c": 3, "d": 4}}
)

}

@doc(
  expr.macro '$expr #{#%ref} [$at_expr]',
  expr.macro '$expr #{#%ref} [$at_expr] := $rhs_expr',
){

 Without @rhombus(:=), accesses the element of the map, array, list, or
 string produced by @rhombus(expr) at the index or key produced by
 @rhombus(at_expr).

 With @rhombus(:=), a mutable array, map, or set element is assigned to
 the value produced by @rhombus(rhs_expr), and the expression result is
 @rhombus(#void).

 See also @rhombus(use_static).

 @see_implicit(@rhombus(#{#%ref}), @rhombus([]), "expression", ~is_infix: #true)

@examples(
  {"a": 1, "b": 2}["a"],
  {"a": 1, "b": 2} #{#%ref} ["a"]
)

}

@doc(
  expr.macro 'Map{$key_val_or_splice, ...}',
  grammar key_val_or_splice:
    $key_expr: $val_expr
    @rhombus(&) $map_expr,
  fun Map([key :: Any, value :: Any], ...) :: Map
){

 Constructs an immutable map containing given keys mapped to the given
 values, equivalent to using @rhombus({key_val_or_splice, ...}) for the
 @rhombus({}) form, or @rhombus({key: value, ...}) for the function form.

@examples(
  def m: Map{"x": 1, "y": 2},
  m,
  m["x"],
  Map(["x", 1], ["y", 2]),
  Map{"a": 4, "b": 4, & m}
)

}

@doc(
  bind.macro 'Map{$key_expr: $value_binding, ...}',
  bind.macro 'Map{$key_expr: $value_binding, ..., & $map_binding}',
  bind.macro 'Map([$key_expr, $value_binding], ...)',
  bind.macro '#{#%braces} {$key_expr: $value_binding, ...}',
  bind.macro '#{#%braces} {$key_expr: $value_binding, ..., & $map_binding}',
){

 Matches a map of the keys computed by @rhombus(key_expr) to values
 that match the corresponding @rhombus(value_binding)s.
 The matched map may have additional keys and values.
 If @rhombus(& map_binding) is supplied, the rest of the map excluding
 the given @rhombus(key_expr)s must match the @rhombus(map_binding).

 @see_implicit(@rhombus(#{#%braces}, ~bind), @rhombus({}), "binding") 

@examples(
  def Map{"x": x, "y": y}: {"x": 1, "y": 2},
  y,
  def Map{"a": a}: {"a": 1, "b": 2, "c": 3},
  a,
  def Map{"a": _, & rst}: {"a": 1, "b": 2, "c": 3},
  rst
)

}

@doc(
  annot.macro 'Map',
  annot.macro 'Map.of($key_annotation, $value_annotation)',
){

 Matches any map in the form without @rhombus(of). The @rhombus(of)
 variant matches a map whose keys satisfy @rhombus(key_annotation)
 and whose values satisfy @rhombus(value_annotation).

}

@doc(
  reducer.macro 'Map'
){

 A @tech{reducer} used with @rhombus(for), expects two results from a
 @rhombus(for) body, and accumulates them into a map using the first
 result as a key and the second result as a value.

}

@doc(
  expr.macro 'MutableMap{key: value, ...}',
  fun MutableMap(key :: Any, value:: Any, ...) :: Map
){

 Similar to @rhombus(Map) as a constructor, but creates a mutable map
 that can be updated using @rhombus(=).

 Note that @rhombus(&) rest is not supported on mutable maps, only
 immutable maps.

@examples(
  def m: MutableMap{"x": 1, "y": 2},
  m,
  m["x"],
  m["x"] := 0,
  m
)

}


@doc(
  operator ((v1 :: Map) ++ (v2 :: Map)) :: Map,
  operator ((v1 :: Set) ++ (v2 :: Set)) :: Set,
  operator ((v1 :: List) ++ (v2 :: List)) :: List,
  operator ((v1 :: String) ++ (v2 :: String)) :: String,
){

 Appends @rhombus(v1) and @rhombus(v2) to create a new map, set, list, or
 string. In the case of maps, mappings for keys in @rhombus(v2) replace
 ones that exist already in @rhombus(v1). In the case of sets, the new
 set has all of the elements of @rhombus(v1) and @rhombus(v2).
 In the case of lists and strings, the elements of @rhombus(v1) appear
 first in the result followed by the elements of @rhombus(v2).

 The combination
 @rhombus($$(@rhombus(map_expr, ~var)) ++ {$$(@rhombus(key_expr, ~var)): $$(@rhombus(value_expr, ~var))})
 is recognized by the compiler and turned into an efficient functional update of the
 map produced by @rhombus(map_expr), as opposed to creating an intermediate map.
 Set update is handled similarly.

@examples(
  def m: {"x": 1, "y": 2},
  m ++ {"x": 0},
  m,
  {1, 2, 3} ++ {"four", "five"},
  [1, 2, 3] ++ [4, 5],
  "hello" ++ " " ++ "world"
)

}

@doc(
  bind.macro 'Map.empty',
  expr.macro 'Map.empty'
){

 An empty map. The @rhombus(Map.empty, ~bind) binding form differs
 from from @rhombus({}) or @rhombus(Map()), because @rhombus(Map.empty, ~bind)
 matches only an empty map (possibly mutable), while @rhombus({}) or @rhombus(Map())
 matches any map.

@examples(
  Map.empty,
  match {}
  | Map.empty: "empty map"
  | _: #false,
  match {"x": 1, "y": 2}
  | Map.empty: "empty map"
  | _: #false,
  match {"x": 1, "y": 2}
  | {}: "curly braces allow extra"
  | _: #false,
  match {"x": 1, "y": 2}
  | Map(): "Map binding allows extra"
  | _: #false,
)

}

@doc(
  fun Map.length(map :: Map) :: Integer,
){

 Returns the number of key--value mappings in @rhombus(map).

@examples(
  Map.length({"a": 1, "b": 2}),
  Map.length({}),
  {"a": 1, "b": 2}.length()
)

}


@doc(
  fun Map.keys(map :: Map) :: List,
){

 Returns the keys of @rhombus(map) in a list.

@examples(
  Map.keys({"a": 1, "b": 2})
)

}


@doc(
  fun Map.values(map :: Map) :: List,
){

 Returns the values of @rhombus(map) in a list.

@examples(
  Map.values({"a": 1, "b": 2})
)

}


@doc(
  fun Map.has_key(map :: Map, key) :: Boolean,
){

 Returns @rhombus(#true) if @rhombus(key) is mapped to a value in
 @rhombus(map), @rhombus(#false) otherwise.

@examples(
  Map.has_key({"a": 1, "b": 2}, "a"),
  Map.has_key({"a": 1, "b": 2}, "c")
)

}
