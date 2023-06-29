#lang scribble/rhombus/manual
@(import: "common.rhm" open
          "nonterminal.rhm" open)

@(def dots: @rhombus(..., ~bind))
@(def dots_expr: @rhombus(...))

@title{Appendables}

An @deftech{appendable} value is one that supports @rhombus(++). Maps,
lists, arrays, sets, strings, and byte strings are all indexable, as are
instances of classes that implement @rhombus(Appendable, ~class).

@doc(
  operator ((v1 :: ReadableMap) ++ (v2 :: ReadableMap)) :: Map
  operator ((v1 :: ReadableSet) ++ (v2 :: ReadableSet)) :: Set
  operator ((v1 :: List) ++ (v2 :: List)) :: List
  operator ((v1 :: Array) ++ (v2 :: Array)) :: Array
  operator ((v1 :: ReadableString) ++ (v2 :: ReadableString)) :: String
  operator ((v1 :: Bytes) ++ (v2 :: Bytes)) :: Bytes
  operator ((v1 :: Appendable) ++ (v2 :: Appendable))
){

 Appends @rhombus(v1) and @rhombus(v2) to create a new map, set, list,
 array, string, or byte string, or calls the @rhombus(append, ~datum)
 method for an @rhombus(Appendable, ~class) instance.

 In the case of maps, mappings for keys in @rhombus(v2) replace ones
 that exist already in @rhombus(v1). In the case of sets, the new set has
 all of the elements of @rhombus(v1) and @rhombus(v2). In the case of
 lists, strings, and byte strings, the elements of @rhombus(v1) appear
 first in the result followed by the elements of @rhombus(v2).

 The combination
 @rhombus(#,(@rhombus(map_expr, ~var)) ++ {#,(@rhombus(key_expr, ~var)): #,(@rhombus(value_expr, ~var))})
 is recognized by the compiler and turned into an efficient functional update of the
 map produced by @rhombus(map_expr, ~var), as opposed to creating an intermediate map.
 Set update is handled similarly.

 When @rhombus(v1) is an instance of a class that implements
 @rhombus(Appendable, ~class), then @rhombus(v2) must be an instance of
 the same class or a subclass that inherits the same
 @rhombus(append, ~datum) method.

 The @rhombus(use_static) declaration constrains @rhombus(++) to work
 only when the left-hand argument static information indicating that it
 satisfies @rhombus(Appendable).

@examples(
  ~repl:
    {1, 2, 3} ++ {"four", "five"}
    [1, 2, 3] ++ [4, 5]
    "hello" ++ " " ++ "world"
  ~repl:
    def m: {"x": 1, "y": 2}
    m ++ {"x": 0}
    m
)

}



@doc(
  interface Appendable
){

@provided_interface_and_other_annotation_only()

 An interface that a class can implement (publicly or privately) to make
 instances of the class work with @rhombus(++). As an annotation,
 @rhombus(Appendable, ~annot) matches all @tech{appendable} objects, not
 just instances of classes that publicly implement the
 @rhombus(Appendable, ~class) interface.

 The interface has a single abstract method:

@itemlist(

 @item{@rhombus(#,(@rhombus(append, ~datum))(#,(@rhombus(other, ~var))))
  --- the @rhombus(other, ~var) value is the right-hand argument to
  @rhombus(++), and it is always an instance of the same class or a
  subclass that inherits the same @rhombus(append, ~datum)
  implementation.}

)

 There is no requirement on the result of the @rhombus(append, ~datum)
 method, but by convention, the result is the same class as the
 arguments.

@examples(
  ~defn:
    class Posn(x, y):
      private implements Appendable
      private override method append(other :: Posn):
        Posn(x + other.x, y + other.y)
  ~repl:
    Posn(1, 2) ++ Posn(200, 100)
)

}
