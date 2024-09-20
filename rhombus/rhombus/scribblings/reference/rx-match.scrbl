#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/rx open)

@(def rx_eval = make_rhombus_eval())
@examples(
  ~eval: rx_eval
  ~hidden:
    import rhombus/rx open
)

@title{Regexp Match Results}

@doc(
  class RXMatch(whole,
                captures :: List,
                capture_names :: Map)
){

 Represents a successful match result from methods like
 @rhombus(RX.match). The @rhombus(whole) field corresponds to the overall
 matched input (which can be a portion of the input for a @rhombus(rx_in)
 or @rhombus(RX.match_in) match, for example). The @rhombus(captures)
 list contains portions of the overall match that correspond to
 @tech{capture groups} in the pattern. The @rhombus(capture_names) field
 maps capture-group names to indices, which count from 1.

 The @rhombus(whole) value and elements of @rhombus(captures) are
 intended to be @tech{strings}, @tech{byte strings}, or @tech{ranges},
 depending on how the match is produced. For example, @rhombus(RX.match)
 with a regexp in character mode on a string input will produce a
 @rhombus(RXMatch, ~annot) object whose @rhombus(whole) is a string. The
 @rhombus(RX.match_range) method produces a @rhombus(RXMatch, ~annot)
 object whose @rhombus(whole) is a @tech{range}.

 A @rhombus(RXMatch, ~annot) object is @rhombus(Listable, ~annot) and
 @rhombus(Indexable, ~annot). Its @rhombus(Listable.to_list) conversion
 is same as @rhombus([whole] ++ captures). Indexing by an integer
 accesses the same result as indexing the object's list, but
 capture-group names (which are the keys of @rhombus(capture_names)) can
 also be used as indices.

@examples(
  ~eval: rx_eval
  ~repl:
    rx'any "x" any'.match("axz")
    rx'any "x" any'.match("axz").whole
  ~repl:
    rx'any "x" any'.match(#"axz")
    rx'byte "x" byte'.match("axz")
    rx'any "x" any'.match_range("axz")
  ~repl:
    def m = rx'($pre: any) "x" ($post: any)'.match("axz")
    m
    m[0]
    m[1]
    m[#'pre]
)

}


@doc(
  method (rx :: RXMatch).to_list() :: List
){

 Implements @rhombus(Listable.to_list), returning
 @rhombus([RXMatch.whole(rx)] ++ RXMatch.captures(rx)).

}


@doc(
  method (rx :: RXMatch).get(index)
){

 Implements @rhombus(get, ~datum) operation of @rhombus(Indexable). The
 @rhombus(index) argument normally satisfies @rhombus(NonnegInt, ~annot)
 or @rhombus(Symbol, ~annot), but in general it can be a
 @rhombus(NonnegInt, ~annot) or any value that exists as a key in
 @rhombus(RX.capture_names(rx)).

}

@close_eval(rx_eval)
