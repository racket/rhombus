#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Sequences}

A @deftech{sequence} supplies elements to a @rhombus(for) iteration.
@tech{Lists}, @tech{mutable lists}, @tech{pair lists}, @tech{maps},
@tech{sets}, @tech{arrays}, @tech{strings}, @tech{byte strings}, and
sequenceable @tech{ranges} are sequences, and new kinds of sequences
can be defined by calling @rhombus(Sequence.make),
@rhombus(Sequence.instantiable), or implementing
@rhombus(Sequenceable, ~class).

A sequence is more general than a list or @tech{stream} in that it can have
internal state, and the state can even be specific to a particular
@deftech{instantiation} of the sequence for a new iteration.

@doc(
  annot.macro 'Sequence'
  annot.macro 'Sequence.assume_of($annot, ...)'
){

 Matches any @tech{sequence}.

 Static information associated by @rhombus(Sequence, ~annot) makes an
 expression acceptable as a sequence to @rhombus(for) in static mode, and
 it is suitable when a more specialized annotation (such as
 @rhombus(List) or @rhombus(Array)) is not available.

 A @rhombus(Sequence.assume_of(annot, ...), ~annot) annotation is the same
 as @rhombus(Sequence, ~annot), but elements drawn from the sequence via
 @rhombus(for) have the static information of @rhombus(annot)s (where
 multiple @rhombus(annot)s correspond to multiple values for each element,
 such as the key and value from a @tech{map}). The extracted elements are
 not checked, however; each @rhombus(annot) is used only
 for its static information, and each @rhombus(annot) must be a
 @tech(~doc: guide_doc){predicate annotation}.

}

@doc(
  fun Sequence.generate(seq :: Sequence)
    :: values(() -> Boolean,
              () -> ~any)
){

 To support cases where @rhombus(for) is not convenient for extracting
 elements from a sequence, @rhombus(Sequence.generate)
 @tech(~key: "instantiation"){instantiates} a sequence and returns two
 stateful functions:

@itemlist(

 @item{@rhombus(#,(@rhombus(has_more, ~var)) :: () -> Boolean): Reports
  whether @rhombus(next, ~var) has an element to return.}

 @item{@rhombus(#,(@rhombus(next, ~var)) :: () -> ~any): Returns the
  next element of the sequence. Note that an element may consist of
  multiple values, such as for a sequence produced by
  @rhombus(Map.to_sequence).}

)

 The @rhombus(has_more, ~var) function does not have to be called before
 each use of @rhombus(next, ~var), but @rhombus(next, ~var) throws an
 exception at a point where @rhombus(has_more, ~var) would return
 @rhombus(#false).

@examples(
  def (has_more, next) = Sequence.generate((0 .. 3).to_sequence())
  has_more()
  next()
  has_more()
  next()
  next()
  has_more()
  ~error:
    next()
)

}

@doc(
  fun Sequence.to_stream(seq :: Sequence)
    :: Stream.assume_of(Any.like_element(seq))
){

 Converts a @tech{sequence} to a @tech{stream} by lazily demanding and
 caching elements of the sequence.

@examples(
  def stm = Sequence.to_stream(0 ..)
  stm.rest.rest.rest.first
  stm.rest.rest.rest.first
)

}


@doc(
  fun Sequence.make(
    ~initial_position:
      init_pos :: Any,
    ~continue_at_position:
      continue_at_pos :: maybe(Function.of_arity(1)) = #false,
    ~position_to_element:
      pos_to_element :: Function.of_arity(1),
    ~continue_at_value:
      continue_at_val :: maybe(Function) = #false,
    ~early_position_to_next:
      early_next_pos :: maybe(Function.of_arity(1)) = #false,
    ~continue_after_position_and_value:
      continue_at_pos_val :: maybe(Function) = #false,
    ~position_to_next:
      next_pos :: Function.of_arity(1)
  ) :: Sequence
){

 Creates a @tech{sequence} by supplying the index value and stepper,
 element-to-index function, and continue conditions:

@itemlist(

 @item{@rhombus(init_pos): A value that represents the initial sequence
  position. Any kind of value is allowed, because it is used only by other
  supplied functions, such as @rhombus(pos_to_element) and
  @rhombus(next_pos).}

 @item{@rhombus(continue_at_pos): An optional function that takes the
  current position and reports whether iteration should continue with that
  position. A @rhombus(#false) for @rhombus(continue_at_pos) is equivalent
  to a function that always returns @rhombus(#true). This function is
  applied before an attempt to map the position to an element using
  @rhombus(pos_to_element).}

 @item{@rhombus(pos_to_element): A function that takes the current
  position and returns a value (or multiple values) for the element at
  that position. This function will be called once per element in an
  iteration, and only if iteration has not been stopped by a
  @rhombus(#false) return from one of the continue functions.}

 @item{@rhombus(continue_at_val): An optional function that takes the
  current element (which may be multiple values supplied as multiple
  arguments) and reports whether iteration should continue with that
  element. A @rhombus(#false) for @rhombus(continue_at_value) is equivalent
  to a function that always returns @rhombus(#true). This function is
  applied before exposing an element to an iteration, and it is also
  called before @rhombus(early_next_pos).}

 @item{@rhombus(early_next_pos): An optional function that takes the
  current position and returns an updated position. A @rhombus(#false) for
  @rhombus(early_next_pos) is equivalent to a function that returns its
  argument. This function is called before the current element is exposed
  to an iteration, and it is called before @rhombus(continue_at_value). It
  is intended for cases where retaining the position after extracting an
  element would incorrectly retain the element via the position.
  Typically, if @rhombus(early_next_pos) advances a position, then
  @rhombus(next_pos) will return its argument, and
  @rhombus(continue_at_pos_val) needs to be @rhombus(#false) (otherwise
  the value must be retained for that predicate).}

 @item{@rhombus(continue_at_pos_val): An optional function that takes
  both the current position and elements (which may be multiple values
  supplied as multiple arguments) and reports whether iteration should
  continue with that element. A @rhombus(#false) for
  @rhombus(continue_at_pos_val) is equivalent to a function that always
  returns @rhombus(#true). This function is applied after the element is
  exposed to iteration. If @rhombus(early_next_pos) is supplied, its
  return is the first argument to @rhombus(continue_at_pos_val).}

 @item{@rhombus(next_pos): A function that takes the current position
  and returns the next position.}

)

 The arguments to @rhombus(Sequence.make) are required to be a mixture
 of @rhombus(#false) and function values, beware that the arguments are
 not checked. Errors due to invalid or inconsistent values may be
 detected later.

 This same initial position and functions are used for every
 @tech{instantiation} of the result sequence. To distinguish different
 instantiations, use @rhombus(Sequence.instantiate).

@examples(
  ~defn:
    fun even_strings_up_to(n :: Int):
      Sequence.make(
        ~initial_position: 0,
        ~continue_at_position: fun (i): i < n,
        ~position_to_element: to_string,
        ~position_to_next: fun (i): i + 2
      )
  ~repl:
    for List (i in even_strings_up_to(5)): i
)

}

@doc(
  fun Sequence.instantiable(thunk :: Function.of_arity(0))
    :: Sequence
){

 A delaying form of @rhombus(Sequence.make), where @rhombus(thunk) is
 called for every @tech{instantiation} of the sequence. The given
 @rhombus(thunk) should return the results of
 @rhombus(Sequence.instantiate).

@examples(
  ~defn:
    fun even_strings_up_to(n :: Int):
      Sequence.instantiable(
        fun ():
          let mutable i = 0
          Sequence.instantiate(
            ~initial_position: #void,
            ~continue_at_position: fun (_): i < n,
            ~position_to_element: fun (_): to_string(i),
            ~position_to_next: fun (_): i := i + 2
          ))
  ~repl:
    for List (i in even_strings_up_to(5)): i
)

}

@doc(
  fun Sequence.instantiate(
    ~initial_position:
      init_pos :: Any,
    ~continue_at_position:
      continue_at_pos :: maybe(Function.of_arity(1)) = #false,
    ~position_to_element:
      pos_to_element :: Function.of_arity(1),
    ~continue_at_value:
      continue_at_val :: maybe(Function) = #false,
    ~early_position_to_next:
      early_next_pos :: maybe(Function.of_arity(1)) = #false,
    ~continue_after_position_and_value:
      continue_at_pos_val :: maybe(Function) = #false,
    ~position_to_next:
      next_pos :: Function.of_arity(1)
  ) :: (Any, Any, Any, Any, Any, Any, Any)
){

 Takes arguments of the same form as @rhombus(Sequence.make), but simply
 returns them as multiple values in an unspecified order. This function is
 meant to be called from a function passed to
 @rhombus(Sequence.instantiable).

}


@doc(
  interface Sequenceable
){

@provided_interface_only()

 An interface that a class can implement (publicly or privately) to make
 instances of the class work with as a sequence for @rhombus(for)---in
 dynamic mode, but see also @rhombus(sequence, ~class_clause) for
 statically optimizing @rhombus(for) expansions.@margin_note{When a
 class both implements @rhombus(Sequenceable, ~class) and includes a
 @rhombus(sequence, ~class_clause) clause, an instance of the class
 by itself in an @rhombus(each, ~for_clause) clause of @rhombus(for)
 will be optimized, while an explicit invocation of the
 @rhombus(to_sequence, ~datum) method will not go through the
 optimization. This also applies to the primitive sequenceable
 objects.} The interface has a single abstract method:

@itemlist(

 @item{@rhombus(#,(@rhombus(to_sequence, ~datum))())
  --- returns a sequence value, possibly constructed with
  @rhombus(Sequence.make) or @rhombus(Sequence.instantiable).}

)

@examples(
  ~defn:
    class Posn(x, y):
      private implements Sequenceable
      private override method to_sequence():
        [x, y]
  ~repl:
    for List (i in Posn(10, 20)): i
)

}
