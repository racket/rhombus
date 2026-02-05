#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Observables}

An @deftech{observable} holds a value plus a set of subscribers that are
known as @deftech{observers}. When the value in an observable changes,
observers are notified.

Besides registering observers directly, an application can provide an
observable instead of an immediate value when constructing a
@tech{view}. Each rendering of the view then becomes and observer that
reacts to changes in the observable's value. For example, the label or
enable state for a button view can be supplied as an observable, and if
the observable's value changes from @rhombus(#true) to @rhombus(#false),
then a button (or, more precisely, a button in a GUI @tech{render}ed
from the view) becomes disabled.

@doc(
  class gui.Obs():
    constructor (v :: Any,
                 ~name: name :: String = "anon",
                 ~is_derived: is_derived :: Any = #false)

  annot.macro 'gui.Obs.of($annot)'
  annot.macro 'gui.ObsOrValue.of($annot)'
  annot.macro 'gui.Obs.later_of($annot)'
  annot.macro 'gui.Obs.now_of($annot)'
  annot.macro 'gui.ObsOrValue.later_of($annot)'
  annot.macro 'gui.ObsOrValue.now_of($annot)'
){

 The @rhombus(Obs) constructor creates an @tech{observable} whose initial
 value is @rhombus(v).

 The annotation @rhombus(#,(@rhombus(Obs.of, ~annot))(annot)) is
 satisfied by an annotation whose value satisfies @rhombus(annot). The
 predicate and conversion (if any) associated with @rhombus(annot) is not
 applied immediately, but it is applied every time a value is extracted
 or put into the observable. The annotation
 @rhombus(#,(@rhombus(ObsOrValue.of, ~annot))(annot)) is satisfied by a
 value that satisfies either @rhombus(annot) or
 @rhombus(#,(@rhombus(Obs.of, ~annot))(annot)).

 The @rhombus(#,(@rhombus(Obs.now_of, ~annot))(annot)) annotation is
 @rhombus(#,(@rhombus(Obs.of, ~annot))(annot)), but the current value in
 the observable is checked immediately, and it is not checked later or
 when a new value is supplied to the observable. In other words,
 @rhombus(#,(@rhombus(Obs.now_of, ~annot))(annot)) is a predicate
 annotation, while @rhombus(#,(@rhombus(Obs.of, ~annot))(annot)) is a
 converter annotation. The @rhombus(annot) argument in
 @rhombus(#,(@rhombus(Obs.now_of, ~annot))(annot)) must be a predicate
 annotation. The @rhombus(#,(@rhombus(ObsOrValue.now_of, ~annot))(annot))
 annotation is analogously like
 @rhombus(#,(@rhombus(ObsOrValue.of, ~annot))(annot)).

 The @rhombus(Obs.later_of(annot), ~annot) and
 @rhombus(ObsOrValue.later_of(annot), ~annot) annotations are aliases for
 @rhombus(Obs.of(annot), ~annot) and
 @rhombus(ObsOrValue.of(annot), ~annot), respectively.

}

@doc(
  property
  | (obs :: gui.Obs).value :: Any
  | (obs :: gui.Obs).value := (v :: Any)
){

 Returns the value via @rhombus(Obs.peek) or updates the value via
 @rhombus(Obs.update) (ignoring the current value).

@examples(
  ~hidden:
    // fake observable, because we can't instantiate `rhombus/gui` here
    class Obs(mutable v):
      property
      | value: v
      | value := x: v := x
  ~defn:
    def o = Obs("apple")
  ~repl:
    o.value
    o.value := "banana"
    o.value
)

}


@doc(
  method (obs :: gui.Obs).observe(f :: Function.of_arity(1))
    :: Void
){

 Adds @rhombus(f) as a function to be called when the value of
 @rhombus(obs) changes.

}

@doc(
  method (obs :: gui.Obs).unobserve(f :: Function.of_arity(1))
    :: Void
){

 Removes @rhombus(f) as a function to be called when the value of
 @rhombus(obs) changes.

}

@doc(
  method (obs :: gui.Obs).update(f :: Function.of_arity(1))
    :: Any
  operator ((obs :: gui.Obs) <~ (f :: Function.of_arity(1)))
    :: Any
){

 Changes the value @rhombus(v, ~var) of @rhombus(obs) to
 @rhombus(f(#,(@rhombus(v, ~var)))) and returns the new value.

}

@doc(
  method (obs :: gui.Obs).peek() :: Any
){

 Returns the current value of @rhombus(obs).

 Normally, instead of peeking an observable's value, you should either
 register an observer or pass an observer to a constructor that expects
 one. For example, when an observer's value should affect drawing in a
 @rhombus(Canvas, ~class), then the first argument to
 @rhombus(Canvas, ~class) should the observer (or one derived from it),
 and then a rendered canvas will be updated when the observable changes.

}

@doc(
  property (obs :: gui.Obs).name :: String
  defn.macro 'gui.Obs.def $id_name = $expr'
  defn.macro 'gui.Obs.def $id_name: $body; ...'
  defn.macro 'gui.Obs.let $id_name = $expr'
  defn.macro 'gui.Obs.let $id_name: $body; ...'
  method (obs :: gui.Obs).rename(name :: String) :: Obs
){

 The @rhombus(Obs.name) property returns an observer's name as assigned
 by passing a @rhombus(~name) argument to the @rhombus(Obs) constructor
 or by using @rhombus(Obs.def) or @rhombus(Obs.let).

 The @rhombus(Obs.def) and @rhombus(Obs.let) forms are like
 @rhombus(def) and @rhombus(let), but the result of the right-hand
 @rhombus(expr) or @rhombus(body) sequence is wrapped with a call to the
 @rhombus(Obs) constructor and using the string form of the defined
 @rhombus(id_name) as the @rhombus(~name) argument.

 The @rhombus(Obs.rename) method returns an observer that behaves the
 same as @rhombus(obs), but named as @rhombus(name). In particular,
 changng the value of the returned observer is the same as changing the
 value of @rhombus(obs).

}

@doc(
  method (obs :: gui.Obs).map(f :: Function.of_arity(1))
    :: Obs
  operator ((obs :: gui.Obs) ~> (f :: Function.of_arity(1)))
    :: Obs
){

 Returns a @deftech{derived observable} whose value changes each time
 that @rhombus(obs)'s value changes, where the new observer's value is
 changed to @rhombus(f(#,(@rhombus(v, ~var)))) when @rhombus(obs) is
 changed to @rhombus(v, ~var).

}

@doc(
  method (obs :: gui.Obs).debounce(
    ~duration: msec :: Nat = 200
  ) :: Obs
){

 Returns a new observable whose value changes to the value of
 @rhombus(obs) when there is at least a @rhombus(msec) millisecond pause
 in changes to @rhombus(obs).

}

@doc(
  method (obs :: gui.Obs).throttle(
    ~duration: msec :: Nat = 200
  ) :: Obs
){

 Returns a new observable whose value changes to the value of
 @rhombus(obs) at most once per @rhombus(msec) milliseconds.

}

@doc(
  fun gui.Obs.combine(f :: Function, obs :: Obs, ...)
    :: Obs
  fun gui.Obs.combine({key: obs :: Obs, ...})
    :: Obs
){

 Returns a new observable whose value changes to the value of
 @rhombus(f(obs.value, ...)) or
 @rhombus({key: obs.value, ...}) when the value
 of any @rhombus(obs) changes.

}

@doc(
  property (obs :: gui.Obs).handle :: Any
  fun gui.Obs.from_handle(handle) :: Obs
){

 Converts to and from a Racket object for the observable, for use
 directly with @racketmodname(racket/gui/easy, ~indirect).

}
