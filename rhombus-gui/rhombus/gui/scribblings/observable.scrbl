#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Observables}

An @deftech{observable} holds a value plus a set of subscribers that are
known as @deftech{observers}. When the value in an observable changes,
observers are notified. Passing an observable to a view constructor
causes each rendering of the view to create an observer that reacts to
changes. For example, the label or enable state for a button view can be
supplied as an observable, and if the observable's value changes, then
every visible rendering of the button changes automatically to match.

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

 Returns the value via @rhombus(Obs.peek) (which you shouldn't normally
 do) or updates the value via @rhombus(Obs.update) (ignoring the current
 value).

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
 @rhombus(f(#,(@rhombus(v, ~var)))).  Returns the new value.

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
  method (obs :: gui.Obs).rename(name :: String) :: Obs
){

 Returns an observer like @rhombus(obs), but named as @rhombus(name).

}

@doc(
  method (obs :: gui.Obs).map(f :: Function.of_arity(1))
    :: Obs
  operator ((obs :: gui.Obs) ~> (f :: Function.of_arity(1)))
    :: Obs
){

 Returns an observer whose value changes each time that @rhombus(obs)'s
 value changes, where the new observer's value is changed to
 @rhombus(f(#,(@rhombus(v, ~var)))) when @rhombus(obs) is changed to
 @rhombus(v, ~var).

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

 Converts to and from a Racket object for the observable for use
 directly with @racketmodname(racket/gui/easy, ~indirect).

}
