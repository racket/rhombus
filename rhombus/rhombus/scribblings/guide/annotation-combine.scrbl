#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "../macro.rhm")

@(def ann_eval = macro.make_macro_eval())

@title(~tag: "annotation-combine"){Combining Annotations}

The @rhombus(||, ~annot) annotation operator combines two annotations to
produce a new one that is satisfied by values that satisfy @emph{either} of the
two annotations.

@examples(
  ~eval: ann_eval
  ~defn:
    fun label(x :: Number || String):
      "figure " +& x
  ~repl:
    label(1)
    label("a")
    ~error:
      label(#false)
)

A binding that uses an @rhombus(||, ~annot) annotation has only
@tech{static information} that is common to the two annotations. If is
possible, for example, that two annotations both imply a third one.

@examples(
  ~eval: ann_eval
  ~defn:
    class Posn(x, y):
      nonfinal
    class Posn3D(z):
      extends Posn
    class ColorPosn(hue, alpha):
      extends Posn
    fun f(p :: Posn3D || ColorPosn):
      use_static
      p.x // both `Posn3D` and `ColorPosn` imply `Posn`
  ~repl:
    f(Posn3D(1, 2, 3))
    f(ColorPosn(10, 20, "red", 1.0))
)


The @rhombus(&&, ~annot) annotation operator combines two annotations to
produce a new one that is satisfied by values that satisfy @emph{both} of the
two annotations.

@examples(
  ~eval: ann_eval
  ~defn:
    fun label(x :: Indexable && Appendable):
      use_static
      if x[0]
      | x ++ x
      | x
  ~repl:
    label("apple")
    label([#false, 2, 3])
)

As illustrated in this example, a binding that uses an
@rhombus(&&, ~annot) annotation has @tech{static information} combined
from the two annotations. If the two annotations provide differing
static information, the left-hand annotation's information is used. (If
the static information from the two annotations is inherently in
conflict, then presumably no value can satify both annotations at once.)
