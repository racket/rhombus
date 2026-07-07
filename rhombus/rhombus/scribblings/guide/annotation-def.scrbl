#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "../macro.rhm")

@(def bind_eval = macro.make_macro_eval())

@title(~tag: "annotation-def"){Defining Annotations}

The @rhombus(annot, ~defn) form defines a new annotation in terms of
existing annotations.

@examples(
  ~defn:
    annot EvenInt = Int && satisfying(fun (x): x mod 2 == 0)
  ~repl:
    10 :: EvenInt
    ~error:
      9 :: EvenInt
)

The @rhombus(annot.macro, ~defn) form from @rhombusmodname(rhombus/meta)
also defines an annotation, but as a macro, in which case it can have
argument forms or other syntactic variations. (See @secref("bind-macro")
for more information.) The @rhombus(annot, ~defn) form can only bind an
identifier as a shorthand for another annotation, but that other
annotation is parsed eagerly and centralizes run-time support for the
annotation. Also, @rhombus(annot, ~defn) is provided by
@rhombuslangname(rhombus), so @rhombusmodname(rhombus/meta) does not
need to be imported to use it. Prefer @rhombus(annot, ~defn) to
@rhombus(annot.macro, ~defn).
