#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "bind-macro"){Binding and Annotation Macros}

Macros can extend binding-position syntax, too, via @rhombus(bind.rule) and
@rhombus(bind.macro). In the simplest case, a binding operator is implemented
by expanding to other binding operators, like this definition of @rhombus($$$)
as a prefix operator to constrain a pattern to number inputs:

@(demo:
    ~defn:
      import:
        rhombus/meta open

      bind.rule '$$$ $n':
        ~parsed_right
        '$n :: Number'
    ~repl:
      def $$$salary = 100.0

      salary
  )

More expressive binding operators can use a lower-level protocol where a
binding is represented by transformers that generate checking and
binding code. It gets complicated, and it’s tied up with the propagation
of static information, so the details are in @secref("bind-macro-protocol").
After an expressive set of binding forms are implemented with the
low-level interface, however, many others can be implemented though
simple expansion.

The @rhombus(annot.macro) form is similar to @rhombus(bind.macro), but for
annotations. 

@(rhombusblock:
    use_static

    annot.rule 'PosnList': 'List.of(Posn)'

    fun nth_x(ps -: PosnList, n):
      ps[n].x
  )

For details on the low-level annotation protocol, see @secref("annotation-macro").

