#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(val method_eval: make_rhombus_eval())

@title(~tag: "private-implement"){Private Implementation}

An @rhombus(internal, ~class_clause) name for a class can provide access
to all internal methods of the class. For more selective control over
sets of private methods, an interface can be privately implemented.
Private implementation of a method also supports implementing a publicly
known interface but without exposing the implementation of of the method
to untrusted callers.

For example, suppose that we'd like to customize printing by
implementing the @rhombus(Printer) interface, but we don't want a public
@rhombus(print) method. Printing and string conversion access
customization methods using an internal name for @rhombus(Printer), so
privately implementing @rhombus(Printer) will achieve the goal.

To privately implement an interface, use
@rhombus(private, ~class_clause) @rhombus(implements, ~class_clause),
and override the interfaces methods with
@rhombus(private, ~class_clause) methods.

@(demo:
    ~defn:
      import: rhombus
    ~defn:      
      class Posn(x, y):
        private implements: Printer
        private override print(op):
          rhombus.display("⟨⟨⟨", op)
          rhombus.print(x, op)
          rhombus.display(", ", op)
          rhombus.print(y, op)       
          rhombus.display("⟩⟩⟩", op)
    ~repl:
      Posn(1, 2)
      ~error:
        Posn(1, 2).print(current_output_port())
  )

@close_eval(method_eval)
