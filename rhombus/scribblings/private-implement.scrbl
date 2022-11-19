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

@close_eval(method_eval)
