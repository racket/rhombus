#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Hierarchical Naming}

A language implemented with the Rhombus expander may have another
dimension of name resolution that is orthogonal to different mapping
spaces. For example, a language include a hierarchical naming strategy
to reach a binding through sequence of identifiers separated by
@rhombus[.], and hierarchical references might be used to reach mappings
for expressions, bindings, or more. In the initial example in this
proposal @rhombus[weather.currently_raining] is that kind of access, as
is @rhombus[expr.macro] and @rhombus[bind.macro].

The example language overloads @rhombus[.] for hierarchical namespace
use as well as field access, but the Rhombus expander minimizes any
assumptions about the form of hierarchical names. A hierarchical
reference must start with an identifier or operator that is mapped in
the default space to a @deftech{name root}, and that identifier must be
followed with the use of a designated name-path operator. A name root is
implemented by a transformer that is similar to a prefix macro
transformer (as explained in the next section), but it ``expands'' to a
new identifier whose binding is checked in a space that's suitable to
the context—or it expands to a reference to another name root, in which
case the new root is expanded recursively.
