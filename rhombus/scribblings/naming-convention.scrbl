#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title(~tag: "naming-convention"){Naming Conventions}

The @rhombuslangname(rhombus) language's bindings follow certain naming
conventions. While Rhombus does not impose a naming convention
syntactically, authors are new libraries are encouraged to stick to
@rhombuslangname(rhombus) conventions where possible.

Most names use ``snake case,'' where the leading character is lowercase
and multiple words are joined by @litchar{_}. Spelled-out words are
preferred, but abbreviations are used in place of long words that must
be written especially often. Examples:

@itemlist(

 @item{Syntactic form names like @rhombus(fun), @rhombus(def), and
 @rhombus(cond) are lowercase and abbreviated forms of the words
 ``function,'' ``definition,'' and ``conditional,'' respectively.}

 @item{The names @rhombus(match) and @rhombus(class) are short enough
 that no abbreviation would be needed, while @rhombus(namespace) and
 @rhombus(interface) are spelled out because they are needed much less
 frequently than forms like @rhombus(def).}

 @item{The function @rhombus(to_string) and the syntactic form
 @rhombus(use_static) include @litchar{_} to join its words, as do
 function names like @rhombus(Bytes.copy_from) and
 @rhombus(Bytes.utf8_string) within the @rhombus(Bytes) namespace.}

)

A leading capital letter and ``Pascal case'' (where multiple words are
joined by capitalizing each word) are used for a name that refers to a
@tech{class}, @tech{interface}, a @tech{syntax class}, or an
@tech{annotation} that in principle refers to an interface. Examples:

@itemlist(

 @item{The @rhombus(Posn) class defined as an example through this
 manual is capitalized, since it's a class.}

 @item{Both the @rhombus(Identifier, ~annot) annotation and
 @rhombus(Identifier, ~stxclass) syntax class are capitalized. Although
 the @rhombus(Identifier, ~annot) annotation is not implemented through
 an interface internally, it refers to an in-principle interface that
 would be implemented by identifier syntax objects.}

 @item{The @rhombus(NonnegInt, ~annot) annotation matches a subset of
 integers, and much like @rhombus(Identifier, ~annot), it corresponds in
 principle to an interface that is implemented by nonnegative integers.}

 @item{The @rhombus(ReadableString.to_string, ~annot) annotation is a
 @tech{converter annotation}, which means that it does not simply refer
 to an interface (even in principle), so the name @rhombus(to_string) is
 not capitalized.}

 @item{Annotation constructors are @emph{not} capitalized, including
 @rhombus(maybe), @rhombus(matching), or the @rhombus(of, ~datum) in
 @rhombus(Posn.of).}

 @item{Namespaces that do not also refer to a class or interface are
 @emph{not} capitalized, including the @rhombus(math) of
 @rhombus(math.pi).}

)
