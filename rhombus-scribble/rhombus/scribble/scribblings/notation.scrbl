#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def shrubbery_doc: ModulePath 'lib("shrubbery/scribblings/shrubbery.scrbl")')

@title(~tag: "notation"){Notation}

@secref("getting-started") provides many examples of Rhombus notation.
Here, we recap the main points.

The @rhombuslangname(rhombus/scribble) language is like the
@rhombuslangname(rhombus) language, but with three key differences:

@itemlist(

 @item{A
 @rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus/scribble))) module
 body starts in text mode as if inside @litchar("@{}") (see
 @secref(~doc: shrubbery_doc, "at-notation")), which means that
 @litchar("@") escapes must be used to write Rhombus definitions or
 expressions for implementing a document.}

 @item{Since it starts in text mode, the body of a
 @rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus/scribble))) module
 produces a mixture of string literals and other forms. Those results are
 gathered together and decoded as described in @secref("decoding") to
 construct a document representation as described in @secref("structure").}

 @item{The @rhombuslangname(rhombus/scribble) module provides all of the
 binding of @rhombuslangname(rhombus) plus a group of bindings for writing
 documents. Those bindings are described in @secref("base"). The
 @rhombuslangname(rhombus/scribble/manual) module provides even more
 bindings as described in @secref("manual").}

)

Functions and forms added by @rhombuslangname(rhombus/scribble) to
@rhombuslangname(rhombus) are primarily intended to be used with
@litchar("@") notation. For example, the @rhombus(italic) function could
be called as @rhombus(italic(["Hello"])), but the intended use syntax is
@(rhombusblock_etc(~inline: #true): @italic{Hello}).

Some functions accept extra arguments, especially
keyword-based optional arguments, and those can be supplied in
parentheses before a final text argument written with @litchar{{}}.
For example,

@rhombusblock(
  @hyperlink("https://rhombus-lang.org", ~underline: #true){Rhombus}
)

supplies a string argument and an @rhombus(~underline) keyword argument
to @rhombus(hyperlink), while @rhombus(["Rhombus"]) (as written in
@litchar{{}} with @litchar("@") notation) is provided as the text of the
hyperlink.

The text content of a function like @rhombus(italic) or
@rhombus(hyperlink) is represented by an argument with the
@rhombus(PreContent, ~annot) annotation, which allows a mixture of
strings, @rhombus(Element, ~annot) results as produced by nested calls
to functions like @rhombus(italic) and @rhombus(hyperlink), lists of
@rhombus(PreContent, ~annot) values, and a few other kinds of values.
The ``Pre'' part of @rhombus(PreContent, ~annot) refers to the fact that
string arguments are lightly decoded, turning @litchar{---} into an
em dash and @litchar{``} into curly open quotes, for example.

The @rhombus(PreContent, ~annot) annotation roughly corresponds to a
paragraph in output. Functions like @rhombus(nested) or @rhombus(item)
take @rhombus(PreFlow, ~annot) arguments, because they accept with
multiple paragraphs of text. Functions like @rhombus(nested) and
@rhombus(item) are still mean to be used with @litchar("@") notation and
@litchar{{}}, but part of the decoding of a @rhombus(PreFlow, ~annot) is
recognizing two newlines in a row (possibly with whitespace in between)
to create a break between paragraphs. The immediate, non-definition
content of a
@rhombus(#,(@hash_lang()) #,(@rhombuslangname(rhombus/scribble))) module
is implicitly a @rhombus(PreFlow, ~annot).

Some functions, such as @rhombus(secref), do not have text-content
arguments with an immediate @rhombus(PreContent, ~annot) or
@rhombus(PreFlow, ~annot) annotation, and so they are @emph{not} meant
to be used with @litchar("{}") arguments. The @rhombus(tabular) function
is another example that is normally used without @litchar("{}"), because
it works in terms of a list of list of @rhombus(PreFlow, ~annot)s.
