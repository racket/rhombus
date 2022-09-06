#lang scribble/rhombus/manual

@title(~tag: "at-notation"){At-Notation Using @litchar("@")}

An @litchar("@") form of the shape

@verbatim(~indent: 2){
 @litchar("@«") @italic{command} ... @litchar{»}@litchar{(} @italic{arg} @litchar{,} ... @litchar{)}@litchar("{") @italic{body} @litchar("}")...
}

is parsed into the same representation as

@verbatim(~indent: 2){
 @italic{command} ...@litchar{(}@italic{arg}@litchar{,} ...@litchar{,} @litchar{[}@italic{parsed_body}, ...@litchar{]}, ...@litchar{)}
}

That is, the command part is left at the front and spliced into its
enclosing group, while the argument and body parts are wrapped with
parentheses to make them like arguments. Each body text is parsed into
a list of string literals and escapes, and multiple body texts can
be provided in multiple @litchar("{")...@litchar("}")s.

When there's a single @italic{command} that is a identifier, operator,
parenthesized term, bracketed term, or sequence of identifiers
separated by operators with no spaces in between, the @litchar{«»} can
be omitted a long as no space is before or after the command:

@verbatim(~indent: 2){
 @litchar("@")@italic{command}@litchar{(} @italic{arg} @litchar{,} ... @litchar{)}@litchar("{") @italic{body} @litchar("}")...
}

The argument and body parts always use @litchar{()} and @litchar{{}},
respectively. Any of the three parts can be omitted, but a command
must be present to include arguments, and at least one part must be
present. When multiple parts are present, they must have no space
between them or the leading @litchar("@"). When the argument and body
parts are both omitted, the command part is simply spliced into its
context. For splicing with no arguments even when subsequent text
looks like an argument tor body part, surround the command part with
@litchar{(«} and @litchar{»)}:

@verbatim(~indent: 2){
 @litchar("@(«") @italic{command} ... @litchar("»)")
}

A @litchar("[") is not allowed in a position that could start the
arguments part of a @litchar("@") escape. This prohibition is intended
to support better error reporting when S-expression @litchar("@")
syntax is misused among shrubbery notation.

Compared to @rhombusmodname(#{at-exp}) notation, the main differences
are @litchar{()} instead of @litchar{[]} for arguments, keeping each
body in a separate list in the converted call, and allowing multiple
body arguments. Other differences are the use of
@litchar{(«}...@litchar{»)} instead of @litchar{|}...@litchar{|} for
delimiting a command without arguments, and the use of @litchar("@//")
instead of @litchar("@;") for comments. The details are otherwise
meant to be the same, and the rest of this section is mostly a recap.

A body part is treated as literal text, except where @litchar("@") is
used in a body to escape. An unescaped @litchar("}") closes a body,
except that an unescaped @litchar("{") must be balanced by an unescaped
@litchar("}"), with both treated as part of the body text. Instead of
@litchar("{"), a body-starting opener can be @litchar{|} plus
@litchar("{") with any number of ASCII punctuation and symbol characters
(other than @litchar("{")) in between; the corresponding closer is then
the same sequence in reverse, except that some characters are flipped:
@litchar("{") to @litchar("}"), @litchar{(} to @litchar{)}, @litchar{)}
to @litchar{(}, @litchar{[} to @litchar{]}, @litchar{]} to @litchar{[},
@litchar{<} to @litchar{>}, and @litchar{>} to @litchar{<}. With an
@litchar{|}...@litchar("{") opener, an escape is formed by using the
opener followed by @litchar("@"), while opener–closer pairs balance
within the body text. When multiple body parts are provided, each can
use a different opener and closer. The parsed form of the body breaks up
the body text into lines and @litchar{"\n"} as separate string literals
in the parsed list form, with each escape also being its own element in
the list form. Parsed body text also has leading and trailing whitespace
adjusted the same as with @rhombusmodname(#{at-exp}).

After the @litchar("@") of an escape in body text, the escape has the
same form as an at-notaton form that starts with @litchar("@") as a
shubbery. That is, @litchar("@") forms are essentially the same whether
starting in shrubbery mode or body-text mode.

In body text, there are two additional comment forms that are not
supported in shrubbery mode. A @litchar("@//{") starts a block comment
that ends with @litchar("}"), and the comment form is not part of the
body text. The @litchar("@//") comment form must be prefixed with an
opener when its enclosing body is started with an opener that isn't just
@litchar("{"), and the @litchar("{") after @litchar("@//") can more
generally be an @litchar{|}...@litchar("{") opener with the
corresponding closer. Opener–closer pairs must be balanced in the
commented block, the same as in body text. A @litchar("@//") comment
form (prefixed with an opener as needed to form an escape) that is not
followed by @litchar("{") or an @litchar{|}...@litchar("{") opener
comments out the rest of the line, including a comment-terminating
newline.