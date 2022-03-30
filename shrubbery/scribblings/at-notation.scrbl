#lang scribble/rhombus/manual

@title[~tag: "at-notation"]{At-Notation Using @litchar["@"]}

An @litchar["@"] form of the shape

@verbatim[~indent: 2]|{
 @«|@italic{command} ...»[|@italic{arg}, ...]{ |@italic{body} }...
}|

is parsed into the same representation as

@verbatim[~indent: 2]{
    @italic{command} ...(@italic{arg}, ..., [@italic{parsed_body}, ...], ...)
}

That is, the command part is left at the front and spliced into its
enclosing group, while the argument and body parts are wrapped with
parentheses to make them like arguments. Each body text is parsed into
a list of string literals and escapes, and multiple body texts can
be provided in multiple @litchar["{"]...@litchar["}"]s.

The command part usually does not have @litchar{«»}, and it is instead
usually written as an identifier, operator, or parenthesized term. The
argument and body parts, when present, always use @litchar{[]} and @litchar{{}},
respectively. Any of the three kinds parts can be omitted, but when
multiple parts are present, they must have no space between them or
the leading @litchar["@"]. When the argument and body parts are both
omitted, the command part is simply spliced into its context.

The conversion to a call-like form, keeping each body in a separate
list, and allowing multiple body arguments are the three main ways
that shrubbery @litchar["@"] notation differs from @litchar{#lang at-exp} notation. The
other differences are the use of @litchar{«}...@litchar{»} instead of @litchar{|}...@litchar{|} for
delimiting a command, and the use of @litchar["@//"] instead of @litchar["@;"] for
comments. The details are otherwise meant to be the same, and the rest
of this section is mostly a recap.

A body part is treated as literal text, except where @litchar["@"] is
used in a body to escape. An unescaped @litchar["}"] closes a body,
except that an unescaped @litchar["{"] must be balanced by an unescaped
@litchar["}"], with both treated as part of the body text. Instead of
@litchar["{"], a body-starting opener can be @litchar{|} plus
@litchar["{"] with any number of ASCII punctuation and symbol characters
(other than @litchar["{"]) in between; the corresponding closer is then
the same sequence in reverse, except that some characters are flipped:
@litchar["{"] to @litchar["}"], @litchar{(} to @litchar{)}, @litchar{)}
to @litchar{(}, @litchar{[} to @litchar{]}, @litchar{]} to @litchar{[},
@litchar{<} to @litchar{>}, and @litchar{>} to @litchar{<}. With an
@litchar{|}...@litchar["{"] opener, an escape is formed by using the
opener followed by @litchar["@"], while opener–closer pairs balance
within the body text. When multiple body parts are provided, each can
use a different opener and closer. The parsed form of the body breaks up
the body text into lines and @litchar{"\n"} as separate string literals
in the parsed list form, with each escape also being its own element in
the list form. Parsed body text also has leading and trailing whitespace
adjusted the same as with @litchar{#lang at-exp}.

After the @litchar["@"] of an escape in body text, the escape has the
same form as an at-notaton form that starts with @litchar["@"] as a
shubbery. That is, @litchar["@"] forms are essentially the same whether
starting in shrubbery mode or body-text mode.

In body text, there are two additional comment forms that are not
supported in shrubbery mode. A @litchar["@//{"] starts a block comment
that ends with @litchar["}"], and the comment form is not part of the
body text. The @litchar["@//"] comment form must be prefixed with an
opener when its enclosing body is started with an opener that isn't just
@litchar["{"], and the @litchar["{"] after @litchar["@//"] can more
generally be an @litchar{|}...@litchar["{"] opener with the
corresponding closer. Opener–closer pairs must be balanced in the
commented block, the same as in body text. A @litchar["@//"] comment
form (prefixed with an opener as needed to form an escape) that is not
followed by @litchar["{"] or an @litchar{|}...@litchar["{"] opener
comments out the rest of the line, including a comment-terminating
newline.