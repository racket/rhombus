#lang scribble/rhombus/manual

@title(~tag: "at-notation"){At-Notation Using @litchar("@")}

Groups and blocks provide a convenient general notation for structured
forms, such as typical elements of programming language. Basic
shrubbery elements are less well suited for representing blocks of
free-form text, however. String literals work well enough for simple
text, but they're awkward for representing multi-line paragraphs and
interpolated text formatting.

To better support free-form text and escapes, shrubbery notation
includes a text support that is based on @rhombusmodname(#{at-exp})
notation for S-expressions. A @litchar("@") in shrubbery notation
starts a term that normally includes @litchar("{") and @litchar("}"),
where @litchar("@") changes the meaning of @litchar("{") and
@litchar("}") to delimit free-form text instead of shrubbery groups.
For example,

@rhombusblock(
  @typeset{Write "hello" to C:\greet.txt.}
)
    
is equivalent to

@rhombusblock(
  typeset(["Write \"hello\" to C:\\greet.txt."])
)

Note that text in @litchar("{}") in this example did not need escapes
for the literal quotes and backslashes. The conversion puts the
literal string in a list, where the list has more elements in the case
of multiline text or escapes.

Overall, @litchar("@") notation has three key properties:

@itemlist(

 @item{Within text @litchar("{") and @litchar("}"), nearly all content
       is literal, except that @litchar("@") can be used again to
       escape. Longer paired delimiters, such as @litchar("|<<{") and
       @litchar("}>>|"), imply a corresponding longer escape, such as
       @litchar("|<<@"), so that text notation itself embeds easily
       within another text context (as long as the outer context uses a
       distinct escape).},

 @item{Between @litchar("@") and the opening delimiter like
       @litchar("{"), optional additional terms are parsed as a normal
       shrubbery terms. Thus, a single @litchar("@") notation is
       useful both for escaping to text in a shrubbery context, and
       escaping back to a shrubbery notation in a text context.},

 @item{Every @litchar("@") form can be translated to a shrubbery form
       without @litchar("@"). This transformation is performed
       automatically during shrubbery parsing, as opposed to leaving
       the translation to a language that is built on shrubbery
       notation.}

)

Each input of the form

@verbatim(~indent: 2){
 @litchar("@")@italic{command}@litchar{(} @italic{arg} @litchar{,} ... @litchar{)}@litchar("{") @italic{text} @litchar("}")...
}

is parsed into the same representation as

@verbatim(~indent: 2){
 @italic{command}@litchar{(}@italic{arg}@litchar{,} ...@litchar{,} @litchar{[}@italic{converted_text}, ...@litchar{]}, ...@litchar{)}
}

Each component of the original form---@italic{command}, parenthesized
@italic{arg}s, and braced @italic{text}---is optional, as long as one
component is present, and as long as @italic{command} is present
before parenthesized @italic{arg}s. The @italic{command} and
@italic{arg} components are in shrubbery notation, while @italic{text}
is in text mode and converted to @italic{converted_text}. The
@italic{converted_text} translation includes elements that are not
string literals in places where @italic{text} has escapes. An
@litchar("@") form can have multiple @litchar("{}") @italic{text}
blocks, in which case the translation has multiple
@italic{converted_list} list arguments.

More examples:

@rhombusblock(
  @typeset(~style: bold){Write "hello"}
  typeset(~style: bold, ["Write \"hello\""])

  @typeset{Write @bold{"hello"}}
  typeset(["Write ", bold(["\"hello\""])])

  @typeset{Write @url{https://example.com}{"hello"}}
  typeset(["Write ", url(["https://example.com"], ["\"hello\""])])

  @typeset{Write @get_link(home_page) out...}
  typeset(["Write ", get_link(home_page), " out ..."])

  @typeset|{Example: @bold{"hello"}}|
  typeset(["Example: @bold{\"hello\"}"])
)

Some additional @litchar("@") rules:

@itemlist(

 @item{When multiple lines of text are within @litchar("{}"), then
       leading indentation common to all lines is discarded.},

 @item{While the @italic{command} component itself can be parenthesized, it
       can also have the form
       @rhombus(#,(@litchar("«")) #,(@italic{command}) ... #,(@litchar{»})),
       for a multi-part command component that is spliced into the translation
       without surrounding parentheses.},

 @item{A multi-part @italic{command} that is a sequence of
       identifiers separated by operators (usually @litchar{.}) can be
       written without grouping @litchar{«»}, as long as no space
       appears between the identifiers and operators.},

 @item{When @litchar{(} @italic{arg} @litchar{,} ... @litchar{)} is
       present, the separating commas are optional. That is, arguments
       can be provided as different newline-separated groups without a
       @litchar{,} in between.}

 @item{The form @rhombus(#,(@litchar("@(«")) #,(@italic{command}) ... #,(@litchar{»)}))
       splices as-is with no arguments, even if the subsequent text has
       the shape of parenthesed @italic{arg}s or braced @italic{text}.},

 @item{The @litchar("@//") comment form works both in normal shrubbery
       mode and as a comment escape within text.}

)

See @secref("at-parsing") for complete details.
