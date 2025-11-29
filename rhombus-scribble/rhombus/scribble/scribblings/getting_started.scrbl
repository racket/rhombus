#lang rhombus/scribble/manual
@(import:
    rhombus/meta open
    scribble/bnf
    "common.rhm" open)

@(fun commandline(args): nested(~style: #'inset, exec(args)))

@(expr.macro 'samplemod $(args :: Term)':
    '[
       @rhombusblock(
         #,(hash_lang()) #,(@rhombuslangname(rhombus/scribble))
       ),
       @(rhombusblock_etc(~text: #true): $args)
     ]')

@(expr.macro 'sample $(args :: Term)':
    '@(rhombusblock_etc(~text: #true): $args)')

@(fun result(text): nested(~style: #'inset, text))

@(def at: @litchar("@"))
@(def parens: @elem{@litchar{(}…@litchar{)}})
@(def braces: @elem{@litchar("{")…@litchar("}")})

@title(~tag: "getting-started"){Getting Started}

No matter what you want to do with Scribble, it's best to start by
generating a few simple HTML and/or PDF documents. This chapter steps
you through the basics.

@//----------------------------------------
@section(~tag: "first-example"){A First Example}

Create a file @filepath{mouse.scrbl} with this content:

@samplemod{
  @title{On the Cookie-Eating Habits of Mice}

  If you give a mouse a cookie, he's going to ask for a
  glass of milk.
}

The first line's
@rhombus(#,(hash_lang()) #,(@rhombuslangname(rhombus/scribble)))
indicates that the file implements a Rhombus Scribble document. The document
starts in ``text mode,'' and the @litchar("@") character escapes to
functions like @rhombus(title), where the curly braces return to text
mode for the arguments to the function. The rest is document content.

Now run the @exec{scribble} command-line program, specifying a mode
for the kind of document that you want as output:

@itemlist(

  @item{Run

               @commandline{scribble mouse.scrbl}

 to generate HTML as @filepath{mouse.html}. You may notice that the
 apostrophe in ``he's'' turned into a curly apostrophe.}

  @item{Run

               @commandline{scribble --htmls mouse.scrbl}

 to generate HTML as @filepath{mouse/index.html}. Sub-sections (which we
 add next) will appear as separate HTML files in the @filepath{mouse}
 directory.}

  @item{Run

                @commandline{scribble --pdf mouse.scrbl}

 to generate PDF as @filepath{mouse.pdf}. This will work only if you
 have @exec{pdflatex} installed. If you'd like to see the intermediate
 Latex, try

               @commandline{scribble --latex mouse.scrbl}

  to generate @filepath{mouse.tex}.}

)

See
@secref("running", ~doc: ModulePath'lib("scribblings/scribble/scribble.scrbl")')
in the Racket Scribble documentation for more information on the
@exec{scribble} command-line tool.

@//----------------------------------------
@section{Multiple Sections}

Add more text to @filepath{mouse.scrbl} so that it looks like this:

@samplemod{
            @title{On the Cookie-Eating Habits of Mice}

            If you give a mouse a cookie, he's going to ask for a
            glass of milk.

            @section{The Consequences of Milk}

            That ``squeak'' was the mouse asking for milk. Let's
            suppose that you give him some in a big glass.

            He's a small mouse. The glass is too big---way too
            big. So, he'll probably ask you for a straw. You might as
            well give it to him.

            @section{Not the Last Straw}

            For now, to handle the milk moustache, it's enough to give
            him a napkin. But it doesn't end there... oh, no.
          }

Now, after the first paragraph of the paper, we have two sub-sections,
each created by calling @rhombus(section) to generate a sub-section
declaration. The first sub-section has two paragraphs. The second
section, as initiated by the result of the second @rhombus(section) call,
has a single paragraph.

Run the @exec{scribble} command(s) from @secref("first-example")
again. You may notice the curly double-quotes in the output, and
the @litchar{---} turned into an em dash.

@//----------------------------------------
@section{Splitting the Document Source}

As a document grows larger, it's better to split sections into
separate source files. The @rhombus(include_section) function
incorporates a document defined by a @filepath{.scrbl} file into a
larger document.

To split the example document into multiple files, change
@filepath{mouse.scrbl} to just

@samplemod{
            @title{On the Cookie-Eating Habits of Mice}

            If you give a mouse a cookie, he's going to ask for a
            glass of milk.

            @include_section("milk.scrbl")
            @include_section("straw.scrbl")
}

Create @filepath{milk.scrbl} and @filepath{straw.scrbl} in the same
directory as @filepath{mouse.scrbl}. In @filepath{milk.scrbl}, put

@samplemod{
            @title{The Consequences of Milk}

            That ``squeak'' was the mouse asking for milk...
}

and in @filepath{straw.scrbl}, put

@samplemod{
            @title{Not the Last Straw}

            For now, to handle the milk moustache, ...
}

Notice that the new files both start with @hash_lang(), like the
original document, and the @rhombus(section)s from the original
document become @rhombus(title)s in the new documents. Both
@filepath{milk.scrbl} and @filepath{straw.scrbl} are documents in
their own right with their own titles, and they can be individually
rendered using @exec{scribble}. Running @exec{scribble} on
@filepath{mouse.scrbl}, meanwhile, incorporates the smaller documents
into one document that is the same as before.

@// ----------------------------------------
@section{More Functions}

The @rhombuslangname(rhombus/scribble) language provides a collection of
basic functions (and @rhombuslangname(rhombus/scribble/manual) is a supersets of
@rhombuslangname(rhombus/scribble)). Many of the functions are style
variations that you can apply to text:

@sample{
            He's a @smaller{small mouse}. The glass is too
            @larger{big}---@bold{way @larger{too @larger{big}}}. So, he'll
            @italic{probably} ask you for a straw.
}

which renders as

@result{
            He's a @smaller{small mouse}. The glass is too
            @larger{big}---@bold{way @larger{too @larger{big}}}. So, he'll
            @italic{probably} ask you for a straw.
}

As you would expect, calls to functions like @rhombus(smaller),
@rhombus(larger), and @rhombus(bold) can be nested in other calls. They
can also be nested within calls to @rhombus(title) or @rhombus(section):

@sample{
            @section{@italic{Not} the Last Straw}
}

@subsection{Centering}

The @rhombus(centered) function centers a flow of text:

@sample{
           If a mouse eats all your cookies, put up a sign that says
           @centered{
             @bold{Cookies Wanted}

             @italic{Chocolate chip preferred!}
           }
           and see if anyone brings you more.
}

which renders as

@result{
           If a mouse eats all your cookies, put up a sign that says
           @centered{
             @bold{Cookies Wanted}

             @italic{Chocolate chip preferred!}
           }
           and see if anyone brings you more.
}

@subsection{Margin Notes}

The @rhombus(margin_note) function is used in a similar way, but the
rendered text is moved to the margins.
@margin_note{If you use @rhombus(margin_note), then the content shows
             up over here.}

@subsection{Itemizations}

The @rhombus(itemlist) function creates a sequence of bulleted text,
where the @rhombus(item) function groups text to appear in a single
bullet. The @rhombus(itemlist) function is different from the others
that we have seen before, because it only accepts values produced by
@rhombus(item) instead of arbitrary text. This difference is reflected
in the use of @parens for the arguments to
@rhombus(itemlist) instead of @braces:

@sample{
           @centered{@bold{Notice to Mice}}

           @itemlist(@item{We have cookies for you.}
                     @item{If you want to eat a cookie,
                           you must bring your own straw.})
}

which renders as

@result{
           @centered{@bold{Notice to Mice}}

           @itemlist(@item{We have cookies for you.}
                     @item{If you want to eat a cookie,
                           you must bring your own straw.})
}

@subsection{Tables}

The @rhombus(tabular) function takes a list of lists to organize into a
two-dimensional table. By default, no spacing is added between columns,
so supply a @rhombus(~sep) argument to act as a column separator.
For example,

@sample{
     @tabular(~sep: @hspace(1),
              [[@bold{Animal}, @bold{Food}],
               ["mouse", "cookie"],
               ["moose", "muffin"]])
}

renders as

@result{
     @tabular(~sep: @hspace(1),
              [[@bold{Animal}, @bold{Food}],
               ["mouse", "cookie"],
               ["moose", "muffin"]])
}

@// ----------------------------------------
@section{Text Mode vs. Rhombus Mode for Arguments}

When @parens surrounds the arguments of a function, the argument
expressions are in Rhombus mode rather than text mode. Even in Rhombus
mode, @at can be used to apply functions. In
@rhombuslangname(rhombus/scribble) or even plain
@rhombuslangname(rhombus), @at behaves the same in both Rhombus mode and
text mode.

One advantage of using Rhombus mode for the arguments to
@rhombus(itemlist) is that we can pass a keyword-tagged optional
argument to @rhombus(itemlist). In particular, if you want a list with
numbers instead of bullets, supply the @rhombus(#'ordered) style to
@rhombus(itemlist) using the @rhombus(~style) keyword:

@sample{
           @itemlist(~style: #'ordered,
                     @item{Eat cookie.}
                     @item{Drink milk.}
                     @item{Wipe mouth.}
                     @item{...})
}

A function doesn't care whether it's used with @parens or @braces.
Roughly, @braces forms an argument that is a list containing a string.
(Only roughly, though. Newlines or uses of @at within @braces
complicate the picture, and we'll get back to that soon.) So,

@sample{
  @italic{Yummy!}
}

is equivalent to

@sample{
  @italic(["Yummy!"])
}

which is equivalent to the Rhombus expression

@rhombusblock(
  italic(["Yummy!"])
)

These equivalences explain why Scribble functions are documented in
Rhombus notation. If you're reading this in HTML format, you can click
@rhombus(italic) above to access its documentation. The documentation
won't completely make sense, yet, but it will by the end of this
chapter.

What if you want to provide arguments in text mode, but you also want to
supply other optional arguments? You can use both @parens and @braces
for a function, as long as the @parens part is first, and as long as no
characters separate the closing @litchar{)} from the opening
@litchar("{"). For example, calling @rhombus(italic) is the same as using
@rhombus(elem) with the @rhombus(#'italic) style:

@sample{
   @elem(~style: #'italic){Yummy!}
}

You can also @emph{omit} both @parens and @braces. In that case, the
Rhombus expression after @at is used directly instead of applied as an
operation---although that expression often needs to be within @parens,
anyway, to tell @at where to stop. For example,

@sample{
       1 plus 2 is @(to_string(1 + 2))
}

renders as

@result{
       1 plus 2 is @(to_string(1 + 2))
}

The call to @rhombus(to_string) is needed because a naked number
is not valid as document content.


@// ----------------------------------------
@section(~tag: "how-to:reader"){@"@" Syntax Basics}

The @at notation used by Scribble is just another way of writing Rhombus
expressions. Scribble documents could be constructed using normal
Rhombus notation, without using @at at all, but that would be
inconvenient for most purposes. The @at notation makes dealing with
textual content much easier.

Whether in text mode or Rhombus mode, @at in a document provides an
escape to Rhombus mode. The basic syntax of @at is

@nested(
  ~style: #'inset,
  bnf.seq(@litchar("@"),
          @bnf.nt{cmd},
          @litchar{(}, @bnf.star(@bnf.nt{group}), @litchar{)},
          @litchar("{"), @bnf.nt{text-body}, @litchar("}"))
)

where all three parts after @at are optional, but at least
one must be present. No spaces are allowed between

@itemlist(

 @item{@at and @bnf.nt{cmd}, @litchar{(}, or @litchar("{");}

 @item{@bnf.nt{cmd} and @litchar{(} or @litchar("{"); or}

 @item{@litchar{)} and @litchar("{").}

)

A @bnf.nt{cmd} or @bnf.nt{group} is normal Rhombus notation, while a
@bnf.nt{text-body} is itself in text mode. A @bnf.nt{cmd} obviously
must not start with @litchar{(} or @litchar("{"), even though Rhombus
forms could otherwise start with those characters.

The expansion of just @at@bnf.nt{cmd} into Rhombus code is

@rhombusblock(
  #,(@bnf.nt{cmd})
)

When either @parens or @braces are used, the expansion is

@rhombusblock(
  #,(@bnf.nt{cmd})(#,(@bnf.star(@bnf.nt{group})), #,(@bnf.nt{parsed-body}))
)

where @bnf.nt{parsed-body} is the parse result of the
@bnf.nt{text-body}. The @bnf.nt{parsed-body} part often turns out to be
a list of Rhombus strings.

In practice, the @bnf.nt{cmd} is normally a Rhombus identifier that is
bound to a function or macro. If the function or macro expects further
text to typeset, then @braces supplies the text. If the form expects
other data, typically @parens is used to surround Rhombus arguments,
instead. Even if a function's argument is a string, if the string is not
used as content text (but instead used as, say, a hyperlink label), then
the string is typically provided through @parens instead of @braces.
Sometimes, both @parens and @braces are used, where the former surround
Rhombus arguments that precede text to typeset.

Finally, if a form is a purely Rhombus-level form with no typeset
result, such as a @rhombus(import) to import more operations, then
typically @at is used with immediate @parens. This is s second allowed
form of @at:

@nested(
  ~style: #'inset,
  bnf.seq(@at, @litchar{(}, @bnf.nt{form}, @litchar{)})
)

where the expansion of @rhombus(#,(@at)(#,(@bnf.nt{form}))) is just
@bnf.nt{form}.

For example the text-mode stream

@sample{
    @(import: scriblib/figure open)

    @section(~tag: "poetry"){Of Mice and Cookies}
    See @secref("milk").

    @section(~tag: "milk"){@italic{Important} Milk Supplies}
    @figure("straw", @elem{A straw}){@image("straw.png")}
}

is equivalent to the Rhombus-mode sequence

@rhombusblock(
    import: scriblib/figure open
    "\n"
    "\n"
    section(~tag: "poetry", ["Of Mice and Cookies"]) "\n"
    "See " @secref("milk") "." "\n"
    "\n"
    section(~tag "milk", [italic(["Important"]), " Milk Supplies"]) "\n"
    figure("straw", elem("A straw"), [@image("straw.png")]) "\n"
)

Besides showing how different argument conventions are used for
different operations, the above example illustrates how whitespace is
preserved in the Racket form of a text-mode stream---including
newlines preserved as their own strings. Notice how the second
@rhombus(section) gets two list elements for its content, since the
argument content for @rhombus(section) in the source stream includes
both the use of an operator and additional text. When an operation
like @rhombus(section) or @rhombus(italic) accepts content to typeset,
it normally accepts a list whose elements form the content.

In addition to its role for command, a @at can be followed by
@litchar{//} to start a @indexed(["Scribble", "comments"]){comment}. If
the character after @litchar{//} is @litchar("{"), then the comment runs
until a matching @litchar("}"), otherwise the comment runs until the
end-of-line:

@nested(
  ~style: #'inset,
  [bnf.seq(@litchar("@//{"), @bnf.nt{text-mode-multi-line-comment}, @litchar("}")),
   "\n", "\n",
   bnf.seq(@litchar("@//"), @bnf.nt{single-line-comment})]
)

For more information on the syntax of @at, see
@secref("at-parsing", ~doc: ModulePath'lib("shrubbery/scribblings/shrubbery.scrbl")').
The full syntax includes a few more details, such as brackets like
@litchar("|{")…@litchar("}|") for text-mode arguments while disabling
@litchar("@") between the brackets.
