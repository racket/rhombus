#lang rhombus/scribble/manual

@title{Quick Start}

To get started with Rhombus:

@itemlist(

 @item{Install
 @hyperlink("https://racket-lang.org/download"){Racket}.

 To try out Rhombus quickly, it may be easiest to install a
 @hyperlink("https://users.cs.utah.edu/plt/rhombus-snapshots/"){snapshot Racket with Rhombus included}.
 For all but the most common machine types or for long-term support,
 however, a @hyperlink("https://racket-lang.org/download"){Regular Racket release} is the best option.

 Depending on your operating system, you may be able to install through
 its package manager, but the
 @hyperlink("https://racket-lang.org/download"){Racket download site} is
 always a fine option.

 Installation will give you the DrRacket programming environment and
 command-line programs @exec{racket} and @exec{raco}.}


 @item{Install the @bold{@tt{rhombus}} package for Racket.

 This step is not needed if you installed a snapshot with Rhombus
 included.

 In DrRacket, select the @onscreen{Install Package...} item in the
 @onscreen{File} menu. Alternatively, on the command line, use

 @nested(~style: #'inset){@exec{raco pkg install rhombus}}}


 @item{Type your first Rhombus program into the top area of DrRacket,

@rhombusblock(
    #,(hash_lang()) #,(@rhombuslangname(rhombus))
    "Hello, World!"
  )

 and click the @onscreen{Run} button.}

)

For alternatives and other ways of working with Rhombus, see
@secref("running").
