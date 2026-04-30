#lang rhombus/scribble/manual

@title{Quick Start}

To get started with Rhombus:

@itemlist(

 @item{Install
 @hyperlink("https://rhombus-lang.org/download.html"){Rhombus}.

 Installation will give you the DrRacket programming environment and
 command-line programs @exec{rhombus}, @exec{racket}, and @exec{raco}.}

 @item{Start DrRacket.}

 @item{Type your first Rhombus program into the top area of DrRacket,

@rhombusblock(
    #,(hash_lang()) #,(@rhombuslangname(rhombus))
    "Hello, World!"
  )

 and click the @onscreen{Run} button.}

)

For alternatives and other ways of working with Rhombus, see
@secref("running").
