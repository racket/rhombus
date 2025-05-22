#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "editor"){Editing Rhombus Programs}

The
@seclink("top",
         ~indirect: #true,
         ~doc: ModulePath'lib("scribblings/drracket/drracket.scrbl")'){DrRacket}
programming environment directly supports Rhombus. As long as your
program source starts

@rhombusblock(
  #,(hash_lang()) #,(@rhombuslangname(rhombus))
)

then DrRacket will load an use an editor mode suitable for Rhombus. In
particular, the Tab key will cycle though valid indentations of the
current line.

Rhombus is supported in Emacs through
@hyperlink("https://github.com/greghendershott/racket-mode"){Racket
 mode} with its @tt{racket-hash-lang-mode} major mode. As in DrRacket,
using @tt{racket-hash-lang-mode} adapts the editor to the language
specified by the initial @hash_lang() line.

The
@hyperlink("https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket"){Magic
 Racket} extension for Visual Studio Code provide support for running
Rhombus programs, and adds rudimentary support for syntax highlighting as well.
