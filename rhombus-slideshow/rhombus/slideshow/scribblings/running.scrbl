#lang rhombus/scribble/manual

@(import:
    scribble/bnf
    meta_label:
      rhombus open
      slideshow open
      draw.Font)

@title(~tag: "running"){Running Slideshow}

Running a module that imports @rhombusmodname(slideshow) (either
directly or transitively) runs a slide presentation. The Slideshow
application also runs a presentation, and by default runs with
restricted filesystem and network access. (Running Slideshow with no
command-line arguments starts a Racket-oriented tutorial.)

@section(~tag: "keybinding"){Presentation Key Bindings}

When @rhombus(slide) is called then first time and a window appears,
you can control the presentation using these key bindings:

@tabular(
  ~pad: [1, 0],
  [
    ["Alt-q, Meta-q, or Cmd-q",             "end slide show"],
    ["Esc",                                 "if confirmed, end show"],
    ["Right/Down arrow, Space, f, n, or click", "next slide"],
    ["Left/Up arrow, Backspace, Delete, or b",  "previous slide"],
    ["g",                                   "last slide"],
    ["1",                                   "first slide"],
    ["s",                                   "next slide with a different title/name"],
    ["a",                                   "previous slide starting different title/name"],
    ["Alt-g, Cmd-g, or Meta-g",             "select a slide"],
    ["Alt-p, Cmd-p, or Meta-p",             "show/hide slide number"],
    ["Alt-c, Cmd-c, or Meta-c",             "show/hide commentary"],
    ["Alt-d, Cmd-d, or Meta-d",             "show/hide preview"],
    ["Alt-m, Cmd-m, or Meta-m",             "show/hide mouse cursor"],
    ["Alt-l, Cmd-l, or Meta-l",             "show/hide ``spotlight''"],
    ["Shift with arrow",                    "move window 1 pixel"],
    ["Alt, Meta, or Cmd with arrow",        "move window 10 pixels"]
  ]
)

@section(~tag: "cmdline"){Command Line}

Whether running the Slideshow executable or a module that imports
@racketmodname(slideshow), command-line arguments affect the initial
configuration and adjust the presentation interface. Run @exec{slideshow
 --help} for information on all flags, but here are a few of the most
useful:

@itemlist(

 @item{@exec_flag{-x} or @exec_flag{--export}: Exports the slide
 presentation @filepath{@bnf.nt{name}.rhm} as PDF. The output file is
 named @filepath{@bnf.nt{name}.pdf}, but a different name can be
 specified with @exec_flag{-o}. This flag implies @tech{condensed mode}
 like @exec_flag{-c} or @exec_flag{--condense}.}

 @item{@exec_flag{-p} or @exec_flag{--print}: Exports by printing,
 normally involving interactive platform-specific print dialogs.}

 @item{@exec_flag{-D} or @exec_flag{--pdf}: Exports to PDF (without
 print dialogs).}

 @item{@exec_flag{-o} @bnf.nt{file}: Exports to @bnf.nt{file} when
 exporting via @exec_flag{-p}, @exec_flag{--print}, @exec_flag{-D}, or
 @exec_flag{--pdf}.}

 @item{@exec_flag{-c} or @exec_flag{--condense}: Enables
 @tech{condensed mode}, which skips slides with content that has the
 @rhombus(#'nonarchival) property. See @rhombus(nonarchival).}

)

Note that @exec_flag{--widescreen} and @exec_flag{--fullscreen} have no
effect, because a @rhombus(slide) call's @rhombus(~aspect) argument
defaults to @rhombus(#'widecsreen).

@section(~tag: "startup"){Startup Configuration}

@docmodule(slideshow/config)

The @rhombusmodname(slideshow/config) module is imported by
@rhombusmodname(slideshow) and @rhombusmodname(slideshow/content), so it
is transitively imported by a module that uses any of those.
When the @rhombusmodname(slideshow/config) module is instantiated, it sets
@rhombus(current_para_width) and adjusts the size of
@rhombus(current_font).

To configure defaults differently for your slides, create your own
@filepath{config.rhm} module that first imports
@rhombusmodname(slideshow/config) and then adjusts parameters further.
As long as your slides consistently import @filepath{config.rhm} before
other modules, the adjustments in @filepath{config.rhm} will take effect
consistently.
