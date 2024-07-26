#lang rhombus/scribble/manual
@(import:
    "quote.rhm" open)

@title(~tag: "tool-support"){Editor Support}

The shrubbery package includes support for shrubbery syntax coloring and
editor operations for DrRacket. Where possible, this section also
provides hints for using other editors.

@section(~tag: "drracket-shrubbery"){Shrubbery Support in DrRacket}

Tab cycles through the possible indentations for a line based on
preceding lines. The indentation possibilities can be different if a
line is empty or starts with @litchar{|} or an operator. If multiple
lines are selected, if they start out with a valid indentation relative
to that first line, and if all lines can be shifted by the same amount
in tandem with the first line, then Tab cycles through possibilities for
the first selected line and shifts remaining lines by same amount;
otherwise, Tab attempts to indent each line independently.

Meta-A toggles the armor of the currently selected region. Armoring adds
@guillemets to make the selected region indentation-insensitive, and
unarmoring removes the added @guillemets. A potential use of armoring is
to protect a region of code while moving it from one context to another
context with a different indentation: armor in the source context, copy
and reindent with Tab in the target context, and then unarmor.

@section(~tag: "type-guillemet"){How to Type @guillemets}

The way to type @guillemets on a keyboard depends on your operating
system, program editor, and configuration, but here are some
possibilities:

@itemlist(
  
 @item{@bold{DrRacket}:
 @//
 Type @litchar{\guillemetleft} for @litchar{«} or
 @litchar{\guillemetright} for @litchar{»}, then hit Control-@litchar{\}
 or Meta-@litchar{\} (where a Meta key combination is an Alt combination,
 Option combination, or Esc prefix, depending on your keyboard and
 configuration). Note that @litchar{\gui} followed by Control-@litchar{\}
 or Meta-@litchar{\} expands to @litchar{\guillemet} via autocomplete,
 and then @litchar{l} or @litchar{r} followed by Ctl-@litchar{\} or
 Meta-@litchar{\} again will complete to @litchar{«} or @litchar{»}.}

 @item{@bold{Mac OS with an English keyboard layout}:
 @//
 Hit Option-@litchar{\} for @litchar{«}, or hit
 Option-Shift-@litchar{|} for @litchar{»}.}

 @item{@bold{Windows}:
 @//
 Hold Alt while typing @litchar{174} on the numpad for @litchar{«}, or
 hold Alt and type @litchar{175} on the numpad for @litchar{»}.}

 @item{@bold{Unix variants with a typical window manager}:
 @//
 Hit Control-Shift-U, then type @litchar{00AB} (or just @litchar{AB})
 for @litchar{«}, or type @litchar{00BB} (or just @litchar{BB}) for
 @litchar{»}, then hit Enter or Space.}

 @item{@bold{Unix variants with a Compose key configured}:
 @//
 Hit Compose, then type @litchar{<<} for @litchar{«} or @litchar{>>} for
 @litchar{»}.}

 @item{@bold{Vim}:
 @//
 Type Control-K then @litchar{<<} for @litchar{«} or @litchar{>>} for
 @litchar{»}.}

 @item{@bold{Emacs}:
 @//
 Run @litchar{insert-char} (via Control-X, 8, Enter), then provide
 @litchar{00AB} (or just @litchar{AB}) for @litchar{«}, or provide
 @litchar{00BB} (or just @litchar{BB}) for @litchar{»}.}

)

See also the
@hyperlink("https://en.wikipedia.org/wiki/Guillemet#Keyboard_entry"){Keyboard
 entry section of the Guillemet article on Wikipedia}.
