#lang rhombus/scribble/manual

@(import:
    meta_label:
      rhombus open
      slideshow open      
      draw.Font
    "show.rhm" open)

@(def pict_doc = ModulePath 'lib("rhombus/pict/scribblings/rhombus-pict.scrbl")')

@title(~tag: "overview"){Using Slideshow in Rhombus}

The main function provided by the @rhombusmodname(slideshow) library is
@rhombus(slide):

@itemlist(
  
@item{When the @rhombus(slide) function is given a
 @tech(~doc: pict_doc){static pict}, it registers a single slide with the
 pict. If a title is given as a @rhombus(~title) argument to
 @rhombus(slide), the title is added to the top of the pict. Whether or
 not a title is provided, the pict is combined with a blank pict that
 represents the slide client area, and it is positioned based on a
 @rhombus(~layout) argument that defaults to centering the pict (unless
 it's too tall, in which case the pict is top-aligned to the client area
 of the screen).}

@item{When the @rhombus(slide) function is given an
 @tech(~doc: pict_doc){animated pict}, it registers a slide for each
 @tech(~doc: pict_doc){epoch} in the pict's
 @tech(~doc: pict_doc){duration}. For each epoch with a non-@rhombus(0)
 @tech(~doc: pict_doc){extent}, additional auto-advancing slides are
 registered to render a transition between stopping slides. Arguments for
 @rhombus(~title) and @rhombus(~layout) are used to compose each
 registered slide, the same as for a static-pict slide.}

)

Thus, a presentation can be generated by calling @rhombus(slide) many
times to register a sequence of static picts, calling @rhombus(slide)
once with an animated pict that implements many slides at once,
or---most commonly---a hybrid of those approaches.

@section{Multiple versus Single @rhombus(slide) Calls}

For example, this talk

@rhombusblock(
  slide(@t{Hello})
  slide(@t{World})
)

and this talk

@rhombusblock(
  slide(switch(@t{Hello},
               @t{World}))
)

produce the same result, which is a slide presentation that has two
slides, ``Hello'' followed by ``World.''

@show_result(
  slide(switch(@t{Hello},
               @t{World}))
)

Here's an example that takes more advantage of passing a single animated
pict to @rhombus(slide)---where the @rhombus(Pict.sustain) call is
needed to create an ending slide with ``World'' fully faded in:

@show(
  fun fade_out(p :: Pict): animate(fun (n): p.alpha(1-n))
  fun fade_in(p :: Pict): animate(fun (n): p.alpha(n))

  slide(overlay(fade_out(@t{Hello}),
                fade_in(@t{World}).sustain()))
)

Even in that case, the same effect could be achieved with two
@rhombus(slide) calls, avoiding the need for @rhombus(Pict.sustain), but
requiring repetition of the ``World'' pict:

@rhombusblock(
  def world = @t{World}
  slide(overlay(fade_out(@t{Hello}),
                fade_in(world)))
  slide(world)
)

This last example works only because ``World'' is positioned the same on
the screen whether or not it is centered with respect to ``Hello.'' It's
generally a good strategy to have transitions arrive at a same-sized
pict, but when that's inconvenient, a single animated pict is often
better.

@section{Staging with the @rhombus(slide) Sublanguage}

Suppose that instead of fading from ``Hello'' to ``World'', we want to
end up with both ``Hello'' and ``World'' on the ending slide but reveal
them one at a time. We could implement that using @rhombus(sequential):

@show(
  slide(stack(~sep: 24,
              & sequential(@t{Hello},
                           @t{World})))
)

Two aspects of this example are especially common in slide
presentations: vertical layout of multiple picts (especially text) with
some space in between, and staged display of the stacked components.
Because those patterns are so common, @rhombus(slide) includes some
shorthands to implement them. First, when @rhombus(slide) receives
multiple pict arguments, it combines them with @rhombus(stack) and an
amount of space that defaults to @rhombus(slide.gap), which is the
constant @rhombus(24).

@show(
  slide(@t{Hello},
        @t{World})
)

Second, the special value @rhombus(slide.next) is recognized as a
separator in a sequence of picts. The separator causes a stack of picts
after the @rhombus(slide.next) to be shifted sequentially after a stack
of picts before @rhombus(slide.next), achieving the same result as
@rhombus(stack) and @rhombus(sequential) above:

@show(
  slide(@t{Hello},
        slide.next,
        @t{World})
)

The special value @rhombus(slide.sync) is somewhat like
@rhombus(slide.next), but instead of hiding the part after
@rhombus(slide.next) until all epochs have completed in the part before,
a snapshot of the part after is used during the part before's epochs.

The @rhombus(slide.alts) function is similar to @rhombus(switch). It
takes any number of arguments and produces those arguments in sequence.

@show(
  slide(@t{1. Make a talk},
        slide.alts(@para{draft},
                   @para{@strikethrough{draft} complete}),
        slide.next,
        @t{2. Show the talk to others})
)

The main difference between @rhombus(slide.alts) and @rhombus(switch) is
that each argument to @rhombus(slide.alts) is treated as content in the
@rhombus(slide) sublanguage, so it can include @rhombus(slide.next).
Multiple content elements for a single alternative are grouped using a
list. More generally, the @rhombus(slide) sublanguage allows a list
anywhere that it allows other content, and the list's elements are
spliced into the content sequence.

@show(
  slide(@t{1. Make a talk},
        slide.alts([@para{draft},
                    slide.next,
                    @para{draft2, ...}],
                   [@strikethrough{drafts},
                    @para{... complete}]),
        slide.next,
        @t{2. Show the talk to others})
)

Another difference between @rhombus(slide.alts) and @rhombus(switch) is
that @rhombus(slide.alts) adjusts each of its alternatives so that they
have the same height. That adjustment prevents slide content from
jumping around on the screen when it appears after the alternatives, and
it prevents the slide content as a whole from shifting due a height
change (since content is centered on the screen by default).

@show(
  slide(@t{1. Make a talk},
        slide.alts([@para{draft},
                    slide.next,
                    @para{draft2, ...}],
                   @para{complete}),
        @t{2. Show the talk to others})
)

@section{Alignment with the @rhombus(slide) Sublanguage}

The staging example illustrates an alignment problem that is common when
layout out slide content: the ``1. Make...'' and ``2. Show...'' text
would look better left-aligned instead of centered. The
@rhombus(slide.align) function takes slide content to be spliced into an
enclosing sequence, but it adjusts every pict to be stacked in the
content so that it has the same width, left-aligning the widened picts
by default (but a @rhombus(~horiz) argument can specify a different
alignment).

@show(
  slide(slide.align(
          @t{1. Make a talk},
          slide.alts(@para{draft},
                     @para{@strikethrough{draft} complete}),
          slide.next,
          @t{2. Show the talk to others}
        ))
)

While using @rhombus(slide.align) in our running example looks better,
the ``draft'' to ``complete'' portion would look better centered. The
@rhombus(slide.center) function adjusts the effect of an enclosing
@rhombus(slide.align) to override alignment for the arguments of
@rhombus(slide.center). The @rhombus(slide.center) function is a
shorthand for @rhombus(slide.horiz) with @rhombus(~horiz: #'center).

@show(
  slide(slide.align(
          @t{1. Make a talk},
          slide.center(
            slide.alts(@para{draft},
                       @para{@strikethrough{draft} complete})
          ),
          slide.next,
          @t{2. Show the talk to others}
        ))
)

The running example almost looks right, but independently centering
``draft'' and its strikethrough with ``complete'' makes the ``draft''
text shift leftward as the slides advance. To make those nested picts to
be left-aligned with respect to each other, we can use a nested
@rhombus(slide.align).

@show(
  slide(slide.align(
          @t{1. Make a talk},
          slide.center(
            slide.align(
              slide.alts(@para{draft},
                         @para{@strikethrough{draft} complete})
            )
          ),
          slide.next,
          @t{2. Show the talk to others}
        ))
)

@section{Slide Content as a Pict}

The staging and alignment features of the @rhombus(slide) sublanguage
could be implemented directly with @rhombusmodname(pict) animation
primitives, but the @rhombus(slide) sublanguage can save some tedious
orchestration of @rhombus(sequential) and @rhombus(Pict.pad) over lists
of content picts. Meanwhile, when an animated pict is used as content
for the slide sublanguage, that animation composes naturally with any
animation steps that are created by the sublanguage.

The reverse direction is also possible. The @rhombus(slide_pict)
function takes slide content and produces the same (potentially)
animated pict that @rhombus(slide) would create for the same content.
That pict can then be passed to other functions that work on animated
picts instead of the @rhombus(slide) sublanguage.

@show(
  def steps = slide_pict(@para{Step 1},
                         slide.next,
                         @para{Step2})
  slide(rectangle(~around: steps.pad(32)))
)

