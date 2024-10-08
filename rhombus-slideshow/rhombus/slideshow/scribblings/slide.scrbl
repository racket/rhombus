#lang rhombus/scribble/manual

@(import:
    meta_label:
      rhombus open
      slideshow open
      draw.Font)

@title(~tag: "slide"){Creating Slides}

@doc(
  annot.macro 'SlideContent'
){

 Satisfied by allowed arguments to @rhombus(slide): a pict,
 @rhombus(slide.next), @rhombus(slide.sync), a value produced by @rhombus(slide.alts),
 @rhombus(slide.align), or @rhombus(slide.horiz), or a list of values
 that satisfy @rhombus(SlideContent, ~annot).

}

@doc(
  fun slide(~title: title :: maybe(String || Pict) = #false,
            ~name: name = title,
            ~layout: layout :: SlideLayout = #'auto,
            ~sep: sep :: Real = slide.gap,
            ~horiz: horiz_align :: HorizAlignment = #'center,
            ~lead_in: lead_in = #false,
            content :: SlideContent, ...) :: Void
){

 Registers one or more slides. In the sample case, each
 @rhombus(content) produces a pict, and the picts are combined with
 @rhombus(stack) and a separation of @rhombus(sep) and the horizontal
 alignment @rhombus(horiz_align). A slide is produced for each epoch in
 the resulting pict's duration, and extra transitions slides are
 registered for epochs that have a non-@rhombus(0) extent. If
 @rhombus(lead_in) is true, then slides are also registered for the
 transition from epoch @rhombus(-1).

 The @rhombus(title) and @rhombus(layout) arguments are used to combine
 content picts via the @rhombus(current_assembler) parameter's value.

 Besides immediate picts, the @rhombus(content) values can produce
 descriptions of slides to construct using lists, @rhombus(slide.next), @rhombus(slide.sync),
 @rhombus(slide.alts), @rhombus(slide.align), and @rhombus(slide.horiz)
 (or one of its shorthands: @rhombus(slide.left), @rhombus(slide.center),
 or @rhombus(slide.right)):

 @margin_note{See @secref("overview") for an introduction to this
  @rhombus(slide) staging and alignment sublanguage.}

 @itemlist(

 @item{Nested lists are flattened and spliced.}

 @item{A @rhombus(slide.next) creates two picts: one with everything
  before the @rhombus(slide.next), and one with everything after. The two
  picts are then made sequential with @rhombus(sequential) before
  combining them with @rhombus(stack). The @rhombus(sequential) function is
  used with its default duration mode, so it sustains the first pict. The
  first pict is also marked as @rhombus(nonarchival) for its pre-sustained
  duration. Note that @rhombus(slide.next) might be used in the part
  before this use, after this use, or both, creating a multi-epoch pict
  wherever it's used.}

 @item{A @rhombus(slide.sync) is similar to @rhombus(slide.next), but it
  combines a snapshot of the pict from after @rhombus(slide.sync) for
  all but the last epoch of the pict before @rhombus(slide.sync), and it
  shifts the time box of the pict after @rhombus(slide.sync) by one less
  (so that the last epoch of the pict before and the first epoch of the
  pict after are the same).}

 @item{A value produced by @rhombus(slide.alts) creates one pict for
  each argument to @rhombus(slide.alts), and the picts are sequentialized
  and then combined with @rhombus(overlay). (That's similar to using
  @rhombus(transition), but the bounding boxes for all alternatives are
  preserved for the combined duration.) The @rhombus(overlay) combination
  uses @rhombus(#'top) alignment. A @rhombus(slide.next) or @rhombus(slide.sync) can be used in
  any alternative, and @rhombus(slide.alts) can be nested; in either of
  those cases, the corresponding alternative will itself be a multi-epoch
  pict.}

 @item{A value produced by @rhombus(slide.align) causes all picts as
  arguments to @rhombus(slide.align) to get the same width by padding on
  the left, right, or both. This padding applies to picts in nested
  @rhombus(slide.alts) alternatives as well as nested
  @rhombus(slide.horiz) constructions---but not nested
  @rhombus(slide.align) constructions, which perform their own nested
  alignments. The @rhombus(~horiz) argument to @rhombus(slide.align)
  determines how padding is added by default to contained elements, but
  nested @rhombus(slide.horiz) constructions can change alignment.}

 @item{A value produced by @rhombus(slide.horiz) changes alignment and
  potentially vertical spacing for its arguments.}

)

 When a pict representing a slide has the @rhombus(#'nonarchival) key
 mapped to a true value in its metadata for some epoch, then the slide
 for that epoch is skipped in condense mode---as are any slides for
 transitions in the epoch. See also @rhombus(nonarchival).

}

@doc(
  fun slide_pict(~title: title :: maybe(String || Pict) = #false,
                 ~layout: layout :: SlideLayout = #'auto,
                 ~sep: sep :: Real = slide.gap,
                 ~horiz: horiz_align :: HorizAlignment = #'center,
                 ~full: full = title && #true,
                 content :: SlideContent, ...) :: Pict
){

 Like @rhombus(slide), except that the result is a pict (potentially with
 a multi-epoch duration) instead of registering a slide.

 If @rhombus(full) is true, then the resulting pict represents a slide
 including its title as combined via @rhombus(current_assembler).
 Otherwise, @rhombus(current_assembler) is not used, and the resulting
 pict is just the result of combining the @rhombus(content) values.

}

@doc(
  def slide.next
  def slide.sync
  fun slide.alts([content :: SlideContent, ...], ...)
  fun slide.align(~sep: sep :: Real || matching(#'inherit) = #'inherit,
                  ~horiz: horiz :: pict.HorizAlignment = #'left,
                  content :: SlideContent, ...)
  fun slide.horiz(~sep: sep :: Real || matching(#'inherit) = #'inherit,
                  ~horiz: horiz :: pict.HorizAlignment = #'left,
                  content :: SlideContent, ...)
  fun slide.left(~sep: sep :: Real || matching(#'inherit) = #'inherit,
                 content :: SlideContent, ...)
  fun slide.center(~sep: sep :: Real || matching(#'inherit) = #'inherit,
                   content, ...)
  fun slide.right(~sep: sep :: Real || matching(#'inherit) = #'inherit,
                  content :: SlideContent, ...)
){

 Constructors for slide descriptions hat are recognized by
 @rhombus(slide) and @rhombus(slide_pict). See @rhombus(slide) for
 more information.

 The @rhombus(slide.left), @rhombus(slide.center), and
 @rhombus(slide.right) functions are shorthands for @rhombus(slide.horiz)
 with a specific @rhombus(~horiz) argument.

}

@doc(
  Parameter.def current_assembler :: Function.of_arity(3)
){

 A context parameter for a function used to combine a slide title, layout mode,
 and content pict. The title can be a pict or @rhombus(#false), and the
 layout mode is a value that satisfies @rhombus(SlideLayout, ~annot).

 The default slide assembler vertically combines a title with the pict
 content using @rhombus(slide.gap) space for the @rhombus(#'tall) layout
 and @rhombus(2*slide.gap) space for the @rhombus(#'top) layout. In
 @rhombus(#'center) mode, the content pict is centered with respect to
 the full slide client area, and then the title is combined with
 @rhombus(overlay) and @rhombus(#'top) alignment. The @rhombus(#'auto)
 mode is treated like @rhombus(#'tall) or @rhombus(#'center), whichever
 would cause the pict to appear lower in the client area.

}

@doc(
  Parameter.def current_title_font :: Font
){

 A context parameter for the font used by @rhombus(titlet).

}

@doc(
 fun titlet(content, ...) :: Pict
){

 Like @rhombus(t) from @rhombusmodname(pict/text), but using
 @rhombus(current_title_font()).

}

@doc(
 expr.macro 'titlely($content_expr, ...)'
 expr.macro 'titlely: $body; ...'
){

 Like @rhombus(boldly), etc., but for the slide title font configured
 via @rhombus(current_title_font).

}

@doc(
  fun nonarchival(pict ::Pict) :: Pict
  fun nonarchival(pict ::Pict, epoch :: Int) :: Pict
){

 Returns a pict that is like @rhombus(pict), but with metadata that
 indicates that the pict should be skipped in some epochs in condensed
 mode, which might be used when printing to PDF form. Metadata is
 installed via @rhombus(Pict.epoch_set_metadata) using the key
 @rhombus(#'nonarchival).

}

@doc(
  enum SlideLayout:
    auto
    center
    top
    tall
){

 Slide layout options used with @rhombus(slide) and
 @rhombus(current_assembler).

}

@doc(
  fun interactive(pict :: Pict,
                  callback :: Function.of_arity(1)) :: Pict
){

 Creates a pict that renders as @rhombus(pict) for printing or other
 non-interactive contexts, but in Slideshow GUI creates a floating window
 at the same position as the pict would appear, and @rhombus(callback) is
 called a window handle to initialize the window. The @rhombus(callback)
 function must produce a function of zero arguments, which is called to
 deinitialize the floating when when the slide changes so that the
 interactive pict is not longer on the screen.

}


@doc(
  fun is_printing() :: Boolean
  fun is_condensing() :: Boolean
){

 Functions that report whether slides are being collected to be printed
 (e.g,. to PDF) instead of shown interactively, and whether slides are
 being collected in ``condensed'' mode (which is normally enabled when
 printing, but also can be enabled separately).

}

@doc(
  fun start_at_recent()
){

 When slides are being accumulated to show interactively, causes the
 slide viewer to fast-forward to the most recently registered slide as
 soon as it's available.

}

@doc(
  fun retract_recent()
){

 Unregisters the most recently registered slide.

}
