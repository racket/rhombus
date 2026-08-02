#lang rhombus/scribble/manual

@(import:
    meta_label:
      rhombus open
      slideshow open
      pict.Pict
      pict.StaticPict
      draw.Font)

@title(~tag: "slide"){Creating Slides}

@doc(
  annot.macro 'SlideContent'
){

 Satisfied by allowed arguments to @rhombus(slide): a @rhombus(Pict, ~annot),
 @rhombus(slide.next), @rhombus(slide.sync), a value produced by @rhombus(slide.alts)
 or @rhombus(slide.align), or a list of values
 that satisfy @rhombus(SlideContent, ~annot).

}

@doc(
  fun slide(~title: title :: maybe(String || Pict) = #false,
            ~name: name = title,
            ~layout: layout :: SlideLayout = #'auto,
            ~aspect: aspect :: Aspect = #'widescreen,
            ~sep: sep :: Real = slide.gap,
            ~horiz: horiz_align :: HorizAlignment = #'center,
            ~lead_in: lead_in = #false,
            content :: SlideContent, ...) :: Void
){

 Registers one or more slides. In the simple case, each
 @rhombus(content) produces a pict, and the picts are combined with
 @rhombus(stack) with a separation of @rhombus(sep) and the horizontal
 alignment @rhombus(horiz_align). A slide is produced for each epoch in
 the resulting pict's duration, and extra transitions slides are
 registered for epochs that have a non-@rhombus(0) extent. If
 @rhombus(lead_in) is true, then slides are also registered for the
 transition from epoch @rhombus(-1).

 The @rhombus(title), @rhombus(layout), and @rhombus(aspect) arguments are used to combine
 content picts via the @rhombus(current_assembler) parameter's value.
 See also @rhombus(slide_pict), which performs that combination
 without registering a slide.

 Besides immediate picts, the @rhombus(content) values can produce
 descriptions of slides to construct using lists, @rhombus(slide.next), @rhombus(slide.sync),
 and @rhombus(slide.alts)
 (or one of its shorthands: @rhombus(slide.horiz), @rhombus(slide.left), @rhombus(slide.center),
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
  @rhombus(switch), but the bounding boxes for all alternatives are
  preserved for the combined duration.)

  The @rhombus(overlay) combination
  uses the vertical alignment that is supplied to @rhombus(slide.alts), while
  its horizontal alignment and vertical spacing depends on an enclosing @rhombus(slide.align)
  (or one of the @rhombus(slide.left), @rhombus(slide.horiz),
  @rhombus(slide.center), or @rhombus(slide.right) aliases). Spacing and alignment
  default to @rhombus(slide.gap) and @rhombus(#'center), respectively, if
  @rhombus(slide.align) or @rhombus(slide.horiz) is enclosing.

  A @rhombus(slide.next) or @rhombus(slide.sync) can be used in
  any alternative with @rhombus(slide.alts), and @rhombus(slide.alts) can be nested. In either of
  those cases, the corresponding alternative will itself be a multi-epoch
  pict.}

 @item{A value produced by @rhombus(slide.align) causes all picts as
  arguments to @rhombus(slide.align) to get the same width by padding on
  the left, right, or both. This padding applies to picts in nested
  @rhombus(slide.alts) alternatives as well as nested
  @rhombus(slide.horiz) constructions---but for nested
  @rhombus(slide.align) constructions with local alignment (which is the
  case unless @rhombus(~local: #false) is provided), padding applies only
  after the nested @rhombus(slide.align) applies it own
  padding.

  The @rhombus(~horiz) argument to @rhombus(slide.align)
  determines how padding is added by default to contained elements, but
  nested @rhombus(slide.align) constructions can change alignment. Supplying
  @rhombus(#'inherit) for the @rhombus(~sep) or @rhombus(~horiz) argument to @rhombus(slide.align)
  means that vertical spacing and alignment are determined by an enclosing @rhombus(slide.align) or
  @rhombus(slide.horiz), defaulting to @rhombus(slide.gap) and @rhombus(#'left) is none is enclosing.}

)

 When a pict representing a slide has the @rhombus(#'nonarchival) key
 mapped to a true value in its metadata for some epoch, then the slide
 for that epoch is skipped in @tech{condensed mode}---as are any slides for
 transitions in the epoch. See also @rhombus(nonarchival).

 When a pict representing a slide has the @rhombus(#'continued) key
 mapped to a true value in its metadata for some epoch, then the slide
 for that epoch does not increment the page number that the slideshow
 viewer or printer displays.

@(history:
    ~changed "1.1": @elem{Added @rhombus(#'continued) support.})

}

@doc(
  fun slide_pict(~title: title :: maybe(String || Pict) = #false,
                 ~layout: layout :: SlideLayout = #'auto,
                 ~aspect: aspect :: Aspect = #'widescreen,
                 ~sep: sep :: Real = slide.gap,
                 ~horiz: horiz_align :: HorizAlignment = #'center,
                 ~full: full = title && #true,
                 content :: SlideContent, ...) :: Pict
){

 Like @rhombus(slide), except that the result is a pict (potentially with
 a multi-epoch duration) instead of registering a slide.

 If @rhombus(full) is a true value, then the resulting pict represents a slide
 including its title as combined via @rhombus(current_assembler).
 Otherwise, @rhombus(current_assembler) is not used, and the resulting
 pict is just the result of combining the @rhombus(content) values.

}

@doc(
  def slide.next :: slide.Next
  def slide.sync :: slide.Next
  class slide.Next():
    constructor ~none
  method (n :: slide.Next).continued() :: slide.Next
){

 The @rhombus(slide.next) and @rhombus(slide.sync) descriptions are
 recognized by @rhombus(slide) and @rhombus(slide_pict). See
 @rhombus(slide) for more information.

 The last epoch of the pict created for the step before
 @rhombus(slide.next) or @rhombus(slide.sync) is suppressed for condensed
 mode (including printing) via @rhombus(nonarchival). The
 @rhombus(slide.Next.continued) method creates a variant of
 @rhombus(slide.next) or @rhombus(slide.sync) that also suppresses a new
 page number for the last epoch by using @rhombus(continued).

@(history:
    ~changed "1.1": @elem{Added @rhombus(slide.Next, ~class)
                          and the @rhombus(slide.Next.continued) method.})

}

@doc(
  fun slide.alts(~vert: vert :: pict.VertAlignment = #'top,
                 [content :: SlideContent, ...], ...)
    :: slide.Alts

  class slide.Alts():
    constructor ~none

  method (a :: slide.Alts).continued() :: slide.Alts
){

 The @rhombus(slide.alts) function constructs a description that is
 recognized by @rhombus(slide) and @rhombus(slide_pict). See
 @rhombus(slide) for more information.

 The @rhombus(slide.Alts.continued) method produces an adjusted
 description that suppress a new page number of the last epoch of all but
 the last alternative pict in @rhombus(content) by using
 @rhombus(continued).

@(history:
    ~changed "1.1": @elem{Added @rhombus(slide.Alts, ~class)
                          and the @rhombus(slide.Alts.continued) method.})

}

@doc(
  fun slide.align(~sep: sep :: slide.Sep = #'inherit,
                  ~horiz: horiz :: slide.HorizAlignment = #'left,
                  ~local: local :: Any.to_boolean = #true,
                  content :: SlideContent, ...)
  fun slide.horiz(~sep: sep :: slide.Sep = #'inherit,
                  ~horiz: horiz :: slide.HorizAlignment = #'left,
                  content :: SlideContent, ...)
  fun slide.left(~sep: sep :: slide.Sep = #'inherit,
                 ~local: local :: Any.to_boolean = #false,
                 content :: SlideContent, ...)
  fun slide.center(~sep: sep :: slide.Sep = #'inherit,
                   ~local: local :: Any.to_boolean = #false,
                   content, ...)
  fun slide.right(~sep: sep :: slide.Sep = #'inherit,
                  ~local: local :: Any.to_boolean = #false,
                  content :: SlideContent, ...)

  enum slide.Sep
  | ~is_a Real
  | inherit
  enum slide.HorizAlignment
  | ~is_a pict.HorizAlignment
  | inherit
){

 Constructors for descriptions that are recognized by @rhombus(slide)
 and @rhombus(slide_pict). See @rhombus(slide) for more information.

 The @rhombus(slide.horiz) functions is a shorthand for
 @rhombus(slide.align) with @rhombus(~local: #false).

 The @rhombus(slide.left), @rhombus(slide.center), and
 @rhombus(slide.right) functions are shorthands for @rhombus(slide.pict)
 with a specific @rhombus(~horiz) argument and with the @rhombus(~local)
 argument defaulting to @rhombus(#false) instead of @rhombus(#true).

}

@doc(
  fun slide_transition(
    transition :: (StaticPict, Pict, Map) -> (Pict, Boolean)
  ) :: Void
){

 Registers @rhombus(transition) to be invoked on the next call to
 @rhombus(slide). The @rhombus(transition) function is called with a an
 assembled @rhombus(StaticPict, ~annot) for the previous slide, an
 assembled @rhombus(Pict, ~annot) for the new slide, and a
 @rhombus(Map, ~annot) describing extra properties of the old an new
 slide. The result is a @rhombus(Pict, ~annot) to be used in place of the
 given one for a new slide, plus a @rhombus(Boolean, ~annot) that is used
 in place of the @rhombus(~lead_in) argument for the new slide (see
 @rhombus(slide)). A @rhombus(transition) function is used only once
 per registration via @rhombus(slide_transition).

 The @rhombus(StaticPict, ~annot) for the previous slide corresponds to
 a snapshot at the end of the last epoch for the previous slide. The
 @rhombus(Pict, ~annot) for the new slide can be animated with multiple
 epochs, reflecting the case that a single call to @rhombus(slide)
 generates a animated or multi-epoch @rhombus(Pict, ~annot). Both the
 @rhombus(StaticPict, ~annot) and @rhombus(Pict, ~annot) are assembled in
 the sense that they have been combined along with their titles, if any,
 as by @rhombus(slide_pict).

 The @rhombus(Map, ~annot) argument to @rhombus(transition) provides
 additional information through the following keys (with more
 potentially added in the future):

@itemlist(

 @item{@rhombus(#'prev_name): The name (defaults to the title) for the
  previous slide.}

 @item{@rhombus(#'name): The name (defaults to the title) that will be
  used for the new slide.}

 @item{@rhombus(#'lead_in): The value of the @rhombus(~lead_in) argument
  for the @rhombus(slide) call that triggered a new slide.}

 @item{@rhombus(#'prev_aspect): An aspect ration as an
  @rhombus(Aspect, ~annot) for the previous slide.}

 @item{@rhombus(#'aspect): An aspect ration as an
  @rhombus(Aspect, ~annot) for the new slide.}

)

 A @rhombus(transition) function can call @rhombus(slide) and/or
 @rhombus(slide_transition). If it calls @rhombus(slide), then newly
 created slides will appear before the result of @rhombus(transition). If
 it calls @rhombus(slide_transition), then the registered function is
 invoked on the next call to @rhombus(slide) and not to the result of the
 @rhombus(transition) function.

@(history: ~added "1.1")

}

@doc(
  fun retract_recent() :: Slide
  class Slide():
    constructor ~none
  method (s :: Slide).reissue() :: Void
  property (s :: Slide).handle
){

 The @rhombus(retract_recent) function unregisters the most recently
 registered slide instant and returns a representation of that slide as a
 @rhombus(Slide, ~class) object. The @rhombus(Slide.reissue) method
 registers a copy of the slide. Together, the function and method provide
 limited support for post-hoc reordering of slides, but beware that each
 step of an animation counts as a different slide instant.

}
