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
  def slide.next
  def slide.sync
  fun slide.alts(~vert: vert :: pict.VertAlignment = #'top,
                 [content :: SlideContent, ...], ...)
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

 Constructors for slide descriptions that are recognized by
 @rhombus(slide) and @rhombus(slide_pict). See @rhombus(slide) for
 more information.

 The @rhombus(slide.horiz) functions is a shorthand for
 @rhombus(slide.align) with @rhombus(~local: #false).

 The @rhombus(slide.left), @rhombus(slide.center), and
 @rhombus(slide.right) functions are shorthands for @rhombus(slide.pict)
 with a specific @rhombus(~horiz) argument and with the @rhombus(~local)
 argument defaulting to @rhombus(#false) instead of @rhombus(#true).

}

@doc(
  fun retract_recent() :: Slide
  class Slide():
    constructor ~none
  method (s :: Slide).reissue() :: Void
  property (s :: Slide).handle
){

 The @rhombus(retract_recent) function unregisters the most recently
 registered slide and returns a representation of that slide as a
 @rhombus(Slide, ~class) object. The @rhombus(Slide.reissue) method
 registers a copy of the slide. Together, the function and method provide
 limited support for post-hoc reordering of slides, but beware that each
 step of an animation counts as a different slide for these operations.

}
