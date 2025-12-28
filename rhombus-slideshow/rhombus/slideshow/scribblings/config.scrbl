#lang rhombus/scribble/manual

@(import:
    meta_label:
      rhombus open
      slideshow open
      draw.Font)

@title(~tag: "config"){Slide Configuration}

@doc(
  def slide.gap = 24
){

 The default separation used by @rhombus(slide) and @rhombus(slide_pict).

}

@doc(
  fun blank_client(
    ~aspect: aspect :: Aspect = #'widescreen,
    ~title: title :: maybe(String || Pict) = #false,
    ~layout: layout :: SlideLayout = #'auto
  ) :: Pict
){

 Creates a blank pict that corresponds to the ``client'' area of a
 slide, which is below the title (if any) and inset by a small margin.

 If @rhombus(title) is not @rhombus(#false), then @rhombus(title) and
 @rhombus(layout) determine a height for a client area that is consistent
 with the default value of @rhombus(current_assembler). The
 @rhombus(layout) argument matters only in whether it is @rhombus(#'tall)
 or not.

}

@doc(
  Parameter.def current_assembler
    :: (title :: False || Pict,
        layout:: SlideLayout,
        aspect :: Aspect,
        p :: Pict) -> Pict
){

 A context parameter for a function used to combine a slide title, layout mode,
 aspect, and content pict to produce an overall pict.

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
  enum SlideLayout
  | auto
  | center
  | top
  | tall
){

 Slide layout options used with @rhombus(slide) and
 @rhombus(current_assembler).

}

@doc(
  enum Aspect
  | widescreen
  | fullscreen
){

 Slide aspect options used with @rhombus(slide) and
 @rhombus(current_assembler). The @rhombus(#'widescreen) apsect is 16:9
 as 1360 by 766 in drawing units. The @rhombus(#'fullscreen) aspect if
 4:3 as 1024 by 768 in drawing units.

}
