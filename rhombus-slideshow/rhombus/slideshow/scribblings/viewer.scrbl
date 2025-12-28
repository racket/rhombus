#lang rhombus/scribble/manual

@(import:
    meta_label:
      rhombus open
      slideshow open
      draw.Font)

@title(~tag: "viewer"){Interacting with the Viewer}

@doc(
  fun is_printing() :: Boolean
  fun is_condensing() :: Boolean
){

 Functions that report whether slides are being collected to be printed
 (e.g,. to PDF) instead of shown interactively, and whether slides are
 being collected in @deftech{condensed mode} (which is normally enabled when
 printing, but also can be enabled separately).

 These modes are enabled through command-line arguments. See
 @secref("cmdline").

}

@doc(
  fun start_at_recent()
){

 When slides are being accumulated to show interactively, causes the
 slide viewer to fast-forward to the most recently registered slide as
 soon as it's available.

}

@doc(
  fun nonarchival(pict ::Pict) :: Pict
  fun nonarchival(pict ::Pict, epoch :: Int) :: Pict
){

 Returns a pict that is like @rhombus(pict), but with metadata that
 indicates that the pict should be skipped in some epochs in @tech{condensed mode},
 which might be used when printing to PDF form. Metadata is
 installed via @rhombus(Pict.epoch_set_metadata) using the key
 @rhombus(#'nonarchival).

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
