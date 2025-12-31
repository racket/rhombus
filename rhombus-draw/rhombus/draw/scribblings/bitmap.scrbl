#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "bitmap"){Bitmaps}

@doc(
  class draw.Bitmap():
    constructor (
      size :: SizeLike,
      ~backing_scale: backing_scale :: PosReal = 1,
      ~has_color: has_color :: Any = #true,
      ~has_alpha: has_alpha :: Any = #true
    )
){

 Creates a bitmap of the given size in drawing units. If
 @rhombus(has_color) is @rhombus(#false), then the result is a monochrome
 bitmap and @rhombus(has_alpha) is ignored. If @rhombus(has_color) and
 @rhombus(has_alpha) are both true, then the bitmap includes an alpha
 channel to record the opacity of each pixel in the bitmap. The initial
 content of the bitmap is ``empty'': all white, and with zero opacity in
 the case of a bitmap with an alpha channel.

 The @rhombus(backsing_scale) argument determines the bitmap's
 @deftech{backing scale}, which is the number of pixels that correspond
 to a drawing unit for the bitmap, either when the bitmap is used as a
 target for drawing or when the bitmap is drawn into another context. A
 monochrome bitmap always has a backing scale of @rhombus(1.0).

 A bitmap is convertible to @rhombus(#'#{png-bytes}) through the
 @racketmodname(file/convertible, ~indirect) protocol.

}

@doc(
  fun draw.Bitmap.make_platform(
      size :: SizeLike,
      ~backing_scale: backing_space :: PosReal = 1
  ) :: Bitmap
){

 Creates a bitmap that uses platform-specific drawing operations as much
 as possible, which is different than a @rhombus(Bitmap) constructor
 result on Windows and Mac OS. See @secref("portability") for more
 information.

}

@doc(
  property (bm :: draw.Bitmap).width :: PosInt
  property (bm :: draw.Bitmap).height :: PosInt
  property (bm :: draw.Bitmap).size :: Size
  property (bm :: draw.Bitmap).backing_scale :: Real.above(0.0)
  property (bm :: draw.Bitmap).depth :: Nat
  property (bm :: draw.Bitmap).has_color :: Boolean
  property (bm :: draw.Bitmap).has_alpha :: Boolean
  property (bm :: draw.Bitmap).is_ok :: Boolean
){

 Properties to access bitmap components. The @rhombus(Bitmap.size)
 property combines the @rhombus(Bitmap.width) and @rhombus(Bitmap.height)
 properties. The @rhombus(Bitmap.depth) property returns the number of
 bits used to represent each pixel, so it is @rhombus(1) for a monochrome
 bitmap.

}

@doc(
  method (bm :: draw.Bitmap).make_dc() :: DC
){

 Creates a drawing context whose destination is the bitmap.

}

@doc(
  method (bm :: draw.Bitmap).argb_pixels(
    ~x: x :: Nat = 0,
    ~y: y :: Nat = 0,
    ~width: width :: Nat = width,
    ~height: height :: Nat = height,
    ~dest: dest :: Bytes = Bytes.make(width * height * 4),
    ~just_alpha: just_alpha :: Any.to_boolean = #false,
    ~premultiplied: premultiplied :: Any.to_boolean = #false,
    ~unscaled: unscaled :: Any.to_boolean = #false
  ) :: Bytes
){

 Copies a rectangle of bitmap content into @rhombus(dest) and returns
 @rhombus(dest).

 The pixel red, green blue, and alpha values are copied into
 @rhombus(dest) (or just alpha values if @rhombus(just_alpha) is true).
 The first byte represents an alpha value of the pixel at (@rhombus(x),
 @rhombus(y)), the second byte represents a red value of the pixel at
 (@rhombus(x), @rhombus(y)), the third byte is the green value, etc. In
 this way, the first @rhombus(width*height*4) bytes of @rhombus(dest) are
 set to reflect the current pixel values in the DC. The pixels are in
 row-major order: left to right then top to bottom.

 If the bitmap has an alpha channel, then the alpha value for each pixel
 is always set in @rhombus(dest). If @rhombus(just_alpha) is false and
 the bitmap does not have an alpha channel, then the alpha value for each
 pixel is set to @rhombus(255). If @rhombus(just_alpha) is true, then
 only the alpha value is set for each pixel; if the bitmap has no alpha
 channel, then the alpha value is based on each pixel's inverted
 red-green-blue average.

 If @rhombus(premultiplied) is true, @rhombus(just_alpha) is false, and
 the bitmap has an alpha channel, then red, green, and blue values in the
 result are scaled by the corresponding alpha value (i.e., multiplied by
 the alpha value and then divided by 255).

 If the bitmap has a @tech{backing scale} other than @rhombus(1.0) and
 @rhombus(unscaled) is @rhombus(#false), the result of
 @rhombus(Bitmap.argb_pixels) is as if the bitmap is drawn to a bitmap
 with a backing scale of @rhombus(1.0) and the pixels of the target
 bitmap are returned.

}

@doc(
  method (bm :: draw.Bitmap).set_argb_pixels(
    src :: Bytes,
    ~x: x :: Nat = 0,
    ~y: y :: Nat = 0,
    ~width: width :: Nat = width,
    ~height: height :: Nat = height,
    ~just_alpha: just_alpha :: Any.to_boolean = #false,
    ~premultiplied: premultiplied :: Any.to_boolean = #false,
    ~unscaled: unscaled :: Any.to_boolean = #false
  ) :: Void
){

 The reverse of @rhombus(Bitmap.argb_pixels): adjusts @rhombus(bm) so
 that @rhombus(Bitmap.argb_pixels) with all of the same arguments would
 produce the content in @rhombus(src) as its result, or as close as
 possible.

}

@doc(
  method (bm :: draw.Bitmap).write(
    out :: PathString || Port.Output,
    ~kind: kind :: Any.of(#'png, #'jpeg, #'xbm, #'xpm, #'bmp),
    ~quality: quality :: Int.in(0 ..= 100) = 75,
    ~as_unscaled: as_unscaled :: Any = #false,
  ) :: Void
){

 Writes the bitmap to @rhombus(out). The @rhombus(kind) argument
 determines the file format used.

 The @rhombus(quality) argument applies only to the @rhombus(#'jpeg)
 format, in which case it specifies the trade-off between image precision
 (high quality matches the content of the @rhombus(Bitmap, ~annot) object
 more precisely) and size (low quality is smaller).

 The @rhombus(as_unscaled) argument is relevant when the bitmap has a
 @tech{backing scale} other than @rhombus(1.0). In that case, if
 @rhombus(as_unscaled) is @rhombus(#false), the bitmap is effectively
 converted to a single pixel per drawing unit before writing.

}

@doc(
  fun draw.Bitmap.read(in :: PathString || Port.Input) :: Bitmap
){

 Reads a bitmap from @rhombus(in). The file format is detected
 automatically among the same formats as recognized by
 @rhombus(Bitmap.write).

}

@doc(
  property (path :: draw.Bitmap).handle :: Any
  fun draw.Bitmap.from_handle(hand :: Any) :: Bitmap
){

 The @rhombus(Bitmap.handle) property returns a Racket object that
 corresponds to the bitmap for use directly with
 @racketmodname(racket/draw). The @rhombus(Bitmap.from_handle) function
 creates a @rhombus(Bitmap, ~class) from such a Racket object.

}
