#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Bitmap}

@doc(
  class Bitmap():
    constructor (
      width :: PosInt,
      height :: PosInt,
      ~backing_scale: backing_space :: Real.above(0.0) = 1,
      ~has_color: has_color :: Any = #true,
      ~has_alpha: has_alpha :: Any = #true,
    )
){

}

@doc(
  property (bm :: Bitmap).width :: PosInt
  property (bm :: Bitmap).height :: PosInt
  property (bm :: Bitmap).size :: Size
  property (bm :: Bitmap).backing_scale :: Real.above(0.0)
  property (bm :: Bitmap).depth :: NonnegInt
  property (bm :: Bitmap).has_color :: Boolean
  property (bm :: Bitmap).has_alpha :: Boolean
  property (bm :: Bitmap).is_ok :: Boolean
){

 Properties to access bitmap components. The @rhombus(Bitmap.size)
 property combines the @rhombus(Bitmap.width) and @rhombus(Bitmap.height)
 properties.

}

@doc(
  method (bm :: Bitmap).make_dc() :: DC
){

 Creates a drawing context that writes to the bitmap.

}

@doc(
  method (bm :: Bitmap).argb_pixels(
    ~x: x :: NonnegInt = 0,
    ~y: y :: NonnegInt = 0,
    ~width: width :: NonnegInt = width,
    ~height: height :: NonnegInt = height,
    ~dest: dest :: Bytes = Bytes.make(width * height * 4),
  ) :: Bytes
){

 Copies the bitmap content into @rhombus(dest) and returns it.

}

@doc(
  method (bm :: Bitmap).set_argb_pixels(
    src :: Bytes,
    ~x: x :: NonnegInt = 0,
    ~y: y :: NonnegInt = 0,
    ~width: width :: NonnegInt = width,
    ~height: height :: NonnegInt = height,
  ) :: Void
){

 Sets a region of the bitmap to @rhombus(src).

}

@doc(
  method (bm :: Bitmap).write(
    dest :: Path,
    ~kind: kind :: Any.of(#'png, #'jpeg, #'xbm, #'xpm, #'bmp),
    ~quality: quality :: Int.in(0, 100) = 75,
    ~as_unscaled: as_unscaled :: Any = #false,
  ) :: Void
){

 Writes the bitmap to a file.

}

@doc(
  fun Bitmap.from_file(path :: String || Path) :: Bitmap
){

  Reads a bitmap from @rhombus(path).

}
