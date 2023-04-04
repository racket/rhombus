#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Bitmap}

@doc(
  class Bitmap():
    constructor (width :: PosInt,
                 height :: PosInt,
                 ~backing_scale: backing_space :: Real.above(0.0) = 1,
                 ~has_color = #true,
                 ~has_alpha = #true)
){

}

@doc(
  property Bitmap.width(bm :: Bitmap) :: PosInt
  property Bitmap.height(bm :: Bitmap) :: PosInt
  property Bitmap.backing_scale(bm :: Bitmap) :: Real.above(0.0)
  property Bitmap.depth(bm :: Bitmap) :: NonnegInt
  property Bitmap.has_color(bm :: Bitmap) :: Boolean
  property Bitmap.has_alpha(bm :: Bitmap) :: Boolean
  property Bitmap.is_ok(bm :: Bitmap) :: Boolean
){

 Properties to access bitmap components.

}

@doc(
  method Bitmap.make_dc(bm :: Bitmap) :: DC
){

 Creates a drawing context that writes to the bitmap.

}

@doc(
  method Bitmap.argb_pixels(
    bm :: Bitmap,
    ~x: x :: NonnegInt = 0,
    ~y: y :: NonnegInt = 0,
    ~width: width :: NonnegInt = width,
    ~height: height :: NonnegInt = height,
    ~dest: dest :: Bytes = Bytes.make(width * height * 4)
  ) :: Bytes
){

 Copies the bitmap content into @rhombus(dest) and returns it.

}

@doc(
  method Bitmap.write(bm :: Bitmap,
                      dest :: Path,
                      ~kind: kind :: Any.of(#'png, #'jpeg, #'xbm,
                                            #'xpm, #'bmp),
                      ~quality: quality :: Int.in(0, 100) = 75,
                      ~as_unscaled: as_unscaled :: Boolean = #false)
    :: Void
){

 Writes the bitmap to a file.

}
