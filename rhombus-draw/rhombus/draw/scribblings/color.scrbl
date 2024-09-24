#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      lib("racket/draw.rkt") open:
        only:
          #{color-database<%>})

@title{Color}

@doc(
  class draw.Color():
    constructor
    | (name :: String)
    | (red :: Byte, green :: Byte, blue :: Byte)
    | (red :: Byte, green :: Byte, blue :: Byte, alpha :: Real.in(0.0, 1.0))
){

 When @rhombus(name) is given, it must be one of the predefined names
 listed with @rhombus(#{color-database<%>}).

 When @rhombus(alpha) is not supplied, @rhombus(1.0) is used.

 Color like an existing one can be constructed using @rhombus(with)
 and the field names @rhombus(red), @rhombus(green), @rhombus(blue),
 and/or @rhombus(alpha).

}

@doc(
  property (col :: draw.Color).red :: Byte
  property (col :: draw.Color).green :: Byte
  property (col :: draw.Color).blue :: Byte
  property (col :: draw.Color).alpha :: Real.in(0, 1)
){

 Properties to access color components.

}

@doc(
  method (col :: draw.Color).scale(factor :: NonnegReal) :: draw.Color
){

 Scales a color, making it brighter or darker. If @rhombus(factor) is
 less than @rhombus(1), the color is darkened by multiplying the red,
 green, and blue components by @rhombus(factor). If @rhombus(factor) is
 greater than @rhombus(1), the color is lightened by dividing the gap
 between the red, green, and blue components and @rhombus(255) by
 @rhombus(factor).

}

@doc(
  method (col :: draw.Color).blend(other :: draw.Color) :: draw.Color
){

 Blends two colors to produce a new one. Each red, green, and blue
 component contibutes to the corresponding blended component by its
 weighted average, where each color's alpha is is weight. The result
 color's alpha is the average of the two color's alphas.

}

@doc(
  property (col :: draw.Color).handle :: Any
  fun draw.Color.from_handle(hand :: Any) :: draw.Color
){

 The @rhombus(draw.Color.handle) property returns a Racket object that
 corresponds to the drawing context for use directly with
 @racketmodname(racket/draw). The @rhombus(draw.Color.from_handle) function
 creates a @rhombus(draw.Color, ~class) from such a Racket object.

}

