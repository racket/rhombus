#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    meta_label:
      lib("racket/draw.rkt"):
        expose:
          #{color-database<%>})

@title{Color}

@doc(
  class Color():
    constructor
    | (name :: String)
    | (red :: Byte, green :: Byte, blue :: Byte)
    | (red :: Byte, green :: Byte, blue :: Byte,
       alpha :: Real.in(0.0, 1.0))
){

 When @rhombus(name) is given, it must be one of the predefined names
 listed with @rhombus(#{color-database<%>}).

 When @rhombus(alpha) is not supplied, @rhombus(1.0) is used.

 Color like an existing one can be constructed using @rhombus(with)
 and the field names @rhombus(red), @rhombus(green), @rhombus(blue),
 and/or @rhombus(alpha).

}

@doc(
  property (col :: Color).red :: Byte
  property (col :: Color).green :: Byte
  property (col :: Color).blue :: Byte
  property (col :: Color).alpha :: Real.in(0, 1)
){

 Properties to access color components.

}

@doc(
  method (col :: Color).scale(factor :: NonnegReal) :: Color
){

 Scales a color, making it brighter or darker. If @rhombus(factor) is
 less than @rhombus(1), the color is darkened by multiplying the red,
 green, and blue components by @rhombus(factor). If @rhombus(factor) is
 greater than @rhombus(1), the color is lightened by dividing the gap
 between the red, green, and blue components and @rhombus(255) by
 @rhombus(factor).

}

@doc(
  method (col :: Color).blend(other :: Color) :: Color
){

 Blends two colors to produce a new one. Each red, green, and blue
 component contibutes to the corresponding blended component by its
 weighted average, where each color's alpha is is weight. The result
 color's alpha is the average of the two color's alphas.

}
