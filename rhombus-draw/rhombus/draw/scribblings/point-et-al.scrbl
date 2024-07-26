#lang rhombus/scribble/manual
@(
  import:
    "common.rhm" open
    meta_label:
      lib("racket/draw.rkt"):
        expose:
          #{color-database<%>}
)

@(def x_sym = @rhombus(#'x))
@(def y_sym = @rhombus(#'y))
@(def width_sym = @rhombus(#'width))
@(def height_sym = @rhombus(#'height))
@(def point_sym = @rhombus(#'point))
@(def size_sym = @rhombus(#'size))

@title(~tag: "point-et-al"){Point, Size, and Rectangle}

@doc(
  class Point(x :: Real, y :: Real)
  annot.macro 'PointLike'
  annot.macro 'PointLike.to_point'
  fun PointLike.to_point(pt :: PointLike) :: Point
  def Point.zero :: Point = Point(0, 0)
){

 The @rhombus(Point, ~class) class represents a point in two dimensions.

 Methods that expect a point typically accept a value satisfying
 @rhombus(PointLike, ~annot), which is any of the following:

@itemlist(

 @item{@rhombus(Point, ~annot): a @rhombus(Point, ~class) instance;}

 @item{@rhombus(matching([_ :: Real, _ :: Real]), ~annot): a
  @rhombus(List) containing two @rhombus(Real, ~annot) values; or}

 @item{@rhombus(matching({#'x: _ :: Real, #'y: _ :: Real}), ~annot):
  a @rhombus(Map) containing at least the keys @x_sym and
  @y_sym, each mapped to a @rhombus(Real, ~annot) value.}

)

 The @rhombus(PointLike.to_point, ~annot) annotation is satified by any
 value that satisfis @rhombus(PointLike, ~annot), and the value is
 converted to an equivalent @rhombus(Point, ~class) object if it is not one
 already.

 The @rhombus(PointLike.to_point) function converts a
 @rhombus(PointLike, ~annot) value to a @rhombus(Point, ~class), like the
 @rhombus(PointLike.to_point, ~annot) annotation. An expression
 @rhombus(pt) with static information from @rhombus(PointLike, ~annot)
 can call @rhombus(PointLike.to_point(pt)) using @rhombus(pt.to_point()).

 @rhombus(Point.zero) is a @rhombus(Point, ~class) object with @rhombus(0)
 values.

}


@doc(
  class Size(width :: NonnegReal, height :: NonnegReal)
  annot.macro 'SizeLike'
  annot.macro 'SizeLike.to_size'
  fun SizeLike.to_size(sz :: SizeLike) :: Size
  def Size.zero :: Size = Size(0, 0)
){

 The @rhombus(Size, ~class) class represents a size in two dimensions.

 Methods that expect a size typically accept a value satisfying
 @rhombus(SizeLike, ~annot), which is any of the following:

@itemlist(

 @item{@rhombus(Size, ~annot); a @rhombus(Size, ~class) instance;}

 @item{@rhombus(matching([_ :: NonnegReal, _ :: NonnegReal]), ~annot):
  a @rhombus(List) containing two @rhombus(NonnegReal, ~annot)
  values; or}

 @item{@rhombus(matching({#'width: _ :: NonnegReal, #'height: _ :: NonnegReal}), ~annot):
  a @rhombus(Map) containing at least the keys @width_sym and
  @height_sym, each mapped to a @rhombus(NonnegReal, ~annot) value.}

)

 The @rhombus(SizeLike.to_size, ~annot) annotation,
 @rhombus(SizeLike.to_size) function, and @rhombus(Size.zero) value are
 anaologous to @rhombus(PointLike.to_point, ~annot),
 @rhombus(PointLike.to_point), and @rhombus(Point.zero).

}


@doc(
  class Rect(x :: Real, y :: Real,
             width :: NonnegReal, height :: NonnegReal):
    constructor
    | (x :: Real, y :: Real,
       width :: NonnegReal, height :: NonnegReal)
    | (point :: PointLike, size :: SizeLike)
  property (r :: Rect).point :: Point
  property (r :: Rect).size :: Size
  annot.macro 'RectLike'
  annot.macro 'RectLike.to_rect'
  fun RectLike.to_rect(sz :: RectLike) :: Rect
  def Rect.zero :: Rect = Rect(0, 0, 0, 0)
){

 The @rhombus(Rect) class represents a rectagular region, where
 @rhombus(x) and @rhombus(y) correspond to the top-left of the rectangle.
 The @rhombus(Rect.point) property produces @rhombus(x) and @rhombus(y)
 in a @rhombus(Point), while @rhombus(Rect.size) property produces
 @rhombus(width) and @rhombus(height) in a @rhombus(Size).

 Methods that expect a rectangle typically accept a value satisfying
 @rhombus(RectLike, ~annot), which is any of the following:

@itemlist(

 @item{a @rhombus(Rect, ~class) instance;}

 @item{@rhombus(matching([_ :: Real, _ :: Real, _ :: NonnegReal, _ :: NonnegReal]), ~annot):
  a @rhombus(List) containing two @rhombus(Real, ~annot) values for the top-left point
  followed by two @rhombus(NonnegReal, ~annot) values for the size;}

 @item{@rhombus(matching([_ :: PointLike, _ :: SizeLike]), ~annot):
  a @rhombus(List) containing a @rhombus(PointLike, ~annot) value
  followed by a @rhombus(SizeLike, ~annot) value;}

 @item{@rhombus(matching({#'x: _ :: Real, #'y: _ :: Real, #'width: _ :: NonnegReal, #'height: _ :: NonnegReal}), ~annot):
  a @rhombus(Map) containing at least the keys @x_sym,
  @y_sym, @width_sym, and @height_sym, where each of
  the first two are mapped to a @rhombus(Real, ~annot) value, and each of
  the last two are mapped to a @rhombus(NonnegReal, ~annot) value; or}

 @item{@rhombus(matching({#'point: _ :: PointLike, #'size: _ :: SizeLike}), ~annot):
  a @rhombus(Map) containing at least the keys @point_sym
  and @size_sym, where the first is mapped to a
  @rhombus(PointLike, ~annot) value and the second is mapped to a
  @rhombus(SizeLike, ~annot) value.}

)

 The @rhombus(RectLike.to_rect, ~annot) annotation,
 @rhombus(RectLike.to_rect) function, and @rhombus(Rect.zero) value are
 anaologous to @rhombus(PointLike.to_point, ~annot),
 @rhombus(PointLike.to_point), and @rhombus(Point.zero).

}
