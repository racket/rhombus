#lang rhombus/scribble/manual
@(
  import:
    "common.rhm" open
    meta_label:
      lib("racket/draw.rkt") open:
        only:
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
  class draw.Point(x :: Real, y :: Real)
  annot.macro 'draw.PointLike'
  annot.macro 'draw.PointLike.to_point'
  fun draw.PointLike.to_point(pt :: draw.PointLike) :: draw.Point
  def draw.Point.zero :: draw.Point = draw.Point(0, 0)
){

 The @rhombus(draw.Point, ~class) class represents a point in two dimensions.

 Methods that expect a point typically accept a value satisfying
 @rhombus(draw.PointLike, ~annot), which is any of the following:

@itemlist(

 @item{@rhombus(draw.Point, ~annot): a @rhombus(draw.Point, ~class) instance;}

 @item{@rhombus(matching([_ :: Real, _ :: Real]), ~annot): a
  @rhombus(List) containing two @rhombus(Real, ~annot) values; or}

 @item{@rhombus(matching({#'x: _ :: Real, #'y: _ :: Real}), ~annot):
  a @rhombus(Map) containing at least the keys @x_sym and
  @y_sym, each mapped to a @rhombus(Real, ~annot) value.}

)

 The @rhombus(draw.PointLike.to_point, ~annot) annotation is satified by any
 value that satisfis @rhombus(draw.PointLike, ~annot), and the value is
 converted to an equivalent @rhombus(draw.Point, ~class) object if it is not one
 already.

 The @rhombus(draw.PointLike.to_point) function converts a
 @rhombus(draw.PointLike, ~annot) value to a @rhombus(draw.Point, ~class), like the
 @rhombus(draw.PointLike.to_point, ~annot) annotation. An expression
 @rhombus(pt) with static information from @rhombus(draw.PointLike, ~annot)
 can call @rhombus(draw.PointLike.to_point(pt)) using @rhombus(pt.to_point()).

 @rhombus(draw.Point.zero) is a @rhombus(draw.Point, ~class) object with @rhombus(0)
 values.

}


@doc(
  class draw.Size(width :: NonnegReal, height :: NonnegReal)
  annot.macro 'draw.SizeLike'
  annot.macro 'draw.SizeLike.to_size'
  fun draw.SizeLike.to_size(sz :: draw.SizeLike) :: draw.Size
  def draw.Size.zero :: draw.Size = draw.Size(0, 0)
){

 The @rhombus(draw.Size, ~class) class represents a size in two dimensions.

 Methods that expect a size typically accept a value satisfying
 @rhombus(draw.SizeLike, ~annot), which is any of the following:

@itemlist(

 @item{@rhombus(draw.Size, ~annot); a @rhombus(draw.Size, ~class) instance;}

 @item{@rhombus(matching([_ :: NonnegReal, _ :: NonnegReal]), ~annot):
  a @rhombus(List) containing two @rhombus(NonnegReal, ~annot)
  values; or}

 @item{@rhombus(matching({#'width: _ :: NonnegReal, #'height: _ :: NonnegReal}), ~annot):
  a @rhombus(Map) containing at least the keys @width_sym and
  @height_sym, each mapped to a @rhombus(NonnegReal, ~annot) value.}

)

 The @rhombus(draw.SizeLike.to_size, ~annot) annotation,
 @rhombus(draw.SizeLike.to_size) function, and @rhombus(draw.Size.zero) value are
 anaologous to @rhombus(draw.PointLike.to_point, ~annot),
 @rhombus(draw.PointLike.to_point), and @rhombus(draw.Point.zero).

}


@doc(
  class draw.Rect(x :: Real, y :: Real,
                  width :: NonnegReal, height :: NonnegReal):
    constructor
    | (x :: Real, y :: Real,
       width :: NonnegReal, height :: NonnegReal)
    | (point :: draw.PointLike, size :: draw.SizeLike)
  property (r :: draw.Rect).point :: draw.Point
  property (r :: draw.Rect).size :: draw.Size
  annot.macro 'draw.RectLike'
  annot.macro 'draw.RectLike.to_rect'
  fun draw.RectLike.to_rect(sz :: draw.RectLike) :: draw.Rect
  def draw.Rect.zero :: draw.Rect = draw.Rect(0, 0, 0, 0)
){

 The @rhombus(draw.Rect, ~class) class represents a rectagular region, where
 @rhombus(x) and @rhombus(y) correspond to the top-left of the rectangle.
 The @rhombus(draw.Rect.point) property produces @rhombus(x) and @rhombus(y)
 in a @rhombus(draw.Point, ~class), while @rhombus(draw.Rect.size) property produces
 @rhombus(width) and @rhombus(height) in a @rhombus(draw.Size, ~class).

 Methods that expect a rectangle typically accept a value satisfying
 @rhombus(draw.RectLike, ~annot), which is any of the following:

@itemlist(

 @item{a @rhombus(draw.Rect, ~class) instance;}

 @item{@rhombus(matching([_ :: Real, _ :: Real, _ :: NonnegReal, _ :: NonnegReal]), ~annot):
  a @rhombus(List) containing two @rhombus(Real, ~annot) values for the top-left point
  followed by two @rhombus(NonnegReal, ~annot) values for the size;}

 @item{@rhombus(matching([_ :: draw.PointLike, _ :: draw.SizeLike]), ~annot):
  a @rhombus(List) containing a @rhombus(draw.PointLike, ~annot) value
  followed by a @rhombus(draw.SizeLike, ~annot) value;}

 @item{@rhombus(matching({#'x: _ :: Real, #'y: _ :: Real, #'width: _ :: NonnegReal, #'height: _ :: NonnegReal}), ~annot):
  a @rhombus(Map) containing at least the keys @x_sym,
  @y_sym, @width_sym, and @height_sym, where each of
  the first two are mapped to a @rhombus(Real, ~annot) value, and each of
  the last two are mapped to a @rhombus(NonnegReal, ~annot) value; or}

 @item{@rhombus(matching({#'point: _ :: draw.PointLike, #'size: _ :: draw.SizeLike}), ~annot):
  a @rhombus(Map) containing at least the keys @point_sym
  and @size_sym, where the first is mapped to a
  @rhombus(draw.PointLike, ~annot) value and the second is mapped to a
  @rhombus(draw.SizeLike, ~annot) value.}

)

 The @rhombus(draw.RectLike.to_rect, ~annot) annotation,
 @rhombus(draw.RectLike.to_rect) function, and @rhombus(draw.Rect.zero) value are
 anaologous to @rhombus(draw.PointLike.to_point, ~annot),
 @rhombus(draw.PointLike.to_point), and @rhombus(draw.Point.zero).

}
