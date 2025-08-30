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

@(~version_at_least "8.14.0.4")

@doc(
  class draw.Point(x :: Real, y :: Real)
  annot.macro 'draw.PointLike'
  annot.macro 'draw.PointLike.to_point'
  fun draw.PointLike.to_point(pt :: PointLike) :: Point
  def draw.Point.zero :: Point = Point(0, 0)
){

 The @rhombus(Point, ~class) class represents a point in two dimensions.

 Methods that expect a point typically accept a value satisfying
 @rhombus(PointLike, ~annot), which is any of the following:

@itemlist(

 @item{@rhombus(Point, ~annot): a @rhombus(Point, ~class) instance;}

 @item{@rhombus([Real, Real], ~annot): a
  @rhombus(List) containing two @rhombus(Real, ~annot) values; or}

 @item{@rhombus(matching({#'x: _ :: Real, #'y: _ :: Real}), ~annot):
  a @rhombus(Map) containing at least the keys @x_sym and
  @y_sym, each mapped to a @rhombus(Real, ~annot) value.}

)

 The @rhombus(PointLike.to_point, ~annot) annotation is satisfied by any
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
  class draw.Size(width :: NonnegReal, height :: NonnegReal)
  annot.macro 'draw.SizeLike'
  annot.macro 'draw.SizeLike.to_size'
  fun draw.SizeLike.to_size(sz :: SizeLike) :: Size
  def draw.Size.zero :: Size = Size(0, 0)
){

 The @rhombus(Size, ~class) class represents a size in two dimensions.

 Methods that expect a size typically accept a value satisfying
 @rhombus(SizeLike, ~annot), which is any of the following:

@itemlist(

 @item{@rhombus(Size, ~annot); a @rhombus(Size, ~class) instance;}

 @item{@rhombus([NonnegReal, NonnegReal], ~annot):
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
  class draw.Rect(x :: Real, y :: Real,
                  width :: NonnegReal, height :: NonnegReal):
    constructor
    | (x :: Real, y :: Real,
       width :: NonnegReal, height :: NonnegReal)
    | (point :: PointLike, size :: SizeLike)
  property (r :: draw.Rect).point :: Point
  property (r :: draw.Rect).size :: Size
  annot.macro 'draw.RectLike'
  annot.macro 'draw.RectLike.to_rect'
  fun draw.RectLike.to_rect(sz :: RectLike) :: Rect
  def draw.Rect.zero :: Rect = Rect(0, 0, 0, 0)
){

 The @rhombus(Rect, ~class) class represents a rectagular region, where
 @rhombus(x) and @rhombus(y) correspond to the top-left of the rectangle.
 The @rhombus(Rect.point) property produces @rhombus(x) and @rhombus(y)
 in a @rhombus(Point, ~class), while @rhombus(Rect.size) property produces
 @rhombus(width) and @rhombus(height) in a @rhombus(Size, ~class).

 Methods that expect a rectangle typically accept a value satisfying
 @rhombus(RectLike, ~annot), which is any of the following:

@itemlist(

 @item{a @rhombus(Rect, ~class) instance;}

 @item{@rhombus([Real, Real, NonnegReal, NonnegReal], ~annot):
  a @rhombus(List) containing two @rhombus(Real, ~annot) values for the top-left point
  followed by two @rhombus(NonnegReal, ~annot) values for the size;}

 @item{@rhombus([PointLike, SizeLike], ~annot):
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

@doc(
  class draw.Transformation(xx :: Real, yx :: Real,
                            xy :: Real, yy :: Real,
                            x0 :: Real, y0 :: Real)
  method (t :: Transformation).translate(dx :: Real, dy :: Real)
    :: Transformation
  method (t :: Transformation).scale(sx :: Real, sy :: Real)
    :: Transformation
  method (t :: Transformation).rotate(a :: Real)
    :: Transformation
  method (t :: Transformation).transform(by_t :: Transformation)
    :: Transformation
){

 A @rhombus(Transformation, ~class) object represents a transformation
 matrix:

@itemlist(

  @item{@rhombus(xx, ~var): a scale from the logical @rhombus(x, ~var) to the device @rhombus(x, ~var)}

  @item{@rhombus(yx, ~var): a scale from the logical @rhombus(y, ~var) added to the device @rhombus(x, ~var)}

  @item{@rhombus(xy, ~var): a scale from the logical @rhombus(x, ~var) added to the device @rhombus(y, ~var)}

  @item{@rhombus(yy, ~var): a scale from the logical @rhombus(y, ~var) to the device @rhombus(y, ~var)}

  @item{@rhombus(x0, ~var): an additional amount added to the device @rhombus(x, ~var)}

  @item{@rhombus(y0, ~var): an additional amount added to the device @rhombus(y, ~var)}

)

 The @rhombus(Transformation.translate), @rhombus(Transformation.scale),
 @rhombus(Transformation.rotate), and @rhombus(Transformation.transform)
 methods produce a new @rhombus(Transformation, ~class) object that
 represents the starting transformation followed by an additional one.

}


@doc(
  annot.macro 'draw.LayeredTransformation'
){

 A @rhombus(LayeredTransformation, ~annot) represents the internal
 transformation state of a @rhombus(DC, ~annot) object. It is useful only
 as a result or new value for the @rhombus(DC.layered_transformation)
 property.

}
