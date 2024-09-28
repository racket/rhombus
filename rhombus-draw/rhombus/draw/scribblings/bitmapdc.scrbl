#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Bitmap Drawing Contexts}

@(~version_at_least "8.14.0.4")

To draw to a bitmap, use the target bitmap's @rhombus(Bitmap.make_dc)
method to get a @rhombus(DC, ~class) object.
