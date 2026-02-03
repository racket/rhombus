#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "all-window-view", ~style: #'toc){Window Views}

A @defterm{window view} is a @tech{view} that is a
@rhombuslink(Window, ~class){window},
@rhombuslink(Dialog, ~class){dialog}, or a GUI widget inside a window or
dialog.

@local_table_of_contents()

@include_section("window-view.scrbl")
@include_section("window.scrbl")
@include_section("panel.scrbl")
@include_section("canvas.scrbl")
@include_section("control.scrbl")
