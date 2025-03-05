#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~style: #'toc, ~tag: "concurrency"){Threads and Concurrency}

@docmodule(rhombus/thread)

Most of Rhombus's concurrency facilities are provided by the
@rhombusmodname(rhombus/thread) module.

@local_table_of_contents()

@include_section("thread.scrbl")
@include_section("semaphore.scrbl")
@include_section("evt.scrbl")
