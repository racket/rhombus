#lang info

(define collection 'multi)

(define deps
  '("base"
    "rhombus-lib"
    "rhombus-draw-lib"))

(define build-deps
  '("rhombus"
    "rhombus-draw"
    "rhombus-pict"
    "rhombus-scribble-lib"))

(define pkg-desc "Rhombus logo drawing library")

(define license '(Apache-2.0 OR MIT))
