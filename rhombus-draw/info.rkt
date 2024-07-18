#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-draw-lib"))
(define implies '("rhombus-draw-lib"))

(define build-deps
  '("draw-lib"
    "rhombus"
    "rhombus-lib"
    "rhombus-scribble-lib"
    "at-exp-lib"
    "math-lib"
    "draw-doc"))

(define pkg-desc "Rhombus drawing library")

(define license '(Apache-2.0 OR MIT))
