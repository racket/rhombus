#lang info

(define collection 'multi)

(define deps '("rhombus-pict-lib"))
(define implies '("rhombus-pict-lib"))

(define build-deps '("base"
                     "pict-doc"
                     "rhombus"
                     "shrubbery"
                     "rhombus-draw"
                     "rhombus-lib"
                     "rhombus-scribble-lib"
                     "rhombus-slideshow"))

(define pkg-desc "Rhombus functional-picture library")

(define license '(Apache-2.0 OR MIT))
