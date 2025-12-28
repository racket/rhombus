#lang info

(define collection 'multi)

(define deps '("rhombus-slideshow-lib"))
(define implies '("rhombus-slideshow-lib"))

(define build-deps '("base"
                     "rhombus"
                     "rhombus-draw"
                     "rhombus-lib"
                     "rhombus-pict"
                     "rhombus-scribble-lib"
                     "slideshow-doc"
                     "racket-doc"))

(define pkg-desc "Rhombus slide-presentation library")

(define license '(Apache-2.0 OR MIT))
