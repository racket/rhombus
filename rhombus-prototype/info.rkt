#lang info

(define collection 'multi)

(define implies
  '("rhombus"
    "shrubbery"
    "enforest"
    "rhombus-draw"
    "rhombus-gui"
    "rhombus-pict"
    "rhombus-scribble"
    "rhombus-slideshow"))

(define deps implies)

(define pkg-desc "A compatibility package that combines original Rhombus packages")

(define license '(Apache-2.0 OR MIT))

(define version "0.27")
