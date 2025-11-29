#lang info

(define collection 'multi)

(define deps '("rhombus-scribble-lib"))
(define implies '("rhombus-scribble-lib"))

(define build-deps
  '("base"
    "at-exp-lib"
    "rhombus-lib"
    "racket-doc"
    "rhombus"
    "shrubbery"
    "rhombus-pict"
    "scribble-lib"
    "scribble-doc"))

(define pkg-desc "Rhombus text-document and documentation library")

(define license '(Apache-2.0 OR MIT))
