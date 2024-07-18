#lang info

(define collection 'multi)

(define deps '("rhombus-lib"))
(define implies '("rhombus-lib"))

(define build-deps
  '("at-exp-lib"
    "math-lib"
    "racket-doc"
    "rackunit-lib"
    "scribble-lib"
    "shrubbery"
    "shrubbery-lib"
    "testing-util-lib"
    "base"
    "rhombus-scribble-lib"))

(define pkg-desc "Rhombus base language")

(define license '(Apache-2.0 OR MIT))
