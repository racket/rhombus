#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-html-lib"))
(define implies '("rhombus-html-lib"))

(define build-deps
  '("rhombus"
    "rhombus-scribble-lib"))

(define pkg-desc "Rhombus HTML library")

(define license '(Apache-2.0 OR MIT))
