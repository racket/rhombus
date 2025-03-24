#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-xml-lib"))
(define implies '("rhombus-xml-lib"))

(define build-deps
  '("rhombus"
    "rhombus-scribble-lib"))

(define pkg-desc "Rhombus XML library")

(define license '(Apache-2.0 OR MIT))
