#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-json-lib"))
(define implies '("rhombus-json-lib"))

(define build-deps
  '("rhombus"
    "rhombus-scribble-lib"))

(define pkg-desc "Rhombus JSON library")

(define license '(Apache-2.0 OR MIT))
