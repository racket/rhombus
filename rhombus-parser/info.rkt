#lang info

(define collection 'multi)

(define deps '("rhombus-parser-lib"))
(define implies '("rhombus-parser-lib"))

(define build-deps '("base"
                     "rhombus"
                     "rhombus-scribble-lib"))

(define pkg-desc "Rhombus parser library")

(define license '(Apache-2.0 OR MIT))
