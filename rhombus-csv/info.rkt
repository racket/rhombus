#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-csv-lib"))
(define implies '("rhombus-csv-lib"))

(define build-deps
  '("rhombus"
    "rhombus-scribble-lib"))

(define pkg-desc "Rhombus CSV library")

(define license '(Apache-2.0 OR MIT))
