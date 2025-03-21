#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-lib"
               "rhombus-url-lib"))
(define implies '("rhombus-url-lib"))

(define build-deps
  '("rhombus"
    "rhombus-scribble-lib"))

(define pkg-desc "Rhombus URL library")

(define license '(Apache-2.0 OR MIT))
