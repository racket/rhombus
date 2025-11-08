#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-ssl-lib"))
(define implies '("rhombus-ssl-lib"))

(define build-deps
  '("rhombus"
    "rhombus-scribble-lib"
    "racket-test"))

(define pkg-desc "Rhombus SSL library")

(define license '(Apache-2.0 OR MIT))
