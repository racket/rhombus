#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-html"
               "rhombus-html-page-lib"))
(define implies '("rhombus-html-page-lib"))

(define build-deps
  '("rhombus"
    "rhombus-scribble-lib"))

(define pkg-desc "Rhombus HTML page-generation library")

(define license '(Apache-2.0 OR MIT))
