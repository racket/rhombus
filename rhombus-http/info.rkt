#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-http-lib"))
(define implies '("rhombus-http-lib"))

(define build-deps
  '("rhombus"
    "rhombus-scribble-lib"
    "rhombus-json"
    "rhombus-url"
    "rhombus-net-cookie"
    "rhombus-ssl"))

(define pkg-desc "Rhombus HTTP library")

(define license '(Apache-2.0 OR MIT))

(define language-families '("Rhombus"))
