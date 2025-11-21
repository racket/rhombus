#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-net-cookie-lib"))
(define implies '("rhombus-net-cookie-lib"))

(define build-deps
  '("rhombus"
    "rhombus-scribble-lib"
    "rhombus-url"
    "rhombus-http"
    "racket-test"))

(define pkg-desc "Rhombus library for RFC 6265 cookies")

(define license '(Apache-2.0 OR MIT))
