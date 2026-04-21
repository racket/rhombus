#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-ffi-lib"))
(define implies '("rhombus-ffi-lib"))

(define build-deps
  '("rhombus"
    "racket-doc" ; via `hash_lang`
    "rhombus-scribble-lib"
    "racket-test"
    "ffi2"
    "rhombus-draw"))

(define pkg-desc "Rhombus foreign-function interface")

(define license '(Apache-2.0 OR MIT))
