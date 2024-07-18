#lang info

(define collection 'multi)

(define deps '("base"
               "enforest-lib"))
(define implies '("enforest-lib"))

(define build-deps '("at-exp-lib"
                     "compatibility-lib"
                     "racket-doc"
                     "rhombus"
                     "rhombus-lib"
                     "rhombus-scribble-lib"
                     "scribble-lib"
                     "shrubbery"))

(define pkg-desc "Enforestation parsing/expansion engine")

(define license '(Apache-2.0 OR MIT))
