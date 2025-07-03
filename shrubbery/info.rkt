#lang info

(define collection 'multi)

(define deps '("base"
               "shrubbery-lib"))
(define implies '("shrubbery-lib"))

(define build-deps '("at-exp-lib"
                     "enforest"
                     "racket-doc"
                     "rhombus"
                     "rhombus-lib"
                     "rhombus-scribble-lib"
                     "scribble-doc"
                     "scribble-lib"
                     "syntax-color-lib"
                     "gui-doc"
                     "gui-lib"
                     "syntax-color-doc"))

(define pkg-desc "Shrubbery notation parser and printer")

(define license '(Apache-2.0 OR MIT))
