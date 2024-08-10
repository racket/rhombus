#lang info

(define collection 'multi)

(define deps '("shrubbery-render-lib"))
(define implies '("shrubbery-render-lib"))

(define build-deps '("base"
                     "rhombus"
                     "rhombus-scribble-lib"))

(define pkg-desc "Tools for rendering shrubbery forms to external documents")

(define license '(Apache-2.0 OR MIT))
