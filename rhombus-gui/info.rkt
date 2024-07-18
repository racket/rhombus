#lang info

(define collection 'multi)

(define deps '("base"
               "rhombus-gui-lib"))
(define implies '("rhombus-gui-lib"))

(define build-deps
  '("gui-easy"
    "gui-easy-lib"
    "rhombus"
    "rhombus-draw"
    "rhombus-lib"
    "rhombus-scribble-lib"))

(define pkg-desc "Rhombus GUI library")

(define license '(Apache-2.0 OR MIT))
