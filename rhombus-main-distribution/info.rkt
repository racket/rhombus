#lang info

(define collection 'multi)

(define deps
  '("rhombus"

    "rhombus-draw"
    "rhombus-gui"
    "rhombus-pict"
    "rhombus-scribble"

    "rhombus-html"
    "rhombus-url"

    "rhombus-icons"
    "rhombus-logo"

    "drracket-core"))

(define pkg-desc "A package that combines all of the packages in the main Rhombus distribution")

(define license '(Apache-2.0 OR MIT))
