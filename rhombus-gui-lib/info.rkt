#lang info

(define collection 'multi)

(define deps
  '("base"
    "rhombus-lib"
    "rhombus-draw-lib"
    "draw-lib"
    ["gui-easy-lib" #:version "0.9"]
    "gui-lib"))

(define pkg-desc "implementation (no documentation) part of \"rhombus-gui\"")

(define license '(Apache-2.0 OR MIT))

(define version "0.1")
