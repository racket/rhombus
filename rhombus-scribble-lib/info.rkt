#lang info

(define collection 'multi)

(define deps
  '("base"
    "shrubbery-lib"
    "syntax-color-lib"
    "rhombus-lib"
    ["scribble-lib" #:version "1.50"]
    "sandbox-lib"
    "enforest-lib"
    "shrubbery-render-lib"))

(define pkg-desc "implementation (no documentation) part of \"rhombus-scribble\"")

(define license '(Apache-2.0 OR MIT))

(define version "0.2")
