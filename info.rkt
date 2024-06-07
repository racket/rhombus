#lang info

(define collection 'multi)

(define deps
  '(["base" #:version "8.8.0.5"]
    ["pretty-expressive" #:version "1.1"]
    "syntax-color-lib"
    "parser-tools-lib"
    ["scribble-lib" #:version "1.50"]
    "sandbox-lib"
    "testing-util-lib"
    "draw-lib"
    ["gui-easy-lib" #:version "0.9"]
    "gui-lib"
    "pict-lib"
    "pict-balloon2"
    "slideshow-lib"
    "compiler-lib"))

(define build-deps
  '("at-exp-lib"
    "math-lib"
    "racket-doc"
    "rackunit-lib"
    "scribble-doc"
    "draw-doc"
    "pict-doc"
    "slideshow-doc"
    "gui-easy"
    "compatibility"))

(define version "0.27")
