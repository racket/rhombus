#lang info

(define collection 'multi)

(define deps
  '(["base" #:version "8.8.0.5"]
    "pretty-expressive"
    "syntax-color-lib"
    "parser-tools-lib"
    ["scribble-lib" #:version "1.50"]
    "sandbox-lib"
    "testing-util-lib"
    "draw-lib"
    ["gui-easy-lib" #:version "0.9"]
    "gui-lib"))

(define build-deps
  '("at-exp-lib"
    "math-lib"
    "racket-doc"
    "rackunit-lib"
    "scribble-doc"
    "draw-doc"
    "gui-easy"))

(define version "0.7")
