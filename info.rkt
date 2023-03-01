#lang info

(define collection 'multi)

(define deps
  '(["base" #:version "8.8.0.5"]
    "syntax-color-lib"
    "parser-tools-lib"
    ["scribble-lib" #:version "1.50"]
    "sandbox-lib"
    "testing-util-lib"
    "draw-lib"
    "gui-easy-lib"))

(define build-deps
  '("at-exp-lib"
    "math-lib"
    "racket-doc"
    "rackunit-lib"
    "scribble-doc"))

(define version "0.1")
