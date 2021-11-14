#lang info

(define collection 'multi)

(define deps
  '(["base" #:version "8.3.0.8"]
    "syntax-color-lib"
    "parser-tools-lib"
    "scribble-lib" #;["scribble-lib" #:version "1.41"]))

(define build-deps
  '("at-exp-lib"))
