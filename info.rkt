#lang info

(define collection 'multi)

(define deps
  '(["base" #:version "8.5.0.3"]
    "syntax-color-lib"
    "parser-tools-lib"
    ["scribble-lib" #:version "1.43"]
    "sandbox-lib"))

(define build-deps
  '("at-exp-lib"
    "racket-doc"))

(define version "0.1")
