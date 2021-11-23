#lang info

(define collection 'multi)

(define deps
  '(["base" #:version "8.3.0.8"]
    "syntax-color-lib"
    "parser-tools-lib"
    ["scribble-lib" #:version "1.42"])) ; FIXME: should be 1.43

(define build-deps
  '("at-exp-lib"
    "racket-doc"))
