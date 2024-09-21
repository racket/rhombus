#lang info

(define collection 'multi)

(define deps
  '("rhombus-lib"
    ;; The following dependencies serve a similar role to the
    ;; "rhombus-main-distribution" package, which is to ensure a
    ;; certain basic set of libraries when someone installs just
    ;; "rhombus":
    "shrubbery"
    "rhombus-draw"
    "rhombus-gui"
    "rhombus-pict"
    "rhombus-scribble"))
    
(define implies '("rhombus-lib"))

(define build-deps
  '("at-exp-lib"
    "math-lib"
    "racket-doc"
    "rackunit-lib"
    "scribble-lib"
    "shrubbery"
    "shrubbery-lib"
    "testing-util-lib"
    ["base" #:version "8.12"]
    "rhombus-scribble-lib"))

(define pkg-desc "Rhombus base language")

(define license '(Apache-2.0 OR MIT))
