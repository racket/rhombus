#lang info

(define collection 'multi)

(define deps
  '("base"
    "rhombus-lib"
    "rhombus-exe"
    ;; The following dependencies serve a similar role to the
    ;; "rhombus-main-distribution" package, which is to ensure a
    ;; certain basic set of libraries when someone installs just
    ;; "rhombus":
    "shrubbery"
    "rhombus-ffi"
    "rhombus-draw"
    "rhombus-gui"
    "rhombus-pict"
    "rhombus-scribble"
    "xrepl"
    "expeditor"))

(define implies '("rhombus-lib"))

(define build-deps
  '("at-exp-lib"
    "racket-index"
    "racket-doc"
    "scribble-lib"
    "shrubbery"
    "shrubbery-lib"
    "rhombus-scribble-lib"
    "pict-doc"
    "pict-lib"
    "draw-lib"
    ["compiler-lib" #:version "1.13"]))

(define pkg-desc "Rhombus base language")

(define license '(Apache-2.0 OR MIT))

(define language-families '("Rhombus"))
