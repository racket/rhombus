#lang info

(define collection 'multi)

(define deps
  '(["base" #:version "8.8.0.5"]
    "compiler-lib"
    ["pretty-expressive" #:version "1.1"]
    ["shrubbery-lib" #:version "0.2"]
    "enforest-lib"
    "syntax-color-lib"
    "parser-tools-lib"
    "testing-util-lib"))

(define pkg-desc "implementation (no documentation) part of \"rhombus\"")

(define license '(Apache-2.0 OR MIT))

;; keep in sync with runtime version at "rhombus/private/amalgam/version.rkt"
(define version "0.34")
