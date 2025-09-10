#lang info

(define collection 'multi)

(define deps
  '(["base" #:version "8.14"]
    ["compiler-lib" #:version "1.13"]
    ["pretty-expressive" #:version "1.1"]
    ["shrubbery-lib" #:version "0.3"]
    "enforest-lib"
    "syntax-color-lib"
    "parser-tools-lib"))

(define pkg-desc "implementation (no documentation) part of \"rhombus\"")

(define license '(Apache-2.0 OR MIT))

;; keep in sync with runtime version at "rhombus/private/amalgam/version.rkt"
(define version "0.39")
