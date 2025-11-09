#lang info

(define collection 'multi)

(define deps
  '("base"
    "rhombus-lib"
    ["rhombus-json-lib" #:version "0.2"]
    "rhombus-url-lib"
    "rhombus-ssl-lib"
    "http-easy-lib"))

(define pkg-desc "implementation (no documentation) part of \"rhombus-http\"")

(define license '(Apache-2.0 OR MIT))

(define version "0.1")
