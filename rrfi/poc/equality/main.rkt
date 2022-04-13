#!/usr/bin/env racket
#lang racket

(require (prefix-in b:
                    "base/interface.rkt")
         (prefix-in b:
                    "base/api.rkt")
         (prefix-in k:
                    "key/interface.rkt")
         (prefix-in k:
                    "key/api.rkt")
         "benchmarks.rkt"
         "util.rkt")

(define (measure fn . args)
  (second
   (values->list
    (time-apply fn args))))

(define (run-benchmark b =)
  (measure (benchmark-load b) =))

(module+ main
  (displayln "Running benchmarks...")
  (for ([b benchmarks])
    (displayln (~a (benchmark-name b)))
    (displayln (~a "Base: " (run-benchmark b b:=) " ms"))
    (displayln (~a "Key: " (run-benchmark b k:=) " ms"))))
