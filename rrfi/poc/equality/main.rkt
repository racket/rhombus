#!/usr/bin/env racket
#lang racket

(require (prefix-in c: "comparison.rkt")
         (prefix-in h: "hashing.rkt")
         "util.rkt")

(struct benchmark (name load)
  #:transparent)

(module+ main
  (displayln "Running benchmarks...")
  (define benchmarks (list (benchmark "Comparison" c:run)
                           (benchmark "Hashing" h:run)))
  (for ([b benchmarks])
    (displayln (~a (benchmark-name b)))
    (display-results ((benchmark-load b)))))
