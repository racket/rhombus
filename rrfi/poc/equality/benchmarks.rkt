#lang racket/base

(provide benchmarks
         (all-from-out "benchmarks/types.rkt"))

(require "benchmarks/types.rkt"
         (prefix-in comparison: "benchmarks/comparison.rkt")
         (prefix-in hashing: "benchmarks/hashing.rkt"))

(define benchmarks
  (append comparison:benchmarks hashing:benchmarks))
