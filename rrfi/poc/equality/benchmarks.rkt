#lang racket/base

(provide benchmarks
         (all-from-out "benchmarks/types.rkt"))

(require "benchmarks/types.rkt"
         "benchmarks/key-types.rkt"
         "benchmarks/extended-key-types.rkt"
         "benchmarks/adhoc-comparison.rkt")

(define benchmarks
  (list key-types extended-key-types adhoc-comparison))
