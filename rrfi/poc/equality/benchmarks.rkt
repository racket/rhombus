#lang racket/base

(provide
 (all-from-out "benchmarks/key-types.rkt"
               "benchmarks/extended-key-types.rkt"
               "benchmarks/adhoc-comparison.rkt"))

(require "benchmarks/key-types.rkt"
         "benchmarks/extended-key-types.rkt"
         "benchmarks/adhoc-comparison.rkt")
