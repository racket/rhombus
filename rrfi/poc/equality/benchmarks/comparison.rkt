#lang racket/base

(provide benchmarks)

(require (rename-in "comparison/key-types.rkt"
                    [bm key-types])
         (rename-in "comparison/extended-key-types.rkt"
                    [bm extended-key-types])
         (rename-in "comparison/adhoc-comparison.rkt"
                    [bm adhoc-comparison]))

(define benchmarks
  (list key-types extended-key-types adhoc-comparison))
