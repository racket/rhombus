#lang racket/base

(provide benchmarks)

(require (rename-in "hashing/key-types.rkt"
                    [bm key-types])
         (rename-in "hashing/extended-key-types.rkt"
                    [bm extended-key-types])
         (rename-in "hashing/adhoc-comparison.rkt"
                    [bm adhoc-comparison]))

(define benchmarks
  (list key-types extended-key-types adhoc-comparison))
