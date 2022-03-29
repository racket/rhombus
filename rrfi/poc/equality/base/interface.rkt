#lang racket/base

(provide gen:equalhash
         equalhash?
         equalb
         hashb
         hash2b)

(require racket/generic)

(define-generics equalhash
  (equalb equalhash other)
  (hashb equalhash)
  (hash2b equalhash))
