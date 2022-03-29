#lang racket/base

(provide adhoc-comparison)

(require "types.rkt")

(define (load =)
  (= 1 1))

(define adhoc-comparison
  (benchmark "Ad hoc comparison" load))
