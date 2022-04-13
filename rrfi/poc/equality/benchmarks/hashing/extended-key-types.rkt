#lang racket/base

(provide bm)

(require "../types.rkt")

(define (load =)
  (= 1 1))

(define bm
  (benchmark "Extended key types" load))
