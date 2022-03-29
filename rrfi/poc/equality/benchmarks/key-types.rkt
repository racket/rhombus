#lang racket/base

(provide key-types)

(require "types.rkt")

(define (load =)
  (= 1 1))

(define key-types
  (benchmark "Key types" load))
