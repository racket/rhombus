#lang racket/base

(provide extended-key-types)

(require "types.rkt")

(define (load =)
  (= 1 1))

(define extended-key-types
  (benchmark "Extended key types" load))
