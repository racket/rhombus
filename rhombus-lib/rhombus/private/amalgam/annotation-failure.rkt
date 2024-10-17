#lang racket/base
(require "binding-failure.rkt")

(provide raise-annotation-failure)

(define (raise-annotation-failure who val ctc)
  (raise-binding-failure who "value" val ctc))
