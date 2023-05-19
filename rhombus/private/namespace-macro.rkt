#lang racket/base
(require (for-syntax racket/base)
         "space.rkt")

(provide (for-space rhombus/space
                    namespace))

(define-space-syntax namespace
  (space-syntax rhombus/namespace))
