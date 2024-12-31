#lang racket/base
(require "version-case.rkt")

(provide flbit-field)

(meta-if-version-at-least
 "8.15.0.3"
 (require racket/flonum)
 (define (flbit-field fl start end)
   (bitwise-bit-field (integer-bytes->integer (real->floating-point-bytes fl 8) #f)
                      start
                      end)))
