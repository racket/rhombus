#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(define-static-info-key-syntax/provide #%append
  (static-info-key static-info-identifier-union
                   static-info-identifier-intersect))
