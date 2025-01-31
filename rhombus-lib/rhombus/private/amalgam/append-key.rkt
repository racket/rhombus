#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(define-static-info-key-syntax/provide #%append
  (static-info-key static-info-identifier-or
                   static-info-identifier-and))
