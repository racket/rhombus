#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(define-static-info-key-syntax/provide #%index-get
  (static-info-key static-info-identifier-or
                   static-info-identifier-and))

(define-static-info-key-syntax/provide #%index-set
  (static-info-key static-info-identifier-or
                   static-info-identifier-and))
