#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(define-static-info-key-syntax/provide #%index-result
  (static-info-key static-infos-union
                   static-infos-intersect))
