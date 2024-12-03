#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(define-static-info-key-syntax/provide #%maybe
  (static-info-key static-infos-result-union
                   static-infos-result-intersect))
