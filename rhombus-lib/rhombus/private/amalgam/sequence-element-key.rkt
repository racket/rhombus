#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(define-static-info-key-syntax/provide #%sequence-element
  (static-info-key static-infos-or
                   static-infos-and))
