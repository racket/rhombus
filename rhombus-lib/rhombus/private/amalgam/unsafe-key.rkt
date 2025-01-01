#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(define-static-info-key-syntax/provide #%unsafe
  (static-info-key (lambda (a b)
                     (or a b))
                   (lambda (a b)
                     (and (identifier? a)
                          (identifier? b)
                          (free-identifier=? a b)
                          a))))
