#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(define-static-info-key-syntax/provide #%syntax-class-attributes
  (static-info-key (lambda (a b)
                     ;; FIXME
                     a)
                   (lambda (a b)
                     ;; FIXME
                     #'())))
