#lang racket/base
(require (for-syntax racket/base))

(provide (for-syntax wrap-class-clause)
         rhombus-class)

(define-syntax rhombus-class 'placeholder)

(define-for-syntax (wrap-class-clause parsed)
  #`[(group (parsed (quote-syntax (rhombus-class #,parsed) #:local)))]) ; `quote-syntax` + `rhombus-class` wrapper => clause
