#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide rhombus-together
         (for-syntax wrap-for-together))

(define-syntax rhombus-together 'placeholder)

(define-for-syntax (wrap-for-together together?-stx stx)
  (if (syntax-e together?-stx)
      #`(quote-syntax (class-together #,stx) #:local)
      stx))
