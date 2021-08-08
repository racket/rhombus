#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt")

(provide (for-syntax wrap-expression))

(define-for-syntax (wrap-expression form)
  (syntax-parse form
    #:datum-literals (parsed)
    [(parsed e) #'e]
    [_ #`(rhombus-expression (group #,form))]))
