#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "parse.rkt"
         "expression.rkt")

(provide #%effect)

(define-syntax #%effect
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        #:with e::expression #`(group . tail)
        (values #'(call-with-values (lambda () e.parsed) (current-print))
                #'())]))))
