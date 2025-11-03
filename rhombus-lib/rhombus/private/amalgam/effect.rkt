#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "parse.rkt"
         "expression.rkt")

(provide #%effect)

(define (print-values . vals)
  (for-each (current-print) vals)
  (apply values vals))

(define-syntax #%effect
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        #:with e::expression #`(group . tail)
        (values #'(call-with-values (lambda () e.parsed) print-values)
                #'())]))))
