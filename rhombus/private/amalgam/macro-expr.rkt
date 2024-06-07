#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "macro-expr-parse.rkt"
         "entry-point.rkt")

(provide macro-expression)

(define-syntax macro-expression
  (entry-point-transformer
   ;; parse macro
   (lambda (stx adjustments)
     (parse-macro-expression stx adjustments))
   ;; extract arity
   (lambda (stx)
     1)))
