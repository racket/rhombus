#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "declaration.rkt"
         "parse.rkt"
         (for-syntax "parse.rkt"))

(provide for_meta)

(define-syntax for_meta
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(form-id (block form ...))
        #'((begin-for-syntax
             (rhombus-top form ...)))]))))
