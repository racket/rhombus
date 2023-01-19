#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "pack.rkt")
         "name-root.rkt"
         "syntax-class-clause.rkt"
         "space.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(provide syntax_class_clause)

(define-name-root syntax_class_clause
  #:root (space-syntax rhombus/syntax_class_clause)
  #:fields
  (macro))

(define-identifier-syntax-definition-transformer macro
  rhombus/syntax_class_clause
  #'make-syntax-class-clause-transformer)

(define-for-syntax (make-syntax-class-clause-transformer proc)
  (syntax-class-clause-transformer
   (lambda (req stx)
     (error "TBD"))))
