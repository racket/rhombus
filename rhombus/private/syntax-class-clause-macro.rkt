#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "pack.rkt")
         "space-provide.rkt"
         "name-root.rkt"
         "syntax-class-clause.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(define+provide-space syntax_class_clause rhombus/syntax_class_clause
  #:fields
  (macro))

(define-identifier-syntax-definition-transformer macro
  rhombus/syntax_class_clause
  #'make-syntax-class-clause-transformer)

(define-for-syntax (make-syntax-class-clause-transformer proc)
  (syntax-class-clause-transformer
   (lambda (req stx)
     (error "TBD"))))
