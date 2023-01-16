#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "pack.rkt")
         "name-root.rkt"
         "pattern-clause.rkt"
         "syntax.rkt"
         "parse.rkt")

(provide pattern_clause)

(define-simple-name-root pattern_clause
  macro)

(define-identifier-syntax-definition-transformer macro
  rhombus/pattern_clause
  #'make-pattern-clause-transformer)

(define-for-syntax (make-pattern-clause-transformer proc)
  (pattern-clause-transformer
   (lambda (req stx)
     (error "TBD"))))
