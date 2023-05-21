#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt"
                     "macro-result.rkt")
         "provide.rkt"
         "name-root.rkt"
         "pattern-clause.rkt"
         "space.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/space)
                     pattern_clause))

(define-space-syntax pattern_clause
  (space-syntax rhombus/pattern_clause))

(define-name-root pattern_clause
  #:fields
  (macro))

(define-identifier-syntax-definition-transformer macro
  rhombus/pattern_clause
  #'make-pattern-clause-transformer)

(define-for-syntax (make-pattern-clause-transformer proc)
  (pattern-clause-transformer
   (lambda (stx)
     (define clauses (syntax-parse stx
                       [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (unless (syntax? clauses)
       (raise-bad-macro-result (proc-name proc) "pattern clauses" clauses))
     #`(#:splice #,@(unpack-multi clauses proc #f)))))
