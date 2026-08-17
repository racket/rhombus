#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "macro-result.rkt"
                     "origin.rkt")
         "space-provide.rkt"
         "syntax-class-clause.rkt"
         "macro-macro.rkt"
         "parens.rkt")

(define+provide-space syntax_class_clause rhombus/syntax_class_clause
  #:fields
  (macro))

(define-identifier-syntax-definition-transformer macro
  rhombus/syntax_class_clause
  #'make-syntax-class-clause-transformer)

(define-for-syntax (make-syntax-class-clause-transformer proc)
  (syntax-class-clause-transformer
   (lambda (stx)
     (define clauses (syntax-parse stx
                       [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (unless (syntax? clauses)
       (raise-bad-macro-result (proc-name proc) "syntax class clauses" clauses))
     (syntax-parse (unpack-group clauses proc #f)
       [(_ (_::alts alt ...))
        #`(#:splice/alts #,stx (alt ...))]
       [(_ (_::block clause::syntax-class-clause ...))
        (transfer-origins
         (syntax->list #'(clause.parsed ...))
         #`(#:splice #,stx (clause.parsed ...)))]
       [(_ (_::block clause::syntax-class-clause ...)
           (_::alts alt ...))
        (transfer-origins
         (syntax->list #'(clause.parsed ...))
         #`(#:splice/alts #,stx (clause.parsed ...) (alt ...)))]))))
