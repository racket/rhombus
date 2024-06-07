#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "macro-result.rkt")
         "space-provide.rkt"
         "for-clause.rkt"
         (submod "for-clause.rkt" for-class)
         "name-root.rkt"
         "macro-macro.rkt")

(define+provide-space for_clause rhombus/for_clause
  #:fields
  (macro))

(define-identifier-syntax-definition-transformer macro
  rhombus/for_clause
  #'make-for-clause-transformer)

(define-for-syntax (make-for-clause-transformer proc)
  (for-clause-transformer
   (lambda (stx)
     (define defns (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (unless (syntax? defns)
       (raise-bad-macro-result (proc-name proc) "`for` clause" defns))
     (datum->syntax #f (unpack-multi defns proc #f)))))
