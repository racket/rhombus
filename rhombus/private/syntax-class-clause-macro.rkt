#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt"
                     "macro-result.rkt")
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
   (lambda (stx)
     (define clauses (syntax-parse stx
                       [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (unless (syntax? clauses)
       (raise-bad-macro-result (proc-name proc) "syntax class clauses" clauses))
     #`(#:splice
        #,@(for/list ([clause (in-list (unpack-multi clauses proc #f))])
             (syntax-parse clause
               [cl::syntax-class-clause #'cl.parsed]))))))
