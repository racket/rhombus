#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/proc-name
                     "pack.rkt"
                     "realm.rkt")
         "for-clause.rkt"
         (submod "for-clause.rkt" for-class)
         "name-root.rkt"
         "syntax.rkt")

(provide for_clause)

(define-simple-name-root for_clause
  macro
  only)

(define-name-root only
  #:fields
  ([macro macro-only]))

(define-identifier-syntax-definition-transformer+only macro macro-only
  in-for-clause-space
  #'make-for-clause-transformer)

(define-for-syntax (make-for-clause-transformer proc)
  (for-clause-transformer
   (lambda (stx)
     (define defns (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (unless (syntax? defns)
       (raise-result-error* (proc-name proc) rhombus-realm "Syntax" defns))
     (datum->syntax #f (unpack-multi defns proc #f)))))
