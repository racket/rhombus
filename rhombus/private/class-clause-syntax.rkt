#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/proc-name
                     "pack.rkt"
                     "realm.rkt")
         "class+interface.rkt"
         "class-clause.rkt"
         (submod "class-clause.rkt" for-class)
         (submod "interface-clause.rkt" for-interface)
         "name-root.rkt"
         "syntax.rkt")

(provide class_clause
         class_and_interface_clause)

(define-simple-name-root class_clause
  macro)

(define-name-root class_and_interface_clause
  #:fields
  ([macro both_macro]))

(define-identifier-syntax-definition-transformer macro
  in-class-clause-space
  #'make-class-clause-transformer)

(define-identifier-syntax-definition-transformer both_macro
  #:multi (in-class-clause-space in-interface-clause-space)
  #'make-class-and-interface-clause-transformer)

(define-for-syntax (make-class-clause-transformer proc)
  (class-clause-transformer
   (build-transformer proc)))

(define-for-syntax (make-class-and-interface-clause-transformer proc)
  (make-class+interface-clause-transformer
   (build-transformer proc)))

(define-for-syntax (build-transformer proc)
  (lambda (stx)
    (define defns (syntax-parse stx
                    [(head . tail) (proc (pack-tail #'tail) #'head)]))
    (unless (syntax? defns)
      (raise-result-error* (proc-name proc) rhombus-realm "Syntax" defns))
    (datum->syntax #f (unpack-multi defns proc #f))))
