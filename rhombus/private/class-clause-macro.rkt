#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "realm.rkt"
                     (submod "class-meta.rkt" for-static-info))
         "class+interface.rkt"
         "class-clause.rkt"
         (submod "class-clause.rkt" for-class)
         (submod "interface-clause.rkt" for-interface)
         "space.rkt"
         "name-root.rkt"
         "macro-macro.rkt")

(provide class_clause
         class_and_interface_clause)

(define-name-root class_clause
  #:root (space-syntax rhombus/class_clause)
  #:fields
  (macro
   only))

(define-name-root class_and_interface_clause
  #:fields
  ([macro both_macro]
   [only both_only]))

(define-name-root only
  #:fields
  ([macro macro-only]))

(define-name-root both_only
  #:fields
  ([macro both_macro-only]))

(define-identifier-syntax-definition-transformer+only macro macro-only
  rhombus/class_clause
  #:extra [#:info class-data-static-infos]
  #'make-class-clause-transformer)

(define-identifier-syntax-definition-transformer both_macro
  #f
  #:extra [#:info #'()]
  #'make-class-and-interface-clause-transformer)

(define-identifier-syntax-definition-transformer both_macro-only
  #:multi (rhombus/class_clause
           rhombus/interface_clause)
  #:extra [#:info #'()]
  #'make-class-and-interface-clause-transformer)

(define-for-syntax (make-class-clause-transformer proc)
  (class-clause-transformer
   (build-transformer proc)))

(define-for-syntax (make-class-and-interface-clause-transformer proc)
  (make-class+interface-clause-transformer
   (build-transformer proc)))

(define-for-syntax (build-transformer proc)
  (lambda (stx data)
    (define defns (syntax-parse stx
                    [(head . tail) (proc (pack-tail #'tail) #'head data)]))
    (unless (syntax? defns)
      (raise-result-error* (proc-name proc) rhombus-realm "Syntax" defns))
    (datum->syntax #f (unpack-multi defns proc #f))))
