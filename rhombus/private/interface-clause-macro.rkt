#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "realm.rkt"
                     (submod "interface-meta.rkt" for-static-info))
         "interface-clause.rkt"
         (submod "interface-clause.rkt" for-interface)
         "name-root.rkt"
         "macro-macro.rkt")

(provide interface_clause)

(define-simple-name-root interface_clause
  macro
  only)

(define-name-root only
  #:fields
  ([macro macro-only]))

(define-identifier-syntax-definition-transformer+only macro macro-only
  rhombus/interface_clause
  #:extra [#:info interface-data-static-infos]
  #'make-interface-clause-transformer)

(define-for-syntax (make-interface-clause-transformer proc)
  (interface-clause-transformer
   (lambda (stx data)
     (define defns (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head data)]))
     (unless (syntax? defns)
       (raise-result-error* (proc-name proc) rhombus-realm "Syntax" defns))
     (datum->syntax #f (unpack-multi defns proc #f)))))
