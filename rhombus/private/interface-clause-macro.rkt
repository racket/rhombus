#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "realm.rkt"
                     (submod "interface-meta.rkt" for-static-info))
         "space-provide.rkt"
         "interface-clause.rkt"
         (submod "interface-clause.rkt" for-interface)
         "space.rkt"
         "name-root.rkt"
         "macro-macro.rkt")

(define+provide-space interface_clause rhombus/interface_clause
  #:fields
  (macro))

(define-identifier-syntax-definition-transformer macro
  rhombus/interface_clause
  #:extra ([#:info interface-data-static-infos value])
  #'make-interface-clause-transformer)

(define-for-syntax (make-interface-clause-transformer proc)
  (interface-clause-transformer
   (lambda (stx data)
     (define defns (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head data)]))
     (unless (syntax? defns)
       (raise-result-error* (proc-name proc) rhombus-realm "Syntax" defns))
     (datum->syntax #f (unpack-multi defns proc #f)))))
