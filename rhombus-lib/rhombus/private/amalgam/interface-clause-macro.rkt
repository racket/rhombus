#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "macro-result.rkt"
                     (submod "interface-meta.rkt" for-static-info)
                     "syntax-wrap.rkt")
         "space-provide.rkt"
         "interface-clause.rkt"
         "macro-macro.rkt"
         "definition.rkt"
         "bounce-to-definition.rkt")

(define+provide-space interface_clause rhombus/interface_clause
  #:fields
  (macro))

(define-identifier-syntax-definition-transformer macro
  rhombus/interface_clause
  #:extra ([#:info get-interface-data-static-infos value])
  #'make-interface-clause-transformer)

(define-for-syntax (make-interface-clause-transformer proc)
  (interface-clause-transformer
   (lambda (stx data)
     (define defns (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head data)]))
     (unless (syntax*? defns)
       (raise-bad-macro-result (proc-name proc) "`interface` clause" defns
                               #:single-group? #f))
     (datum->syntax #f (unpack-multi defns proc #f)))))

;; See use of `bounce-to-definition` in "class-clause-macro.rkt"
(define-interface-clause-syntax macro
  (interface-clause-transformer
   (lambda (stx data)
     (bounce-to-definition (defn-quote macro) stx))))
