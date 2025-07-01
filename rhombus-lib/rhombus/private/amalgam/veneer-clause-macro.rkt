#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "pack.rkt"
                     "macro-result.rkt"
                     (submod "veneer-meta.rkt" for-static-info)
                     "syntax-wrap.rkt")
         "space-provide.rkt"
         "veneer-clause.rkt"
         "macro-macro.rkt"
         "definition.rkt"
         "bounce-to-definition.rkt")

(define+provide-space veneer_clause rhombus/veneer_clause
  #:fields
  (macro))

(define-identifier-syntax-definition-transformer macro
  rhombus/veneer_clause
  #:extra ([#:info get-veneer-data-static-infos value])
  #'make-veneer-clause-transformer)

(define-for-syntax (make-veneer-clause-transformer proc)
  (veneer-clause-transformer
   (lambda (stx data)
     (define defns (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head data)]))
     (unless (syntax*? defns)
       (raise-bad-macro-result (proc-name proc) "`veneer` clause" defns))
     (datum->syntax #f (unpack-multi defns proc #f)))))

;; See use of `bounce-to-definition` in "class-clause-macro.rkt"
(define-veneer-clause-syntax macro
  (veneer-clause-transformer
   (lambda (stx data)
     (bounce-to-definition (defn-quote macro) stx))))
