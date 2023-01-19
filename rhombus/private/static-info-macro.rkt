#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     "pack.rkt"
                     "static-info-pack.rkt")
         "definition.rkt"
         "name-root.rkt"
         "quasiquote.rkt"
         "static-info.rkt"
         "space.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         "parens.rkt"
         (for-syntax "name-root.rkt")
         (for-syntax "parse.rkt")
         "call-result-key.rkt"
         "map-ref-set-key.rkt"
         "ref-result-key.rkt"
         "dot-provider-key.rkt")

(provide statinfo
         (for-syntax statinfo_meta))

(define-name-root statinfo
  #:root (space-syntax rhombus/statinfo)
  #:fields
  (macro
   only))

(define-name-root only
  #:fields
  ([macro macro-only]))

(begin-for-syntax
  (define-name-root statinfo_meta
    #:fields
    (pack
     unpack
     wrap
     lookup
     
     call_result_key
     ref_result_key
     map_ref_key
     map_set_key
     map_append_key
     dot_provider_key)))

(define-for-syntax (make-static-info-macro-macro in-space)
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (block quotes)
        [(_ (quotes (group name::name)) (body-tag::block body ...))
         #`((define-syntax #,(in-space #'name.name)
              (convert-static-info 'name.name (rhombus-body-at body-tag body ...))))]))))

(define-syntax macro
  (make-static-info-macro-macro (lambda (x) x)))
(define-syntax macro-only
  (make-static-info-macro-macro in-static-info-space))
   
(define-for-syntax (convert-static-info who stx)
  (unless (syntax? stx)
    (raise-result-error who "syntax?" stx))
  (static-info (syntax->list (pack stx))))

(define-for-syntax (pack v)
  (pack-static-infos (unpack-term v 'statinfo_meta.pack #f) 'statinfo_meta.pack))

(define-for-syntax (unpack v)
  (unpack-static-infos v))

(define-for-syntax (wrap form info)
  (pack-term #`(parsed #,(wrap-static-info* (wrap-expression form)
                                            (pack info)))))

(define-for-syntax (lookup form key)
  (define si (extract-static-infos form))
  (and si (static-info-lookup si key)))

(define-for-syntax call_result_key #'#%call-result)
(define-for-syntax ref_result_key #'#%ref-result)
(define-for-syntax map_ref_key #'#%map-ref)
(define-for-syntax map_set_key #'#%map-set!)
(define-for-syntax map_append_key #'#%map-append)
(define-for-syntax dot_provider_key #'#%dot-provider)
