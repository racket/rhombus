#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     enforest/hier-name-parse
                     "pack.rkt"
                     "static-info-pack.rkt"
                     "macro-result.rkt"
                     "name-path-op.rkt"
                     "realm.rkt")
         "space-provide.rkt"
         "definition.rkt"
         "name-root.rkt"
         "name-root-ref.rkt"
         "name-root-space.rkt"
         "quasiquote.rkt"
         "static-info.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         "parens.rkt"
         (for-syntax "name-root.rkt")
         (for-syntax "parse.rkt")
         "call-result-key.rkt"
         "append-key.rkt"
         "index-key.rkt"
         "index-result-key.rkt"
         "dot-provider-key.rkt"
         "values-key.rkt"
         "sequence-constructor-key.rkt")

(provide (for-syntax (for-space rhombus/namespace
                                statinfo_meta)))

(define+provide-space statinfo rhombus/statinfo
  #:fields
  (macro))

(begin-for-syntax
  (define-name-root statinfo_meta
    #:fields
    (pack
     unpack
     pack_group
     unpack_group
     wrap
     lookup
     
     call_result_key
     index_result_key
     index_get_key
     index_set_key
     [map_ref_key index_get_key] ; temporary
     [map_set_key index_set_key] ; temporary
     append_key
     dot_provider_key
     values_key
     sequence_constructor_key)))

(define-for-syntax (make-static-info-macro-macro in-space)
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (block quotes)
        [(_ (quotes (group name::name)) (body-tag::block body ...))
         #`((define-syntax #,(in-space #'name.name)
              (convert-static-info 'name.name (rhombus-body-at body-tag body ...))))]))))

(define-syntax macro
  (make-static-info-macro-macro in-static-info-space))
   
(define-for-syntax (convert-static-info who stx)
  (unless (syntax? stx)
    (raise-bad-macro-result who "static info" stx))
  (define si (syntax->list (pack stx)))
  (static-info (lambda () si)))

(define-for-syntax (pack v)
  (pack-static-infos (unpack-term v 'statinfo_meta.pack #f) 'statinfo_meta.pack))

(define-for-syntax (unpack v)
  (unpack-static-infos v))

(define-for-syntax (pack_group v)
  (datum->syntax
   #f
   (map (lambda (v) (pack-static-infos v 'statinfo_meta.pack_group))
        (cdr (syntax->list (unpack-group v 'statinfo_meta.pack_group #f))))))

(define-for-syntax (unpack_group v)
  #`(group . #,(map unpack-static-infos (syntax->list v))))

(define-for-syntax (wrap form info)
  (pack-term #`(parsed #:rhombus/expr #,(wrap-static-info* (wrap-expression form)
                                                           (pack info)))))

(define-for-syntax (lookup form key)
  (define who 'statinfo_meta.lookup)
  (unless (syntax? form) (raise-argument-error* who rhombus-realm "Syntax" form))
  (unless (identifier? key) (raise-argument-error* who rhombus-realm "Identifier" key))
  (define si (extract-static-infos (syntax-parse (unpack-group form who #f)
                                     #:datum-literals (parsed group)
                                     [(group (parsed #:rhombus/expr e)) #'e]
                                     [(group . (~var name (:hier-name-seq in-name-root-space (lambda (x) x) name-path-op name-root-ref/maybe)))
                                      (and (null? (syntax-e #'name.tail))
                                           #'name.name)]
                                     [(group t) #'t]
                                     [g #'g])))
  (and si (static-info-lookup si key)))

(define-for-syntax call_result_key #'#%call-result)
(define-for-syntax index_result_key #'#%index-result)
(define-for-syntax index_get_key #'#%index-get)
(define-for-syntax index_set_key #'#%index-set)
(define-for-syntax append_key #'#%append)
(define-for-syntax dot_provider_key #'#%dot-provider)
(define-for-syntax values_key #'#%values)
(define-for-syntax sequence_constructor_key #'#%sequence-constructor)
