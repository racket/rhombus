#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     enforest/hier-name-parse
                     "name-root.rkt"
                     "parse.rkt"
                     "pack.rkt"
                     "static-info-pack.rkt"
                     "macro-result.rkt"
                     "name-path-op.rkt"
                     "srcloc.rkt"
                     "static-info.rkt"
                     "realm.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     (for-syntax racket/base))
         "space-provide.rkt"
         "definition.rkt"
         "name-root-ref.rkt"
         "name-root-space.rkt"
         "static-info.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         "parens.rkt"
         "call-result-key.rkt"
         "append-key.rkt"
         "index-key.rkt"
         "index-result-key.rkt"
         "dot-provider-key.rkt"
         "sequence-constructor-key.rkt"
         "sequence-element-key.rkt"
         "values-key.rkt")

(provide (for-syntax (for-space rhombus/namespace
                                statinfo_meta)))

(define+provide-space statinfo rhombus/statinfo
  #:fields
  (macro))

(begin-for-syntax
  (define-name-root statinfo_meta
    #:fields
    ([pack statinfo_meta.pack]
     [unpack statinfo_meta.unpack]
     [pack_group statinfo_meta.pack_group]
     [unpack_group statinfo_meta.unpack_group]
     [wrap statinfo_meta.wrap]
     [lookup statinfo_meta.lookup]

     call_result_key
     index_result_key
     index_get_key
     index_set_key
     [map_ref_key index_get_key] ; temporary
     [map_set_key index_set_key] ; temporary
     append_key
     dot_provider_key
     sequence_constructor_key
     sequence_element_key
     values_key)))

(define-for-syntax (make-static-info-macro-macro in-space)
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (block quotes)
        [(_ (quotes (group name::name)) (body-tag::block body ...))
         #`((define-syntax #,(in-space #'name.name)
              (convert-static-info 'name.name (rhombus-body-at body-tag body ...))))]))))

(define-defn-syntax macro
  (make-static-info-macro-macro in-static-info-space))

(define-for-syntax (convert-static-info who stx)
  (unless (syntax? stx)
    (raise-bad-macro-result who "static info" stx))
  (define si (syntax->list (pack who stx)))
  (static-info (lambda () si)))

(define-for-syntax (pack who stx)
  (pack-static-infos who (unpack-term stx who #f)))

(define-for-syntax (check-syntax who s)
  (unless (syntax? s)
    (raise-argument-error* who rhombus-realm "Syntax" s)))

(define-for-syntax (check-identifier who id)
  (unless (identifier? id)
    (raise-argument-error* who rhombus-realm "Identifier" id)))

(begin-for-syntax
  (define/arity (statinfo_meta.pack stx)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (check-syntax who stx)
    (pack who stx))

  (define/arity (statinfo_meta.unpack stx)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (check-syntax who stx)
    (unpack-static-infos who stx))

  (define/arity (statinfo_meta.pack_group stx)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (check-syntax who stx)
    (datum->syntax
     #f
     (map (lambda (stx) (pack-static-infos who stx))
          (cdr (syntax->list (unpack-group stx who #f))))))

  (define/arity (statinfo_meta.unpack_group stx)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (check-syntax who stx)
    #`(group . #,(map (lambda (stx) (unpack-static-infos who stx))
                      (syntax->list stx))))

  (define/arity (statinfo_meta.wrap form info)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (check-syntax who form)
    (check-syntax who info)
    (define e
      (wrap-static-info* (wrap-expression form) (pack who info)))
    (pack-term (relocate+reraw e #`(parsed #:rhombus/expr #,e))))

  (define/arity (statinfo_meta.lookup form key)
    (check-syntax who form)
    (check-identifier who key)
    (define si
      (extract-static-infos
       (syntax-parse (unpack-group form who #f)
         #:datum-literals (parsed group)
         [(group (parsed #:rhombus/expr e)) #'e]
         [(group . (~var name (:hier-name-seq in-name-root-space
                                              (lambda (x) x)
                                              name-path-op
                                              name-root-ref/maybe)))
          (and (null? (syntax-e #'name.tail))
               #'name.name)]
         [(group t) #'t]
         [g #'g])))
    (and si (static-info-lookup si key)))
  )

(define-syntax-rule (define-key key id)
  (begin-for-syntax
    (define key (quote-syntax id))
    (define-static-info-syntax key
      #:defined syntax-static-infos)))

(define-key call_result_key #%call-result)
(define-key index_result_key #%index-result)
(define-key index_get_key #%index-get)
(define-key index_set_key #%index-set)
(define-key append_key #%append)
(define-key dot_provider_key #%dot-provider)
(define-key sequence_constructor_key #%sequence-constructor)
(define-key sequence_element_key #%sequence-element)
(define-key values_key #%values)
