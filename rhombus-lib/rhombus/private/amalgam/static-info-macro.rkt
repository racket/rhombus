#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     enforest/hier-name-parse
                     "treelist.rkt"
                     "name-root.rkt"
                     "parse.rkt"
                     "pack.rkt"
                     "static-info-pack.rkt"
                     "macro-result.rkt"
                     "name-path-op.rkt"
                     "srcloc.rkt"
                     "static-info.rkt"
                     "realm.rkt"
                     "annotation-failure.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     (for-syntax racket/base)
                     (only-in (submod "list.rkt" for-listable)
                              get-treelist-static-infos))
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
         "list-bounds-key.rkt"
         "maybe-key.rkt"
         "values-key.rkt"
         "indirect-static-info-key.rkt"
         "is-static.rkt")

(provide (for-syntax (for-space rhombus/namespace
                                statinfo_meta)))

(define+provide-space statinfo rhombus/statinfo
  #:fields
  (macro
   key))

(begin-for-syntax
  (define-name-root statinfo_meta
    #:fields
    ([pack statinfo_meta.pack]
     [unpack statinfo_meta.unpack]
     [pack_group statinfo_meta.pack_group]
     [unpack_group statinfo_meta.unpack_group]
     [pack_call_result statinfo_meta.pack_call_result]
     [unpack_call_result statinfo_meta.unpack_call_result]
     [wrap statinfo_meta.wrap]
     [lookup statinfo_meta.lookup]
     [gather statinfo_meta.gather]
     [replace statinfo_meta.replace]
     [find statinfo_meta.find]
     [union statinfo_meta.union]
     [intersect statinfo_meta.intersect]

     call_result_key
     index_result_key
     index_get_key
     index_set_key
     append_key
     dot_provider_key
     sequence_constructor_key
     sequence_element_key
     list_bounds_key
     pairlist_bounds_key
     maybe_key
     values_key
     indirect_key)))

(define-for-syntax (make-static-info-macro-macro in-space convert-id)
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ (_::quotes (group name::name)) (body-tag::block body ...))
         #`((define-syntax #,(in-space #'name.name)
              (#,convert-id 'name.name (rhombus-body-at body-tag body ...))))]))))

(define-defn-syntax macro
  (make-static-info-macro-macro in-static-info-space #'convert-static-info))

(define-defn-syntax key
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ name::name (body-tag::block
                        (~and
                         (~seq (group kw clause-block) ...)
                         (~seq
                          (~alt (~optional (group #:union
                                                  (union-tag::block
                                                   union-body ...)))
                                (~optional (group #:intersect
                                                  (intersect-tag::block
                                                   intersect-body ...))))
                          ...))))
         (unless (attribute union-tag)
           (raise-syntax-error #f "missing a `~union` clause" stx))
         (unless (attribute intersect-tag)
           (raise-syntax-error #f "missing an `~intersect` clause" stx))
         #`((define-syntax name.name
              (make-key (~@ kw (rhombus-body-expression clause-block)) ...)))]))))

(define-for-syntax (convert-static-info who stx)
  (unless (syntax? stx)
    (raise-bad-macro-result who "static info" stx))
  (define si (syntax->list (pack who stx)))
  (static-info (lambda () si)))

(define-for-syntax (convert-static-info-key who val)
  (unless (static-info-key? val)
    (raise-annotation-failure 'statinfo.key val "static info key"))
  val)

(define-for-syntax (pack who stx)
  (pack-static-infos who (unpack-term stx who #f)))

(define-for-syntax (check-syntax who s)
  (unless (syntax? s)
    (raise-annotation-failure who s "Syntax")))

(define-for-syntax (unpack-identifier who id-in)
  (define id (unpack-term/maybe id-in))
  (unless (identifier? id)
    (raise-annotation-failure who id-in "Identifier"))
  id)

(define-for-syntax (make-key #:union union #:intersect intersect)
  (define (check-proc union)
    (unless (and (procedure? union)
                 (procedure-arity-includes? union 2))
      (raise-annotation-failure 'statinfo.key union "Function.of_arity(2)")))
  (check-proc union)
  (check-proc intersect)
  (static-info-key union intersect))

(define-for-syntax (extract-expr-static-infos who form)
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

(define-for-syntax (static-infos-merge who statinfos-unpacked merge)
  (define statinfos
    (for/list ([stx (in-list statinfos-unpacked)])
      (check-syntax who stx)
      (pack-static-infos who stx)))
  (unpack-static-infos
   who
   (if (null? statinfos)
       #'()
       (for/fold ([merged (car statinfos)]) ([statinfo (in-list (cdr statinfos))])
         (merge merged statinfo)))))

(begin-for-syntax
  (define/arity (statinfo_meta.pack stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (pack who stx))

  (define/arity (statinfo_meta.unpack stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (unpack-static-infos who stx))

  (define/arity (statinfo_meta.pack_group stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (datum->syntax
     #f
     (map (lambda (stx) (pack-static-infos who stx))
          (cdr (syntax->list (unpack-group stx who #f))))))

  (define/arity (statinfo_meta.unpack_group stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    #`(group . #,(map (lambda (stx) (unpack-static-infos who stx))
                      (syntax->list stx))))

  (define/arity (statinfo_meta.pack_call_result infos)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (unless (and (treelist? infos)
                 (for/and ([r (in-treelist infos)])
                   (and (treelist? r)
                        (= 2 (treelist-length r))
                        (exact-integer? (treelist-ref r 0))
                        (syntax? (treelist-ref r 1)))))
      (raise-annotation-failure who
                                infos
                                "matching([[_ :: Int, _ :: Syntax], ...])"))
    (cond
      [(and (= 1 (treelist-length infos))
            (eqv? -1 (treelist-ref (treelist-ref infos 0) 0)))
       (pack-static-infos who (treelist-ref (treelist-ref infos 0) 1))]
      [else
       (datum->syntax
        #f
        `(#:at_arities
          ,@(for/list ([i (in-treelist infos)])
              `(,(treelist-ref i 0)
                ,(pack-static-infos who (treelist-ref i 1))))))]))

  (define/arity (statinfo_meta.unpack_call_result stx)
    #:static-infos ((#%call-result #,(get-treelist-static-infos)))
    (check-syntax who stx)
    (syntax-parse stx
      [(#:at_arities rs)
       (for/treelist ([r (in-list (syntax->list #'rs))])
         (syntax-parse r
           [(mask:exact-integer results)
            (treelist (syntax-e #'mask)
                      (unpack-static-infos who #'results))]
           [_
            (raise-arguments-error* who rhombus-realm
                                    "ill-formed unpacked call result"
                                    "syntax object" stx)]))]
      [_
       (treelist (treelist -1 (unpack-static-infos who stx)))]))

  (define/arity (statinfo_meta.wrap form info)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who form)
    (check-syntax who info)
    (define e
      (wrap-static-info* (wrap-expression form) (pack who info)))
    (pack-term (relocate+reraw e #`(parsed #:rhombus/expr #,e))))

  (define/arity (statinfo_meta.lookup form key-in)
    #:static-infos ((#%call-result ((#%maybe #,(get-syntax-static-infos)))))
    (check-syntax who form)
    (define key (unpack-identifier who key-in))
    (define si (extract-expr-static-infos who form))
    (and si (static-info-lookup si key)))

  (define/arity (statinfo_meta.gather form)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who form)
    (define si (extract-expr-static-infos who form))
    (unpack-static-infos who (or si #'())))

  (define/arity (statinfo_meta.replace form statinfos)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who form)
    (check-syntax who statinfos)
    (define e
      (wrap-static-info* (discard-static-infos (wrap-expression form))
                         (pack-static-infos who statinfos)))
    (pack-term (relocate+reraw e #`(parsed #:rhombus/expr #,e))))

  (define/arity (statinfo_meta.find si-stx key-in)
    #:static-infos ((#%call-result ((#%maybe #,(get-syntax-static-infos)))))
    (define si (pack-static-infos who si-stx))
    (define key (unpack-identifier who key-in))
    (static-info-lookup si key))

  (define/arity (statinfo_meta.union . statinfos)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (static-infos-merge who statinfos static-infos-union))

  (define/arity (statinfo_meta.intersect . statinfos)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (static-infos-merge who statinfos static-infos-intersect)))

(define-syntax-rule (define-key key id)
  (begin-for-syntax
    (define key (quote-syntax id))
    (define-static-info-syntax key
      #:getter get-syntax-static-infos)))

(define-key call_result_key #%call-result)
(define-key index_result_key #%index-result)
(define-key index_get_key #%index-get)
(define-key index_set_key #%index-set)
(define-key append_key #%append)
(define-key dot_provider_key #%dot-provider)
(define-key sequence_constructor_key #%sequence-constructor)
(define-key sequence_element_key #%sequence-element)
(define-key list_bounds_key #%treelist-bounds)
(define-key pairlist_bounds_key #%list-bounds)
(define-key maybe_key #%maybe)
(define-key values_key #%values)
(define-key indirect_key #%indirect-static-info)
