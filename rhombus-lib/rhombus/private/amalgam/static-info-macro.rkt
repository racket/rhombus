#lang racket/base
(require (for-syntax racket/base
                     racket/treelist
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
                     "annotation-failure.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     (for-syntax racket/base)
                     (only-in (submod "list.rkt" for-listable)
                              get-treelist-static-infos)
                     "syntax-wrap.rkt")
         "space-provide.rkt"
         "definition.rkt"
         "name-root-ref.rkt"
         "name-root-space.rkt"
         "static-info.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         "parens.rkt"
         "function-arity.rkt"
         "function-arity-key.rkt"
         "call-result-key.rkt"
         "append-key.rkt"
         "contains-key.rkt"
         "index-key.rkt"
         "index-result-key.rkt"
         "flonum-key.rkt"
         "fixnum-key.rkt"
         "dot-provider-key.rkt"
         "sequence-constructor-key.rkt"
         "sequence-element-key.rkt"
         "list-bounds-key.rkt"
         "maybe-key.rkt"
         "values-key.rkt"
         "indirect-static-info-key.rkt"
         "is-static.rkt"
         (submod "arithmetic.rkt" static-infos))

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
     [pack_dependent_result statinfo_meta.pack_dependent_result]
     [unpack_dependent_result statinfo_meta.unpack_dependent_result]
     [check_function_arity statinfo_meta.check_function_arity]
     [pack_index_result statinfo_meta.pack_index_result]
     [unpack_index_result statinfo_meta.unpack_index_result]
     [unpack_index_result_at_index statinfo_meta.unpack_index_result_at_index]
     [unpack_uniform_index_result statinfo_meta.unpack_uniform_index_result]
     [wrap statinfo_meta.wrap]
     [lookup statinfo_meta.lookup]
     [gather statinfo_meta.gather]
     [replace statinfo_meta.replace]
     [find statinfo_meta.find]
     [or statinfo_meta.or]
     [and statinfo_meta.and]

     function_arity_key
     call_result_key
     dependent_result_key
     index_result_key
     index_get_key
     index_set_key
     append_key
     contains_key
     flonum_key
     fixnum_key
     dot_provider_key
     sequence_constructor_key
     sequence_element_key
     list_bounds_key
     pairlist_bounds_key
     maybe_key
     values_key
     indirect_key
     none_key)))

(define-for-syntax (make-static-info-macro-macro in-space convert-id)
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ (_::quotes (group name::name)) (body-tag::block body ...))
         #`((define-syntax #,(in-space #'name.name)
              (#,convert-id 'name.name (rhombus-body-at body-tag body ...))))]))))

(define-defn-syntax macro
  (make-static-info-macro-macro in-static-info-space #'convert-static-info))

(define-defn-syntax key
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ name::name (body-tag::block
                        (~and
                         (~seq (group kw clause-block) ...)
                         (~seq
                          (~alt (~optional (group #:or
                                                  (or-tag::block
                                                   or-body ...)))
                                (~optional (group #:and
                                                  (and-tag::block
                                                   and-body ...))))
                          ...))))
         (unless (attribute or-tag)
           (raise-syntax-error #f "missing a `~and` clause" stx))
         (unless (attribute and-tag)
           (raise-syntax-error #f "missing an `~and` clause" stx))
         #`((define-syntax name.name
              (make-key (~@ kw (rhombus-body-expression clause-block)) ...)))]))))

(define-for-syntax (convert-static-info who stx)
  (unless (syntax*? stx)
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
  (unless (syntax*? s)
    (raise-annotation-failure who s "Syntax")))

(define-for-syntax (unpack-identifier who id-in)
  (define id (unpack-term/maybe id-in))
  (unless (identifier? id)
    (raise-annotation-failure who id-in "Identifier"))
  id)

(define-for-syntax (make-key #:or or-proc #:and and-proc)
  (define (check-proc or-proc)
    (unless (and (procedure? or-proc)
                 (procedure-arity-includes? or-proc 2))
      (raise-annotation-failure 'statinfo.key or-proc "Function.of_arity(2)")))
  (check-proc or-proc)
  (check-proc and-proc)
  (static-info-key or-proc and-proc))

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

(define-for-syntax (static-infos-merge who statinfos-unpacked merge zero)
  (define statinfos
    (for/list ([stx (in-list statinfos-unpacked)])
      (check-syntax who stx)
      (pack-static-infos who stx)))
  (unpack-static-infos
   who
   (if (null? statinfos)
       zero
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
                        (syntax*? (treelist-ref r 1)))))
      (raise-annotation-failure who
                                infos
                                "[[Int, Syntax], ...]"))
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
    (syntax-parse (syntax-unwrap stx)
      [(#:at_arities rs)
       (for/treelist ([r (in-list (syntax->list #'rs))])
         (syntax-parse r
           [(mask:exact-integer results)
            (treelist (syntax-e #'mask)
                      (unpack-static-infos who #'results))]
           [_
            (raise-arguments-error* who rhombus-realm
                                    "ill-formed packed call result"
                                    "syntax object" stx)]))]
      [_
       (treelist (treelist -1 (unpack-static-infos who stx)))]))

  (define/arity (statinfo_meta.pack_dependent_result id-stx data)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (define id (unpack-term id-stx #f #f))
    (unless (identifier? id) (raise-annotation-failure who id-stx "Identifier"))
    (unless (syntax*? id) (raise-annotation-failure who data "Syntax"))
    #`(#,id #,data))

  (define/arity (statinfo_meta.unpack_dependent_result stx)
    #:static-infos ((#%call-result ((#%values #,(get-treelist-static-infos)
                                              #,(get-treelist-static-infos)))))
    (syntax-parse (unpack-term stx who #f)
      [(id:identifier data) (values #'id #'data)]
      [_ (raise-arguments-error* who rhombus-realm
                                 "ill-formed packed dependent result"
                                 "syntax object" stx)]))

  (define/arity (statinfo_meta.check_function_arity stx n kws)
    (check-syntax who stx)
    (unless (exact-nonnegative-integer? n) (raise-annotation-failure who n "NonnegInt"))
    (unless (and (treelist? kws)
                 (for/and ([e (in-treelist kws)])
                   (keyword? e)))
      (raise-annotation-failure who kws "List.of(Keyword)"))
    (define a (syntax->datum (syntax-unwrap stx)))
    (unless (or (exact-integer? a)
                (and (list? a)
                     (= (length a))
                     (exact-integer? (car a))
                     (hash? (cadr a))
                     (or (not (caddr a)) (hash? (caddr a)))))
      (raise-arguments-error* who rhombus-realm
                              "ill-formed packed arity description"
                              "syntax object" stx))
    (check-arity #f #f a n (treelist->list kws) #f #f #f #:always? #t))
    
  (define/arity (statinfo_meta.pack_index_result infos keyed-infos)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (unless (or (not infos) (syntax*? infos))
      (raise-annotation-failure who infos "maybe(Syntax)"))
    (unless (and (treelist? keyed-infos)
                 (for/and ([r (in-treelist keyed-infos)])
                   (and (treelist? r)
                        (= 2 (treelist-length r))
                        (syntax*? (treelist-ref r 1)))))
      (raise-annotation-failure who keyed-infos "[[Any, Syntax], ...]"))
    (define packed-infos (and infos (pack-static-infos who infos)))
    (cond
      [(and (treelist-empty? keyed-infos)
            packed-infos)
       packed-infos]
      [else
       #`(#:at_index
          #,packed-infos
          #,@(for/list ([i (in-treelist infos)])
               `(,(treelist-ref i 0)
                 ,(pack-static-infos who (treelist-ref i 1)))))]))

  (define/arity (statinfo_meta.unpack_index_result stx)
    #:static-infos ((#%call-result ((#%values #,(get-syntax-static-infos)
                                              ((#%index-result
                                                ((#%index-result (#:at_index
                                                                  #f
                                                                  ((0 ())
                                                                   (1 #,(get-syntax-static-infos)))))
                                                 . #,(get-treelist-static-infos)))
                                               . #,(get-treelist-static-infos))))))
    (check-syntax who stx)
    (syntax-parse stx
      [(#:at_index other-si (idx i-si) ...)
       (values (and (syntax-e #'othersi) #'othersi)
               (for/treelist ([idx (in-list (syntax->list #'(idx ...)))]
                              [i-si (in-list (syntax->list #'(i-si ...)))])
                 (treelist (syntax->datum idx) i-si)))]
      [_
       (values stx empty-treelist)]))
    
  (define/arity (statinfo_meta.unpack_index_result_at_index stx key)
    #:static-infos ((#%call-result #,(get-treelist-static-infos)))
    (check-syntax who stx)
    (extract-index-result stx key))

  (define/arity (statinfo_meta.unpack_uniform_index_result stx key)
    #:static-infos ((#%call-result #,(get-treelist-static-infos)))
    (check-syntax who stx)
    (extract-index-uniform-result stx))

  (define/arity (statinfo_meta.wrap form info)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who form)
    (check-syntax who info)
    (define e
      (wrap-static-info* (wrap-expression (syntax-unwrap form)) (pack who info)))
    (pack-term (relocate+reraw e #`(parsed #:rhombus/expr #,e))))

  (define/arity (statinfo_meta.lookup form key-in)
    #:static-infos ((#%call-result ((#%maybe #,(get-syntax-static-infos)))))
    (check-syntax who form)
    (define key (unpack-identifier who key-in))
    (define si (extract-expr-static-infos who (syntax-unwrap form)))
    (and si (static-info-lookup si key)))

  (define/arity (statinfo_meta.gather form)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who form)
    (define si (extract-expr-static-infos who (syntax-unwrap form)))
    (unpack-static-infos who (or si #'())))

  (define/arity (statinfo_meta.replace form statinfos)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who form)
    (check-syntax who statinfos)
    (define e
      (wrap-static-info* (discard-static-infos (wrap-expression (syntax-unwrap form)))
                         (pack-static-infos who statinfos)))
    (pack-term (relocate+reraw e #`(parsed #:rhombus/expr #,e))))

  (define/arity (statinfo_meta.find si-stx key-in)
    #:static-infos ((#%call-result ((#%maybe #,(get-syntax-static-infos)))))
    (define si (pack-static-infos who si-stx))
    (define key (unpack-identifier who key-in))
    (static-info-lookup si key))

  (define/arity (statinfo_meta.or . statinfos)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (static-infos-merge who statinfos static-infos-or #'((#%none #t))))

  (define/arity (statinfo_meta.and . statinfos)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (static-infos-merge who statinfos static-infos-and #'())))

(define-syntax-rule (define-key key id)
  (begin-for-syntax
    (define key (quote-syntax id))
    (define-static-info-syntax key
      #:getter get-syntax-static-infos)))

(define-key function_arity_key #%function-arity)
(define-key call_result_key #%call-result)
(define-key dependent_result_key #%dependent-result)
(define-key index_result_key #%index-result)
(define-key index_get_key #%index-get)
(define-key index_set_key #%index-set)
(define-key append_key #%append)
(define-key contains_key #%contains)
(define-key dot_provider_key #%dot-provider)
(define-key sequence_constructor_key #%sequence-constructor)
(define-key sequence_element_key #%sequence-element)
(define-key list_bounds_key #%treelist-bounds)
(define-key pairlist_bounds_key #%list-bounds)
(define-key maybe_key #%maybe)
(define-key flonum_key #%flonum)
(define-key fixnum_key #%fixnum)
(define-key values_key #%values)
(define-key indirect_key #%indirect-static-info)
(define-key none_key #%none)
