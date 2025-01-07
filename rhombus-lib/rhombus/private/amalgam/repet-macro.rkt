#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     enforest/transformer-result
                     "pack.rkt"
                     "static-info-pack.rkt"
                     "macro-result.rkt"
                     "tail-returner.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     "realm.rkt"
                     "annotation-failure.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     "maybe-key.rkt"
                     "syntax-wrap.rkt"
                     "srcloc.rkt"
                     (for-syntax racket/base))
         (only-in "space.rkt" space-syntax)
         "treelist.rkt"
         "space-provide.rkt"
         "repetition.rkt"
         "space.rkt"
         "macro-macro.rkt"
         "parse.rkt"
         "parens.rkt"
         "wrap-expression.rkt"
         (submod "dot.rkt" for-syntax-meta))

(define+provide-space repet rhombus/repet
  #:fields
  (macro))

(provide (for-syntax (for-space rhombus/namespace
                                repet_meta)))

(begin-for-syntax
  (define-name-root repet_meta
    #:fields
    (space
     [pack_list repet_meta.pack_list]
     [unpack_list repet_meta.unpack_list]
     [pack_generator repet_meta.pack_generator]
     [unpack_generator repet_meta.unpack_generator]
     [parse_dot repet_meta.parse_dot]
     Parsed
     AfterPrefixParsed
     AfterInfixParsed
     NameStart)))

(define-operator-definition-transformer macro
  'macro
  rhombus/repet
  #'make-repetition-prefix-operator
  #'make-repetition-infix-operator
  #'prefix+infix)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :repetition #:rhombus/repet
    NameStart in-repet-space
    AfterPrefixParsed :prefix-op+repetition-use+tail
    AfterInfixParsed :infix-op+repetition-use+tail))

(define-for-syntax space
  (space-syntax rhombus/repet))

(begin-for-syntax
  (struct prefix+infix (prefix infix)
    #:property prop:repetition-prefix-operator (lambda (self) (prefix+infix-prefix self))
    #:property prop:repetition-infix-operator (lambda (self) (prefix+infix-infix self))))

(define-for-syntax (extract-repetition form proc)
  (syntax-parse (if (syntax*? form)
                    (unpack-group form proc #f)
                    #'#f)
    [b::repetition #'b.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "repetition" form)]))

(define-for-syntax (wrap-parsed stx)
  #`(parsed #:rhombus/repet #,stx))

(define-for-syntax (make-repetition-infix-operator order prec protocol proc assc)
  (repetition-infix-operator
   order
   prec
   protocol
   (if (eq? protocol 'macro)
       (lambda (form1 tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (wrap-parsed form1) (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-repetition form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc))
       (lambda (form1 form2 stx)
         (extract-repetition (proc (wrap-parsed form1) (wrap-parsed form2) stx)
                          proc)))
   assc))

(define-for-syntax (make-repetition-prefix-operator order prec protocol proc)
  (repetition-prefix-operator
   order
   prec
   protocol
   (if (eq? protocol 'macro)
       (lambda (tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-repetition form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc))
       (lambda (form stx)
         (extract-repetition (proc (wrap-parsed form) stx)
                             proc)))))

(define-for-syntax (check-syntax who s)
  (unless (syntax*? s)
    (raise-annotation-failure who s "Syntax")))

(begin-for-syntax
  (define/arity (repet_meta.pack_list stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (group)
      [(_::parens orig-form
                  seq-expr
                  (group bind-depth:exact-nonnegative-integer)
                  (group use-depth:exact-nonnegative-integer)
                  (group element-static-infos))
       #:with (elem) (generate-temporaries '(elem))
       (wrap-parsed
        (if (= 0 (syntax-e #'bind-depth))
            (make-repetition-info (unpack-tail #'orig-form who #f)
                                  #'()
                                  (wrap-expression #'seq-expr)
                                  (pack-static-infos who #'element-static-infos)
                                  #'use-depth)
            (make-repetition-info (unpack-tail #'orig-form who #f)
                                  #`(([(elem) (in-treelist #,(wrap-expression #'seq-expr))])
                                     #,@(for/list ([i (in-range (sub1 (syntax-e #'bind-depth)))])
                                          #`([(elem) (in-treelist elem)])))
                                  #'elem
                                  (pack-static-infos who #'element-static-infos)
                                  #'use-depth)))]
      [_ (raise-arguments-error* who rhombus-realm
                                 "ill-formed unpacked repetition info"
                                 "syntax object" stx)]))

  (define/arity (repet_meta.unpack_list stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/repet r::repetition-info)
       (define depth (length (syntax->list #'r.for-clausess)))
       (pack-term
        #`(parens #,(unpack-group (pack-tail #'r.rep-expr) #f #f)
                  (group (parsed #:rhombus/expr #,(repetition-as-nested-lists #'r depth #'for/treelist)))
                  (group #,depth)
                  (group r.used-depth)
                  (group #,(unpack-static-infos who #'r.element-static-infos))))]))

  (define/arity (repet_meta.pack_generator stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (group parens)
      [(_::parens orig-form
                  (group (parens
                          (group (parens (group (parens (group (parens (group iter-id:identifier) ...))
                                                        r-rhs))
                                         ...))
                          ...))
                  body-expr
                  (group use-depth:exact-nonnegative-integer)
                  (group element-static-infos))
       #:with ((r-rhs-e ...) ...) (for/list ([r-rhss (in-list (syntax->list #'((r-rhs ...) ...)))])
                                    (map wrap-expression (syntax->list r-rhss)))
       (wrap-parsed
        (make-repetition-info (unpack-tail #'orig-form who #f)
                              #`(([(iter-id ...) r-rhs-e] ...) ...)
                              (wrap-expression #'body-expr)
                              (pack-static-infos who #'element-static-infos)
                              #'use-depth))]
      [_ (raise-arguments-error* who rhombus-realm
                                 "ill-formed unpacked repetition info"
                                 "syntax object" stx)]))

  (define/arity (repet_meta.unpack_generator stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/repet r::repetition-info)
       (define depth (length (syntax->list #'r.for-clausess)))
       (pack-term
        #`(parens #,(unpack-group (pack-tail #'r.rep-expr) #f #f)
                  (group (parens
                          (group (parens (group (parens (group (parens (group r.iter-id) ...))
                                                        (group (parsed #:rhombus/expr r.iter-rhs))))
                                         ...))
                          ...))
                  (group (parsed #:rhombus/expr r.body))
                  (group r.used-depth)
                  (group #,(unpack-static-infos who #'r.element-static-infos))))]))

  (define/arity (repet_meta.parse_dot form1 tail
                                      #:as_static [more-static? #f]
                                      #:disable_generic [no-generic? #t])
    #:static-infos ((#%call-result ((#%values (((#%maybe #,(get-syntax-static-infos)))
                                               ((#%maybe #,(get-syntax-static-infos))))))))
    (define-values (repet new-tail)
      (syntax-parse (unpack-term form1 who #f)
        #:datum-literals (parsed)
        [(parsed #:rhombus/repet v)
         (parse-dot-repet #'v (unpack-tail tail who #f)
                          #:as-static? more-static?
                          #:no-generic? no-generic?)]
        [_
         (raise-syntax-error who "not a parsed repetition" form1)]))
    (if repet
        (values (relocate+reraw repet #`(parsed #:rhombus/repet #,repet))
                (pack-tail new-tail))
        (values #f #f)))

  )
