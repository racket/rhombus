#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "class-parse.rkt")
         (submod "annotation.rkt" for-class)
         (submod "annot-macro.rkt" for-class)
         "class-transformer.rkt"
         (submod "dot.rkt" for-dot-provider)
         "dot-provider-key.rkt"
         "dotted-sequence-parse.rkt")

(provide (for-syntax build-class-annotation-form
                     build-guard-expr
                     build-extra-internal-id-aliases)
         compose-annotation-check)

(define-for-syntax (build-class-annotation-form super annotation-rhs
                                                super-constructor-fields
                                                exposed-internal-id internal-of-id intro
                                                names)
  (with-syntax ([(name name-extends tail-name
                       name-instance name? name-of
                       internal-name-instance indirect-static-infos internal-indirect-static-infos
                       dot-providers internal-dot-providers
                       make-converted-name make-converted-internal
                       constructor-name-fields constructor-public-name-fields super-name-fields super-public-name-fields
                       field-keywords public-field-keywords super-field-keywords super-public-field-keywords)
                 names])
    (define (make-ann-defs id of-id no-super?
                           name-fields keywords
                           super-name-fields super-field-keywords
                           name-instance-stx
                           make-converted-id indirect-static-infos-stx
                           dot-providers-stx)
      (define len (length (syntax->list name-fields)))
      (with-syntax ([(constructor-name-field ...) name-fields]
                    [(field-keyword ...) keywords]
                    [(super-name-field ...) (if no-super? '() super-name-fields)]
                    [(super-field-keyword ...) (if no-super? '() super-field-keywords)])
        (define name-build-convert
          (and (syntax-e make-converted-id)
               ((make-syntax-introducer)
                (datum->syntax #f
                               (string->symbol
                                (format "~a-build-convert" (syntax-e #'name)))))))
        (append
         (list
          #`(define-annotation-constructor (#,id #,of-id)
              ([accessors (list (quote-syntax super-name-field) ...
                                (quote-syntax constructor-name-field) ...)])
              (quote-syntax name?)
              #,(with-syntax ([dot-providers dot-providers-stx]
                              [indirect-static-infos indirect-static-infos-stx])
                  #`((#%dot-provider dot-providers)
                     . indirect-static-infos))
              (quote #,(+ len (if no-super? 0 (length super-constructor-fields))))
              (super-field-keyword ... field-keyword ...)
              (make-class-instance-predicate accessors)
              (make-class-instance-static-infos accessors)
              #,(if name-build-convert
                    #`(quote-syntax #,name-build-convert)
                    not-supported-due-to-internal-reasons)
              (quote-syntax #,(if name-build-convert
                                  #'(super-name-field ...
                                     constructor-name-field ...)
                                  #'()))
              #:extends name-extends))
         (if name-build-convert
             (list
              #`(define-syntax #,name-build-convert
                  (make-class-instance-converter (quote-syntax #,make-converted-id))))
             null))))
    (append
     (if exposed-internal-id
         (make-ann-defs exposed-internal-id internal-of-id #t
                        #'constructor-name-fields #'field-keywords
                        #'super-name-fields #'super-field-keywords
                        #'internal-name-instance
                        #'make-converted-internal
                        #'internal-indirect-static-infos
                        #'internal-dot-providers)
         null)
     (cond
       [annotation-rhs
        (list
         ;; build `(define-annotation-syntax name ....)`:
         (build-syntax-definition/maybe-extension
          'rhombus/annot #'name #'name-extends
          (wrap-class-transformer #'name #'tail-name
                                  (intro annotation-rhs)
                                  #'make-annotation-prefix-operator
                                  "class")))]
       [else
        (make-ann-defs #'name #'name-of #f
                       #'constructor-public-name-fields #'public-field-keywords
                       #'super-public-name-fields #'super-public-field-keywords
                       #'name-instance
                       #'make-converted-name
                       #'indirect-static-infos
                       #'dot-providers)]))))

(define-for-syntax (make-class-instance-predicate accessors)
  (lambda (arg predicate-stxs)
    #`(and #,@(for/list ([acc (in-list accessors)]
                         [pred (in-list predicate-stxs)])
                #`(#,pred (#,acc #,arg))))))

(define-for-syntax (make-class-instance-converter constructor)
  (lambda (arg-id build-convert-stxs kws accessors)
    (define orig-args (generate-temporaries accessors))
    (let loop ([build-convert-stxs build-convert-stxs] [accessors (syntax->list accessors)] [args orig-args])
      (cond
        [(null? accessors)
         #`(#,constructor #,@(if kws
                                 (apply
                                  append
                                  (for/list ([arg (in-list orig-args)]
                                             [kw (in-list kws)])
                                    (if kw
                                        (list kw arg)
                                        (list arg))))
                                 orig-args))]
        [else
         #`(#,(car build-convert-stxs)
            (#,(car accessors) #,arg-id)
            (lambda (#,(car args))
              #,(loop (cdr build-convert-stxs) (cdr accessors) (cdr args)))
            (lambda () #f))]))))

(define-for-syntax (make-class-instance-static-infos accessors)
  (lambda (static-infoss)
    (for/list ([acc (in-list accessors)]
               [static-infos (in-list static-infoss)])
      #`(#,acc #,static-infos))))

(define-for-syntax (build-guard-expr super-fields fields converters annotation-strs
                                     #:super [super-guard-id #f])
  (and (or (any-stx? converters)
           (and super-guard-id
                (syntax-e super-guard-id)))
       #`(lambda (#,@super-fields #,@fields who)
           (let-values #,(if (and super-guard-id
                                  (syntax-e super-guard-id))
                             #`([#,super-fields (#,super-guard-id #,@super-fields who)])
                             #'())
             (values #,@super-fields
                     #,@(for/list ([field (in-list fields)]
                                   [converter (in-list converters)]
                                   [annotation-str (in-list annotation-strs)])
                          (cond
                            [(not (syntax-e converter)) field]
                            [else #`(#,converter
                                     #,field
                                     who
                                     (lambda (val who)
                                       (raise-annotation-failure who val '#,annotation-str)))])))))))

(define-syntax (compose-annotation-check stx)
  (syntax-parse stx
    [(_ mutator who #f _) #'mutator]
    [(_ mutator who converter annotation-str)
     #`(let ([m mutator])
         (lambda (obj val)
           (let ([val (converter val
                                 'who
                                 (lambda (val who)
                                   (raise-annotation-failure who
                                                             val
                                                             'annotation-str)))])
             (m obj val))))]))

(define-for-syntax (build-extra-internal-id-aliases internal extra-internals)
  (for/list ([extra (in-list extra-internals)])
    #`(define-syntax #,extra (make-rename-transformer (quote-syntax #,internal)))))

      
(define-for-syntax not-supported-due-to-internal-reasons
  (string-append "converter annotations are not supported for fields;\n"
                 " internally, the class may be non-final, have a mutable field, or\n"
                 " have a non-default expression form but default annotation form"))
