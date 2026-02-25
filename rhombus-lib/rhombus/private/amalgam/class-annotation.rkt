#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "class-parse.rkt")
         (submod "annotation.rkt" for-class)
         (submod "annot-macro.rkt" for-class)
         "class-transformer.rkt"
         "dotted-sequence-parse.rkt"
         "static-info.rkt"
         "expression.rkt"
         "annotation.rkt"
         "binding.rkt"
         "name-root-space.rkt")

(provide (for-syntax build-class-annotation-form
                     build-guard-expr
                     build-extra-internal-id-aliases
                     no-annotation-transformer)
         compose-annotation-check)

(define-for-syntax (build-class-annotation-form super annotation-rhs
                                                super-constructor-fields
                                                exposed-internal-id internal-of-id intro
                                                names)
  (with-syntax ([(name name-extends tail-name
                       name? name-of
                       all-static-infos internal-all-static-infos
                       make-converted-name make-converted-internal
                       constructor-name-fields constructor-public-name-fields
                       constructor-name-field-mutable?s constructor-public-name-field-mutable?s
                       super-name-fields super-public-name-fields
                       super-name-field-mutable?s super-public-name-field-mutable?s
                       field-keywords public-field-keywords super-field-keywords super-public-field-keywords
                       orig-stx)
                 names])
    (define (make-ann-defs id of-id no-super?
                           name-fields keywords field-mutable?s
                           super-name-fields super-field-keywords super-field-mutable?s
                           make-converted-id
                           all-static-infos-stx)
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
        (define mutable?s (map syntax-e (append (syntax->list field-mutable?s)
                                                (syntax->list super-field-mutable?s))))
        (define has-mutable? (for/or ([mutable? (in-list mutable?s)])
                               mutable?))
        (append
         (list
          #`(define-annotation-constructor (#,id #,of-id)
              ([accessors (list (quote-syntax super-name-field) ...
                                (quote-syntax constructor-name-field) ...)]
               #,@(if has-mutable?
                      #`([immutable-accessors
                          (list #,@(for/list ([mutable? (in-list mutable?s)]
                                              [accessor (in-list (syntax->list
                                                                  #'(super-name-field ... constructor-name-field ...)))])
                                     (if mutable?
                                         #f
                                         #`(quote-syntax #,accessor))))])
                      '()))
              (quote-syntax name?)
              #,all-static-infos-stx
              (quote #,(+ len (if no-super? 0 (length super-constructor-fields))))
              (super-field-keyword ... field-keyword ...)
              (make-class-instance-predicate accessors)
              (quote-syntax class-instance-static-infos) #,(if has-mutable? #'immutable-accessors #'accessors)
              #,(if name-build-convert
                    #`(quote-syntax #,name-build-convert)
                    #'not-supported-due-to-internal-reasons)
              (quote-syntax #,(if name-build-convert
                                  #'(super-name-field ...
                                     constructor-name-field ...)
                                  #'()))
              #:extends name-extends
              #:stx orig-stx))
         (if name-build-convert
             (list
              #`(define-syntax #,name-build-convert
                  (make-class-instance-converter (quote-syntax #,make-converted-id))))
             null))))
    (append
     (if exposed-internal-id
         (make-ann-defs exposed-internal-id internal-of-id #t
                        #'constructor-name-fields #'field-keywords #'constructor-name-field-mutable?s
                        #'super-name-fields #'super-field-keywords #'super-name-field-mutable?s
                        #'make-converted-internal
                        #'internal-all-static-infos)
         null)
     (cond
       [annotation-rhs
        (cond
          [(eq? '#:none (syntax-e annotation-rhs))
           null]
          [else
           (list
            ;; build `(define-annotation-syntax name ....)`:
            (build-syntax-definition/maybe-extension
             'rhombus/annot #'name #'name-extends
             (if (eq? '#:error (syntax-e annotation-rhs))
                 #'no-annotation-transformer
                 (wrap-class-transformer #'name #'tail-name
                                         (intro annotation-rhs)
                                         #'make-annotation-prefix-operator
                                         #:extra-args (list #'ctx)
                                         "class"))
             #:form #'orig-stx))])]
       [else
        (make-ann-defs #'name #'name-of #f
                       #'constructor-public-name-fields #'public-field-keywords #'constructor-public-name-field-mutable?s
                       #'super-public-name-fields #'super-public-field-keywords #'super-public-name-field-mutable?s
                       #'make-converted-name
                       #'all-static-infos)]))))

(define-for-syntax (make-class-instance-predicate accessors)
  (lambda (predicate-stxs)
    (with-syntax ([(pred ...) (generate-temporaries predicate-stxs)]
                  [(pred-stx ...) predicate-stxs]
                  [(acc ...) accessors])
      #`(let ([pred pred-stx]
              ...)
          (lambda (arg)
            (and (pred (acc arg))
                 ...))))))
  
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

(define-syntax (class-instance-static-infos accessors static-infoss)
  (for/list ([acc (in-list (if (syntax? accessors) (syntax->list accessors) accessors))]
             [static-infos (in-list static-infoss)]
             #:when acc)
    #`(#,acc #,static-infos)))

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

(define-for-syntax (build-extra-internal-id-aliases internal extra-internals
                                                    #:interface? [interface? #f])
  (for*/list ([extra (in-list extra-internals)]
              [in-space (in-list (if interface?
                                     (list in-annotation-space
                                           in-name-root-space)
                                     (list in-expression-space
                                           in-annotation-space
                                           in-binding-space
                                           in-name-root-space)))])
    #`(define-syntax #,(in-space extra) (make-rename-transformer (quote-syntax #,(in-space internal))))))

(define-for-syntax no-annotation-transformer
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx ctx)
     (raise-syntax-error #f "cannot be used as an annotation" stx))))

(define-for-syntax not-supported-due-to-internal-reasons
  (string-append "converter annotations are not supported for fields;\n"
                 " internally, the class may be non-final, have a mutable field, or\n"
                 " have a non-default expression form but default annotation form"))
