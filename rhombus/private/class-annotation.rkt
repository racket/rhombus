#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     (only-in enforest/operator operator-proc)
                     "srcloc.rkt"
                     "with-syntax.rkt"
                     "tag.rkt"
                     "class-parse.rkt")
         "annotation.rkt"
         (submod "annotation.rkt" for-class)
         (submod "annotation-syntax.rkt" for-class)
         "parens.rkt"
         (for-syntax "class-transformer.rkt")
         (submod "dot.rkt" for-dot-provider))

(provide (for-syntax build-class-annotation-form
                     build-guard-expr)
         compose-annotation-check)

(define-for-syntax (build-class-annotation-form super annotation-id options
                                                constructor-fields super-constructor-fields
                                                exposed-internal-id intro
                                                names)
  (with-syntax ([(name name-instance name?
                       [constructor-name-field ...] [super-name-field ...]
                       [field-keyword ...] [super-field-keyword ...])
                 names])
    (with-syntax ([core-ann-name (if (hash-ref options 'annotation #f)
                                     (car (generate-temporaries #'(name)))
                                     #'name)]
                  [parse-name-of (and super
                                      annotation-id
                                      (car (generate-temporaries #'(name))))])
      (append
       (list
        #`(define-annotation-constructor core-ann-name
            ([accessors #,(if (syntax-e #'parse-name-of)
                              #'(list (quote-syntax constructor-name-field) ...)
                              #'(list (quote-syntax super-name-field) ...
                                      (quote-syntax constructor-name-field) ...))]
             #,@(if (syntax-e #'parse-name-of)
                    #`([parse-name-of
                        (make-curried-annotation-of-tranformer (quote-syntax #,(class-desc-annotation-id super)))])
                    null))
            (quote-syntax name?)
            (quote-syntax ((#%dot-provider name-instance)))
            #,(if (syntax-e #'parse-name-of)
                  (length constructor-fields)
                  #`(quote #,(+ (length constructor-fields)
                                (length super-constructor-fields))))
            #,(if (syntax-e #'parse-name-of)
                  #'(field-keyword ...)
                  #'(super-field-keyword ... field-keyword ...))
            (make-class-instance-predicate accessors)
            (make-class-instance-static-infos accessors)
            #:parse-of #,(if (syntax-e #'parse-name-of)
                             #'parse-name-of
                             #'parse-annotation-of)))
       (if exposed-internal-id
           (list
            #`(define-annotation-syntax #,exposed-internal-id (make-rename-transformer (quote-syntax core-ann-name))))
           null)
       (cond
         [(hash-ref options 'annotation #f)
          => (lambda (ann)
               (list
                #`(define-annotation-syntax #,(intro annotation-id) (make-rename-transformer
                                                                     (quote-syntax #,(in-annotation-space #'core-ann-name))))
                #`(define-annotation-syntax name
                    (wrap-class-transformer name #,(intro (cadr ann)) make-annotation-prefix-operator))))]
         [else null])))))

(define-for-syntax (make-curried-annotation-of-tranformer super-annotation-id)
  (lambda (tail predicate-stx static-infos
                sub-n kws predicate-maker info-maker)
    (syntax-parse tail
      [(form-id p-term (tag::parens g ...) . new-tail)
       #:with p::annotation #`(#,group-tag #,super-annotation-id (op |.|) of p-term)
       (define-values (ann c-tail) (parse-annotation-of #'(form-id (tag g ...))
                                                        predicate-stx static-infos
                                                        sub-n kws predicate-maker info-maker))
       (with-syntax-parse ([p::annotation-form #'p.parsed]
                           [c::annotation-form ann])
         (values (annotation-form
                  #`(let ([p? p.predicate]
                          [c? c.predicate])
                      (lambda (v) (and (p? v) (c? v))))
                  (append (syntax->list #'p.static-infos)
                          #'c.static-infos))
                 #'new-tail))])))

(define-for-syntax (make-class-instance-predicate accessors)
  (lambda (arg predicate-stxs)
    #`(and #,@(for/list ([acc (in-list accessors)]
                         [pred (in-list predicate-stxs)])
                #`(#,pred (#,acc #,arg))))))

(define-for-syntax (make-class-instance-static-infos accessors)
  (lambda (static-infoss)
    (for/list ([acc (in-list accessors)]
               [static-infos (in-list static-infoss)])
      #`(#,acc #,static-infos))))

(define-for-syntax (build-guard-expr fields predicates annotation-strs)
  (and (any-stx? predicates)
       #`(lambda (#,@fields who)
           (values #,@(for/list ([field (in-list fields)]
                                 [predicate (in-list predicates)]
                                 [annotation-str (in-list annotation-strs)])
                        (cond
                          [(not (syntax-e predicate)) field]
                          [else #`(if (#,predicate #,field)
                                      #,field
                                      (raise-annotation-failure who
                                                                #,field
                                                                '#,annotation-str))]))))))

(define-syntax (compose-annotation-check stx)
  (syntax-parse stx
    [(_ mutator who #f _) #'mutator]
    [(_ mutator who predicate annotation-str)
     #`(let ([m mutator])
         (lambda (obj val)
           (unless (predicate val)
             (raise-annotation-failure 'who
                                       val
                                       'annotation-str))
           (m obj val)))]))
