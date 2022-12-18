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

(define-for-syntax (build-class-annotation-form super annotation-rhs
                                                super-constructor-fields
                                                exposed-internal-id intro
                                                names)
  (with-syntax ([(name name-instance name?
                       internal-name-instance
                       constructor-name-fields constructor-public-name-fields super-name-fields
                       field-keywords public-field-keywords super-field-keywords)
                 names])
    (with-syntax ([core-ann-name (if annotation-rhs
                                     (car (generate-temporaries #'(name)))
                                     #'name)])
      (define (make-ann-def id no-super? name-fields keywords name-instance-stx)
        (define len (length (syntax->list name-fields)))
        (with-syntax ([(constructor-name-field ...) name-fields]
                      [(field-keyword ...) keywords]
                      [(super-name-field ...) (if no-super? '() #'super-name-fields)]
                      [(super-field-keyword ...) (if no-super? '() #'super-field-keywords)]
                      [name-instance name-instance-stx])
          #`(define-annotation-constructor #,id
              ([accessors (list (quote-syntax super-name-field) ...
                                (quote-syntax constructor-name-field) ...)])
              (quote-syntax name?)
              (quote-syntax ((#%dot-provider name-instance)))
              (quote #,(+ len (if no-super? 0 (length super-constructor-fields))))
              (super-field-keyword ... field-keyword ...)
              (make-class-instance-predicate accessors)
              (make-class-instance-static-infos accessors))))
      (append
       (if exposed-internal-id
           (list
            (begin
              (make-ann-def exposed-internal-id #t #'constructor-name-fields #'field-keywords
                            #'internal-name-instance)))
           null)
       (cond
         [annotation-rhs
          (list
           #`(define-annotation-syntax name
               (wrap-class-transformer name
                                       #,(intro annotation-rhs)
                                       make-annotation-prefix-operator
                                       "class")))]
         [else
          (list
           (make-ann-def #'name #f #'constructor-public-name-fields #'public-field-keywords
                         #'name-instance))])))))

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

(define-for-syntax (build-guard-expr super-fields fields predicates annotation-strs)
  (and (any-stx? predicates)
       #`(lambda (#,@super-fields #,@fields who)
           (values #,@super-fields
                   #,@(for/list ([field (in-list fields)]
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
