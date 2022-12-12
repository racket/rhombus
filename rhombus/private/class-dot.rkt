#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     "srcloc.rkt"
                     "tag.rkt"
                     "class-parse.rkt"
                     "interface-parse.rkt")
         "class-method.rkt"
         "class-method-result.rkt"
         (submod "dot.rkt" for-dot-provider)
         "parens.rkt"
         "static-info.rkt"
         "expression.rkt"
         "assign.rkt"
         "name-root.rkt"
         "parse.rkt"
         (submod "function.rkt" for-call))

(provide (for-syntax build-class-dot-handling
                     build-interface-dot-handling

                     make-handle-class-instance-dot))

(define-for-syntax (build-class-dot-handling method-mindex method-vtable final?
                                             has-private? method-private exposed-internal-id
                                             names)
  (with-syntax ([(name constructor-name name-instance name-ref
                       make-internal-name internal-name-instance
                       [public-field-name ...] [private-field-name ...] [field-name ...]
                       [public-name-field ...] [name-field ...]
                       [private-field-desc ...]
                       [ex ...])
                 names])
    (define-values (method-names method-impl-ids method-defns)
      (method-static-entries method-mindex method-vtable #'name-ref final?))
    (with-syntax ([(method-name ...) method-names]
                  [(method-id ...) method-impl-ids])
      (append
       method-defns
       (append
        (list
         #'(define-name-root name
             #:root (class-expression-transformer (quote-syntax name) (quote-syntax constructor-name))
             #:fields ([public-field-name public-name-field]
                       ...
                       [method-name method-id]
                       ...
                       ex ...))
         #'(define-dot-provider-syntax name-instance
             (dot-provider-more-static (make-handle-class-instance-dot (quote-syntax name)
                                                                       #hasheq()
                                                                       #hasheq()))))
        (if exposed-internal-id
            (with-syntax ([([private-method-name private-method-id private-method-id/prop] ...)
                           (for/list ([(sym id/prop) (in-hash method-private)])
                             (define id (if (pair? id/prop) (car id/prop) id/prop))
                             (list sym id id/prop))])
              (list
               #`(define-name-root #,exposed-internal-id
                   #:root (class-expression-transformer (quote-syntax name) (quote-syntax make-internal-name))
                   #:fields ([field-name name-field]
                             ...
                             [method-name method-id]
                             ...
                             [private-method-name private-method-id]
                             ...
                             ex ...))
               #`(define-dot-provider-syntax internal-name-instance
                   (dot-provider-more-static (make-handle-class-instance-dot (quote-syntax name)
                                                                             (hasheq
                                                                              (~@ 'private-field-name
                                                                                  private-field-desc)
                                                                              ...)
                                                                             (hasheq
                                                                              (~@ 'private-method-name
                                                                                  (quote-syntax private-method-id/prop))
                                                                              ...))))))
            null))))))

(define-for-syntax (build-interface-dot-handling method-mindex method-vtable names)
  (with-syntax ([(name name-instance name-ref
                       [ex ...])
                 names])
    (define-values (method-names method-impl-ids method-defns)
      (method-static-entries method-mindex method-vtable #'name-ref #f))
    (with-syntax ([(method-name ...) method-names]
                  [(method-id ...) method-impl-ids])
      (append
       method-defns
       (list
        #'(define-name-root name
            #:fields ([method-name method-id]
                      ...
                      ex ...))
        #'(define-dot-provider-syntax name-instance
            (dot-provider-more-static (make-handle-class-instance-dot (quote-syntax name) #hasheq() #hasheq()))))))))

(define-for-syntax (method-static-entries method-mindex method-vtable name-ref-id final?)
  (for/fold ([names '()] [ids '()] [defns '()])
            ([(name mix) (in-hash method-mindex)])
    (cond
      [(and (not (mindex-final? mix))
            (not final?))
       (define proc-id (car (generate-temporaries (list name))))
       (define stx-id (car (generate-temporaries (list name))))
       (values (cons name names)
               (cons stx-id ids)
               (list* #`(define #,proc-id
                          (make-method-accessor '#,name #,name-ref-id #,(mindex-index mix)))
                      #`(define-syntax #,stx-id
                          (make-method-accessor-transformer (quote-syntax #,stx-id)
                                                            (quote-syntax #,name-ref-id)
                                                            #,(mindex-index mix)
                                                            (quote-syntax #,proc-id)))
                      defns))]
      [else
       (values (cons name names)
               (cons (vector-ref method-vtable (mindex-index mix)) ids)
               defns)])))

(define (make-method-accessor name ref idx)
  (procedure-rename
   (make-keyword-procedure
    (lambda (kws kw-args obj . args)
      (keyword-apply (method-ref ref obj idx) kws kw-args obj args))
    (lambda (obj . args)
      (apply (method-ref ref obj idx) obj args)))
   name))

(define-for-syntax (make-method-accessor-transformer id name-ref-id idx proc-id)
  (expression-transformer
   id
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op)
       [(_ (~and args (tag::parens self arg ...)) . tail)
        (define obj-id #'this)
        (define rator #`(method-ref #,name-ref-id #,obj-id #,idx))
        (define-values (call new-tail)
          (parse-function-call rator (list obj-id) #`(#,obj-id (tag arg ...))))
        (values #`(let ([#,obj-id (rhombus-expression self)])
                    #,call)
                #'tail)]
       [(_ . tail)
        (values proc-id #'tail)]))))

(define-for-syntax (class-expression-transformer id make-id)
  (expression-transformer
   id
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail) (values make-id #'tail)]))))

(define-for-syntax (desc-method-shapes desc)
  (if (class-desc? desc)
      (class-desc-method-shapes desc)
      (interface-desc-method-shapes desc)))

(define-for-syntax (desc-method-vtable desc)
  (if (class-desc? desc)
      (class-desc-method-vtable desc)
      (interface-desc-method-vtable desc)))

(define-for-syntax (desc-method-map desc)
  (if (class-desc? desc)
      (class-desc-method-map desc)
      (interface-desc-method-map desc)))

(define-for-syntax (desc-method-result desc)
  (if (class-desc? desc)
      (class-desc-method-result desc)
      (interface-desc-method-result desc)))

(define-for-syntax (desc-ref-id desc)
  (if (class-desc? desc)
      (class-desc-ref-id desc)
      (interface-desc-ref-id desc)))

;; dot provider for a class instance used before a `.`
(define-for-syntax ((make-handle-class-instance-dot name internal-fields internal-methods)
                    form1 dot field-id
                    tail more-static?
                    success failure)
  (define desc (syntax-local-value* (in-class-desc-space name) (lambda (v)
                                                                 (or (class-desc-ref v)
                                                                     (interface-desc-ref v)))))
  (unless desc (error "cannot find annotation binding for instance dot provider"))
  (define (do-field fld)
    (define accessor-id (field-desc-accessor-id fld))
    (define-values (op-id assign-rhs new-tail)
      (syntax-parse tail
        #:datum-literals (op)
        #:literals (:=)
        [((op :=) rhs ...)
         #:when (syntax-e (field-desc-mutator-id fld))
         (values (field-desc-mutator-id fld)
                 #`(rhombus-expression (#,group-tag rhs ...))
                 #'())]
        [_
         (values accessor-id
                 #f
                 tail)]))
    (define e (datum->syntax (quote-syntax here)
                             (append (list (relocate field-id op-id) form1)
                                     (if assign-rhs
                                         (list field-id)
                                         null))
                             (span-srcloc form1 field-id)
                             #'dot))
    (define full-e
      (cond
        [assign-rhs #`(let ([#,field-id #,assign-rhs])
                        #,e)]
        [else e]))
    
    (define static-infos (field-desc-static-infos fld))
    (define more-static-infos (syntax-local-static-info form1 accessor-id))
    (define all-static-infos (if more-static-infos
                                 (datum->syntax #f
                                                (append (syntax->list more-static-infos)
                                                        static-infos))
                                 static-infos))
    (success (wrap-static-info* full-e all-static-infos)
             new-tail))
  (define (do-method pos/id* ret-info-id nonfinal? property?)
    (define-values (args new-tail)
      (if property?
          (syntax-parse tail
            #:datum-literals (op)
            #:literals (:=)
            [((op :=) rhs ...)
             (values #`(#,(no-srcloc #'parens) (#,group-tag rhs ...))
                     #'())]
            [_ (values #'(parens) tail)])
          (syntax-parse tail
            #:datum-literals (op)
            [((~and args (::parens arg ...)) . tail)
             (values #'args #'tail)]
            [_
             (values #f tail)])))
    (define pos/id
      (cond
        [(identifier? pos/id*) pos/id*]
        [nonfinal? pos/id*] ; dynamic dispatch
        [else (vector-ref (syntax-e (desc-method-vtable desc)) pos/id*)]))
    (cond
      [args
       (define-values (rator obj-e wrap)
         (cond
           [(identifier? pos/id)
            (values pos/id form1 (lambda (e) e))]
           [else
            (define obj-id #'obj)
            (define static-infos
              (or (and ret-info-id
                       (method-result-static-infos (syntax-local-method-result ret-info-id)))
                  #'()))
            (values #`(method-ref #,(desc-ref-id desc) #,obj-id #,pos/id)
                    obj-id
                    (lambda (e)
                      (define call-e #`(let ([#,obj-id #,form1])
                                         #,e))
                      (if (pair? (syntax-e static-infos))
                          (wrap-static-info* call-e static-infos)
                          call-e)))]))
       (define-values (call-stx empty-tail)
         (parse-function-call rator (list obj-e) #`(#,obj-e #,args)))
       (success (wrap call-stx)
                new-tail)]
      [else
       (when (and more-static?
                  (not property?))
         (raise-syntax-error #f
                             "method must be called for static mode"
                             (no-srcloc #`(#,form1 #,dot #,field-id))))
       (cond
         [(identifier? pos/id)
          (success #`(curry-method #,pos/id #,form1) new-tail)]
         [else
          (success #`(method-curried-ref #,(desc-ref-id desc) #,form1 #,pos/id) new-tail)])]))
  (cond
    [(and (class-desc? desc)
          (for/or ([field+acc (in-list (class-desc-fields desc))])
            (and (eq? (field-desc-name field+acc) (syntax-e field-id))
                 field+acc)))
     => (lambda (fld) (do-field fld))]
    [(hash-ref (desc-method-map desc) (syntax-e field-id) #f)
     => (lambda (pos)
          (define shape (vector-ref (desc-method-shapes desc) pos))
          (do-method pos
                     (hash-ref (desc-method-result desc) (syntax-e field-id) #f)
                     ;; non-final?
                     (or (box? shape) (and (pair? shape) (box? (car shape))))
                     ;; property?
                     (pair? shape)))]
    [(hash-ref internal-fields (syntax-e field-id) #f)
     => (lambda (fld) (do-field fld))]
    [(hash-ref internal-methods (syntax-e field-id) #f)
     => (lambda (id/property)
          (define id (if (identifier? id/property)
                         id/property
                         (car (syntax-e id/property))))
          (define property? (pair? (syntax-e id/property)))
          (do-method id #f #f property?))]
    [(hash-ref (get-private-table desc) (syntax-e field-id) #f)
     => (lambda (id/fld)
          (if (identifier? id/fld)
              (do-method id/fld #f #f #f)
              (do-field id/fld)))]
    [more-static?
     (raise-syntax-error #f
                         "no such public field or method"
                         field-id)]
    [else (failure)]))
