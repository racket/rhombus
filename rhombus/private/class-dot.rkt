#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     "srcloc.rkt"
                     "tag.rkt"
                     "class-parse.rkt"
                     "interface-parse.rkt")
         "class-method.rkt"
         (submod "dot.rkt" for-dot-provider)
         "parens.rkt"
         "static-info.rkt"
         "expression.rkt"
         "assign.rkt"
         "name-root.rkt"
         "parse.rkt"
         (submod "function.rkt" for-call))

(provide (for-syntax build-class-dot-handling
                     build-interface-dot-handling))

(define-for-syntax (build-class-dot-handling names)
  (with-syntax ([(name constructor-name name-instance
                       [field-name ...]
                       [name-field ...]
                       [ex ...])
                 names])
    (list
     #'(define-name-root name
         #:root (class-expression-transformer (quote-syntax name) (quote-syntax constructor-name))
         #:fields ([field-name name-field] ... ex ...))
     #'(define-dot-provider-syntax name-instance
         (dot-provider-more-static (make-handle-class-instance-dot (quote-syntax name)))))))

(define-for-syntax (build-interface-dot-handling names)
  (with-syntax ([(name name-instance
                       [ex ...])
                 names])
    (list
     #'(define-name-root name
         #:fields (ex ...))
     #'(define-dot-provider-syntax name-instance
         (dot-provider-more-static (make-handle-class-instance-dot (quote-syntax name)))))))

(define-for-syntax (class-expression-transformer id make-id)
  (expression-transformer
   id
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail) (values make-id #'tail)]))))

(define-for-syntax (desc-method-vtable desc)
  (if (class-desc? desc)
      (class-desc-method-vtable desc)
      (interface-desc-method-vtable desc)))

(define-for-syntax (desc-method-map desc)
  (if (class-desc? desc)
      (class-desc-method-map desc)
      (interface-desc-method-map desc)))

;; dot provider for a class instance used before a `.`
(define-for-syntax ((make-handle-class-instance-dot name) form1 dot field-id
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
                        #,e
                        #,field-id)]
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
  (define (do-method pos/boxed/id)
    (define-values (args new-tail tag)
      (syntax-parse tail
        #:datum-literals (op)
        [((~and args (::parens arg ...)) . tail)
         (values #'args #'tail #'tag)]
        [_
         (values #f tail #f)]))
    (define pos/id
      (cond
        [(identifier? pos/boxed/id) pos/boxed/id]
        [(box? pos/boxed/id) (unbox pos/boxed/id)] ; dynamic dispatch
        [else (vector-ref (syntax-e (desc-method-vtable desc)) pos/boxed/id)]))
    (cond
      [args
       (define obj-id #'obj)
       (define rator
         (cond
           [(identifier? pos/id) pos/id]
           [(class-desc? desc) #`(method-ref #,obj-id #,pos/id)]
           [else #`(interface-method-ref #,(interface-desc-ref-id desc) #,obj-id #,pos/id)]))
       (define-values (call-stx empty-tail)
         (parse-function-call rator (list obj-id) #`(#,obj-id #,args)))
       (success #`(let ([#,obj-id #,form1])
                    #,call-stx)
                new-tail)]
      [else
       (when more-static?
         (raise-syntax-error #f
                             "method must be called for static mode"
                             (no-srcloc #`(#,form1 #,dot #,field-id))))
       (cond
         [(identifier? pos/id)
          (success #`(curry-method #,pos/id #,form1) new-tail)]
         [else
          (success #`(method-curried-ref #,form1 #,pos/id) new-tail)])]))
  (cond
    [(and (class-desc? desc)
          (for/or ([field+acc (in-list (class-desc-fields desc))])
            (and (eq? (field-desc-name field+acc) (syntax-e field-id))
                 field+acc)))
     => (lambda (fld) (do-field fld))]
    [(hash-ref (desc-method-map desc) (syntax-e field-id) #f)
     => (lambda (pos/boxed) (do-method pos/boxed))]
    [(hash-ref (get-private-table desc) (syntax-e field-id) #f)
     => (lambda (id/fld)
          (if (identifier? id/fld)
              (do-method id/fld)
              (do-field id/fld)))]
    [more-static?
     (raise-syntax-error #f
                         "no such public field or method"
                         field-id)]
    [else (failure)]))
