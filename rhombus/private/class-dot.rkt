#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     "srcloc.rkt"
                     "tag.rkt"
                     "class-parse.rkt")
         "class-method.rkt"
         (submod "dot.rkt" for-dot-provider)
         "parens.rkt"
         "static-info.rkt"
         "expression.rkt"
         "assign.rkt"
         "name-root.rkt"
         "parse.rkt"
         (submod "function.rkt" for-call))

(provide (for-syntax build-class-dot-handling))

(define-for-syntax (build-class-dot-handling names)
  (with-syntax ([(name constructor-name name-instance
                       [field-name ...]
                       [name-field ...])
                 names])
    (list
     #'(define-name-root name
         #:root (class-expression-transformer (quote-syntax name) (quote-syntax constructor-name))
         #:fields ([field-name name-field] ...))
     #'(define-dot-provider-syntax name-instance
         (dot-provider-more-static (make-handle-class-instance-dot (quote-syntax name)))))))

(define-for-syntax (class-expression-transformer id make-id)
  (expression-transformer
   id
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail) (values make-id #'tail)]))))

;; dot provider for a class instance used before a `.`
(define-for-syntax ((make-handle-class-instance-dot name) form1 dot field-id
                                                          tail more-static?
                                                          success failure)
  (define desc (syntax-local-value* (in-class-desc-space name) class-desc-ref))
  (unless desc (error "cannot find annotation binding for instance dot provider"))
  (cond
    [(for/or ([field+acc (in-list (class-desc-fields desc))])
      (and (eq? (field-desc-name field+acc) (syntax-e field-id))
           field+acc))
     => (lambda (fld)
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
              [assign-rhs
               (success #`(let ([#,field-id #,assign-rhs])
                            #,e
                            #,field-id))]
              [else e]))
          
          (define static-infos (field-desc-static-infos fld))
          (define more-static-infos (syntax-local-static-info form1 accessor-id))
          (define all-static-infos (if more-static-infos
                                       (datum->syntax #f
                                                      (append (syntax->list more-static-infos)
                                                              static-infos))
                                       static-infos))
          (success (wrap-static-info* full-e all-static-infos)
                   new-tail))]
    [(hash-ref (class-desc-method-map desc) (syntax-e field-id) #f)
     => (lambda (pos/boxed)
          (define-values (args new-tail tag)
            (syntax-parse tail
              #:datum-literals (op)
              [((~and args (::parens arg ...)) . tail)
               (values #'args #'tail #'tag)]
              [_
               (values #f tail #f)]))
          (define pos/id
            (if (box? pos/boxed)
                (unbox pos/boxed)
                (vector-ref (syntax-e (class-desc-method-vtable desc)) pos/boxed)))
          (cond
            [args
             (define obj-id #'obj)
             (define rator
               (cond
                 [(identifier? pos/id) pos/id]
                 [else #`(method-ref #,form1 #,pos/id)]))
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
                (success #`(curried #,pos/id #,form1) new-tail)]
               [else
                (success #`(method-curried-ref #,form1 #,pos/id) new-tail)])]))]
    [else (failure)]))
