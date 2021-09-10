#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/property
                     enforest/syntax-local
                     "operator-parse.rkt")
         "definition.rkt"
         "expression.rkt"
         "static-info.rkt"
         "dot-provider-key.rkt")

(provide |.|
         use_static_dot
         use_dynamic_dot)

(module+ for-dot-provider
  (begin-for-syntax
    (provide (property-out dot-provider)
             
             in-dot-provider-space

             wrap-dot-provider))
  (provide define-dot-provider-syntax
           #%dot-provider
           prop:field-name->accessor))

(begin-for-syntax
  (property dot-provider (handler))

  (define in-dot-provider-space (make-interned-syntax-introducer 'rhombus/dot-provider))

  (define (wrap-dot-provider expr provider-stx)
    (quasisyntax/loc expr
      (begin (quote-syntax (#%dot-provider #,provider-stx))
             #,expr)))

  (define-syntax-class :dot-provider
    #:literals (begin quote-syntax #%dot-provider)
    (pattern id:identifier
             #:when (syntax-local-value* (in-dot-provider-space #'id) dot-provider-ref))
    (pattern (~var ref-id (:static-info #'#%dot-provider))
             #:attr id #'ref-id.val)))

(define-for-syntax (make-|.| strict?)
  (expression-infix-operator
   (quote-syntax |.|)
   '((default . stronger))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(dot::operator field:identifier . tail)
        (define (generic)
          (if strict?
              (raise-syntax-error #f
                                  "strict operator not supported for left-hand side"
                                  #'dot.name
                                  #f
                                  (list form1))
              (values #`(dot-lookup-by-name #,form1 'field)
                      #'tail)))
        (syntax-parse form1
          [dp::dot-provider
           (define p (syntax-local-value* (in-dot-provider-space #'dp.id) dot-provider-ref))
           (if p
               (values
                ((dot-provider-handler p) form1 #'dot #'field)
                #'tail)
               (generic))]
          [_ (generic)])]
       [(dot::operator other . tail)
        (raise-syntax-error #f
                            "expected an identifier for a field name, but found something else"
                            #'dot.name
                            #f
                            (list #'other))]))
   'left))

(define-syntax |.| (make-|.| #f))

(define-syntaxes (use_static_dot use_dynamic_dot)
  (let ([mk (lambda (strict?)
              (definition-transformer
                (lambda (stx)
                  (syntax-parse stx
                    [(form-id)
                     #`((define-syntax #,(datum->syntax #'form-id '|.|) (make-|.| #,strict?)))]))))])
    (values (mk #t)
            (mk #f))))

(define-syntax (define-dot-provider-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-dot-provider-space #'id)
         rhs)]))

(define-values (prop:field-name->accessor field-name->accessor? field-name->accessor-ref)
  (make-struct-type-property 'field-name->accessor
                             (lambda (field-names info)
                               (define gen-acc (list-ref info 3))
                               (for/hasheq ([name (in-list field-names)]
                                            [i (in-naturals)])
                                 (values name
                                         (make-struct-field-accessor gen-acc i name))))))

(define (dot-lookup-by-name v field)
  (define ht (field-name->accessor-ref v #f))
  (define (fail)
    (raise-arguments-error field
                           "no such field"
                           "in value" v))
  (cond
    [(not ht) (fail)]
    [(hash-ref ht field #f) => (lambda (acc) (acc v))]
    [else (fail)]))

   
