#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/property
                     enforest/syntax-local)
         "expression.rkt"
         "static-info.rkt")

(provide |.|)

(module+ for-dot-provider
  (begin-for-syntax
    (provide (property-out dot-provider)
             
             in-dot-provider-space

             wrap-dot-provider))
  (provide define-dot-provider-syntax
           #%dot-provider))

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

(define-syntax #%dot-provider #f)

(define-syntax |.|
  (expression-infix-operator
   (quote-syntax |.|)
   '((default . stronger))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(dot field:identifier . tail)
        (define (fail)
          (raise-syntax-error #f
                              "not supported for left-hand side"
                              #'dot
                              #f
                              (list form1)))
        (syntax-parse form1
          [dp::dot-provider
           (define p (syntax-local-value* (in-dot-provider-space #'dp.id) dot-provider-ref))
           (if p
               (values
                ((dot-provider-handler p) form1 #'dot #'field)
                #'tail)
               (fail))]
          [_ (fail)])]
       [(dot other . tail)
        (raise-syntax-error #f
                            "expected an identifier for a field name, but found something else"
                            #'dot
                            #f
                            (list #'other))]))
   'left))


(define-syntax (define-dot-provider-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-dot-provider-space #'id)
         rhs)]))
