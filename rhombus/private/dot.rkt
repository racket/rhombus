#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/property
                     enforest/syntax-local)
         "expression.rkt")

(provide |.|)

(module+ for-dot-provider
  (begin-for-syntax
    (provide rhombus-dot-provider
             rhombus-dot-provider?
             rhombus-dot-provider-handler
             
             (property-out dot-provider)
             in-dot-provider-space

             wrap-dot-provider))
  (provide define-dot-provider-syntax))

(begin-for-syntax
  (struct rhombus-dot-provider (handler))

  (define in-dot-provider-space (make-interned-syntax-introducer 'rhombus/dot-provider))

  (define (wrap-dot-provider expr provider-stx)
    #`(begin (quote-syntax (#%dot-provider #,provider-stx))
             #,expr))
  
  (property dot-provider rhombus-dot-provider))

(define-syntax #%dot-provider #f)

(define-syntax |.|
  (expression-infix-operator
   (quote-syntax |.|)
   '((default . stronger))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(dot field:identifier . tail)
        (define (apply-provider p form)
          (values
           ((rhombus-dot-provider-handler p) p form #'dot #'field)
           #'tail))
        (define (fail)
          (raise-syntax-error #f
                              "not supported for left-hand side"
                              #'dot
                              #f
                              (list form1)))
        (cond
          [(and (identifier? form1)
                (syntax-local-value* (in-dot-provider-space form1) dot-provider-ref))
           => (lambda (p)
                (apply-provider p form1))]
          [else
           (syntax-parse form1
             #:literals (begin quote-syntax #%dot-provider)
             [(begin (quote-syntax (#%dot-provider id:identifier)) e)
              (define p (syntax-local-value* (in-dot-provider-space #'id) dot-provider-ref))
              (if p
                  (apply-provider p #'e)
                  (fail))]
             [_ (fail)])])]
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
