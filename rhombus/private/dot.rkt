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
             dot-provider-syntax-property))
  (provide define-dot-provider-syntax))

(begin-for-syntax
  (struct rhombus-dot-provider (handler))

  (define in-dot-provider-space (make-interned-syntax-introducer 'rhombus/dot-provider))

  (define dot-provider-syntax-property (gensym))
  
  (property dot-provider rhombus-dot-provider))

(define-syntax |.|
  (expression-infix-operator
   (quote-syntax |.|)
   '((default . stronger))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(dot field:identifier . tail)
        (cond
          [(or (and (identifier? form1)
                    (syntax-local-value* (in-dot-provider-space form1) dot-provider-ref))
               (let ([id (syntax-property form1 dot-provider-syntax-property)])
                 (and id
                      (identifier? id)
                      (syntax-local-value* (in-dot-provider-space id) dot-provider-ref))))
           => (lambda (p)
                (values
                 ((rhombus-dot-provider-handler p) p form1 #'dot #'field)
                 #'tail))]
          [else
           (raise-syntax-error #f
                               "not supported for left-hand side"
                               #'dot
                               #f
                               (list form1))])]
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
