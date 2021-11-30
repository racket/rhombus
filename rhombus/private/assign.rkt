#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local)
         "binding.rkt"
         "expression.rkt")

(provide :=
         mutable)

(define-syntax mutable
  (binding-transformer
   #'mutable
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier . new-tail)
        (values
         (binding-form
          #'mutable-info
          #'id)
         #'new-tail)]))))

(define-syntax (mutable-info stx)
  (syntax-parse stx
    [(_ static-infos id)
     (binding-info #'id
                   #'() ; mutable => don't claim input's static info
                   #'((id))
                   #'mutable-identifier-succeed
                   #'mutable-bind
                   #'id)]))

(define-syntax (mutable-identifier-succeed stx)
  (syntax-parse stx
    [(_ arg-id bind-id IF success fail)
     #'(IF #t success fail)]))

(define-syntax (mutable-bind stx)
  (syntax-parse stx
    [(_ arg-id bind-id)
     #'(begin
         (define mutable-id arg-id)
         (begin (set! mutable-id mutable-id))
         (define-syntax bind-id
           (mutable-variable #'mutable-id)))]))

(begin-for-syntax
  (struct mutable-variable (id)
    #:property prop:rename-transformer (struct-field-index id))
  (define (mutable-variable-ref v) (and (mutable-variable? v) v)))

(define-syntax :=
  (expression-infix-operator
   #':=
   '((default . weaker))
   'automatic
   (lambda (form1 form2 self-stx)
     (define mv (and (identifier? form1)
                     (syntax-local-value* form1 mutable-variable-ref)))
     (unless mv
       (raise-syntax-error #f
                           "left-hand argument is not a mutable identifier"
                           self-stx))
     #`(let ([#,form1 #,form2])
         (set! #,(mutable-variable-id mv) #,form1)
         #,form1))
   'left))
