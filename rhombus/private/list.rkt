#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         "composite.rkt"
         "binding.rkt"
         (submod "contract.rkt" for-struct)
         "static-info.rkt"
         "indexed-ref-set-key.rkt"
         "call-result-key.rkt")

(provide cons
         (for-space rhombus/binding cons)

         List
         (for-space rhombus/contract List)
         (for-space rhombus/static-info List))

(define-binding-syntax cons  
  (binding-transformer
   #'cons
   (make-composite-binding-transformer #'pair? (list #'car #'cdr) (list #'() #'()))))

(define (List l)
  (if (list? l)
      l
      (raise-argument-error 'List
                            "list?"
                            l)))

(define-contract-syntax List
  (identifier-contract #'List #'list? #'((#%indexed-ref list-ref))))

(define-static-info-syntax List
  (#%call-result ((#%indexed-ref list-ref))))
