#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         "composite.rkt"
         "binding.rkt"
         (submod "contract.rkt" for-struct)
         "static-info.rkt"
         "indexed-ref-set-key.rkt"
         "call-result-key.rkt"
         (only-in "quasiquote.rkt"
                  [... rhombus...]))

(provide cons
         (for-space rhombus/binding cons)

         List
         (for-space rhombus/contract List)
         (for-space rhombus/static-info List))

(module+ for-binding
  (provide (for-syntax parse-list-binding)))

(define-binding-syntax cons  
  (binding-transformer
   #'cons
   (make-composite-binding-transformer #'pair? #f (list #'car #'cdr) (list #'() #'()))))

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

(define-for-syntax (parse-list-binding stx)
  (define (generate-binding form-id pred args tail [rest-id #f] [rest-selector #f])
    ((make-composite-binding-transformer pred
                                         (cons #'values
                                               (for/list ([arg (in-list (cdr args))])
                                                 #'cdr))
                                         (for/list ([arg (in-list args)])
                                           #'car)
                                         (for/list ([arg (in-list args)])
                                           #'())
                                         rest-id
                                         rest-selector)
     #`(#,form-id (parens . #,args) . #,tail)))
  (syntax-parse stx
    #:datum-literals (brackets group op)
    #:literals (rhombus...)
    [(form-id ((~and tag brackets) arg ... rest-group (group (op rhombus...))) . tail)
     (define rest-id
       (syntax-parse #'rest-group
         #:datum-literals (group)
         [(group rest-id:identifier) #'rest-id]
         [_ (raise-syntax-error (syntax-e #'form-id)
                                "expected an identifier before ellipsis"
                                #'rest-group)]))
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (define pred #`(lambda (v)
                      (and (list? v)
                           (>= (length v) #,len))))
     (generate-binding #'form-id pred args #'tail rest-id (if (null? args) #'values #'cdr))]
    [(form-id ((~and tag brackets) arg ...) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (define pred #`(lambda (v)
                      (and (list? v)
                           (= (length v) #,len))))
     (generate-binding #'form-id pred args #'tail)]))
