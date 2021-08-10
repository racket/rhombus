#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx)
         "composite.rkt"
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         (submod "contract.rkt" for-struct)
         "static-info.rkt"
         "indexed-ref-set-key.rkt"
         "call-result-key.rkt"
         "ref-result-key.rkt"
         (only-in "quasiquote.rkt"
                  [... rhombus...])
         "parse.rkt")

(provide cons
         (for-space rhombus/binding cons)

         List
         (for-space rhombus/contract List)
         (for-space rhombus/static-info List))

(module+ for-binding
  (provide (for-syntax parse-list-binding
                       parse-list-expression)))

(define-binding-syntax cons  
  (binding-transformer
   #'cons
   (make-composite-binding-transformer #'pair? (list #'car #'cdr) (list #'() #'()))))

(define-syntax List
  (make-expression+binding-prefix-operator
   #'List
   '((default . stronger))
   'macro
   ;; expression - special case for `...`
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group op)
       #:literals (rhombus...)
       [(form-id (parens _ ... _ (group (op rhombus...))) . tail)
        (parse-list-expression stx)]
       [(_ . tail)
        (values #'list #'tail)]))
   ;; binding
   (lambda (stx)
     (syntax-parse stx
       [(form-id ((~and tag (~datum parens)) arg ...) . tail)
        (parse-list-binding stx)]))))

(define-contract-syntax List
  (contract-constructor #'List #'list? #'((#%indexed-ref list-ref))
                        1
                        (lambda (arg-id predicate-stxs)
                          #`(for/and ([e (in-list #,arg-id)])
                              (#,(car predicate-stxs) e)))
                        (lambda (static-infoss)
                          #`((#%ref-result #,(car static-infoss))))))

(define-static-info-syntax List
  (#%call-result ((#%indexed-ref list-ref))))

;; parses a list pattern that has already been checked for use with a
;; suitable `parens` or `brackets` form
(define-for-syntax (parse-list-binding stx)
  (define (generate-binding form-id pred args tail [rest-arg #f] [rest-selector #f])
    ((make-composite-binding-transformer pred
                                         #:steppers (if (null? args)
                                                        null
                                                        (cons #'values
                                                              (for/list ([arg (in-list (cdr args))])
                                                                #'cdr)))
                                         (for/list ([arg (in-list args)])
                                           #'car)
                                         (for/list ([arg (in-list args)])
                                           #'())
                                         #:rest-accessor rest-selector)
     #`(#,form-id (parens . #,args) . #,tail)
     rest-arg))
  (syntax-parse stx
    #:datum-literals (group op)
    #:literals (rhombus...)
    [(form-id (_ arg ... rest-arg (group (op rhombus...))) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (define pred #`(lambda (v)
                      (and (list? v)
                           (>= (length v) #,len))))
     (generate-binding #'form-id pred args #'tail #'rest-arg (if (null? args) #'values #'cdr))]
    [(form-id (_ arg ...) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (define pred #`(lambda (v)
                      (and (list? v)
                           (= (length v) #,len))))
     (generate-binding #'form-id pred args #'tail)]))


(define-for-syntax (parse-list-expression stx)
  (syntax-parse stx
    #:datum-literals (group op)
    #:literals (rhombus...)
    [(form-id (tag arg ... rest-arg (group (op rhombus...))) . tail)
     (values (syntax/loc #'tag
               (list* (rhombus-expression arg) ... (rhombus-expression rest-arg)))
             #'tail)]
    [(form-id (tag arg ...) . tail)
     (values (syntax/loc #'tag
               (list (rhombus-expression arg) ...))
             #'tail)]))

