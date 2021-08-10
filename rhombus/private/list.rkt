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
         (for-space rhombus/binding List)
         (for-space rhombus/contract List)
         (for-space rhombus/static-info List))

(module+ for-binding
  (provide (for-syntax parse-list-binding)))

(define-binding-syntax cons  
  (binding-transformer
   #'cons
   (make-composite-binding-transformer #'pair? (list #'car #'cdr) (list #'() #'()))))

(define (List . l) l)

(define-contract-syntax List
  (identifier-contract #'List #'list? #'((#%indexed-ref list-ref))))

(define-static-info-syntax List
  (#%call-result ((#%indexed-ref list-ref))))

(define-binding-syntax List
  (binding-prefix-operator
   #'List
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(form-id ((~and tag (~datum parens)) arg ...) . tail)
        (parse-list-binding stx)]))))

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
    #:datum-literals (brackets group op)
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
