#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt"
         "binding.rkt"
         "static-info.rkt"
         (submod "contract.rkt" for-struct))

(provide (for-syntax make-composite-binding-transformer))

(define-for-syntax (make-composite-binding-transformer predicate steppers selectors static-infoss [rest-id #f] [rest-selector #f])
  (lambda (tail)
    (syntax-parse tail
      [(form-id ((~datum parens) a::binding ...) . new-tail)
       #:with (a-parsed::binding-form ...) #'(a.parsed ...)
       (define as (syntax->list #'(a ...)))
       (unless (= (length as) (length selectors))
         (raise-syntax-error #f
                             (format (string-append "pattern arguments not the expected number\n"
                                                    "  expected: ~a\n"
                                                    "  given: ~a")
                                     (length selectors)
                                     (length as))
                             (syntax/loc #'form-id
                               #'(group form-id (parens a ...)))))
       (define keep-static-infoss (for/list ([sis (in-list static-infoss)]
                                             [matcher-id (in-list (syntax->list #'(a-parsed.matcher-id ...)))])
                                    (if (free-identifier=? matcher-id #'identifier-succeed)
                                        sis
                                        '())))
       (values
        (binding-form
         #'composite
         #'composite-matcher
         #'composite-binder
         #`(#,predicate
            #,steppers
            #,selectors
            #,keep-static-infoss
            #,(generate-temporaries #'(a-parsed.arg-id ...))
            (a-parsed.arg-id ...)
            (a-parsed.matcher-id ...)
            (a-parsed.binder-id ...)
            (a-parsed.data ...)
            #,rest-id
            #,rest-selector))
        #'new-tail)])))

(define-syntax (composite-matcher stx)
  (syntax-parse stx
    [(_ c-arg-id
        (predicate steppers selectors static-infoss tmp-ids
                   arg-ids matcher-ids binder-ids datas
                   rest-id rest-selector)
        IF success-expr fail-expr)
     #`(IF (predicate c-arg-id)
           #,(let loop ([c-arg-id #'c-arg-id]
                        [steppers (syntax->list #'steppers)]
                        [selectors (syntax->list #'selectors)]
                        [tmp-ids (syntax->list #'tmp-ids)]
                        [arg-ids (syntax->list #'arg-ids)]
                        [matcher-ids (syntax->list #'matcher-ids)]
                        [datas (syntax->list #'datas)])
               (cond
                 [(null? arg-ids)
                  #`(IF #t
                        #,(if (syntax-e #'rest-id)
                              #`(begin
                                  (define rest-id (rest-selector #,c-arg-id))
                                  success-expr)
                              #'success-expr)
                        fail-expr)]
                 [else
                  (define new-c-arg-id (if steppers
                                           (car (generate-temporaries '(c-arg-id)))
                                           c-arg-id))
                  #`(begin
                      #,@(if steppers
                             #`((define #,new-c-arg-id (#,(car steppers) #,c-arg-id)))
                             #'())
                      (define #,(car tmp-ids) (let ([#,(car arg-ids) (#,(car selectors) #,new-c-arg-id)])
                                                #,(car arg-ids)))
                      (#,(car matcher-ids) #,(car tmp-ids) #,(car datas)
                       IF
                       #,(loop new-c-arg-id
                               (and steppers (cdr steppers)) (cdr selectors)
                               (cdr tmp-ids) (cdr arg-ids) (cdr matcher-ids)
                               (cdr datas))
                       fail-expr))]))
           fail-expr)]))

(define-syntax (composite-binder stx)
  (syntax-parse stx
    [(_ c-arg-id (predicate steppers selectors (static-infos ...) (tmp-id ...)
                            (arg-id ...) matcher-ids (binder-id ...) (data ...)
                            rest-id rest-selector))
     #`(begin
         (binder-id tmp-id data)
         ...
         (define-static-info-syntax/maybe arg-id . static-infos)
         ...)]))
