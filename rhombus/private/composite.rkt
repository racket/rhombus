#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt"
         "binding.rkt"
         "static-info.rkt"
         (submod "contract.rkt" for-struct))

(provide (for-syntax make-composite-binding-transformer))

(define-for-syntax (make-composite-binding-transformer predicate selectors static-infoss)
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
            #,selectors
            #,keep-static-infoss
            #,(generate-temporaries #'(a-parsed.arg-id ...))
            (a-parsed.arg-id ...)
            (a-parsed.matcher-id ...)
            (a-parsed.binder-id ...)
            (a-parsed.data ...)))
        #'new-tail)])))

(define-syntax (composite-matcher stx)
  (syntax-parse stx
    [(_ c-arg-id (predicate selectors static-infoss tmp-ids arg-ids matcher-ids binder-ids datas) IF success-expr fail-expr)
     #`(IF (predicate c-arg-id)
           #,(let loop ([selectors (syntax->list #'selectors)]
                        [tmp-ids (syntax->list #'tmp-ids)]
                        [arg-ids (syntax->list #'arg-ids)]
                        [matcher-ids (syntax->list #'matcher-ids)]
                        [datas (syntax->list #'datas)])
               (cond
                 [(null? arg-ids)
                  #`(IF #t success-expr fail-expr)]
                 [else
                  #`(begin
                      (define #,(car tmp-ids) (let ([#,(car arg-ids) (#,(car selectors) c-arg-id)])
                                                #,(car arg-ids)))
                      (#,(car matcher-ids) #,(car tmp-ids) #,(car datas)
                       IF
                       #,(loop (cdr selectors) (cdr tmp-ids) (cdr arg-ids) (cdr matcher-ids) (cdr datas))
                       fail-expr))]))
           fail-expr)]))

(define-syntax (composite-binder stx)
  (syntax-parse stx
    [(_ c-arg-id (predicate selectors (static-infos ...) (tmp-id ...) (arg-id ...) matcher-ids (binder-id ...) (data ...)))
     #`(begin
         (binder-id tmp-id data)
         ...
         (define-static-info-syntax/maybe arg-id . static-infos)
         ...)]))
