#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "rhombus-primitive.rkt")

(provide with-error-adjust-primitive
         call-with-local-error-adjust)

(define-syntax (with-error-adjust-primitive stx)
  (syntax-parse stx
    [(_ () body) #'body]
    [(_ () body ...) #'(let () body ...)]
    [(_ ([rkt-sym rhm-sym] ...) body ...)
     (with-syntax ([table (for/hash ([rkt-sym (in-list (syntax->list #'(rkt-sym ...)))]
                                     [rhm-sym (in-list (syntax->list #'(rhm-sym ...)))])
                            (values (syntax-e rkt-sym) (syntax-e rhm-sym)))])
       #'(with-continuation-mark
             primitive-who-table-key table
             (let ()
               body
               ...)))]))

(define (call-with-local-error-adjust from to thunk)
  (with-continuation-mark
      primitive-who-table-key (hasheq from to)
      (thunk)))
