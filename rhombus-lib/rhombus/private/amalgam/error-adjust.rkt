#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "rhombus-primitive.rkt"
         "expression.rkt"
         "parens.rkt"
         "parse.rkt")

(provide with-error-adjust-primitive
         local_error_adjust)

(define-syntax (with-error-adjust-primitive stx)
  (syntax-parse stx
    [(_ () body) #'body]
    [(_ () body ...) #'(let () body ...)]
    [(_ ([rkt-sym rhm-sym]) body ...)
     #'(with-continuation-mark
           primitive-who-table-key '(rkt-sym . rhm-sym)
           (let ()
             body
             ...))]
    [(_ ([rkt-sym rhm-sym] ...) body ...)
     (with-syntax ([table (for/hash ([rkt-sym (in-list (syntax->list #'(rkt-sym ...)))]
                                     [rhm-sym (in-list (syntax->list #'(rhm-sym ...)))])
                            (values (syntax-e rkt-sym) (syntax-e rhm-sym)))])
       #'(with-continuation-mark
             primitive-who-table-key table
             (let ()
               body
               ...)))]))

(define-syntax local_error_adjust
  (expression-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ (_::braces (group rkt ... (rhs::block rhs-g ...)))
            (blk::block g ...))
         (values
          #'(with-continuation-mark
                primitive-who-table-key (cons (rhombus-expression (group rkt ...))
                                              (rhombus-body-at rhs rhs-g ...))
                (rhombus-body-at blk g ...))
          #'())]))))
