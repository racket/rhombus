#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "transformer.rkt")
         "parse.rkt")

(provide (rename-out [rhombus-cons cons]))

(begin-for-syntax
  (struct binding-function (macro-proc binding-proc)
    #:property prop:procedure (struct-field-index macro-proc)
    #:property
    prop:rhombus-binding-transformer
    (lambda (self) (binding-function-binding-proc self))))

(define-syntax rhombus-cons
  (binding-function
   (lambda (stx)
     (syntax-parse stx
       [_:identifier #'cons]
       [(_ arg ...) (syntax/loc stx (cons arg ...))]))
   (rhombus-binding-transformer
    (lambda (tail)
      (syntax-parse tail
        [(_ ((~datum parens) a::binding d::binding) . new-tail)
         (with-syntax ([(a-id ...) #'a.variable-ids]
                       [(d-id ...) #'d.variable-ids]
                       [(a-stx-id ...) #'a.syntax-ids]
                       [(d-stx-id ...) #'d.syntax-ids]
                       [falses (for/list ([b (in-range (+ (length (syntax->list #'a.variable-ids))
                                                          (length (syntax->list #'d.variable-ids))))])
                                 #'#f)])
           (values
            #'(a-id ... d-id ...)
            #'(lambda (v)
                (if (pair? v)
                    (let-values ([(match? . a.variable-ids) (a.matcher-form (car v))])
                      (if match?
                          (let-values ([(match? . d.variable-ids) (d.matcher-form (cdr v))])
                            (if match?
                                (values #t a-id ... d-id ...)
                                (values #f . falses)))
                          (values #f . falses)))
                    (values #f . falses)))
            #'(a-stx-id ... d-stx-id ...)
            #'(let-values ([(a-stx-id ...) a.syntax-form]
                           [(d-stx-id ...) d.syntax-form])
                (values a-stx-id ... d-stx-id ...))
            #'new-tail))])))))
