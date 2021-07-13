#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "transformer.rkt")
         "parse.rkt")

(provide (rename-out [rhombus-cons cons]))

(begin-for-syntax
  (struct pattern-function (macro-proc pattern-proc)
    #:property prop:procedure (struct-field-index macro-proc)
    #:property
    prop:rhombus-pattern-transformer
    (lambda (self) (pattern-function-pattern-proc self))))

(define-syntax rhombus-cons
  (pattern-function
   (lambda (stx)
     (syntax-parse stx
       [_:identifier #'cons]
       [(_ arg ...) (syntax/loc stx (cons arg ...))]))
   (rhombus-pattern-transformer
    (lambda (tail)
      (syntax-parse tail
        [(_ ((~datum parens) a::pattern d::pattern) . new-tail)
         (with-syntax ([(a-binding ...) #'a.bindings]
                       [(d-binding ...) #'d.bindings]
                       [falses (for/list ([b (in-range (+ (length (syntax->list #'a.bindings))
                                                          (length (syntax->list #'d.bindings))))])
                                 #'#f)])
           (values
            #'(a-binding ... d-binding ...)
            #'(lambda (v)
                (if (pair? v)
                    (let-values ([(match? . a.bindings) (a.filter (car v))])
                      (if match?
                          (let-values ([(match? . d.bindings) (d.filter (cdr v))])
                            (if match?
                                (values #t a-binding ... d-binding ...)
                                (values #f . falses)))
                          (values #f . falses)))
                    (values #f . falses)))
            #'new-tail))])))))
