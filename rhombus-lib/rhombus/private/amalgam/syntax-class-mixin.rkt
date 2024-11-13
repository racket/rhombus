#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/pre)

(provide define-syntax-class-mixin
         define-composed-splicing-syntax-class)

(define-syntax (define-syntax-class-mixin stx)
  (syntax-case stx (~alt)
    [(_ mixin-id #:datum-literals (lit-id ...)
        (~alt alt ...)
        . attrs)
     #'(define-syntax mixin-id
         (quote-syntax ((lit-id ...)
                        (alt ...)
                        attrs)))]
    [(_ mixin-id . more)
     #'(define-syntax-class-mixin #:datum-literals () . more)]))

(define-syntax (define-composed-splicing-syntax-class stx)
  (syntax-case stx ()
    [(_ head
        #:desc desc
        mixin-id ...)
     (with-syntax ([(((literal-id ...) (alt ...) (attr ...)) ...)
                    (map syntax-local-value (syntax->list #'(mixin-id ...)))])
       (with-syntax ([(unique-literal-id ...)
                      (hash-values (for/hash ([literal-id (in-list (syntax->list #'(literal-id ... ...)))])
                                     (values (syntax-e literal-id) literal-id)))])
         #`(define-splicing-syntax-class head
             #:datum-literals (unique-literal-id ...)
             #:description desc
             #:opaque
             (pattern (~seq (~alt alt ... ...) (... ...))
                      attr ... ...))))]))
