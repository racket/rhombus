#lang racket/base
(require (for-syntax racket/base
                     racket/keyword
                     syntax/parse/pre
                     "tag.rkt")
         "parens.rkt"
         "parse.rkt"
         "function-arity.rkt"
         "op-literal.rkt")

(provide (for-syntax :class-args))

(begin-for-syntax
  (define (kw->symbol kw-stx)
    (datum->syntax kw-stx
                   (string->symbol
                    (keyword->immutable-string
                     (syntax-e kw-stx)))
                   kw-stx))

  (define-splicing-syntax-class :class-arg
    #:attributes ([formal 1] kw def?)
    #:datum-literals (group op)
    (pattern (group id:identifier)
             #:with (formal ...) (list #'id)
             #:with kw #'#f
             #:with def? #'#f)
    (pattern (group kw:keyword)
             #:with (formal ...) (list #'kw (kw->symbol #'kw))
             #:with def? #'#f)
    (pattern (group kw:keyword (_::block (group id:identifier)))
             #:with (formal ...) (list #'kw #'id)
             #:with def? #'#f)
    (pattern (group id:identifier _::=-bind rhs ...+)
             #:with (formal ...) (list #`[id (rhombus-expression (#,group-tag rhs ...))])
             #:with kw #'#f
             #:with def? #'#t)
    (pattern (group id:identifier (tag::block body ...+))
             #:with (formal ...) (list #'[id (rhombus-body-at tag body ...)])
             #:with kw #'#f
             #:with def? #'#t)
    (pattern (group kw:keyword _::=-bind rhs ...+)
             #:with (formal ...) (list #'kw #`[#,(kw->symbol #'kw) (rhombus-expression (#,group-tag rhs ...))])
             #:with def? #'#f)
    (pattern (group kw:keyword (_::block (group id:identifier _::=-bind rhs ...+)))
             #:with (formal ...) (list #'kw #`[id (rhombus-expression (#,group-tag rhs ...))])
             #:with def? #'#t)
    (pattern (group kw:keyword (_::block (group id:identifier (tag::block body ...+))))
             #:with (formal ...) (list #'kw #'[id (rhombus-body-at tag body ...)])
             #:with def? #'#t))

  (define-splicing-syntax-class :class-args
    #:attributes (formals arity)
    #:datum-literals (group)
    (pattern (~seq)
             #:with formals #'#f
             #:with arity #'#f)
    (pattern (~seq (_::parens arg::class-arg ... (group _::&-bind id:identifier)))
             #:with formals #'(arg.formal ... ... . id)
             #:with arity (datum->syntax
                           #f
                           (summarize-arity #'(arg.kw ...) #'(arg.def? ...) #t #f)))
    (pattern (~seq (_::parens arg::class-arg ...))
             #:with formals #'(arg.formal ... ...)
             #:with arity (datum->syntax
                           #f
                           (summarize-arity #'(arg.kw ...) #'(arg.def? ...) #f #f)))))
