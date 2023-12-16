#lang racket/base
(require (for-syntax racket/base
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
                    (keyword->string
                     (syntax-e kw-stx)))
                   kw-stx))

  (define-splicing-syntax-class :class-arg
    #:attributes ([formal 1] kw def?)
    #:datum-literals (group op)
    (pattern (group id:identifier)
             #:attr [formal 1] (list #'id)
             #:attr kw #'#f
             #:attr def? #'#f)
    (pattern (group kw:keyword)
             #:attr [formal 1] (list #'kw (kw->symbol #'kw))
             #:attr def? #'#f)
    (pattern (group kw:keyword (_::block (group id:identifier)))
             #:attr [formal 1] (list #'kw #'id)
             #:attr def? #'#f)
    (pattern (group id:identifier _::=-bind rhs ...+)
             #:attr [formal 1] (list #`[id (rhombus-expression (#,group-tag rhs ...))])
             #:attr kw #'#f
             #:attr def? #'#t)
    (pattern (group id:identifier (tag::block body ...+))
             #:attr [formal 1] (list #`[id (rhombus-body-at tag body ...)])
             #:attr kw #'#f
             #:attr def? #'#t)
    (pattern (group kw:keyword _::=-bind rhs ...+)
             #:attr [formal 1] (list #'kw #`[#,(kw->symbol #'kw) (rhombus-expression (#,group-tag rhs ...))])
             #:attr def? #'#f)
    (pattern (group kw:keyword (_::block (group id:identifier _::=-bind rhs ...+)))
             #:attr [formal 1] (list #'kw #`[id (rhombus-expression (#,group-tag rhs ...))])
             #:attr def? #'#t)
    (pattern (group kw:keyword (_::block (group id:identifier (tag::block body ...+))))
             #:attr [formal 1] (list #'kw #`[id (rhombus-body-at tag body ...)])
             #:attr def? #'#t))

  (define-splicing-syntax-class :class-args
    #:attributes (formals arity)
    #:datum-literals (group)
    (pattern (~seq)
             #:attr formals #'#f
             #:attr arity #'#f)
    (pattern (~seq (_::parens arg::class-arg ... (group _::&-bind id:identifier)))
             #:attr formals #'(arg.formal ... ... . id)
             #:attr arity (datum->syntax
                           #f
                           (summarize-arity #'(arg.kw ...) #'(arg.def? ...) #t #f)))
    (pattern (~seq (_::parens arg::class-arg ...))
             #:attr formals #'(arg.formal ... ...)
             #:attr arity (datum->syntax
                           #f
                           (summarize-arity #'(arg.kw ...) #'(arg.def? ...) #f #f)))))
