#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         "pattern-clause.rkt"
         "parens.rkt"
         "parse.rkt"
         (rename-in "ellipsis.rkt"
                    [... rhombus...])
         (rename-in "equal.rkt"
                    [= rhombus=]))

(provide (for-space rhombus/pattern_clause
                    field
                    matching_also
                    matching_when
                    matching_unless))

(begin-for-syntax
  (define-syntax-class :field-lhs
    #:datum-literals (brackets group op)
    #:literals (rhombus...)
    (pattern id:identifier
             #:attr depth #'0)
    (pattern (brackets (group a::field-lhs) (group (op rhombus...)))
             #:attr id #'a.id
             #:attr depth #`#,(+ 1 (syntax-e #'a.depth)))))

(define-pattern-clause-syntax field
  (pattern-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:literals (rhombus=)
       #:datum-literals (op)
       [(_ field::field-lhs (tag::block in-block ...))
        #'(#:field field.id field.depth (rhombus-body-at tag in-block ...))]
       [(_ field::field-lhs (op rhombus=) rhs ...)
        #`(#:field field.id field.depth (rhombus-expression (#,group-tag rhs ...)))]))))

(define-pattern-clause-syntax matching_also
  (pattern-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:literals (rhombus=)
       #:datum-literals (op)
       [(_ (~and pat (_::quotes p ...)) (tag::block in-block ...))
        #'(#:also pat (rhombus-body-at tag in-block ...))]
       [(_ (~and pat (_::quotes p ...)) (op rhombus=) rhs ...)
        #`(#:also pat (rhombus-expression (#,group-tag rhs ...)))]))))

(define-pattern-clause-syntax matching_when
  (pattern-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ rhs ...)
        #`(#:when (rhombus-expression (#,group-tag rhs ...)))]))))

(define-pattern-clause-syntax matching_unless
  (pattern-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ rhs ...)
        #`(#:when (not (rhombus-expression (#,group-tag rhs ...))))]))))
