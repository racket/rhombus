#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         "pattern-clause.rkt"
         "parens.rkt"
         "parse.rkt"
         "op-literal.rkt"
         "syntax-wrap.rkt"
         (submod "equal.rkt" for-parse)
         (submod "annotation.rkt" for-class))

(provide (for-space rhombus/pattern_clause
                    field
                    match_def
                    match_when
                    match_unless
                    default
                    description))

(begin-for-syntax
  (define-splicing-syntax-class :field-lhs
    #:attributes (id depth converter static-infos annotation-str)
    #:datum-literals (group)
    (pattern (~seq id:identifier)
             #:with depth #'0
             #:with converter #'#f
             #:with static-infos #'()
             #:with annotation-str "none")
    (pattern (~seq id:identifier ann::inline-annotation)
             #:with depth #'0
             #:with converter #'ann.converter
             #:with static-infos #'ann.static-infos
             #:with annotation-str #'ann.annotation-str)
    (pattern (~seq (_::brackets (group a::field-lhs) (group _::...-bind)))
             #:with id #'a.id
             #:with converter #'a.converter
             #:with static-infos #'a.static-infos
             #:with annotation-str #'a.annotation-str
             #:with depth #`#,(+ 1 (syntax-e #'a.depth)))))

(define (ensure-syntax val who fail)
  (if (syntax*? val)
      val
      (fail val who)))

(define-pattern-clause-syntax field
  (pattern-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op group)
       [(_ fld ... (tag::block in-block ...))
        #:with (group field::field-lhs) #'(group fld ...)
        #'(#:field field.id field.depth (rhombus-body-at tag in-block ...)
           field.converter field.static-infos  field.annotation-str)]
       [(_ fld ... _::equal rhs ...)
        #:with (group field::field-lhs) #'(group fld ...)
        #`(#:field field.id field.depth (rhombus-expression (#,group-tag rhs ...))
           field.converter field.static-infos  field.annotation-str)]))))

(define-pattern-clause-syntax match_def
  (pattern-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op)
       [(_ (~and pat (_::quotes p ...)) (tag::block in-block ...))
        #'(#:also pat (rhombus-body-at tag in-block ...))]
       [(_ (~and pat (_::quotes p ...)) _::equal rhs ...)
        #`(#:also pat (rhombus-expression (#,group-tag rhs ...)))]))))

(define-pattern-clause-syntax match_when
  (pattern-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (tag::block g ...))
        #`(#:when (rhombus-body-at tag g ...))]
       [(_ rhs ...+)
        #`(#:when (rhombus-expression (#,group-tag rhs ...)))]))))

(define-pattern-clause-syntax match_unless
  (pattern-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (tag::block g ...))
        #`(#:when (not (rhombus-body-at tag g ...)))]
       [(_ rhs ...+)
        #`(#:when (not (rhombus-expression (#,group-tag rhs ...))))]))))

(define-pattern-clause-syntax default
  (pattern-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op)
       [(_ field::field-lhs (tag::block in-block ...))
        #'(#:default field.id field.depth (rhombus-body-at tag in-block ...) #,stx)]
       [(_ field::field-lhs _::equal rhs ...)
        #`(#:default field.id field.depth (rhombus-expression (#,group-tag rhs ...)) #,stx)]))))

(define-pattern-clause-syntax description
  (pattern-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ str:string)
        #'(#:description str #,stx)]
       [(_ (_::block (group str:string)))
        #'(#:description str #,stx)]))))
