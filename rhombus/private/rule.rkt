#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "syntax-rhs.rkt"
         "expression.rkt"
         "entry-point.rkt"
         "syntax.rkt"
         "pack.rkt"
         syntax/parse)

(provide rule)

(begin-for-syntax
  (struct entry-point+expression-transformer (cbl exp)
    #:property prop:entry-point-transformer (lambda (self) (entry-point+expression-transformer-cbl self))
    #:property prop:expression-prefix-operator (lambda (self) (entry-point+expression-transformer-exp self)))
  (define (make-entry-point+expression-transformer cbl exp)
    (entry-point+expression-transformer cbl exp)))

(define-for-syntax (parse-rule stx adjustments)
  (syntax-parse stx
    #:datum-literals (parens group block alts op)
    [(form-id ((~and alts-tag alts) (block (group q::operator-syntax-quote
                                                  (~and rhs (block body ...))))
                                    ...+))
     (parse-operator-definitions-rhs
      stx
      (parse-operator-definitions 'rule
                                  #:allowed '(prefix)
                                  stx
                                  (syntax->list #'(q.g ...))
                                  (syntax->list #'(rhs ...))
                                  (lambda (x) x)
                                  #f)
      #'wrap-prefix
      #f
      #f
      #:adjustments adjustments)]
    [(form-id q::operator-syntax-quote
              (~and rhs (block body ...)))
     (parse-operator-definition-rhs
      (parse-operator-definition 'rule
                                 #:allowed '(prefix)
                                 #'q.g
                                 #'rhs
                                 (lambda (x) x)
                                 #f)
      #'wrap-prefix
      #f
      #:adjustments adjustments)]))

(define-syntax rule
  (make-entry-point+expression-transformer
   (entry-point-transformer
    (lambda (stx adjustments)
      (parse-rule stx adjustments)))
   (expression-transformer
    #'rule
    (lambda (stx)
      (values (parse-rule stx no-adjustments)
              #'())))))

(define (wrap-prefix name precedence protocol proc)
  (lambda (stx)
    (syntax-parse (unpack-tail stx #f #f)
      [(head . tail) (proc (pack-tail #'tail) #'head)])))
