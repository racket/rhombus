#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "declaration.rkt"
         "definition.rkt"
         "name-root.rkt"
         "parens.rkt"
         "parse.rkt"
         (for-syntax "parse.rkt"))

(provide (for-spaces (rhombus/namespace
                      #f)
                     meta))

(define-name-root meta
  #:fields
  (bridge))

(define-syntax meta
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block bridge op |.|)
       [(form-id (_::block form ...))
        #'((begin-for-syntax
             (rhombus-top form ...)))]
       ;; since definition contexts overlap declaration contexts,
       ;; we have to handle `bridge` here, too:
       [(form-id (op |.|) bridge . tail)
        #'((rhombus-definition (group bridge . tail)))]))))

(define-syntax bridge
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(form-id lhs:identifier (tag::block form ...))
        #'((define-syntax lhs
             (rhombus-body-at tag form ...)))]))))
