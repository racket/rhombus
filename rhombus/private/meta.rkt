#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "declaration.rkt"
         "definition.rkt"
         "name-root.rkt"
         "parens.rkt"
         "parse.rkt"
         (for-syntax "parse.rkt"))

;; This module should provide bindings only for the
;; definition/declaration space, since `meta` as a declaration
;; form is not meant to be exported from `rhombus`

(provide (for-space rhombus/expr meta))

(define-name-root meta
  #:space rhombus/expr
  #:root
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
        #'((rhombus-definition (group bridge . tail)))])))
  #:fields
  (bridge))

(define-syntax bridge
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(form-id lhs:identifier (tag::block form ...))
        #'((define-syntax lhs
             (rhombus-body-at tag form ...)))]))))
