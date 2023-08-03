#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "declaration.rkt"
         "definition.rkt"
         "name-root.rkt"
         "parens.rkt"
         "parse.rkt"
         (for-syntax "parse.rkt")
         "dotted-sequence-parse.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/decl)
                     meta))

(module+ for-bridge
  (provide (for-syntax make-bridge-definer)))

(define-name-root meta
  #:fields
  (bridge))

(define-decl-syntax meta
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

(define-for-syntax (make-bridge-definer space-sym)
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(form-id name-seq::dotted-operator-or-identifier-sequence (tag::block form ...))
        #:with name::dotted-operator-or-identifier #'name-seq
        #`(#,(build-syntax-definition/maybe-extension
              space-sym #'name.name #'name.extends
              #'(rhombus-body-at tag form ...)))]))))

(define-defn-syntax bridge
  (make-bridge-definer #f))
