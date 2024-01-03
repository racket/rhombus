#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "forwarding-sequence.rkt")
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
       [(form-id (_::block form ...))
        #'((begin-for-syntax
             (rhombus-module-forwarding-sequence
              (rhombus-top form ...))))]
       [(form-id . tail)
        #'((begin-for-syntax
             (rhombus-module-forwarding-sequence
              (rhombus-top (group . tail)))))]))))

(define-for-syntax (make-bridge-definer space-sym)
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id name-seq::dotted-operator-or-identifier-sequence (tag::block form ...))
        #:with name::dotted-operator-or-identifier #'name-seq
        #`(#,(build-syntax-definition/maybe-extension
              space-sym #'name.name #'name.extends
              #'(rhombus-body-at tag form ...)))]))))

(define-defn-syntax bridge
  (make-bridge-definer #f))
