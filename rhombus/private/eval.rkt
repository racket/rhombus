#lang racket/base
(require "provide.rkt"
         "parse.rkt"
         "pack.rkt"
         "expression.rkt"
         "define-arity.rkt"
         "function-arity-key.rkt"
         "static-info.rkt")

(provide (for-spaces (#f
                      rhombus/statinfo)
                     make_rhombus_toplevel
                     make_rhombus_empty_toplevel
                     (rename-out [rhombus-eval eval]
                                 [current-namespace current_toplevel])))

(define/arity (make_rhombus_empty_toplevel)
  (define this-ns (variable-reference->empty-namespace (#%variable-reference)))
  (define ns (make-base-empty-namespace))
  (namespace-attach-module this-ns
                           'rhombus
                           ns)
  ns)

(define/arity (make_rhombus_toplevel)
  (define ns (make_rhombus_empty_toplevel))
  (parameterize ([current-namespace ns])
    (namespace-require 'rhombus))
  ns)

(define/arity #:name eval (rhombus-eval e)
  (unless (syntax? e)
    (raise-argument-error 'eval "Syntax" e))
  (eval #`(rhombus-top #,@(unpack-multi e 'eval #f))))

(define-static-info-syntaxes (current-namespace)
  (#%function-arity 6))
