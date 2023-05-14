#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "parse.rkt"
         "pack.rkt"
         "expression.rkt"
         "define-arity.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         (submod "annotation.rkt" for-class))

(provide (for-spaces (#f
                      rhombus/statinfo)
                     make_rhombus_evaluator
                     make_rhombus_empty_evaluator
                     (rename-out [rhombus-eval eval]
                                 [current-namespace current_evaluator]))
         (for-spaces (rhombus/annot)
                     Evaluator))

(define-annotation-syntax Evaluator (identifier-annotation #'namespace? #'()))

(define/arity (make_rhombus_empty_evaluator)
  (define this-ns (variable-reference->empty-namespace (#%variable-reference)))
  (define ns (make-base-empty-namespace))
  (namespace-attach-module this-ns
                           'rhombus
                           ns)
  ns)

(define/arity (make_rhombus_evaluator)
  (define ns (make_rhombus_empty_evaluator))
  (parameterize ([current-namespace ns])
    (namespace-require 'rhombus))
  ns)

(define/arity #:name eval (rhombus-eval e)
  (unless (syntax? e)
    (raise-argument-error 'eval "Syntax" e))
  (eval #`(rhombus-top #,@(unpack-multi e 'eval #f))))

(define-static-info-syntaxes (current-namespace)
  (#%function-arity 6))
