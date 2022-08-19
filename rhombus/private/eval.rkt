#lang racket/base
(require "parse.rkt"
         "pack.rkt")

(provide make_rhombus_toplevel
         make_rhombus_empty_toplevel
         (rename-out [rhombus-eval eval]
                     [current-namespace current_toplevel]))

(define (make_rhombus_empty_toplevel)
  (define this-ns (variable-reference->empty-namespace (#%variable-reference)))
  (define ns (make-base-empty-namespace))
  (namespace-attach-module this-ns
                           'rhombus
                           ns)
  ns)

(define (make_rhombus_toplevel)
  (define ns (make_rhombus_empty_toplevel))
  (parameterize ([current-namespace ns])
    (namespace-require 'rhombus))
  ns)

(define rhombus-eval
  (let ([eval (lambda (e)
                (unless (syntax? e)
                  (raise-argument-error 'eval "Syntax" e))
                (eval #`(rhombus-top #,@(unpack-multi e 'eval #f))))])
    eval))
