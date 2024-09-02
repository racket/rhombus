#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "parse.rkt"
         "pack.rkt"
         "define-arity.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         (submod "annotation.rkt" for-class)
         "name-root.rkt"
         "realm.rkt"
         (submod "module-path-object.rkt" for-primitive)
         (submod "function.rkt" for-info))

(provide (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [rhombus-eval eval]))
         (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Evaluator))

(define-annotation-syntax Evaluator (identifier-annotation namespace? ()))

(define-name-root Evaluator
  #:fields
  (make_rhombus_empty
   make_rhombus
   [current current-namespace]
   import))

(define/arity (make_rhombus_empty)
  (define this-ns (variable-reference->empty-namespace (#%variable-reference)))
  (define ns (make-base-empty-namespace))
  (namespace-attach-module this-ns
                           'rhombus
                           ns)
  ns)

(define/arity (make_rhombus)
  (define ns (make_rhombus_empty))
  (parameterize ([current-namespace ns])
    (namespace-require 'rhombus))
  ns)

(define/arity #:name eval (rhombus-eval e
                                        #:as_interaction [as_interaction #f])
  (unless (syntax? e)
    (raise-argument-error* who rhombus-realm "Syntax" e))
  (if as_interaction
      (eval `(#%top-interaction . ,#`(multi #,@(unpack-multi e 'eval #f))))
      (eval #`(rhombus-top #,@(unpack-multi e 'eval #f)))))

(define-static-info-syntaxes (current-namespace)
  (#%function-arity 3)
  . #,(get-function-static-infos))

(define/arity (import mod-path)
  (unless (module-path? mod-path)
    (raise-argument-error* who rhombus-realm "ModulePath" mod-path))
  (namespace-require (module-path-raw mod-path)))
