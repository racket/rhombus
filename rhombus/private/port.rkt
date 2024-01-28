#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "name-root.rkt"
         (submod "annotation.rkt" for-class)
         "function-arity-key.rkt"
         "static-info.rkt"
         "define-arity.rkt"
         "class-primitive.rkt"
         "realm.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Port))

(module+ for-builtin
  (provide output-port-method-table))

(define-primitive-class Port port
  #:existing
  #:translucent
  #:fields ()
  #:namespace-fields
  (Input
   Output
   eof
   ;; TEMP see `Input` and `Output`
   [current_input current-input-port]
   [current_output current-output-port]
   [current_error current-error-port])
  #:properties ()
  #:methods ())

(define-primitive-class Input input-port
  #:existing
  #:translucent
  #:fields ()
  #:namespace-fields
  ([current current-input-port])
  #:properties ()
  #:methods ())

(define/method (Output.flush [p (current-output-port)])
  (unless (output-port? p)
    (raise-argument-error* who rhombus-realm "Output" p))
  (flush-output p))

(define-primitive-class Output output-port
  #:existing
  #:translucent
  #:fields ()
  #:namespace-fields
  ([current current-output-port]
   [current_error current-error-port])
  #:properties ()
  #:methods
  (flush))

(define-static-info-syntaxes (current-input-port current-output-port current-error-port)
  (#%function-arity 3)
  (#%indirect-static-info indirect-function-static-info))
