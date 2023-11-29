#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         (submod "bytes.rkt" static-infos)
         (submod "string.rkt" static-infos)
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

(define-primitive-class Output output-port
  #:lift-declaration
  #:existing
  #:translucent
  #:fields ()
  #:namespace-fields
  ([current current-output-port]
   [current_error current-error-port]
   [open_bytes Port.Output.open_bytes]
   [open_string Port.Output.open_string])
  #:properties ()
  #:methods
  ([get_bytes Port.Output.get_bytes]
   [get_string Port.Output.get_string]
   [flush Port.Output.flush]))

(define-static-info-syntaxes (current-input-port current-output-port current-error-port)
  (#%function-arity 3)
  (#%indirect-static-info indirect-function-static-info))

;; TODO these need a more specific annotation
(define/arity Port.Output.open_bytes
  #:inline
  #:primitive (open-output-bytes)
  #:static-infos ((#%call-result #,output-port-static-infos))
  (case-lambda
    [() (open-output-bytes)]
    [(name) (open-output-bytes name)]))

(define/arity Port.Output.open_string
  #:inline
  #:primitive (open-output-string)
  #:static-infos ((#%call-result #,output-port-static-infos))
  (case-lambda
    [() (open-output-string)]
    [(name) (open-output-string name)]))

(define/method (Port.Output.get_bytes port)
  #:inline
  #:primitive (get-output-bytes)
  #:static-infos ((#%call-result #,bytes-static-infos))
  (get-output-bytes port))

(define/method (Port.Output.get_string port)
  #:inline
  #:primitive (get-output-string)
  #:static-infos ((#%call-result #,string-static-infos))
  (string->immutable-string (get-output-string port)))

(define/method (Port.Output.flush [p (current-output-port)])
  (unless (output-port? p)
    (raise-argument-error* who rhombus-realm "Port.Output" p))
  (flush-output p))
