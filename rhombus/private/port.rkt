#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "name-root.rkt"
         (submod "annotation.rkt" for-class)
         "function-arity-key.rkt"
         "static-info.rkt"
         (submod "define-arity.rkt" for-info))

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Port))

(define-annotation-syntax Port (identifier-annotation #'port? #'()))

(define-name-root Port
  #:fields
  (Input
   Output
   ;; TEMP see `Input` and `Output`
   [current_input current-input-port]
   [current_output current-output-port]
   [current_error current-error-port]))

(define-name-root Input
  #:fields
  ([current current-input-port]))

(define-name-root Output
  #:fields
  ([current current-output-port]
   [current_error current-error-port]))

(define-annotation-syntax Input (identifier-annotation #'input-port? #'()))
(define-annotation-syntax Output (identifier-annotation #'output-port? #'()))

(define-static-info-syntaxes (current-input-port current-output-port current-error-port)
  (#%function-arity 3)
  (#%indirect-static-info indirect-function-static-info))
