#lang racket/base

(require (for-syntax racket/base
                     "interface-parse.rkt")
         "name-root.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "realm.rkt"
         "class-dot.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax))

(provide Equatable
         (for-space rhombus/class Equatable)
         (for-space rhombus/annotation Equatable))


(define-values (prop:Equatable Equatable? Equatable-ref)
  (make-struct-type-property
   'Equatable
   #false
   (list (cons prop:equal+hash (lambda (_) bounced-equal+hash-implementation)))))

(define-values (prop:Equatable-public Equatable-public? Equatable-public-ref)
  (make-struct-type-property 'Equatable #false (list (cons prop:Equatable values))))


(define (bounce-to-equal-mode-proc this other recur mode)
  (equals-internal-method this other))

(define (bounce-to-hash-mode-proc this recur mode)
  (hashCode-internal-method this))

(define bounced-equal+hash-implementation
  (list bounce-to-equal-mode-proc bounce-to-hash-mode-proc))


(define-syntax Equatable-internal
  (interface-desc #'Equatable-internal
                  #false
                  #'()
                  #'prop:Equatable
                  #'Equatable-ref
                  (vector-immutable (box-immutable 'equals) (box-immutable 'hashCode))
                  #'#(#:abstract #:abstract)
                  (hasheq 'equals 0 'hashCode 1)
                  #hasheq()))

(define-class-desc-syntax Equatable
  (interface-desc #'Equatable
                  #'Equatable-internal
                  #'()
                  #'prop:Equatable-public
                  #'Equatable-public-ref
                  (vector-immutable (box-immutable 'equals) (box-immutable 'hashCode))
                  #'#(#:abstract #:abstract)
                  (hasheq 'equals 0 'hashCode 1)
                  #hasheq()))

(define-name-root Equatable
  #:fields
  ([equals equals-method]
   [hashCode hashCode-method]))

(define-annotation-syntax Equatable
  (identifier-annotation
   #'equatable-interface #'Equatable-public? #'((#%dot-provider equatable-instance))))

(define-dot-provider-syntax equatable-instance
  (dot-provider-more-static (make-handle-class-instance-dot #'Equatable #hasheq() #hasheq())))

(define (get-equatable who v)
  (define vt (Equatable-public-ref v #false))
  (unless vt
    (raise-argument-error* who rhombus-realm "Equatable" v))
  vt)

(define equals-method
  (let ([equals (lambda (this other)
                  ((vector-ref (get-equatable 'equals this) 0) this other))])
    equals))
  
(define hashCode-method
  (let ([hashCode (lambda (this)
                 ((vector-ref (get-equatable 'hashCode this) 1) this))])
    hashCode))

(define (equals-internal-method v op)
  ((vector-ref (Equatable-ref v) 0) v op))

(define (hashCode-internal-method v op)
  ((vector-ref (Equatable-ref v) 1) v op))
