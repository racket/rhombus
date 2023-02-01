#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt")
         "provide.rkt"
         "name-root.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "realm.rkt"
         "class-dot.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax))

(provide (for-spaces (rhombus/namespace
                      rhombus/class
                      rhombus/annot)
                     Equatable))

(define-values (prop:Equatable Equatable? Equatable-ref)
  (make-struct-type-property
   'Equatable
   #false
   (list (cons prop:equal+hash (lambda (_) bounced-equal+hash-implementation)))))

(define-values (prop:Equatable-public Equatable-public? Equatable-public-ref)
  (make-struct-type-property 'Equatable #false (list (cons prop:Equatable values))))


(define (bounce-to-equal-mode-proc this other recur mode)
  (equal-recur-internal-method this other recur))

(define (bounce-to-hash-mode-proc this recur mode)
  (hash-recur-internal-method this recur))

(define bounced-equal+hash-implementation
  (list bounce-to-equal-mode-proc bounce-to-hash-mode-proc))


(define-syntax Equatable-internal
  (interface-desc #'Equatable-internal
                  #false
                  #'()
                  #'prop:Equatable
                  #'Equatable-ref
                  (vector-immutable (box-immutable 'equal_recur)
                                    (box-immutable 'hash_recur))
                  #'#(#:abstract #:abstract)
                  (hasheq 'equal_recur 0 'hash_recur 1)
                  #hasheq()
                  #t))

(define-class-desc-syntax Equatable
  (interface-desc #'Equatable
                  #'Equatable-internal
                  #'()
                  #'prop:Equatable-public
                  #'Equatable-public-ref
                  (vector-immutable (box-immutable 'equal_recur)
                                    (box-immutable 'hash_recur))
                  #'#(#:abstract #:abstract)
                  (hasheq 'equal_recur 0 'hash_recur 1)
                  #hasheq()
                  #t))

(define-name-root Equatable
  #:fields
  ([equal_recur equal-recur-method]
   [hash_recur hash-recur-method]))

(define-annotation-syntax Equatable
  (identifier-annotation #'Equatable-public? #'((#%dot-provider equatable-instance))))

(define-dot-provider-syntax equatable-instance
  (dot-provider-more-static (make-handle-class-instance-dot #'Equatable #hasheq() #hasheq())))

(define (get-equatable who v)
  (define vt (Equatable-public-ref v #false))
  (unless vt
    (raise-argument-error* who rhombus-realm "Equatable" v))
  vt)

(define equal-recur-method
  (let ([equal_recur
         (lambda (this other recur)
           ((vector-ref (get-equatable 'equal_recur this) 0) this other recur))])
    equal_recur))
  
(define hash-recur-method
  (let ([hash_recur
         (lambda (this recur)
           ((vector-ref (get-equatable 'hash_recur this) 1) this recur))])
    hash_recur))

(define (equal-recur-internal-method this other recur)
  ((vector-ref (Equatable-ref this) 0) this other recur))

(define (hash-recur-internal-method this recur)
  ((vector-ref (Equatable-ref this) 1) this recur))
