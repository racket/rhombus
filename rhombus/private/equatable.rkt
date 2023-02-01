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

(provide (for-spaces (rhombus/class)
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

(define (equal-recur-internal-method this other recur)
  ((vector-ref (Equatable-ref this) 0) this other recur))

(define (hash-recur-internal-method this recur)
  ((vector-ref (Equatable-ref this) 1) this recur))
