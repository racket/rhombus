#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt")
         racket/hash-code
         "provide.rkt"
         "name-root.rkt"
         "realm.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         "define-arity.rkt")

(provide (for-spaces (rhombus/class
                      rhombus/namespace)
                     Equatable))

(define-values (prop:Equatable Equatable? Equatable-ref)
  (make-struct-type-property
   'Equatable
   #false
   (list (cons prop:equal+hash (lambda (_) bounced-equal+hash-implementation)))))

(define (bounce-to-equal-mode-proc this other recur mode)
  (equal-recur-internal-method this other recur))

(define (bounce-to-hash-mode-proc this recur mode)
  (hash-recur-internal-method this recur))

(define bounced-equal+hash-implementation
  (list bounce-to-equal-mode-proc bounce-to-hash-mode-proc))

(define-class-desc-syntax Equatable
  (interface-desc #'()
                  '#(#&equals #&hash_code)
                  #'#(#:abstract #:abstract)
                  (hasheq 'equals 0
                          'hash_code 1)
                  #hasheq()
                  '()
                  #f
                  #'()
                  '()
                  ;; --------------------
                  #'Equatable
                  #'Equatable
                  #'prop:Equatable
                  #'prop:Equatable
                  #'Equatable-ref
                  #t
                  #f
                  null))

(define (equal-recur-internal-method this other recur)
  ((vector-ref (Equatable-ref this) 0) this other recur))

(define (hash-recur-internal-method this recur)
  ((vector-ref (Equatable-ref this) 1) this recur))

(define-name-root Equatable
  #:fields
  ([hash Equatable.hash]
   identity_hash
   hash_code_combine
   hash_code_combine_unordered))

(define/arity (Equatable.hash v)
  (equal-always-hash-code v))

(define/arity (identity_hash v)
  (eq-hash-code v))

(define (check-int who i)
  (unless (exact-integer? i)
    (raise-argument-error* who rhombus-realm "Int" i)))

(define/arity hash_code_combine
  (case-lambda
    [() (hash-code-combine)]
    [(a)
     (check-int who a)
     (hash-code-combine a)]
    [(a b)
     (check-int who a)
     (check-int who b)
     (hash-code-combine a b)]
    [lst
     (for ([e (in-list lst)])
       (check-int who e))
     (hash-code-combine* lst)]))

(define/arity hash_code_combine_unordered
  (case-lambda
    [() (hash-code-combine-unordered)]
    [(a)
     (check-int who a)
     (hash-code-combine-unordered a)]
    [(a b)
     (check-int who a)
     (check-int who b)
     (hash-code-combine-unordered a b)]
    [lst
     (for ([e (in-list lst)])
       (check-int who e))
     (hash-code-combine-unordered* lst)]))
