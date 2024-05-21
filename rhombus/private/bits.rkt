#lang racket/base
(require "name-root.rkt"
         "static-info.rkt"
         "define-operator.rkt"
         "realm.rkt"
         (submod "function.rkt" for-info)
         "function-arity-key.rkt")

(provide (for-space rhombus/namespace
                    bits))

(define-name-root bits
  #:fields
  ([not bits.not]
   [xor bits.xor]
   [and bits.and]
   [or bits.or]
   <<
   >>
   ?
   [length integer-length]
   [field bitwise-bit-field]))

(define-prefix bits.not bitwise-not)
(define-infix bits.and bitwise-and
  #:weaker-than (bits.not))
(define-infix bits.or bitwise-ior
  #:weaker-than (bits.and bits.not))
(define-infix bits.xor bitwise-xor
  #:weaker-than (bits.not)
  #:same-as (bits.or))

(define (arithmetic-shift-left a b)
  (unless (exact-integer? a) (raise-argument-error* 'bits.<< rhombus-realm "Int" a))
  (unless (exact-nonnegative-integer? b) (raise-argument-error* 'bits.<< rhombus-realm "NonnegInt" b))
  (arithmetic-shift a b))

(define (arithmetic-shift-right a b)
  (unless (exact-integer? a) (raise-argument-error* 'bits.<< rhombus-realm "Int" a))
  (unless (exact-nonnegative-integer? b) (raise-argument-error* 'bits.<< rhombus-realm "NonnegInt" b))
  (arithmetic-shift a (- b)))

(define-infix << arithmetic-shift-left)
(define-infix >> arithmetic-shift-right)

(define-infix ? bitwise-bit-set?)

(define-static-info-syntaxes (integer-length)
  (#%function-arity 2)
  . #,(get-function-static-infos))

(define-static-info-syntaxes (bitwise-bit-field)
  (#%function-arity 8)
  . #,(get-function-static-infos))
