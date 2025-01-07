#lang racket/base
(require (for-syntax racket/base)
         "name-root.rkt"
         "define-operator.rkt"
         "define-arity.rkt"
         (submod "arithmetic.rkt" static-infos)
         "call-result-key.rkt"
         "annotation-failure.rkt"
         "order-primitive.rkt")

(provide (for-space rhombus/namespace
                    bits))

(define-name-root bits
  #:fields
  ([not bits.not]
   [xor bits.xor]
   [and bits.and]
   [or bits.or]
   [<< |bits.(<<)|]
   [>> |bits.(>>)|]
   [? |bits.(?)|]
   [length bits.length]
   [field bits.field]))

(define-prefix #:who bits.not bitwise-not
  #:order bitwise_negation
  #:static-infos #,(get-int-static-infos))
(define-infix #:who bits.and bitwise-and
  #:order bitwise_conjunction
  #:static-infos #,(get-int-static-infos))
(define-infix #:who bits.or bitwise-ior
  #:order bitwise_disjunction
  #:static-infos #,(get-int-static-infos))
(define-infix #:who bits.xor bitwise-xor
  #:order bitwise_disjunction
  #:static-infos #,(get-int-static-infos))

(define (check-int who n)
  (unless (exact-integer? n)
    (raise-annotation-failure who n "Int")))

(define (check-nonneg-int who n)
  (unless (exact-nonnegative-integer? n)
    (raise-annotation-failure who n "NonnegInt")))

(define (arithmetic-shift-left a b)
  (define who '|bits.(<<)|)
  (check-int who a)
  (check-nonneg-int who b)
  (arithmetic-shift a b))

(define (arithmetic-shift-right a b)
  (define who '|bits.(>>)|)
  (check-int who a)
  (check-nonneg-int who b)
  (arithmetic-shift a (- b)))

(define-infix |bits.(<<)| arithmetic-shift-left
  #:order bitwise_shift
  #:static-infos #,(get-int-static-infos))
(define-infix |bits.(>>)| arithmetic-shift-right
  #:order bitwise_shift
  #:static-infos #,(get-int-static-infos))

(define-infix #:who |bits.(?)| bitwise-bit-set?
  #:order bitwise_test)

(define/arity (bits.length n)
  #:primitive (integer-length)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (integer-length n))

(define/arity (bits.field n start end)
  #:primitive (bitwise-bit-field)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (bitwise-bit-field n start end))
