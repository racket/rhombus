#lang racket/base
(require (for-syntax racket/base)
         (only-in racket/base
                  [bitwise-bit-set? racket:bitwise-bit-set?])
         "name-root.rkt"
         "define-operator.rkt"
         "realm.rkt"
         "define-arity.rkt"
         (submod "arithmetic.rkt" static-infos)
         "call-result-key.rkt")

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
  #:static-infos #,(get-int-static-infos))
(define-infix #:who bits.and bitwise-and
  #:weaker-than (bits.not)
  #:static-infos #,(get-int-static-infos))
(define-infix #:who bits.or bitwise-ior
  #:weaker-than (bits.and bits.not)
  #:static-infos #,(get-int-static-infos))
(define-infix #:who bits.xor bitwise-xor
  #:weaker-than (bits.not)
  #:same-as (bits.or)
  #:static-infos #,(get-int-static-infos))

(define (check-int who n)
  (unless (exact-integer? n)
    (raise-argument-error* who rhombus-realm "Int" n)))

(define (check-nonneg-int who n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error* who rhombus-realm "NonnegInt" n)))

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
  #:static-infos #,(get-int-static-infos))
(define-infix |bits.(>>)| arithmetic-shift-right
  #:static-infos #,(get-int-static-infos))

;; TEMP do our own error reporting to accommodate for racket/racket#5009
(define (bitwise-bit-set? n m)
  (define who '|bits.(?)|)
  (unless (exact-nonnegative-integer? m)
    (check-int who n)
    (raise-argument-error* who rhombus-realm "NonnegInt" m))
  (racket:bitwise-bit-set? n m))
(define-infix #:who |bits.(?)| bitwise-bit-set?)

(define/arity (bits.length n)
  #:primitive (integer-length)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (integer-length n))

;; TEMP ibid
(define/arity (bits.field n start end)
  #:primitive (bitwise-bit-field)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (unless (and (exact-nonnegative-integer? start)
               (exact-nonnegative-integer? end)
               (start . <= . end))
    (check-int who n)
    (check-nonneg-int who start)
    (check-nonneg-int who end)
    (raise-arguments-error* who rhombus-realm
                            "starting index must be less than or equal to ending index"
                            "starting index" (unquoted-printing-string (number->string start))
                            "ending index" (unquoted-printing-string (number->string end))))
  (bitwise-bit-field n start end))
