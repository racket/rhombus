#lang racket/base
(require "name-root.rkt"
         "static-info.rkt"
         "define-arity.rkt"
         "realm.rkt")

(provide (for-space rhombus/namespace
                    math))


(define-name-root math
  #:fields
  (abs
   min max
   floor ceiling round
   sqrt
   log exp expt
   cos sin tan
   acos asin atan
   [random rhombus-random]
   numerator
   denominator   
   magnitude angle
   [real_part real-part]
   [imag_part imag-part]
   [exact inexact->exact]
   [inexact exact->inexact]))

(define-static-info-syntaxes (abs
                              floor ceiling round
                              sqrt
                              exp
                              cos sin tan
                              acos asin
                              numerator
                              denominator
                              magnitude angle
                              real-part imag-part
                              inexact->exact exact->inexact)
  (#%function-arity 2))

(define-static-info-syntaxes (min max expt)
  (#%function-arity 4))

(define-static-info-syntaxes (log atan)
  (#%function-arity 6))

(define rhombus-random
  (let ([random
         (case-lambda
           [() (random)]
           [(n)
            (unless (exact-positive-integer? n)
              (raise-argument-error* 'random
                                     rhombus-realm
                                     "PosInt"
                                     n))
            (if (n . < . (arithmetic-shift 1 31))
                (random n)
                (let loop ([n n] [r 0] [len (integer-length n)] [shift 0])
                  (if (len . < . 32)
                      (+ r (arithmetic-shift (random n) shift))
                      (loop (arithmetic-shift n -31)
                            (+ r (arithmetic-shift (random #x7FFFFFFF) shift))
                            (- len 32)
                            (+ shift 31)))))])])
    random))
