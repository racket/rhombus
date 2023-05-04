#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "name-root.rkt"
         "static-info.rkt"
         "define-arity.rkt"
         "realm.rkt")

(provide (for-space rhombus/namespace
                    math))


(define-name-root math
  #:fields
  (pi
   abs
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
   [inexact exact->inexact]
   equal
   less less_or_equal
   greater greater_or_equal
   sum product))

(define pi (atan 0 -1))

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

(define-syntax (define-nary stx)
  (syntax-parse stx
    [(_ ok? ok-str 0-value [name op] ...)
     #'(begin
         (define name
           (case-lambda
             [() (0-value op)]
             [(a)
              (unless (ok? a) (raise-argument-error* 'name rhombus-realm ok-str a))
              (op a)]
             [(a b)
              (unless (ok? a) (raise-argument-error* 'name rhombus-realm ok-str a))
              (unless (ok? b) (raise-argument-error* 'name rhombus-realm ok-str b))
              (op a b)]
             [ns
              (for ([a (in-list ns)])
                (unless (ok? a) (raise-argument-error* 'name rhombus-realm ok-str a)))
              (apply op ns)]))
         ...
         (define-static-info-syntaxes (name ...)
           (#%function-arity -1)))]))

(define-nary
  number? "Number" (lambda (op) #t)
  [equal =])

(define-nary
  real? "Real" (lambda (op) #t)
  [less <]
  [less_or_equal <=]
  [greater >]
  [greater_or_equal >=])

(define-nary
  number? "Number" (lambda (op) (op))
  [sum +]
  [product *])