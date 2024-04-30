#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "name-root.rkt"
         "static-info.rkt"
         "realm.rkt"
         "define-arity.rkt"
         (submod "define-arity.rkt" for-info)
         "function-arity-key.rkt"
         "indirect-static-info-key.rkt"
         "call-result-key.rkt"
         (submod "arithmetic.rkt" static-infos))

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
   gcd lcm
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
  (#%function-arity 2)
  (#%indirect-static-info indirect-function-static-info)
  (#%call-result #,number-static-infos))

(define-static-info-syntaxes (expt)
  (#%function-arity 4)
  (#%indirect-static-info indirect-function-static-info)
  (#%call-result #,number-static-infos))

(define-static-info-syntaxes (min max)
  (#%function-arity -2)
  (#%indirect-static-info indirect-function-static-info)
  (#%call-result #,number-static-infos))

(define-static-info-syntaxes (log atan)
  (#%function-arity 6)
  (#%indirect-static-info indirect-function-static-info)
  (#%call-result #,number-static-infos))

(define-static-info-syntaxes (gcd lcm)
  (#%function-arity -1)
  (#%indirect-static-info indirect-function-static-info)
  (#%call-result #,number-static-infos))

(define (check-posint who n)
  (unless (exact-positive-integer? n)
    (raise-argument-error* who rhombus-realm "PosInt" n)))

(define (check-int who n)
  (unless (exact-integer? n)
    (raise-argument-error* who rhombus-realm "Int" n)))

(define/arity #:name random rhombus-random
  #:static-infos ((#%call-result #,number-static-infos))
  (case-lambda
    [() (random)]
    [(n)
     (check-posint who n)
     (if (n . < . (arithmetic-shift 1 31))
         (random n)
         (let rejection-loop ()
           (define maybe-result
             (let ([m (- n 1)])
               (let loop ([r 0] [len (integer-length m)] [shift 0])
                 (if (len . < . 32)
                     (+ r (arithmetic-shift (random (add1 (arithmetic-shift m (- shift)))) shift))
                     (loop (+ r (arithmetic-shift (random #x80000000) shift))
                           (- len 31)
                           (+ shift 31))))))
           (if (maybe-result . < . n)
               maybe-result
               (rejection-loop))))]
    [(start end)
     (check-int who start)
     (check-int who end)
     (unless (start . < . end)
       (raise-arguments-error* who rhombus-realm
                               "start index is not less than end index"
                               "start index" start
                               "end index" end))
     (+ (rhombus-random (- end start)) start)]))

(define-syntax (define-nary stx)
  (syntax-parse stx
    [(_ ok? ok-str 0-value
        #:static-infos static-infos
        [name op] ...)
     #'(begin
         (define/arity name
           #:static-infos static-infos
           (case-lambda
             [() (0-value op)]
             [(a)
              (unless (ok? a) (raise-argument-error* who rhombus-realm ok-str a))
              (op a)]
             [(a b)
              (unless (ok? a) (raise-argument-error* who rhombus-realm ok-str a))
              (unless (ok? b) (raise-argument-error* who rhombus-realm ok-str b))
              (op a b)]
             [ns
              (for ([a (in-list ns)])
                (unless (ok? a) (raise-argument-error* who rhombus-realm ok-str a)))
              (apply op ns)]))
         ...)]))

(define-nary
  number? "Number" (lambda (op) #t)
  #:static-infos ()
  [equal =])

(define-nary
  real? "Real" (lambda (op) #t)
  #:static-infos ()
  [less <]
  [less_or_equal <=]
  [greater >]
  [greater_or_equal >=])

(define-nary
  number? "Number" (lambda (op) (op))
  #:static-infos ((#%call-result #,number-static-infos))
  [sum +]
  [product *])
