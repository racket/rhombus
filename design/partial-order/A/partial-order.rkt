#lang racket/base

(require rhombus/private/provide)
(provide (for-spaces (rhombus/class
                      rhombus/namespace)
                     PartialOrder)
         (for-spaces (#f
                      rhombus/repet)
                     =~
                     !=~
                     <~
                     >~
                     <=~
                     >=~))

(require (for-syntax racket/base
                     "../../../rhombus/private/interface-parse.rkt")
         racket/fixnum
         racket/flonum
         racket/extflonum
         racket/list
         racket/math
         syntax/parse/define
         rhombus/private/define-operator
         (only-in rhombus/private/arithmetic
                  \|\| &&
                  [+ rhombus+]
                  [- rhombus-]
                  [* rhombus*]
                  [/ rhombus/])
         "../../../rhombus/private/name-root.rkt"
         (submod "../../../rhombus/private/annotation.rkt" for-class)
         (submod "../../../rhombus/private/dot.rkt" for-dot-provider)
         "../../../rhombus/private/realm.rkt"
         "../../../rhombus/private/class-dot.rkt"
         (only-in "../../../rhombus/private/class-desc.rkt" define-class-desc-syntax)
         "../../../rhombus/private/static-info.rkt")

;; ---------------------------------------------------------

;; Core ordering datatypes and operations

;; A Ordering is a ComparableReal where:
;;  - zero?     represents =~
;;  - negative? represents <~
;;  - positive? represents >~
;; this excludes NaN

;; ordering-normalize : Ordering -> Ordering
(define (ordering-normalize c)
  (cond
    [(zero? c) 0]
    [(negative? c) -1]
    [(positive? c) 1]
    [else (error 'compare "bad ~v" c)]))

;; A PartialOrdering is a Real where:
;;  - zero?     represents =~
;;  - negative? represents <~
;;  - positive? represents >~
;;  - nan?      represents incomparable

;; partial-ordering-normalize : PartialOrdering -> PartialOrdering
(define (partial-ordering-normalize c)
  (cond
    [(zero? c) 0]
    [(negative? c) -1]
    [(positive? c) 1]
    [(nan? c) +nan.0]
    [else (error 'partial_compare "bad ~v" c)]))

;; ord-and/bool : Boolean ... PartialOrdering -> PartialOrdering
(define-syntax-parser ord-and/bool
  [(_) #'0]
  [(_ c) #'c]
  [(_ b . rst)
   #'(if b (ord-and/bool . rst) +nan.0)])

;; ord-and/prod2 : PartialOrdering PartialOrdering -> PartialOrdering
(define (ord-and/prod2 c0 c1)
  (cond
    [(zero? c0) c1]
    [(zero? c1) c0]
    [(and (negative? c0) (negative? c1)) -1]
    [(and (positive? c0) (positive? c1)) 1]
    [else +nan.0]))

;; ord-and/prod : PartialOrdering ... PartialOrdering
(define-syntax-parser ord-and/prod
  [(_) #'0]
  [(_ c) #'c]
  [(_ c0 c1 . rst)
   #'(let ([c0v c0])
       (if (nan? c0v)
           +nan.0
           (ord-and/prod (ord-and/prod2 c0v c1) . rst)))])

(define (numeric? v)
  (or (real? v) (extflonum? v)))

(define (numeric-key x)
  (cond
    [(or (eq? x +inf.0) (eq? x +inf.f) (eq? x +inf.t)) +inf.0]
    [(or (eq? x -inf.0) (eq? x -inf.f) (eq? x -inf.t)) -inf.0]
    [(or (eq? x +nan.0) (eq? x +nan.f) (eq? x +nan.t)) +nan.0]
    [(extflonum? x) (extfl->exact x)]
    [else (inexact->exact x)]))

(define (partial-compare-numeric a b)
  (cond
    [(and (real? a) (real? b)) (partial-compare-real a b)]
    [(and (extflonum? a) (extflonum? b)) (partial-compare-extflonum a b)]
    [else (partial-compare-real (numeric-key a) (numeric-key b))]))

(define (partial-compare-real a b)
  (cond
    [(= a b) 0]
    [(< a b) -1]
    [(> a b) 1]
    [else +nan.0]))

(define (partial-compare-extflonum a b)
  (cond
    [(extfl= a b) 0]
    [(extfl< a b) -1]
    [(extfl> a b) 1]
    [else +nan.0]))

;; ---------------------------------------------------------

;; PartialOrder struct type property and interface

(define-values (prop:partial-order partial-order? partial-order-ref)
  (make-struct-type-property
   'partial-order
   (λ (value _type-info)
     (unless (and (procedure? value)
                  (procedure-arity-includes? value 3))
       (raise-argument-error* 'partial_compare
                              rhombus-realm
                              "a method of 2 arguments (after this)"
                              value))
     (list (gensym) value))))

(define-values (prop:PartialOrder _PartialOrder? PartialOrder-ref)
  (make-struct-type-property
   'PartialOrder
   #false
   (list (cons prop:partial-order (lambda (v) (vector-ref v 0))))))

(define-class-desc-syntax PartialOrder
  (interface-desc #'PartialOrder
                  #'PartialOrder
                  #'()
                  #'prop:PartialOrder
                  #'PartialOrder-ref
                  (vector-immutable (box-immutable 'partial_compare))
                  #'#(#:abstract)
                  (hasheq 'partial_compare 0)
                  #hasheq()
                  #t))

;; ---------------------------------------------------------

;; Public operations

(define-name-root PartialOrder
  #:fields
  (partial_compare
   compare))

(define (partial-compare/recur a b recur)
  (define (cmp ai bi)
    (partial-ordering-normalize (recur ai bi)))
  (cond
    [(partial-order? a)
     (ord-and/bool
      (partial-order? b)
      (let ([av (partial-order-ref a)])
        (ord-and/bool
         (eq? av (partial-order-ref b))
         (partial-ordering-normalize ((cadr av) a b cmp)))))]
    [(numeric? a)
     (ord-and/bool (numeric? b) (partial-compare-numeric a b))]
    [else
     (product-compare/recur a b cmp)]))

(define (product-compare/recur a b cmp)
  (cond
    [(flvector? a)
     (ord-and/bool
      (flvector? b)
      (= (flvector-length a) (flvector-length b))
      (for/fold ([acc 0])
                ([ai (in-flvector a)]
                 [bi (in-flvector b)])
        #:break (nan? acc)
        (ord-and/prod acc (cmp ai bi))))]
    [(extflvector? a)
     (ord-and/bool
      (extflvector? b)
      (= (extflvector-length a) (extflvector-length b))
      (for/fold ([acc 0])
                ([ai (in-extflvector a)]
                 [bi (in-extflvector b)])
        #:break (nan? acc)
        (ord-and/prod acc (cmp ai bi))))]
    [else
     (define (<=? ai bi) (<= (cmp ai bi) 0))
     (define le (equal?/recur a b <=?))
     (define ge (equal?/recur b a <=?))
     (cond
       [(and le ge) 0]
       [le -1]
       [ge 1]
       [else +nan.0])]))

(define (partial_compare a b)
  (partial-compare/recur a b partial_compare))

(define (compare/recur a b recur)
  (define (cmp ai bi) (ordering-normalize (recur ai bi)))
  (ordering-normalize (partial-compare/recur a b cmp)))

(define (compare a b)
  (compare/recur a b compare))

(define (=~? a b) (zero? (partial_compare a b)))
(define (!=~? a b) (not (zero? (partial_compare a b))))

(define (<~? a b) (negative? (compare a b)))
(define (>~? a b) (positive? (compare a b)))
(define (<=~? a b) (<= (compare a b) 0))
(define (>=~? a b) (>= (compare a b) 0))

(define-syntax-rule (define-comp~-infix name racket-name)
  (define-infix name racket-name
    #:weaker-than (rhombus+ rhombus- rhombus* rhombus/)
    #:same-as (<~ <=~ =~ !=~ >=~ >~)
    #:stronger-than (\|\| &&)
    #:associate 'none))

(define-comp~-infix <~ <~?)
(define-comp~-infix <=~ <=~?)
(define-comp~-infix =~ =~?)
(define-comp~-infix !=~ !=~?)
(define-comp~-infix >=~ >=~?)
(define-comp~-infix >~ >~?)
