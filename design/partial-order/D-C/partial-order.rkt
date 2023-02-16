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
(module+ private
  (provide PartialOrder?
           equal-proc/partial-compare
           hash-proc/compare-key))

(require (for-syntax racket/base
                     rhombus/private/interface-parse)
         rhombus/private/define-operator
         (only-in rhombus/private/arithmetic
                  \|\| &&
                  [+ rhombus+]
                  [- rhombus-]
                  [* rhombus*]
                  [/ rhombus/])
         (prefix-in rkt: (only-in racket/base =~ !=~ <~ >~ <=~ >=~))
         rhombus/private/name-root
         rhombus/private/realm
         (only-in rhombus/private/class-desc define-class-desc-syntax)
         "../common/partial-order-util.rkt")

;; ---------------------------------------------------------

;; PartialOrder struct type property and interface

(define-values (prop:PartialOrder PartialOrder? PartialOrder-ref)
  (make-struct-type-property
   'PartialOrder
   #false
   (list (cons prop:partial-order
               (lambda (v)
                 (define compare_key (vector-ref v 0))
                 (define partial_compare (vector-ref v 1))
                 (define (compare-hash-code-proc x rec)
                   (rec (compare_key x)))
                 (list partial_compare compare-hash-code-proc))))))

(define-class-desc-syntax PartialOrder
  (interface-desc #'PartialOrder
                  #'PartialOrder
                  #'()
                  #'prop:PartialOrder
                  #'PartialOrder-ref
                  (vector-immutable (box-immutable 'compare_key)
                                    (box-immutable 'partial_compare))
                  #'#(#:abstract #:abstract)
                  (hasheq 'compare_key 0 'partial_compare 1)
                  #hasheq()
                  #t))

(module+ private
  ;; equal-proc/partial-compare : Self Imp (Any Any -> Boolean) -> Boolean
  (define (equal-proc/partial-compare a b recur-eql)
    (define partial_compare (vector-ref (PartialOrder-ref a) 1))
    (define (recur-cmp ai bi) (if (recur-eql ai bi) 0 +nan.0))
    (partial_compare a b recur-cmp))

  ;; hash-proc/compare-key : Self (Any -> Integer) -> Integer
  (define (hash-proc/compare-key a recur-hsh)
    (define compare_key (vector-ref (PartialOrder-ref a) 0))
    (recur-hsh (compare_key a))))

;; ---------------------------------------------------------

;; Public operations

(define-name-root PartialOrder
  #:fields
  (partial_compare
   compare
   compare_hash_code
   within))

(define (partial_compare a b) (partial-compare a b))

(define (compare a b)
  (ordering-normalize (partial-compare/recur a b compare)))

(define (compare_hash_code a) (compare-hash-code a))

(define (early-nan/= c) (if (zero? c) 0 +nan.0))

(define (partial-compare/within a b epsilon)
  (early-nan/=
   (cond
     [(realish? a)
      (ord-and/bool (realish? b) (partial-compare-realish/within a b epsilon))]
     [else
      (define (cmp ai bi) (partial-compare/within ai bi epsilon))
      (partial-compare/recur a b cmp)])))

(define (within a b epsilon)
  (zero? (partial-compare/within a b epsilon)))

(define-syntax-rule (define-comp~-infix name racket-name)
  (define-infix name racket-name
    #:weaker-than (rhombus+ rhombus- rhombus* rhombus/)
    #:same-as (<~ <=~ =~ !=~ >=~ >~)
    #:stronger-than (\|\| &&)
    #:associate 'none))

(define-comp~-infix <~ rkt:<~)
(define-comp~-infix <=~ rkt:<=~)
(define-comp~-infix =~ rkt:=~)
(define-comp~-infix !=~ rkt:!=~)
(define-comp~-infix >=~ rkt:>=~)
(define-comp~-infix >~ rkt:>~)
