#lang racket/base
(require (for-syntax racket/base)
         "order.rkt")

(provide (for-space rhombus/operator_order
                    addition
                    multiplication
                    exponentiation
                    integer_division
                    arithmetic
                    equivalence
                    order_comparison
                    comparison
                    assignment
                    enumeration
                    concatenation
                    member_access
                    logical_negation
                    logical_conjunction
                    logical_disjunction
                    bitwise_negation
                    bitwise_shift
                    bitwise_conjunction
                    bitwise_disjunction
                    bitwise_test))

(define-order-syntax addition
  (make-order
   (lambda ()
     (list
      (cons (order-quote exponentiation) 'weaker)
      (cons (order-quote multiplication) 'weaker)
      (cons (order-quote integer_division) 'weaker)))
   'left))

(define-order-syntax multiplication
  (make-order
   (lambda ()
     (list
      (cons (order-quote exponentiation) 'weaker)))
   'left))

(define-order-syntax exponentiation
  (make-order
   (list)
   'right))

(define-order-syntax integer_division
  (make-order
   (lambda ()
     (list
      (cons (order-quote multiplication) 'weaker)
      (cons (order-quote exponentiation) 'weaker)))
   'left))

;; can't use this shorthand internally
(define-order-syntax arithmetic
  (order-set
   (lambda ()
     (list
      (order-quote addition)
      (order-quote multiplication)
      (order-quote exponentiation)
      (order-quote integer_division)))))

(define-order-syntax equivalence
  (make-order
   (lambda ()
     (list
      (cons (order-quote addition) 'weaker)
      (cons (order-quote multiplication) 'weaker)
      (cons (order-quote exponentiation) 'weaker)
      (cons (order-quote integer_division) 'weaker)
      (cons (order-quote enumeration) 'weaker)))
   'none))

(define-order-syntax enumeration
  (make-order
   (lambda ()
     (list
      (cons (order-quote addition) 'weaker)
      (cons (order-quote multiplication) 'weaker)
      (cons (order-quote exponentiation) 'weaker)
      (cons (order-quote integer_division) 'weaker)))
   'none))

(define-order-syntax order_comparison
  (make-order
   (lambda ()
     (list
      (cons (order-quote addition) 'weaker)
      (cons (order-quote multiplication) 'weaker)
      (cons (order-quote exponentiation) 'weaker)
      (cons (order-quote integer_division) 'weaker)))
   'none))

;; can't use this shorthand internally
(define-order-syntax comparison
  (order-set
   (lambda ()
     (list
      (order-quote equivalence)
      (order-quote order_comparison)))))

(define-order-syntax assignment
  (make-order
   (lambda ()
     (list
      (cons 'default 'weaker)))
   'none))

(define-order-syntax member_access
  (make-order
   (lambda ()
     (list
      (cons 'default 'stronger)))
   'left))

(define-order-syntax concatenation
  (make-order
   (lambda ()
     (list
      (cons (order-quote equivalence) 'weaker)
      (cons (order-quote order_comparison) 'weaker)
      (cons (order-quote enumeration) 'weaker)
      (cons (order-quote addition) 'weaker)
      (cons (order-quote multiplication) 'weaker)
      (cons (order-quote exponentiation) 'weaker)
      (cons (order-quote integer_division) 'weaker)))
   'left))

(define-order-syntax logical_negation
  (make-order
   (lambda ()
     (list
      (cons (order-quote logical_conjunction) 'stronger)
      (cons (order-quote logical_disjunction) 'stronger)
      (cons (order-quote equivalence) 'stronger)))
   'none))

(define-order-syntax logical_conjunction
  (make-order
   (lambda ()
     (list
      (cons (order-quote logical_disjunction) 'stronger)
      (cons (order-quote equivalence) 'weaker)
      (cons (order-quote order_comparison) 'weaker)
      (cons (order-quote enumeration) 'weaker)
      (cons (order-quote concatenation) 'weaker)
      (cons (order-quote addition) 'weaker)
      (cons (order-quote multiplication) 'weaker)
      (cons (order-quote exponentiation) 'weaker)
      (cons (order-quote integer_division) 'weaker)))
   'left))

(define-order-syntax logical_disjunction
  (make-order
   (lambda ()
     (list
      (cons (order-quote equivalence) 'weaker)
      (cons (order-quote order_comparison) 'weaker)
      (cons (order-quote enumeration) 'weaker)
      (cons (order-quote concatenation) 'weaker)
      (cons (order-quote addition) 'weaker)
      (cons (order-quote multiplication) 'weaker)
      (cons (order-quote exponentiation) 'weaker)
      (cons (order-quote integer_division) 'weaker)))
   'left))

(define-order-syntax bitwise_negation
  (make-order
   (lambda ()
     (list
      (cons (order-quote bitwise_conjunction) 'stronger)
      (cons (order-quote bitwise_disjunction) 'stronger)
      (cons (order-quote bitwise_shift) 'stronger)
      (cons (order-quote bitwise_test) 'stronger)
      (cons (order-quote equivalence) 'stronger)
      (cons (order-quote order_comparison) 'stronger)))
   'none))

(define-order-syntax bitwise_shift
  (make-order
   (lambda ()
     (list
      (cons (order-quote bitwise_conjunction) 'stronger)
      (cons (order-quote bitwise_disjunction) 'stronger)
      (cons (order-quote bitwise_test) 'stronger)
      (cons (order-quote equivalence) 'stronger)
      (cons (order-quote order_comparison) 'stronger)))
   'left))

(define-order-syntax bitwise_conjunction
  (make-order
   (lambda ()
     (list
      (cons (order-quote bitwise_disjunction) 'stronger)
      (cons (order-quote bitwise_test) 'stronger)
      (cons (order-quote equivalence) 'stronger)
      (cons (order-quote order_comparison) 'stronger)))
   'left))

(define-order-syntax bitwise_disjunction
  (make-order
   (lambda ()
     (list
      (cons (order-quote bitwise_test) 'stronger)
      (cons (order-quote equivalence) 'stronger)
      (cons (order-quote order_comparison) 'stronger)))
   'left))

(define-order-syntax bitwise_test
  (make-order
   (lambda ()
     (list
      (cons (order-quote equivalence) 'stronger)))
   'left))
