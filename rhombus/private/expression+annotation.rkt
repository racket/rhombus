#lang racket/base
(require (for-syntax racket/base)
         "expression.rkt"
         "repetition.rkt"
         "annotation-operator.rkt")

(provide (for-syntax make-expression+repetition+annotation-infix-operator))

(begin-for-syntax
  (struct expression+repetition+annotation-infix-operator (exp-op rep-op ann-op)
    #:property prop:expression-infix-operator (lambda (self) (expression+repetition+annotation-infix-operator-exp-op self))
    #:property prop:repetition-infix-operator (lambda (self) (expression+repetition+annotation-infix-operator-rep-op self))
    #:property prop:annotation-infix-operator (lambda (self) (expression+repetition+annotation-infix-operator-ann-op self)))
  (define (make-expression+repetition+annotation-infix-operator name prec protocol exp rep ann assc)
    (expression+repetition+annotation-infix-operator
     (expression-infix-operator name prec protocol exp assc)
     (repetition-infix-operator name prec protocol rep assc)
     (annotation-infix-operator name prec protocol ann assc))))
