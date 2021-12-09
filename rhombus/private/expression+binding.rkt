#lang racket/base
(require (for-syntax racket/base)
         "expression.rkt"
         "binding.rkt")

(provide (for-syntax make-expression+binding-prefix-operator
                     make-expression+binding-infix-operator))

(begin-for-syntax
  (struct expression+binding-prefix-operator (exp-op bind-op)
    #:property prop:expression-prefix-operator (lambda (self) (expression+binding-prefix-operator-exp-op self))
    #:property prop:binding-prefix-operator (lambda (self) (expression+binding-prefix-operator-bind-op self)))
  (define (make-expression+binding-prefix-operator name prec protocol exp bind)
    (expression+binding-prefix-operator
     (expression-prefix-operator name prec protocol exp)
     (binding-prefix-operator name prec protocol bind))))

(begin-for-syntax
  (struct expression+binding-infix-operator (exp-op bind-op)
    #:property prop:expression-infix-operator (lambda (self) (expression+binding-infix-operator-exp-op self))
    #:property prop:binding-infix-operator (lambda (self) (expression+binding-infix-operator-bind-op self)))
  (define (make-expression+binding-infix-operator name prec protocol assc exp bind)
    (expression+binding-infix-operator
     (expression-infix-operator name prec protocol exp assc)
     (binding-infix-operator name prec protocol bind assc))))
