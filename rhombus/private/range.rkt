#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "expression.rkt"
         "parse.rkt"
         (prefix-in rhombus-a: "arithmetic.rkt"))

(provide ..)

(define-syntax ..
  (expression-infix-operator
   #'..
   (list
    (cons #'rhombus-a:+ 'weaker)
    (cons #'rhombus-a:- 'weaker)
    (cons #'rhombus-a:* 'weaker)
    (cons #'rhombus-a:/ 'weaker))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(_)
        (values #`(in-naturals #,form1)
                #'())]
       [(_ . more)
        #:with rhs::infix-op+expression+tail #'(.. . more)
        (values #`(in-range #,form1 rhs.parsed)
                #'rhs.tail)]))
   'none))
