#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "expression.rkt"
         "static-info.rkt"
         "parse.rkt"
         (prefix-in rhombus-a: "arithmetic.rkt")
         "sequence-constructor-key.rkt")

(provide ..)

(define-syntax ..
  (expression-infix-operator
   (expr-quote ..)
   (list
    (cons (expr-quote rhombus-a:+) 'weaker)
    (cons (expr-quote rhombus-a:-) 'weaker)
    (cons (expr-quote rhombus-a:*) 'weaker)
    (cons (expr-quote rhombus-a:/) 'weaker))
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(_)
        (values (wrap-as-static-sequence #`(in-naturals #,form1))
                #'())]
       [(_ . more)
        #:with (~var rhs (:infix-op+expression+tail #'..)) #'(group . more)
        (values (wrap-as-static-sequence #`(in-range #,form1 rhs.parsed))
                #'rhs.tail)]))
   'none))

(define-for-syntax (wrap-as-static-sequence stx)
  (wrap-static-info stx #'#%sequence-constructor #'#t))
