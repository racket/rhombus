#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression+binding.rkt")

(provide (rename-out [rhombus= =]))

(define-syntax rhombus=
  (make-expression+binding-infix-operator
   #'rhombus=
   '((default . weaker))
   'macro
   'none
   ;; expression
   (lambda (form tail)
     (syntax-parse tail
       #:datum-literals (op)
       [((op o) . _)
        (raise-syntax-error #f
                            "not an expression operator"
                            #'o)]))
   ;; binding
   (lambda (form tail)
     (syntax-parse tail
       #:datum-literals (op)
       [((op o) . _)
        (raise-syntax-error #f
                            "not a binding operator"
                            #'o)]))))
