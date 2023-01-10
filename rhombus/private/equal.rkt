#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression+binding.rkt")

(provide (rename-out [rhombus= =]))

(module+ for-parse
  (provide (for-syntax :equal
                       :not-equal)))

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

(begin-for-syntax
  (define-syntax-class :equal
    #:datum-literals (op)
    #:literals (rhombus=)
    (pattern (op rhombus=)))
  (define-syntax-class :not-equal
    #:datum-literals (op)
    #:literals (rhombus=)
    (pattern (~not (op rhombus=)))))
