#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "operator-parse.rkt")
         "expression.rkt")

(provide (rename-out [rhombus... ...])
         (for-syntax consume-extra-ellipses))

(define-syntax rhombus...
  (expression-transformer
   #'rhombus...
   (lambda (stx)
     (syntax-parse stx
       [(op::operator . tail)
        (raise-syntax-error #f
                            "misuse outside of a binding or constructor"
                            #'op.name)]))))

(define-for-syntax (consume-extra-ellipses stx)
  (let loop ([stx stx] [count 0])
    (syntax-parse stx
      #:datum-literals (group op)
      #:literals (rhombus...)
      [((group (op rhombus...)) . gs)
       (loop #'gs (add1 count))]
      [_ (values stx count)])))
