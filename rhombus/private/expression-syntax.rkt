#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     enforest/transformer-result
                     "srcloc.rkt"
                     "tail.rkt")
         "syntax.rkt"
         "expression.rkt"
         "parse.rkt")

(provide expression_form)

(define-syntax expression_form
  (make-syntax-definition-transformer (lambda (x) x)
                                      #'make-expression-prefix-operator
                                      #'make-expression-infix-operator
                                      #'prefix+infix))

(begin-for-syntax
  (struct prefix+infix (prefix infix)
    #:property prop:expression-prefix-operator (lambda (self) (prefix+infix-prefix self))
    #:property prop:expression-infix-operator (lambda (self) (prefix+infix-infix self))))

(define-for-syntax (make-expression-infix-operator name prec transformer? proc assc)
  (expression-infix-operator
   name
   prec
   transformer?
   (if transformer?
       (lambda (form1 tail)
         (define-values (form new-tail) (syntax-parse tail
                                          [(head . tail) (proc #`(parsed #,form1) (pack-tail #'tail) #'head)]))
         (check-transformer-result #`(rhombus-expression (group #,(check-expression-result form proc)))
                                   (unpack-tail new-tail proc)
                                   proc))
       (lambda (form1 form2 stx)
         #`(rhombus-expression (group #,(check-expression-result
                                         (proc #`(parsed #,form1) #`(parsed #,form2) stx)
                                         proc)))))
   assc))

(define-for-syntax (make-expression-prefix-operator name prec transformer? proc)
  (expression-prefix-operator
   name
   prec
   transformer? 
   (if transformer?
       (lambda (tail)
         (define-values (form new-tail) (syntax-parse tail
                                          [(head . tail) (proc (pack-tail #'tail) #'head)]))
         (check-transformer-result #`(rhombus-expression (group #,(check-expression-result form proc)))
                                   (unpack-tail new-tail proc)
                                   proc))
       (lambda (form stx)
         #`(rhombus-expression (group #,(check-expression-result
                                         (proc #`(parsed #,form) stx)
                                         proc)))))))
