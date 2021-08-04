#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/transformer-result
                     "srcloc.rkt"
                     "tail.rkt")
         "lexicon.rkt"
         "syntax.rkt"
         "expression.rkt"
         "parse.rkt")

(provide expr)

(module+ for-define
  (provide (for-syntax make-expression-infix-operator
                       make-expression-prefix-operator)))

(define-syntax expr
  (simple-lexicon operator
                  macro))

(define-syntax operator
  (make-operator-definition-transformer 'automatic
                                        (lambda (x) x)
                                        #'make-expression-prefix-operator
                                        #'make-expression-infix-operator
                                        #'expression-prefix+infix-operator))

(define-syntax macro
  (make-operator-definition-transformer 'macro
                                        (lambda (x) x)
                                        #'make-expression-prefix-operator
                                        #'make-expression-infix-operator
                                        #'expression-prefix+infix-operator))

(define-for-syntax (make-expression-infix-operator name prec protocol proc assc)
  (expression-infix-operator
   name
   prec
   protocol
   (if (eq? protocol 'macro)
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

(define-for-syntax (make-expression-prefix-operator name prec protocol proc)
  (expression-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'macro)
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
