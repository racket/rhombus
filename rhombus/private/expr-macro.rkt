#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer-result
                     "srcloc.rkt"
                     "pack.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (for-syntax racket/base))
         "space-provide.rkt"
         "name-root.rkt"
         "macro-macro.rkt"
         "expression.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         (for-syntax "name-root.rkt"))

(provide (for-syntax (for-space rhombus/namespace
                                expr_meta)))

(module+ for-define
  (provide (for-syntax make-expression-infix-operator
                       make-expression-prefix-operator)))

(define+provide-space expr #f
  #:fields
  (macro))

(begin-for-syntax
  (define-name-root expr_meta
    #:fields
    (Group
     AfterPrefixGroup
     AfterInfixGroup)))

(define-operator-definition-transformer macro
  'macro
  #f
  #'make-expression-prefix-operator
  #'make-expression-infix-operator
  #'expression-prefix+infix-operator)

(begin-for-syntax
  (define-operator-syntax-classes
    Group :expression
    AfterPrefixGroup :prefix-op+expression+tail
    AfterInfixGroup :infix-op+expression+tail))

(define-for-syntax (parsed-argument form)
  ;; use `rhombus-local-expand` to expose static information
  #`(parsed #,(rhombus-local-expand form)))

(define-for-syntax (make-expression-infix-operator name prec protocol proc assc)
  (expression-infix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx)
         (wrap-expression (check-expression-result
                           (proc (parsed-argument form1) (parsed-argument form2) stx)
                           proc)))
       (lambda (form1 tail)
         (define-values (form new-tail)
           (call-with-values
            (lambda () (syntax-parse tail
                         [(head . tail) (proc (parsed-argument form1) (pack-tail #'tail #:after #'head) #'head)]))
            (case-lambda
              [(form new-tail) (values form new-tail)]
              [(form) (values form #'(group))])))
         (check-transformer-result (wrap-expression (check-expression-result form proc))
                                   (unpack-tail new-tail proc #f)
                                   proc)))
   assc))

(define-for-syntax (make-expression-prefix-operator name prec protocol proc)
  (expression-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form stx)
         (wrap-expression (check-expression-result
                           (proc #`(parsed #,form) stx)
                           proc)))
       (lambda (tail)
         (define-values (form new-tail)
           (call-with-values
            (lambda () (syntax-parse tail
                         [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)]))
            (case-lambda
              [(form new-tail) (values form new-tail)]
              [(form) (values form #'(group))])))
         (check-transformer-result (wrap-expression (check-expression-result form proc))
                                   (unpack-tail new-tail proc #f)
                                   proc)))))
