#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "pack.rkt")
         "parse.rkt"
         "expression.rkt"
         "simple-call.rkt")

(provide (for-syntax build-info-syntax-call))

(begin-for-syntax
  (define (build-info-syntax-call who e . args)
    (define t (unpack-term e #f #f))
    (if (and (identifier? t)
             (not (syntax-local-value* t expression-prefix-operator-ref)))
        #`(#,t #,@args)
        #`(rhombus-expression
           (#,@(unpack-group e who #f)
            (#,(add-call-context #'parens)
             #,@(for/list ([arg (in-list args)])
                  #`(group (parsed #:rhombus/expr #,arg)))))))))
