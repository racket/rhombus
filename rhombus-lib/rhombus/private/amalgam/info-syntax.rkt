#lang racket/base
(require (for-syntax "group.rkt"))
(require (for-syntax racket/base
                     enforest/syntax-local
                     "pack.rkt")
         "parse.rkt"
         "expression.rkt"
         "simple-call.rkt"
         "static-info.rkt")

(provide (for-syntax build-info-syntax-call))

(begin-for-syntax
  (define (build-info-syntax-call who e . args)
    (define t (unpack-term e #f #f))
    (if (and (identifier? t)
             (not (syntax-local-value* t expression-prefix-operator-ref)))
        #`(#,t #,@(map discard-static-infos args))
        #`(rhombus-expression
           (#,@(unpack-group e who #f)
            (#,(add-call-context #'parens)
             #,@(for/list ([arg (in-list args)])
                  (regroup #`((parsed #:rhombus/expr #,arg))))))))))
