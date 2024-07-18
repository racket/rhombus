#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "parens.rkt"
         "parse.rkt"
         (submod "ellipsis.rkt" for-parse)
         "op-literal.rkt"
         "repetition.rkt")

(provide all
         any)

(define-for-syntax (combiner comb for/comb)
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (_::parens g ...) . tail)
        (values
         (let loop ([gs #'(g ...)])
           (define (combine e gs)
             (if (null? (syntax-e gs))
                 e
                 #`(#,comb #,e #,(loop gs))))
           (syntax-parse gs
             #:datum-literals (group)
             [(g (group _::...-expr) . pre-tail)
              (define-values (tail count) (consume-extra-ellipses #'pre-tail))
              (syntax-parse #'g
                [repet::repetition
                 (combine (render-repetition for/comb #'repet.parsed #:depth (add1 count))
                          tail)])]
             [(e::expression . tail)
              (combine #'e.parsed #'tail)]))
         #'tail)]))))

(define-syntax all (combiner #'and #'for/and))
(define-syntax any (combiner #'or #'for/or))
