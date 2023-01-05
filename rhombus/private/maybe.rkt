#lang racket
(require (for-syntax racket/base
                     syntax/parse)
         (submod "annotation.rkt" for-class))

(provide Maybe)

(define-syntax Maybe
  (annotation-prefix-operator
   #'Maybe
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens)
       [(form-id ((~and tag parens) g) . tail)
        #:with ann::annotation #'g
        #:with ann-info::annotation-form #'ann.parsed
        (values
         (annotation-form #`(let ([pred ann-info.predicate])
                              (lambda (v)
                                (or (not v)
                                    (pred v))))
                          #`())
         #'tail)]))))
