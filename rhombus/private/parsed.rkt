#lang racket/base
(require (for-syntax racket/base)
         "parse.rkt")

(provide (for-syntax parsed
                     unparsed))

(begin-for-syntax
  (define (parsed v)
    #`(parsed #,v))

  (define (unparsed d)
    #`(rhombus-expression (group #,d))))
