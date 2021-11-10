#lang racket/base
(require (for-syntax racket/base))

(provide (for-syntax parsed))

(begin-for-syntax
  (define (parsed v)
    #`(parsed #,v)))
