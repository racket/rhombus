#lang racket/base

(provide in-expression-space
         out-of-expression-space)

(define in-expression-space (lambda (x) x))
(define out-of-expression-space (lambda (x) x))
