#lang racket/base
(require "srcloc.rkt")

(provide extract-elem-from-literal)

(define (extract-elem-from-literal form-id lit-stx
                                   pred len ref
                                   elem-type lit-type)
  (define lit (syntax-e lit-stx))
  (unless (and (pred lit)
               (eqv? (len lit) 1))
    (raise-syntax-error #f
                        (string-append "expected a literal single-" elem-type " " lit-type)
                        (respan (datum->syntax #f (list form-id lit-stx)))
                        lit-stx))
  (ref lit 0))
