#lang racket/base
(require (for-syntax racket/base
                     "introducer.rkt"
                     (for-syntax racket/base)))

(provide (for-syntax in-name-root-space
                     name-root-quote
                     out-of-name-root-space))

(define-for-syntax in-name-root-space
  (make-interned-syntax-introducer/add 'rhombus/namespace))
(begin-for-syntax
  (define-syntax (name-root-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/namespace) #'id))])))

(define-for-syntax out-of-name-root-space
  (let ([intro (make-interned-syntax-introducer 'rhombus/namespace)])
    (lambda (id) (intro id 'remove))))

